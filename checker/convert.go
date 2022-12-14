package checker

import "fmt"

// convertType converts src to dst and returns the map of any bound type parameters.
// The second return is a non-nil note on error or nil on success.
// We use a note here instead of an Error to allow the src type to lack a location;
// Errors must have a location, but notes needn't.
func convertType(src, dst TypePattern, mode convertMode) (map[*TypeParm]Type, note) {
	var bind map[*TypeParm]Type
	pat, cvt, n := convertPattern(nil, src, dst, mode, &bind)
	if !pat.isGroundType() {
		cvt = nil
		n = &ConvertError{
			Src:      src,
			Dst:      dst,
			Cause:    newNote("cannot infer type %s", pat),
			Explicit: mode == explicit,
		}
	}
	if cvt == nil {
		n1 := newNote("cannot convert %s to %s", src, dst)
		n1.setLoc(src)
		n1.add(n)
		return nil, n1
	}
	return bind, nil
}

// convertExpr returns the expression resulting from converting expr to the given type pattern.
func convertExpr(expr Expr, dst TypePattern, mode convertMode) (Expr, map[*TypeParm]Type, Error) {
	// TODO: expr should not be nil.
	if expr == nil || expr.Type() == nil {
		return expr, nil, nil
	}
	var bind map[*TypeParm]Type
	var n note
	pat, cvt, n := convertPattern(nil, pattern(expr.Type()), dst, mode, &bind)
	if !pat.isGroundType() {
		cvt = nil
		n = newNote("cannot infer type %s", pat)
	}
	if cvt == nil {
		goto fail
	}
	if src, ok := expr.(*Convert); ok && mode != explicit && src.Explicit && !eqType(src.Type(), cvt.Type()) {
		// If the src expression is an explicit convert,
		// and this is an implicit convert,
		// it is an error if the types are not equal.
		n = newNote("cannot implicitly convert an explicit conversion")
		goto fail
	}
	for p := cvt; p != nil; p, _ = p.Expr.(*Convert) {
		p.L = expr.Loc()
		p.Explicit = mode == explicit
	}
	return doConvertExpr(expr, cvt), bind, nil
fail:
	err := newError(expr, "cannot convert %s (%s) to %s", expr, expr.Type(), dst)
	err.add(n)
	return expr, nil, err
}

// doConvertExpr returns an Expr that is expr converted as-per cvt,
// which is a chain of *Convert nodes as returned by convert().
// The returned Expression may use the cvt *Convert nodes.
func doConvertExpr(expr Expr, cvt *Convert) Expr {
	if cvt == nil {
		return expr
	}
	next, _ := cvt.Expr.(*Convert)
	cvt.Expr = doConvertExpr(expr, next)
	switch {
	case cvt.Kind == Ref:
		// Ref conversions that convert an identifier
		// result in the reference to the identifier's variable.
		if id := identifierRef(cvt.Expr); id != nil {
			if cvt.Explicit {
				return &Convert{
					Kind:     Noop,
					Explicit: true,
					Expr:     id,
					T:        id.Type(),
					L:        id.Loc(),
				}
			}
			return id
		}
		// Ref conversions that convert a call
		// to a reference-returning function
		// result in the returned reference.
		if call := callRef(cvt.Expr); call != nil {
			if cvt.Explicit {
				return &Convert{
					Kind:     Noop,
					Explicit: true,
					Expr:     call,
					T:        call.Type(),
					L:        call.Loc(),
				}
			}
			return call
		}
	case cvt.Kind == funcConvert:
		dstFunc := cvt.T.(*FuncType)
		if blk, ok := cvt.Expr.(*BlockLit); ok {
			// If it is a block literal; just set its return type.
			//
			// Note that this takes advantage of the fact
			// that currently the only function conversions
			// convert the return type, not the parameters.
			// And further, the only return types converted to
			// are compatible with simply setting the blk.Ret.
			blk.Ret = dstFunc.Ret
			blk.Func = dstFunc
			blk.T = dstFunc
			return blk
		}
		capDef := &BlockCap{
			Name: "srcFunc",
			T:    refLiteral(expr.Type()),
			L:    expr.Loc(),
			Expr: cvt.Expr,
		}
		fun := &ExprFunc{
			Expr:     deref(&Cap{Def: capDef, T: capDef.T, L: cvt.Expr.Loc()}),
			FuncType: expr.Type().(*FuncType),
		}
		blk := wrapCallInBlock(fun, dstFunc.Ret, cvt.Expr.Loc())
		blk.Caps = []*BlockCap{capDef}
		return blk
	case cvt.Kind == Noop && !cvt.Explicit && eqType(cvt.Expr.Type(), cvt.Type()):
		// Pop-off meaningless noop conversions.
		return cvt.Expr
	}
	return cvt
}

// identifierRef returns the reference to an identifier
// if Expr is a Var, Local, Parm, or Cap deref;
// otherwise nil.
func identifierRef(expr Expr) Expr {
	deref, ok := expr.(*Convert)
	if !ok || deref.Kind != Deref {
		return nil
	}
	switch deref.Expr.(type) {
	case *Var, *Local, *Parm, *Cap:
		return deref.Expr
	}
	return nil
}

// callRef returns the reference result of a call
// to a reference-returning function
// if Expr is a deref of a deref of a call
// to a reference-returning function;
// otherwise nil.
func callRef(expr Expr) Expr {
	// A call to a ref-returning function has two Derefs:
	// the outer Deref derefs the returned reference,
	// and the inner Deref derefs the hidden return variable.
	//
	// Our goal is to check the inner deref's expr for a call
	// to a reference-returning function,
	// and if we find one, return the outer deref's expr.
	outerDeref, ok := expr.(*Convert)
	if !ok || outerDeref.Kind != Deref {
		return nil
	}
	innerDeref, ok := outerDeref.Expr.(*Convert)
	if !ok || innerDeref.Kind != Deref {
		return nil
	}
	call, ok := innerDeref.Expr.(*Call)
	if !ok || !isRefLiteral(call.Func.ret().Type) {
		return nil
	}
	return outerDeref.Expr
}

type convertMode int

const (
	explicit convertMode = iota
	implicit
)

func (m convertMode) String() string {
	switch m {
	case implicit:
		return "implicit"
	case explicit:
		return "explicit"
	default:
		panic(fmt.Sprintf("bad mode: %d", m))
	}
}

// convertPattern returns the resulting TypePattern and a chain of *Convert nodes
// giving the steps to convert from items of the src pattern to the dst pattern.
//
// If src cannot convert to dst, the returned pattern is the zero value and *Convert is nil;
// the returned notes slice may be non-empty if there is extra information
// to explain why the conversion is not possible.
//
// On success:
// The returned pattern may be dst itself (same .parms slice and .typ).
// The returned Convert nodes have only the kind, T, and Expr fields set.
// The returned Convert.Type() and TypePattern.typ are equal (eqType).
//
// The bind parameter is a pointer to a map from type parameters to their bound types.
// On input, the pointed-to map may be a nil map (the pointer itself must not be nil).
// In this case, convert will lazily allocate a new map if needed.
// If the conversion results in any parameter bindings they are added to *bind.
func convertPattern(cvt *Convert, src, dst TypePattern, mode convertMode, bind *map[*TypeParm]Type) (TypePattern, *Convert, *ConvertError) {
	isect, err := intersection(src, dst, bind)
	if isect != nil {
		return *isect, conversion(cvt, Noop, isect.Type), nil
	}
	switch {
	default:
		// If nothing below matched, return the note from the intersection error.
		return TypePattern{}, nil, &ConvertError{
			Src:      src,
			Dst:      dst,
			Cause:    err,
			Explicit: mode == explicit,
		}

	case isEmptyStruct(dst.Type):
		return dst, conversion(cvt, Drop, _empty), nil

	case isEnd(src.Type):
		return dst, conversion(cvt, Noop, _empty), nil

	case mode == explicit && isRefLiteral(src.Type) && isUintRef(dst.Type):
		return dst, conversion(cvt, NumConvert, dst.Type), nil

	case mode == explicit && isBasicNum(src.Type) && isBasicNum(dst.Type) && !isUintRef(dst.Type):
		return dst, conversion(cvt, NumConvert, dst.Type), nil

	case mode == explicit && isByteArray(src.Type) && isStringType(dst.Type):
		return dst, conversion(cvt, StrConvert, dst.Type), nil

	case (mode == explicit || isLiteralType(dst.Type)) && isVisibleDefinedType(src.Type):
		cvt = conversion(cvt, Noop, src.instType().Type)
		pat, cvt, err := convertPattern(cvt, src.instType(), dst, mode, bind)
		if err != nil {
			return TypePattern{}, nil, &ConvertError{
				Src:      src,
				Dst:      dst,
				DefType:  src.Type.(*DefType),
				Cause:    err,
				Explicit: mode == explicit,
			}
		}
		return pat, cvt, err

	case (mode == explicit || isLiteralType(src.Type)) && isVisibleDefinedType(dst.Type):
		pat, cvt, err := convertPattern(cvt, src, dst.instType(), mode, bind)
		if err != nil {
			return TypePattern{}, nil, &ConvertError{
				Src:      src,
				Dst:      dst,
				DefType:  dst.Type.(*DefType),
				Cause:    err,
				Explicit: mode == explicit,
			}
		}
		pat.Type = subType(*bind, dst.Type)
		return pat, conversion(cvt, Noop, pat.Type), nil

	case mode == explicit && isUnionSubset(src.Type, dst.Type):
		return dst, conversion(cvt, UnionConvert, dst.Type), nil

	case isImplicitFuncConvertible(src.Type, dst.Type):
		return dst, conversion(cvt, funcConvert, dst.Type), nil

	case isRefLiteral(src.Type):
		cvt = conversion(cvt, Deref, src.refElem().Type)
		pat, cvt, err := convertPattern(cvt, src.refElem(), dst, mode, bind)
		if err != nil {
			return pat, cvt, &ConvertError{
				Src:      src,
				Dst:      dst,
				Cause:    err,
				Explicit: mode == explicit,
			}
		}
		return pat, cvt, err

	case isRefLiteral(dst.Type):
		pat, cvt, err := convertPattern(cvt, src, dst.refElem(), mode, bind)
		if err != nil {
			return TypePattern{}, nil, &ConvertError{
				Src:      src,
				Dst:      dst,
				Cause:    err,
				Explicit: mode == explicit,
			}
		}
		pat.Type = &RefType{Type: pat.Type, L: pat.Loc()}
		return pat, conversion(cvt, Ref, pat.Type), nil
	}
}

func isUnionSubset(src, dst Type) bool {
	srcUnion, ok := src.(*UnionType)
	if !ok {
		return false
	}
	dstUnion, ok := dst.(*UnionType)
	if !ok || len(srcUnion.Cases) > len(dstUnion.Cases) {
		return false
	}
	cases := make(map[string]*CaseDef)
	for i, c := range dstUnion.Cases {
		cases[c.Name] = &dstUnion.Cases[i]
	}
	for _, srcCase := range srcUnion.Cases {
		dstCase, ok := cases[srcCase.Name]
		if !ok || (srcCase.Type == nil) != (dstCase.Type == nil) ||
			(srcCase.Type != nil && !eqType(srcCase.Type, dstCase.Type)) {
			return false
		}
	}
	return true
}

func isImplicitFuncConvertible(src, dst Type) bool {
	srcFunc, ok := src.(*FuncType)
	if !ok {
		return false
	}
	dstFunc, ok := dst.(*FuncType)
	if !ok || len(dstFunc.Parms) != len(srcFunc.Parms) {
		return false
	}
	for i := range srcFunc.Parms {
		if !eqType(srcFunc.Parms[i], dstFunc.Parms[i]) {
			return false
		}
	}
	return isEmptyStruct(dstFunc.Ret) || isEnd(srcFunc.Ret)
}

func conversion(cvt *Convert, kind ConvertKind, typ Type) *Convert {
	switch {
	case cvt == nil:
		return &Convert{Kind: kind, Expr: nil, T: typ}
	case kind == Ref && cvt.Kind == Deref:
		// Pop-off the implicit Deref.
		cvt, _ = cvt.Expr.(*Convert) // allow nil
		return conversion(cvt, Noop, typ)
	case cvt.Kind == funcConvert:
		// Never drop an incoming funcConvert for a Noop,
		// because doConvert expects the Noop's T to be a *FuncType.
		return &Convert{Kind: kind, Expr: cvt, T: typ}
	case kind == Noop:
		// Drop this Noop and change the type of the incoming Convert.
		cvt.T = typ
		return cvt
	case cvt.Kind == Noop:
		// Just reuse the incoming Noop Convert.
		cvt.Kind = kind
		cvt.T = typ
		return cvt
	default:
		return &Convert{Kind: kind, Expr: cvt, T: typ}
	}
}
