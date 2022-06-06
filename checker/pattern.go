package checker

import (
	"fmt"
	"strconv"

	"github.com/eaburns/pea/loc"
)

// typePattern is a type pattern.
type typePattern struct {
	parms []*TypeParm
	typ   Type
}

// any returns a new typePattern with a single, bound type variable.
func any() typePattern {
	n := "_"
	p := &TypeParm{Name: n}
	v := &TypeVar{Name: n, Def: p}
	return typePattern{parms: []*TypeParm{p}, typ: v}
}

func (pat typePattern) isAny() bool {
	typ, ok := pat.typ.(*TypeVar)
	return ok && pat.bound(typ)
}

// pattern returns the type pattern for a ground type.
// pattern panics if typ is nil.
func pattern(typ Type) typePattern {
	if typ == nil {
		panic("impossible")
	}
	return typePattern{typ: typ}
}

// patternOrAny returns the pattern for a ground type, or if typ is nil, any().
func patternOrAny(typ Type) typePattern {
	if typ == nil {
		return any()
	}
	return pattern(typ)
}

func (pat typePattern) withType(typ Type) typePattern {
	if typ == nil {
		panic("impossible")
	}
	pat.typ = typ
	return pat
}

// instType returns the type in the definition of a defined type type pattern,
// substituted with the type arguments of the defined type.
// If pat's type is the built-in Bool, the returned pattern is for boolUnion.
// instType panics if pat is not a defined type type pattern or bool.
func (pat typePattern) instType() typePattern {
	if isBool(pat.typ) {
		return pat.withType(boolUnion)
	}
	return pat.withType(pat.typ.(*DefType).Inst.Type)
}

// typeArg returns the type argument type of a defined type type pattern;
// panics if pat is not a defined type type pattern.
func (pat typePattern) typeArg(i int) typePattern {
	return pat.withType(pat.typ.(*DefType).Args[i])
}

// refElem returns the element type of a literal reference type pattern;
// panics if pat is not a literal reference type pattern.
func (pat typePattern) refElem() typePattern {
	return pat.withType(pat.typ.(*RefType).Type)
}

// arrayElem returns the array element type of a literal array type pattern;
// panics if pat is not a literal array type pattern.
func (pat typePattern) arrayElem() typePattern {
	return pat.withType(pat.typ.(*ArrayType).ElemType)
}

// field returns the ith struct field type of a literal struct type pattern;
// panics if pat is not a literal struct type pattern.
func (pat typePattern) field(i int) typePattern {
	return pat.withType(pat.typ.(*StructType).Fields[i].Type)
}

// Case returns the ith union case type of a literal union type pattern;
// panics if pat is not a literal union type pattern.
// panics if the ith field is untyped.
func (pat typePattern) Case(i int) typePattern {
	return pat.withType(pat.typ.(*UnionType).Cases[i].Type)
}

// parm returns the ith function parm type of a literal function type pattern;
// panics if pat is not a literal function type pattern.
func (pat typePattern) parm(i int) typePattern {
	return pat.withType(pat.typ.(*FuncType).Parms[i])
}

// ret returns the return type of a literal function type pattern;
// panics if pat is not a literal function type pattern.
func (pat typePattern) ret() typePattern {
	return pat.withType(pat.typ.(*FuncType).Ret)
}

// groundType returns the ground type of the type pattern;
// it panics if either pat.typ is nil or not grounded.
func (pat typePattern) groundType() Type {
	if pat.typ == nil {
		panic("type is nil")
	}
	if !pat.isGroundType() {
		panic(fmt.Sprintf("not grounded: %s", pat))
	}
	return pat.typ
}

// isGroundType returns whether the type pattern is a ground type;
// whether its type has no referenced type parameters.
func (pat typePattern) isGroundType() bool {
	var isGround func(Type) bool
	isGround = func(t Type) bool {
		switch t := t.(type) {
		case *DefType:
			for i := range t.Args {
				if !isGround(t.Args[i]) {
					return false
				}
			}
		case *RefType:
			return isGround(t.Type)
		case *ArrayType:
			return isGround(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				if !isGround(t.Fields[i].Type) {
					return false
				}
			}
		case *UnionType:
			for i := range t.Cases {
				if !isGround(t.Cases[i].Type) {
					return false
				}
			}
		case *FuncType:
			for i := range t.Parms {
				if !isGround(t.Parms[i]) {
					return false
				}
			}
			return isGround(t.Ret)
		case *TypeVar:
			if pat.bound(t) {
				return false
			}
		case *BasicType:
		case nil:
		default:
			panic(fmt.Sprintf("bad type type: %T", t))
		}
		return true
	}
	return isGround(pat.typ)
}

// bound returns whether the type variable is bound to a type parameter of the typePattern.
func (pat *typePattern) bound(v *TypeVar) bool {
	for _, parm := range pat.parms {
		if v.Def == parm {
			return true
		}
	}
	return false
}

func (pat typePattern) String() string {
	c := copyTypeWithLoc(pat.typ, loc.Loc{})

	refs := make(map[*TypeParm]int)
	var countBoundVarRefs func(Type)
	countBoundVarRefs = func(t Type) {
		switch t := t.(type) {
		case *DefType:
			for i := range t.Args {
				countBoundVarRefs(t.Args[i])
			}
		case *RefType:
			countBoundVarRefs(t.Type)
		case *ArrayType:
			countBoundVarRefs(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				countBoundVarRefs(t.Fields[i].Type)
			}
		case *UnionType:
			for i := range t.Cases {
				countBoundVarRefs(t.Cases[i].Type)
			}
		case *FuncType:
			for i := range t.Parms {
				countBoundVarRefs(t.Parms[i])
			}
			countBoundVarRefs(t.Ret)
		case *TypeVar:
			if pat.bound(t) {
				refs[t.Def] = refs[t.Def] + 1
			}
		case *BasicType:
		case nil:
		default:
			panic(fmt.Sprintf("bad type type: %T", t))
		}
	}
	countBoundVarRefs(c)

	i := 0
	name := make(map[*TypeParm]string)
	var renameBoundVars func(Type)
	renameBoundVars = func(t Type) {
		switch t := t.(type) {
		case *DefType:
			for i := range t.Args {
				renameBoundVars(t.Args[i])
			}
		case *RefType:
			renameBoundVars(t.Type)
		case *ArrayType:
			renameBoundVars(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				renameBoundVars(t.Fields[i].Type)
			}
		case *UnionType:
			for i := range t.Cases {
				renameBoundVars(t.Cases[i].Type)
			}
		case *FuncType:
			for i := range t.Parms {
				renameBoundVars(t.Parms[i])
			}
			renameBoundVars(t.Ret)
		case *TypeVar:
			if !pat.bound(t) {
				return
			}
			if _, ok := name[t.Def]; !ok {
				n := "_"
				if refs[t.Def] > 1 {
					n += strconv.Itoa(i)
					i++
				}
				name[t.Def] = n
			}
			t.Name = name[t.Def]
		case *BasicType:
		case nil:
		default:
			panic(fmt.Sprintf("bad type type: %T", t))
		}
	}
	renameBoundVars(c)
	return c.String()
}

// common returns a new typePattern that is the most specific simple common pattern of pats.
func common(pats ...typePattern) typePattern {
	switch len(pats) {
	case 0:
		return any()
	case 1:
		return pats[0]
	}

	var nextParm int
	var pat typePattern

	var newVar func() *TypeVar
	newVar = func() *TypeVar {
		n := "_" + strconv.Itoa(nextParm)
		nextParm++
		p := &TypeParm{Name: n}
		pat.parms = append(pat.parms, p)
		return &TypeVar{Name: n, Def: p}
	}

	var buildType func(types []Type) Type
	buildType = func(types []Type) Type {
		switch t0 := types[0].(type) {
		case *DefType:
			for _, t := range types {
				if t, ok := t.(*DefType); !ok || t0.Def != t.Def {
					return newVar()
				}
			}
			d := &DefType{
				Name: t0.Name,
				Args: make([]Type, len(t0.Args)),
				Def:  t0.Def,
			}
			ts := make([]Type, len(types))
			for i := range t0.Args {
				for j, t := range types {
					ts[j] = t.(*DefType).Args[i]
				}
				d.Args[i] = buildType(ts)
			}
			return instType(d)
		case *RefType:
			for _, t := range types {
				if _, ok := t.(*RefType); !ok {
					return newVar()
				}
			}
			var r RefType
			ts := make([]Type, len(types))
			for i, t := range types {
				ts[i] = t.(*RefType).Type
			}
			r.Type = buildType(ts)
			return &r
		case *ArrayType:
			for _, t := range types {
				if _, ok := t.(*ArrayType); !ok {
					return newVar()
				}
			}
			var a ArrayType
			ts := make([]Type, len(types))
			for i, t := range types {
				ts[i] = t.(*ArrayType).ElemType
			}
			a.ElemType = buildType(ts)
			return &a
		case *StructType:
			for _, t := range types {
				t, ok := t.(*StructType)
				if !ok || len(t.Fields) != len(t0.Fields) {
					return newVar()
				}
				for i := range t.Fields {
					if t.Fields[i].Name != t0.Fields[i].Name {
						return newVar()
					}
				}
			}

			s := &StructType{Fields: make([]FieldDef, len(t0.Fields))}
			for i := range t0.Fields {
				s.Fields[i].Name = t0.Fields[i].Name
				ts := make([]Type, len(types))
				for j, t := range types {
					ts[j] = t.(*StructType).Fields[i].Type
				}
				s.Fields[i].Type = buildType(ts)
			}
			return s
		case *UnionType:
			for _, t := range types {
				t, ok := t.(*UnionType)
				if !ok || len(t.Cases) != len(t0.Cases) {
					return newVar()
				}
				for i := range t.Cases {
					if t.Cases[i].Name != t0.Cases[i].Name ||
						(t.Cases[i].Type == nil) != (t0.Cases[i].Type == nil) {
						return newVar()
					}
				}
			}
			u := &UnionType{Cases: make([]CaseDef, len(t0.Cases))}
			for i := range t0.Cases {
				u.Cases[i].Name = t0.Cases[i].Name
				if t0.Cases[i].Type == nil {
					u.Cases[i].Type = nil
					continue
				}
				ts := make([]Type, len(types))
				for j, t := range types {
					ts[j] = t.(*UnionType).Cases[i].Type
				}
				u.Cases[i].Type = buildType(ts)
			}
			return u
		case *FuncType:
			for _, t := range types {
				if t, ok := t.(*FuncType); !ok || len(t.Parms) != len(t0.Parms) {
					return newVar()
				}
			}
			var fun FuncType
			for i := range t0.Parms {
				ts := make([]Type, len(types))
				for j, t := range types {
					ts[j] = t.(*FuncType).Parms[i]
				}
				fun.Parms = append(fun.Parms, buildType(ts))
			}
			ts := make([]Type, len(types))
			for i, t := range types {
				ts[i] = t.(*FuncType).Ret
			}
			fun.Ret = buildType(ts)
			return &fun
		case *TypeVar:
			for i, t := range types {
				if t, ok := t.(*TypeVar); !ok || pats[i].bound(t) || t.Def != t0.Def {
					return newVar()
				}
			}
			return copyTypeWithLoc(t0, loc.Loc{})
		case *BasicType:
			for _, t := range types {
				if t, ok := t.(*BasicType); !ok || t.Kind != t0.Kind {
					return newVar()
				}
			}
			return copyTypeWithLoc(t0, loc.Loc{})
		default:
			panic(fmt.Sprintf("impossible type type %T", t0))
		}
	}

	ts := make([]Type, len(pats))
	for i, p := range pats {
		ts[i] = p.typ
	}
	pat.typ = buildType(ts)
	return pat
}

// unify returns a binding of types to type parameters of pat
// that make pat's type implicitly convertible to typ,
// or nil if there is no such binding.
//
// Note that the binding is optimistic.
// Specificially, it assumes any expression of type typ
// can be referenced, but this is not true.
// So unify can succeed with a non-nil binding,
// even if the actual expression cannot be converted
// to the pattern.
func unify(pat typePattern, typ Type) map[*TypeParm]Type {
	if v, ok := pat.typ.(*TypeVar); ok && pat.bound(v) {
		bind := make(map[*TypeParm]Type)
		bind[v.Def] = typ
		return bind
	}

	switch {
	case isLiteralType(pat.typ):
		if lit := literalType(typ); lit != nil {
			typ = lit
		}
	case isLiteralType(typ):
		if lit := literalType(pat.typ); lit != nil {
			pat.typ = lit
		}
	}

	switch typRefDepth, patRefDepth := refDepth(typ), refDepth(pat.typ); {
	case typRefDepth > patRefDepth:
		// Dereference conversion.
		for typRefDepth > patRefDepth {
			typ = typ.(*RefType).Type
			typRefDepth--
		}
	case patRefDepth == typRefDepth+1:
		// Single reference conversion.
		// The actual conversion can fail
		// for non-referencable expressions.
		pat.typ = pat.typ.(*RefType).Type
		patRefDepth--
	}

	bind := make(map[*TypeParm]Type)
	if !unifyStrict(pat, typ, bind) {
		return nil
	}
	return bind
}

// unifyStrict adds to bind bindings of types to type parameters of pat
// such that substituting the binding makes pat's type equivalent to typ,
// and returns whether the unification was successful.
// It is an error for multiple bindings to the same type variable to have differing types.
func unifyStrict(pat typePattern, typ Type, bind map[*TypeParm]Type) bool {
	switch patType := pat.typ.(type) {
	case *DefType:
		typ, ok := typ.(*DefType)
		if !ok || patType.Def != typ.Def {
			return false
		}
		for i, patArg := range patType.Args {
			if !unifyStrict(pat.withType(patArg), typ.Args[i], bind) {
				return false
			}
		}
		return true
	case *RefType:
		typ, ok := typ.(*RefType)
		if !ok {
			return false
		}
		return unifyStrict(pat.withType(patType.Type), typ.Type, bind)
	case *ArrayType:
		typ, ok := typ.(*ArrayType)
		if !ok {
			return false
		}
		return unifyStrict(pat.withType(patType.ElemType), typ.ElemType, bind)
	case *StructType:
		typ, ok := typ.(*StructType)
		if !ok || len(patType.Fields) != len(typ.Fields) {
			return false
		}
		for i := range patType.Fields {
			if patType.Fields[i].Name != typ.Fields[i].Name {
				return false
			}
			patFieldType := patType.Fields[i].Type
			typFieldType := typ.Fields[i].Type
			if !unifyStrict(pat.withType(patFieldType), typFieldType, bind) {
				return false
			}
		}
		return true
	case *UnionType:
		typ, ok := typ.(*UnionType)
		if !ok || len(patType.Cases) != len(typ.Cases) {
			return false
		}
		for i := range patType.Cases {
			patCaseType := patType.Cases[i].Type
			typCaseType := typ.Cases[i].Type
			if patType.Cases[i].Name != typ.Cases[i].Name ||
				(patCaseType == nil) != (typCaseType == nil) {
				return false
			}
			if patCaseType == nil {
				continue
			}
			if !unifyStrict(pat.withType(patCaseType), typCaseType, bind) {
				return false
			}
		}
		return true
	case *FuncType:
		typ, ok := typ.(*FuncType)
		if !ok || len(patType.Parms) != len(typ.Parms) {
			return false
		}
		for i := range patType.Parms {
			if !unifyStrict(pat.withType(patType.Parms[i]), typ.Parms[i], bind) {
				return false
			}
		}
		return unifyStrict(pat.withType(patType.Ret), typ.Ret, bind)
	case *BasicType:
		if !eqType(patType, typ) {
			return false
		}
		return true
	case *TypeVar:
		if !pat.bound(patType) {
			return eqType(patType, typ)
		}
		prev, ok := bind[patType.Def]
		if !ok {
			bind[patType.Def] = typ
			return true
		}
		return eqType(prev, typ)
	default:
		panic(fmt.Sprintf("impossible Type type: %T", pat))
	}
}

// convertType converts src to dst and returns the map of any bound type parameters.
// The second return is a non-nil note on error or nil on success.
// We use a note here instead of an Error to allow the src type to lack a location;
// Errors must have a location, but notes needn't.
func convertType(src, dst typePattern, explicit bool) (map[*TypeParm]Type, note) {
	var bind map[*TypeParm]Type
	if cvt, notes := convert(nil, src, dst, explicit, &bind); cvt == nil {
		n := newNote("cannot convert %s to %s", src, dst)
		n.setLoc(src.typ)
		n.setNotes(notes)
		return nil, n
	}
	return bind, nil
}

// convertExpr returns the expression resulting from converting expr to the given type pattern.
func convertExpr(expr Expr, dst typePattern, explicit bool) (Expr, map[*TypeParm]Type, Error) {
	if expr.Type() == nil {
		return expr, nil, nil
	}

	var bind map[*TypeParm]Type
	cvt, notes := convert(nil, pattern(expr.Type()), dst, explicit, &bind)
	if cvt == nil {
		goto fail
	}
	if src, ok := expr.(*Convert); ok && !explicit && src.Explicit && !eqType(src.Type(), cvt.Type()) {
		// If the src expression is an explicit convert,
		// and this is an implicit convert,
		// it is an error if the types are not equal.
		notes = append(notes, newNote("cannot implicitly convert an explicit conversion"))
		goto fail
	}
	// Set the L and Explicit fields;
	for p := cvt; p != nil; p, _ = p.Expr.(*Convert) {
		p.L = expr.Loc()
		p.Explicit = explicit

		if p.Kind == Ref {
			// Two cases, either p.Expr == nil or p.Expr != nil.
			// If p.Expr != nil, then it must not be a Deref,
			// since convert fixes those, so it's an error.
			// If p.Expr==nil, then we need to check expr.
			if p.Expr != nil {
				notes = append(notes, newNote("cannot reference %s", p.Expr))
				goto fail
			}
			if src, ok := expr.(*Convert); !ok || src.Kind != Deref {
				// It is currently an error for convertExpr to generate a Ref conversion.
				// TODO: implement Ref conversion, and ditch this error.
				notes = append(notes, newNote("cannot reference %s", expr))
				goto fail
			} else {
				p.Kind = Noop
				p.Expr = src.Expr
				if src.Expr == nil {
					// TODO: we should never have a nil expr
					return cvt, bind, nil
				}
				break
			}
		}
		if p.Expr == nil {
			p.Expr = expr
			break
		}
	}
	if cvt.Kind == Noop && !cvt.Explicit && eqType(cvt.Expr.Type(), cvt.Type()) {
		// If this is an implicit no-op that isn't changing the type, just pop it off.
		return cvt.Expr, bind, nil
	}
	return cvt, bind, nil
fail:
	err := newError(expr, "cannot convert %s (%s) to %s", expr, expr.Type(), dst)
	err.setNotes(notes)
	return expr, nil, err
}

// convert returns a chain of *Convert nodes giving the conversion from src to dst.
// The returned *Converts have only the kind, T, and Expr fields set.
//
// The bind parameter is a pointer to a map from type parameters to their bound types.
// The pointed-to map may be a nil map (the pointer itself must not be nil).
// If any type parameters are bound, convert will first allocate a map if the map is nil,
// and it will add the new bindings to the map.
//
// If the conversion fails, the returned notes slice may be non-empty
// if there is extra information to explain why the conversion failed.
func convert(cvt *Convert, src, dst typePattern, explicit bool, bind *map[*TypeParm]Type) (*Convert, []note) {
	isect, isectNote := intersection(src, dst, bind)
	if isect != nil {
		if !isect.isGroundType() {
			return nil, []note{newNote("cannot infer type %s", isect)}
		}
		return conversion(cvt, Noop, isect.typ), nil
	}
	switch {
	default:
		// If nothing below matched, return the note from the intersection error.
		if isectNote == nil {
			return nil, nil
		}
		return nil, []note{isectNote}

	case isEmptyStruct(dst.typ):
		return conversion(cvt, Drop, _empty), nil

	case explicit && isRefLiteral(src.typ) && isUintRef(dst.typ):
		return conversion(cvt, NumConvert, dst.typ), nil

	case explicit && isBasicNum(src.typ) && isBasicNum(dst.typ) && !isUintRef(dst.typ):
		return conversion(cvt, NumConvert, dst.typ), nil

	case explicit && isByteArray(src.typ) && isStringType(dst.typ):
		return conversion(cvt, StrConvert, dst.typ), nil

	case (explicit || isLiteralType(dst.typ)) && isDefinedType(src.typ):
		cvt = conversion(cvt, Noop, src.instType().typ)
		return convert(cvt, src.instType(), dst, explicit, bind)

	case (explicit || isLiteralType(src.typ)) && isDefinedType(dst.typ):
		cvt, notes := convert(cvt, src, dst.instType(), explicit, bind)
		if cvt == nil {
			return nil, notes
		}
		return conversion(cvt, Noop, subType(*bind, dst.typ)), nil

	// isUnionSubsetConvertible checks that both are literal unions
	// and dst is a strict superset.
	// It takes dst as the first argument and src as the second.
	// TODO: make isUnionSubsetConvertible more intuitive.
	// TODO: make union conversion need to be explicit
	case isUnionSubsetConvertible(dst.typ, src.typ):
		return conversion(cvt, UnionConvert, dst.typ), nil

	case isRefLiteral(src.typ):
		cvt = conversion(cvt, Deref, src.refElem().typ)
		return convert(cvt, src.refElem(), dst, explicit, bind)

	case isRefLiteral(dst.typ):
		cvt, notes := convert(cvt, src, dst.refElem(), explicit, bind)
		if cvt == nil {
			return nil, notes
		}
		return conversion(cvt, Ref, subType(*bind, dst.typ)), nil
	}
}

func conversion(cvt *Convert, kind ConvertKind, typ Type) *Convert {
	switch {
	case cvt == nil:
		return &Convert{Kind: kind, Expr: nil, T: typ}
	case kind == Ref && cvt.Kind == Deref:
		// Pop-off the implicit Deref.
		cvt, _ = cvt.Expr.(*Convert) // allow nil
		return conversion(cvt, Noop, typ)
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

// intersection returns the intersection of two type patterns or nil if there intersection is empty.
//
// The bind parameter is a pointer to a map from type parameters to their bound types.
// The pointed-to map may be a nil map (the pointer itself must not be nil).
// If any type parameters are bound, intersection will first allocate a map if the map is nil,
// and it will add the new bindings to the map.
//
// If the intersection is empty returned notes may be non-nil
// if there is a note to give more information as to why.
func intersection(a, b typePattern, bind *map[*TypeParm]Type) (*typePattern, note) {
	if len(a.parms) == 0 && len(b.parms) == 0 {
		// Fast-path the common case of checking two types.
		if !eqType(a.typ, b.typ) {
			return nil, nil
		}
		return &a, nil
	}

	// Make a set for each type parameter with >0 bound variables
	// and unions the sets of type parameters that would bind with each other.
	var sets disjointSets
	findBoundVars(&sets, a)
	findBoundVars(&sets, b)
	if !unionSets(&sets, a, b) {
		return nil, nil
	}

	// Find the unique binding for each type parameter set.
	// If there are multiple bindings, it's an error (note).
	if note := bindSets(&sets, a, b); note != nil {
		return nil, note
	}
	var parms []*TypeParm
	for _, parm := range sets.setSlice {
		set := sets.find(parm)
		if set.bind == nil {
			parm := &TypeParm{Name: fmt.Sprintf("_%d", len(parms))}
			parms = append(parms, parm)
			set.bind = &TypeVar{Name: parm.Name, Def: parm}
		}
	}

	// Substitute type parameters in each binding recursively.
	// If the substitution would create infinite recursion, it's an error (note).
	// Also establish a mapping for each type parameter in the input types
	// to their ultimate substitution.
	if *bind == nil {
		*bind = make(map[*TypeParm]Type)
	}
	for _, parm := range sets.setSlice {
		s := sets.find(parm)
		if !s.substituted {
			sub, note := subSets(&sets, make(map[*set]bool), nil, s.bind)
			if sub == nil {
				return nil, note
			}
			s.bind = sub
			s.substituted = true
		}
		(*bind)[parm] = s.bind
	}
	isect := &typePattern{parms: parms, typ: subType(*bind, a.typ)}
	return isect, nil
}

func findBoundVars(sets *disjointSets, pat typePattern) {
	switch typ := pat.typ.(type) {
	case *DefType:
		for i := range typ.Args {
			findBoundVars(sets, pat.typeArg(i))
		}
	case *RefType:
		findBoundVars(sets, pat.refElem())
	case *ArrayType:
		findBoundVars(sets, pat.arrayElem())
	case *StructType:
		for i := range typ.Fields {
			findBoundVars(sets, pat.field(i))
		}
	case *UnionType:
		for i := range typ.Cases {
			if typ.Cases[i].Type == nil {
				continue
			}
			findBoundVars(sets, pat.Case(i))
		}
	case *FuncType:
		for i := range typ.Parms {
			findBoundVars(sets, pat.parm(i))
		}
		findBoundVars(sets, pat.ret())
	case *BasicType:
	case *TypeVar:
		if pat.bound(typ) {
			sets.find(typ.Def)
		}
	default:
		panic(fmt.Sprintf("impossible Type type: %T", typ))
	}
}

func unionSets(sets *disjointSets, a, b typePattern) bool {
	aVar, aOk := a.typ.(*TypeVar)
	bVar, bOk := b.typ.(*TypeVar)
	switch {
	case aOk && bOk && a.bound(aVar) && b.bound(bVar):
		sets.union(aVar.Def, bVar.Def)
		return true
	case aOk && a.bound(aVar):
		return true
	case bOk && b.bound(bVar):
		return true
	}
	switch bType := b.typ.(type) {
	case *DefType:
		aType, ok := a.typ.(*DefType)
		if !ok || aType.Def != bType.Def {
			return false
		}
		for i := range bType.Args {
			if !unionSets(sets, a.typeArg(i), b.typeArg(i)) {
				return false
			}
		}
		return true

	case *RefType:
		_, ok := a.typ.(*RefType)
		return ok && unionSets(sets, a.refElem(), b.refElem())

	case *ArrayType:
		_, ok := a.typ.(*ArrayType)
		return ok && unionSets(sets, a.arrayElem(), b.arrayElem())

	case *StructType:
		aType, ok := a.typ.(*StructType)
		if !ok || len(aType.Fields) != len(bType.Fields) {
			return false
		}
		for i := range aType.Fields {
			if aType.Fields[i].Name != bType.Fields[i].Name ||
				!unionSets(sets, a.field(i), b.field(i)) {
				return false
			}
		}
		return true

	case *UnionType:
		aType, ok := a.typ.(*UnionType)
		if !ok || len(aType.Cases) != len(bType.Cases) {
			return false
		}
		for i := range aType.Cases {
			aCase := &aType.Cases[i]
			bCase := &bType.Cases[i]
			if aCase.Name != bCase.Name ||
				(aCase.Type == nil) != (bCase.Type == nil) ||
				(aCase.Type != nil && !unionSets(sets, a.Case(i), b.Case(i))) {
				return false
			}
		}
		return true

	case *FuncType:
		aType, ok := a.typ.(*FuncType)
		if !ok || len(aType.Parms) != len(bType.Parms) {
			return false
		}
		for i := range bType.Parms {
			if !unionSets(sets, a.parm(i), b.parm(i)) {
				return false
			}
		}
		return unionSets(sets, a.ret(), b.ret())

	case *BasicType:
		return eqType(a.typ, bType)

	case *TypeVar:
		return eqType(a.typ, bType)

	default:
		panic(fmt.Sprintf("impossible Type type: %T", b))
	}
}

// bindSets adds bindings for the type of each set.
// It panics if the types don't align (since it's assumed to be called after findSets).
func bindSets(sets *disjointSets, a, b typePattern) note {
	aVar, aOk := a.typ.(*TypeVar)
	bVar, bOk := b.typ.(*TypeVar)
	switch {
	case aOk && bOk && a.bound(aVar) && b.bound(bVar):
		return nil
	case aOk && a.bound(aVar):
		set := sets.find(aVar.Def)
		if set.bind == nil {
			set.bind = b.typ
			return nil
		}
		if !eqType(set.bind, b.typ) {
			return newNote("%s binds %s and %s", aVar, set.bind, b.typ)
		}
		return nil
	case bOk && b.bound(bVar):
		set := sets.find(bVar.Def)
		if set.bind == nil {
			set.bind = a.typ
			return nil
		}
		if !eqType(set.bind, a.typ) {
			return newNote("%s binds %s and %s", bVar, set.bind, a.typ)
		}
		return nil
	}
	switch bType := b.typ.(type) {
	case *DefType:
		for i := range bType.Args {
			if note := bindSets(sets, a.typeArg(i), b.typeArg(i)); note != nil {
				return note
			}
		}
		return nil

	case *RefType:
		return bindSets(sets, a.refElem(), b.refElem())

	case *ArrayType:
		return bindSets(sets, a.arrayElem(), b.arrayElem())

	case *StructType:
		for i := range bType.Fields {
			if note := bindSets(sets, a.field(i), b.field(i)); note != nil {
				return note
			}
		}
		return nil

	case *UnionType:
		for i := range bType.Cases {
			if bType.Cases[i].Type == nil {
				continue
			}
			if note := bindSets(sets, a.Case(i), b.Case(i)); note != nil {
				return note
			}
		}
		return nil

	case *FuncType:
		for i := range bType.Parms {
			if note := bindSets(sets, a.parm(i), b.parm(i)); note != nil {
				return note
			}
		}
		return bindSets(sets, a.ret(), b.ret())

	case *BasicType:
		return nil

	case *TypeVar:
		return nil

	default:
		panic(fmt.Sprintf("impossible Type type: %T", b))
	}
}

func subSets(sets *disjointSets, onPath map[*set]bool, path []*TypeParm, typ Type) (Type, note) {
	switch typ := typ.(type) {
	case *RefType:
		copy := *typ
		elem, note := subSets(sets, onPath, path, typ.Type)
		if elem == nil {
			return nil, note
		}
		copy.Type = elem
		return &copy, nil
	case *DefType:
		copy := *typ
		copy.Args = nil
		for _, arg := range typ.Args {
			argCopy, note := subSets(sets, onPath, path, arg)
			if argCopy == nil {
				return nil, note
			}
			copy.Args = append(copy.Args, argCopy)
		}
		return instType(&copy), nil
	case *ArrayType:
		copy := *typ
		elem, note := subSets(sets, onPath, path, typ.ElemType)
		if elem == nil {
			return nil, note
		}
		copy.ElemType = elem
		return &copy, nil
	case *StructType:
		copy := *typ
		copy.Fields = nil
		for i := range typ.Fields {
			f := typ.Fields[i]
			fieldCopy, note := subSets(sets, onPath, path, f.Type)
			if fieldCopy == nil {
				return nil, note
			}
			f.Type = fieldCopy
			copy.Fields = append(copy.Fields, f)
		}
		return &copy, nil
	case *UnionType:
		copy := *typ
		copy.Cases = nil
		for i := range typ.Cases {
			c := typ.Cases[i]
			if c.Type != nil {
				typeCopy, note := subSets(sets, onPath, path, c.Type)
				if typeCopy == nil {
					return nil, note
				}
				c.Type = typeCopy
			}
			copy.Cases = append(copy.Cases, c)
		}
		return &copy, nil
	case *FuncType:
		copy := *typ
		copy.Parms = nil
		for _, p := range typ.Parms {
			parmCopy, note := subSets(sets, onPath, path, p)
			if parmCopy == nil {
				return nil, note
			}
			copy.Parms = append(copy.Parms, parmCopy)
		}
		retCopy, note := subSets(sets, onPath, path, typ.Ret)
		if retCopy == nil {
			return nil, note
		}
		copy.Ret = retCopy
		return &copy, nil
	case *TypeVar:
		if _, ok := sets.setMap[typ.Def]; !ok {
			// Not a bound type variable in any pattern. Just copy it.
			copy := *typ
			return &copy, nil
		}
		set := sets.find(typ.Def)
		if eqType(set.bind, typ) {
			// Self bindings are not an error.
			copy := *typ
			return &copy, nil
		}
		if onPath[set] {
			var pathStr string
			for i, elem := range path {
				if i > 0 {
					pathStr += " -> "
				}
				pathStr += elem.Name
			}
			pathStr += " = " + path[0].Name
			return nil, newNote("recursive intersection: %s", pathStr)
		}
		if set.substituted {
			// set.bind was already successfully substituted.
			// Don't re-substitute it. Just return it.
			return set.bind, nil
		}
		onPath[set] = true
		defer func() { onPath[set] = false }()
		path = append(path, typ.Def)
		copy, note := subSets(sets, onPath, path, set.bind)
		if copy == nil {
			return nil, note
		}
		set.bind = copy
		set.substituted = true
		return copy, nil
	case *BasicType:
		copy := *typ
		return &copy, nil
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}

type disjointSets struct {
	setMap   map[*TypeParm]*set
	setSlice []*TypeParm
}

type set struct {
	name   string
	rank   int
	parent *set

	// bind is only used on root sets after computing unions.
	bind        Type
	substituted bool
}

func (sets *disjointSets) find(t *TypeParm) *set {
	if sets.setMap == nil {
		sets.setMap = make(map[*TypeParm]*set)
	}
	s := sets.setMap[t]
	if s == nil {
		s = &set{name: t.Name, rank: 0}
		s.parent = s
		sets.setMap[t] = s
		sets.setSlice = append(sets.setSlice, t)
	}
	root := s
	for root.parent != root {
		root = root.parent
	}
	for s.parent != root {
		p := s.parent
		s.parent = root
		s = p
	}
	return root
}

func (sets *disjointSets) union(a, b *TypeParm) {
	x := sets.find(a)
	y := sets.find(b)
	if x == y {
		return
	}
	if x.rank < y.rank {
		x, y = y, x
	}
	y.parent = x
	if x.rank != y.rank {
		x.rank++
	}
}
