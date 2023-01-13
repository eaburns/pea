package checker

import (
	"fmt"

	"github.com/eaburns/pea/loc"
)

// appendTypeParmsToCopy appends bs to the end of a copy of as
// eliding items of bs that are already in as
// (as is assumed to already be deduplicated);
// however if len(bs) == 0, then as is simply returned.
func appendTypeParmsToCopy(as, bs []*TypeParm) []*TypeParm {
	if len(bs) == 0 {
		return as
	}
	cs := make([]*TypeParm, len(as), len(as)+len(bs))
	copy(cs, as)
next:
	for _, b := range bs {
		for _, c := range cs {
			if c == b {
				goto next
			}
		}
		cs = append(cs, b)
	}
	return cs
}

func (f *FuncDecl) arity() int                                                  { return len(f.Parms) }
func (f *FuncDecl) ret() TypePattern                                            { return pattern(f.Ret) }
func (f *FuncDecl) parm(i int) TypePattern                                      { return pattern(f.Parms[i]) }
func (f *FuncDecl) sub([]*TypeParm, map[*TypeParm]Type) (Func, *CandidateError) { return f, nil }

func (f *FuncDecl) eq(other Func) bool {
	o, ok := other.(*FuncDecl)
	if !ok || f.Name != o.Name || len(f.Parms) != len(o.Parms) {
		return false
	}
	for i := range f.Parms {
		if !eqType(f.Parms[i], o.Parms[i]) {
			return false
		}
	}
	return eqType(f.Ret, o.Ret)
}

func (f *FuncInst) arity() int { return len(f.T.Parms) }

func (f *FuncInst) ret() TypePattern {
	return makeTypePattern(f.typeParms, f.T.Ret)
}

func (f *FuncInst) parm(i int) TypePattern {
	if f.T.Parms[i] == nil {
		// TODO: A function that failed to check should not be a candidate for overload resolution.
		// Parms[i]==nil indicates that this function failed to check,
		// but we must be calling parm() during overload resolution.
		panic(fmt.Sprintf("impossible: %s\n", f.Name()))
	}
	return makeTypePattern(f.typeParms, f.T.Parms[i])
}

func (f *FuncInst) sub(parms []*TypeParm, sub map[*TypeParm]Type) (Func, *CandidateError) {
	return subFuncInst(f, parms, sub), nil
}

func subFuncInst(f *FuncInst, parms []*TypeParm, sub map[*TypeParm]Type) *FuncInst {
	copy := *f
	copy.typeParms = appendTypeParmsToCopy(copy.typeParms, parms)
	copy.TypeArgs = subTypes(sub, copy.TypeArgs)
	copy.ConstraintParms = make([]FuncDecl, len(f.ConstraintParms))
	for i, c := range f.ConstraintParms {
		copy.ConstraintParms[i] = *subFuncDecl(sub, &c)
	}
	copy.ConstraintArgs = append([]Func{}, f.ConstraintArgs...)
	copy.T = subType(sub, copy.T).(*FuncType)
	return &copy
}

func instParmConstraints(x scope, funcInst *FuncInst, i int) (*FuncInst, *CandidateError) {
	var s int
	for _, p := range funcInst.Def.Parms[0:i] {
		s += len(p.Constraints)
	}
	return instConstraints(x, funcInst, s, s+len(funcInst.Def.Parms[i].Constraints))
}

func instRetConstraints(x scope, funcInst *FuncInst) (*FuncInst, *CandidateError) {
	var s int
	for _, p := range funcInst.Def.Parms {
		s += len(p.Constraints)
	}
	return instConstraints(x, funcInst, s, s+len(funcInst.Def.Constraints))
}

func instConstraints(x scope, funcInst *FuncInst, start, end int) (*FuncInst, *CandidateError) {
	if recursiveIfaceDepth(x, funcInst.Def) >= 10 || seenIfaceInst(x, funcInst) {
		return funcInst, &CandidateError{
			Candidate: funcInst,
			Msg:       fmt.Sprintf("%s: is excluded from the scope", funcInst.Def),
		}
	}
	x = &ifaceLookup{parent: x, def: funcInst.Def, inst: funcInst}

	for i := start; i < end; i++ {
		c := funcInst.ConstraintParms[i]
		bind, fun, err := findConstraintFunc(x, funcInst.typeParms, &c)
		if err != nil {
			return funcInst, &CandidateError{
				Candidate: funcInst,
				Msg:       "failed to instantiate interface",
				Cause:     err,
			}
		}
		funcInst = subFuncInst(funcInst, nil, bind)
		funcInst.ConstraintArgs[i] = fun
	}
	return funcInst, nil
}

// findConstraintFunc returns a function that satisfies funInst.Def.Iface[i] if any,
// along with any type parameter bindings needed to instantiate that function.
func findConstraintFunc(x scope, typeParms []*TypeParm, decl *FuncDecl) (map[*TypeParm]Type, Func, Error) {
	if decl.Name == "::" && len(decl.Parms) == 1 {
		src := pattern(decl.Parms[0])
		dst := pattern(decl.Ret)
		bind, fun, cause := convertWithScope(x, src, dst, explicit, decl.L)
		if cause != nil {
			// This will always succeed, because convertWithScope
			// always returns an Error when mode==explicit.
			return nil, nil, cause.(Error)
		}
		if fun != nil {
			return bind, fun, nil
		}
		// This is a built-in conversion.
		// We need to wrap the conversion in a BlockLit:
		// 	(x S){ D :: x }
		// so that it can implement the Func interface.
		parms := []ParmDef{{Name: "x", T: src.Type}}
		parm := &Parm{Def: &parms[0], T: refLiteral(src.Type), L: decl.L}
		expr, _, err := convertExpr(x, parm, dst, explicit)
		if err != nil {
			// Cannot happen. convertWithScope
			// returned nil for Func and Cause,
			// so a built-in conversion must succeed.
			panic(fmt.Sprintf("impossible error: %s", err))
		}
		t := &FuncType{Parms: []Type{src.Type}, Ret: dst.Type, L: decl.L}
		fun = &ExprFunc{
			Expr: &BlockLit{
				Parms: parms,
				Ret:   dst.Type,
				Exprs: []Expr{expr},
				Func:  t,
				T:     t,
			},
			FuncType:          t,
			NoCaptureConstraintArg: true,
		}
		return bind, fun, nil
	}

	var funcs []Func
	var candidateErrs []CandidateError
	for _, id := range findIDs(x, decl.Name) {
		switch id := id.(type) {
		case Func:
			funcs = append(funcs, id)
			continue
		case *VarDef:
			if t := funcType(id.T); t != nil {
				funcs = append(funcs, &idFunc{id: id, funcType: t, l: id.L})
				continue
			}
		case *ParmDef:
			if t := funcType(id.T); t != nil {
				funcs = append(funcs, &idFunc{id: id, funcType: t, l: id.L})
				continue
			}
		case *LocalDef:
			if t := funcType(id.T); t != nil {
				funcs = append(funcs, &idFunc{id: id, funcType: t, l: id.L})
				continue
			}
		}
		candidateErrs = append(candidateErrs, CandidateError{
			Candidate: id,
			Msg:       fmt.Sprintf("type %s: not a function", id.Type()),
		})
		continue
	}
	fs, ces := ifaceADLookup(x, decl)
	candidateErrs = append(candidateErrs, ces...)
	funcs = append(funcs, fs...)

	var n int
	binds := make([]map[*TypeParm]Type, len(funcs))
	declPat := makeTypePattern(typeParms, decl.Type())
	for _, f := range funcs {
		f2, bind, err := unifyFunc(x, decl.L, f, decl.RefLit, declPat)
		if err != nil {
			candidateErrs = append(candidateErrs, *err)
			continue
		}
		funcs[n] = f2
		binds[n] = bind
		n++
	}
	funcs = funcs[:n]
	binds = binds[:n]
	switch {
	case len(funcs) == 0:
		return nil, nil, &NotFoundError{
			Item:       decl,
			Candidates: candidateErrs,
			scope:      x,
		}
	case len(funcs) > 1:
		candidates := make([]fmt.Stringer, len(funcs))
		for i, f := range funcs {
			candidates[i] = f
		}
		return nil, nil, &AmbiguousError{
			Item:       decl,
			Candidates: candidates,
			scope:      x,
		}
	default:
		fun := useFunc(x, decl.L, funcs[0])
		if builtin, ok := fun.(*Builtin); ok && builtin.Op == Return {
			// We need to wrap the return in a block, since it's not a static function.
			block := wrapCallInBlock(x, fun, _end, decl.L)
			fun = &ExprFunc{Expr: block, FuncType: block.Func}
		}
		return binds[0], fun, nil
	}
}

func ifaceADLookup(x scope, decl *FuncDecl) ([]Func, []CandidateError) {
	file := file(x)
	imports := make(map[string]*Import)
	for _, imp := range file.Imports {
		if !imp.Exp {
			imports[imp.Path] = imp
		}
	}

	var funcs []Func
	var errs []CandidateError
	seen := make(map[*Import]bool)
	for _, typ := range append(decl.Parms, decl.Ret) {
		defType, ok := typ.(*DefType)
		if !ok {
			continue
		}
		imp, ok := imports[defType.Def.Mod]
		if !ok || seen[imp] || isModuleInScope(x, defType.Def.Mod) {
			continue
		}
		seen[imp] = true
		ids := findIDs(imp, decl.Name)
		fs, ces := filterToFuncs(ids, decl.L)
		errs = append(errs, ces...)
		funcs = append(funcs, fs...)
	}
	return funcs, errs
}

// unifyFunc determines whether dst can be called in place of a function of type src.
// That means, that all parameters of src are implicitly convertable to parameters of dst,
// and the return type of dst is implicitly convertable to the return type of src.
// If all of that holds, dst is a candidate function where a function of type src is expected.
// The returned map is non-nil on success; it contains all TypeParam bindings
// in both dst and src when converting types.
//
// If refLit is either empty or the length of src's arity+1.
// If it is non-empty it indicates for each parameter
// or for the return type (refLit[len(refLit)-1])
// whether it must be a reference literal.
func unifyFunc(x scope, l loc.Loc, dst Func, refLit []bool, src TypePattern) (Func, map[*TypeParm]Type, *CandidateError) {
	funcType, ok := valueType(literalType(src.Type)).(*FuncType)
	if !ok {
		return nil, nil, &CandidateError{
			Candidate: dst,
			Msg:       "not a function",
		}
	}
	srcFunPat := src.withType(funcType)
	if dst.arity() != len(funcType.Parms) {
		return nil, nil, &CandidateError{
			Candidate: dst,
			Msg: fmt.Sprintf("arity mismatch: got %d expected %d",
				dst.arity(), len(funcType.Parms)),
		}
	}
	bind := make(map[*TypeParm]Type)
	for i := 0; i < dst.arity(); i++ {
		pat, cvt, err := convertPattern(nil, srcFunPat.parm(i), dst.parm(i), implicit, &bind)
		if !pat.isGroundType() {
			return nil, nil, &CandidateError{
				Candidate: dst,
				Msg:       fmt.Sprintf("parameter %d: cannot infer type %s", i, pat),
			}
		}
		if err != nil {
			return nil, nil, &CandidateError{
				Candidate: dst,
				Msg:       fmt.Sprintf("parameter %d", i),
				Cause:     err,
			}
		}
		if len(refLit) != 0 && refLit[i] && cvt.Kind == Deref {
			return nil, nil, &CandidateError{
				Candidate: dst,
				Msg:       fmt.Sprintf("parameter %d: has type %s, but expected a reference literal %s", i, dst.parm(i), srcFunPat.parm(i)),
			}
		}
		var subErr *CandidateError
		if dst, subErr = dst.sub(nil, bind); subErr != nil {
			return nil, nil, subErr
		}
		if funcInst, ok := dst.(*FuncInst); ok {
			var instErr *CandidateError
			if dst, instErr = instParmConstraints(x, funcInst, i); instErr != nil {
				return dst, bind, instErr
			}
		}
	}
	pat, cvt, err := convertPattern(nil, dst.ret(), srcFunPat.ret(), implicit, &bind)
	if !pat.isGroundType() {
		return nil, nil, &CandidateError{
			Candidate: dst,
			Msg:       fmt.Sprintf("return value: cannot infer type %s", pat),
		}
	}
	if err != nil {
		return nil, nil, &CandidateError{
			Candidate: dst,
			Msg:       "return value",
			Cause:     err,
		}
	}
	if len(refLit) != 0 && refLit[len(refLit)-1] && cvt.Kind == Ref {
		return nil, nil, &CandidateError{
			Candidate: dst,
			Msg:       fmt.Sprintf("return value: has type %s, but expected a reference literal %s", dst.ret(), srcFunPat.ret()),
		}
	}
	var subErr *CandidateError
	if dst, subErr = dst.sub(nil, bind); subErr != nil {
		return dst, bind, subErr
	}
	if funcInst, ok := dst.(*FuncInst); ok {
		var instErr *CandidateError
		if dst, instErr = instRetConstraints(x, funcInst); instErr != nil {
			return dst, bind, instErr
		}
	}
	return dst, bind, nil
}

// captureExprConstraintArgs returns the memoized FuncInst
// or an ExprFunc wrapping a call to the FuncInst
// with non-static ConstraintArgs converted to parameters of the FuncInst,
// passed as block captures.
func captureExprConstraintArgs(x scope, f *FuncInst) Func {
	caps := captureConstraintArgs(f)
	f = memoizeFuncInst(f)
	if len(caps) == 0 {
		return f
	}

	// Some IfaceArgs were captured. Wrap the call in a block literal,
	// add captures to the block literal for the IfaceArg expressions,
	// and patch the call inside the block literal to pass the captures
	// as tailing arguments to the function.
	block, call := _wrapCallInBlock(x, f, f.T.Ret, f.T.L)
	block.Caps = caps
	for i := 0; i < len(caps); i++ {
		call.Args[len(call.Args)-len(caps)+i] = deref(&Cap{
			Def: caps[i],
			T:   refLiteral(caps[i].T),
			L:   caps[i].L,
		})
	}
	block.Parms = block.Parms[:len(block.Parms)-len(caps)]
	block.Func.Parms = block.Func.Parms[:len(block.Func.Parms)-len(caps)]
	return &ExprFunc{
		Expr:     block,
		FuncType: block.Func,
	}
}

func memoizeFuncInst(f *FuncInst) *FuncInst {
	for _, arg := range f.TypeArgs {
		if hasTypeVariable(arg) {
			return f
		}
	}
	for _, inst := range f.Def.Insts {
		if f.eq(inst) {
			return inst
		}
	}
	f.Def.File.Mod.toSub = append(f.Def.File.Mod.toSub, f)
	f.Def.Insts = append(f.Def.Insts, f)
	return f
}

// captureConstraintArgs adds a parameter to the function
// for every ConstraintArg that is not a static function,
// replaces the ConstraintArg's expression with a load of that parameter,
// and returns a slice of *BlockCaps capturing the replaced expressions.
func captureConstraintArgs(f *FuncInst) []*BlockCap {
	var caps []*BlockCap
	// FuncInst may already have the f.T.Parms expr IfaceArg parms.
	// This happens when canonicaling a FuncInst during substitution.
	// We don't want to double-up on the args here, so chomp off the old ones.
	f.T.Parms = f.T.Parms[:len(f.Def.Parms)]
	for i := range f.ConstraintArgs {
		exprFunc, ok := f.ConstraintArgs[i].(*ExprFunc)
		if !ok || exprFunc.NoCaptureConstraintArg{
			continue
		}
		decl := funcDeclByIndex(f.Def, i)
		// This ParmDef is later added to f.Parms by subFuncInst.
		p := &ParmDef{
			Name: fmt.Sprintf("%s[%d]", decl.Name, i),
			T:    exprFunc.Type(),
			L:    exprFunc.Loc(),
		}
		f.T.Parms = append(f.T.Parms, p.T)
		caps = append(caps, &BlockCap{
			Name: p.Name,
			T:    refLiteral(exprFunc.Expr.Type()),
			L:    p.L,
			Expr: exprFunc.Expr,
		})
		exprFunc.Expr = deref(&Parm{
			Def: p,
			T:   refLiteral(p.T),
			L:   p.L,
		})
	}
	return caps
}

func funcDeclByIndex(def *FuncDef, i int) *FuncDecl {
	var n int
	for _, p := range def.Parms {
		for j := range p.Constraints {
			if n == i {
				return &p.Constraints[j]
			}
			n++
		}
	}
	for j := range def.Constraints {
		if n == i {
			return &def.Constraints[j]
		}
		n++
	}
	panic(fmt.Sprintf("constraint index %d out of bounds (n=%d)", i, n))
}

func (f *FuncInst) eq(other Func) bool {
	o, ok := other.(*FuncInst)
	if !ok || f.Def != o.Def {
		return false
	}
	for i := range f.TypeArgs {
		if !eqType(f.TypeArgs[i], o.TypeArgs[i]) {
			return false
		}
	}
	for i := range f.ConstraintArgs {
		switch {
		case f.ConstraintArgs[i] == nil && o.ConstraintArgs[i] == nil:
			break
		case f.ConstraintArgs[i] != nil && o.ConstraintArgs[i] != nil &&
			f.ConstraintArgs[i].eq(o.ConstraintArgs[i]):
			break
		default:
			return false
		}
	}
	return true
}

func (*Select) arity() int { return 1 }

func (f *Select) parm(i int) TypePattern {
	return makeTypePattern(f.typeParms, f.T.Parms[i])
}

func (f *Select) ret() TypePattern {
	return makeTypePattern(f.typeParms, f.T.Ret)
}

func (f *Select) sub(parms []*TypeParm, bind map[*TypeParm]Type) (Func, *CandidateError) {
	copy := *f
	copy.typeParms = appendTypeParmsToCopy(copy.typeParms, parms)
	if copy.Struct != nil {
		// We have already substituted for the struct type,
		// this Select is fully grounded; nothing more to do.
		return &copy, nil
	}

	// If this sub is not for the 0th parameter, there is nothing more to do.
	// However, if it is for the 0th type, we need to make sure it's a struct literal,
	// and check that the return type (if already substituted) matches the field.
	typ, ok := bind[copy.typeParms[0]]
	if !ok {
		// This is not substituting the 0th parameter type.
		copy.T = subType(bind, copy.T).(*FuncType)
		return &copy, nil
	}

	if copy.Struct, ok = valueType(literalType(typ)).(*StructType); !ok {
		return f, &CandidateError{
			Candidate: f,
			Msg:       fmt.Sprintf("parameter 0: %s is not a struct type", typ),
		}
	}
	for i := range copy.Struct.Fields {
		if copy.Struct.Fields[i].Name == copy.N {
			copy.Field = &copy.Struct.Fields[i]
			break
		}
	}
	if copy.Field == nil {
		return f, &CandidateError{
			Candidate: f,
			Msg:       fmt.Sprintf("%s has no field %s", typ, copy.N),
		}
	}
	if copy.Field.Type == nil {
		panic(fmt.Sprintf("impossible nil field type: %s\n", copy.N))
	}
	copy.T = &FuncType{
		Parms: []Type{refLiteral(copy.Struct)},
		Ret:   refLiteral(copy.Field.Type),
	}
	return &copy, nil
}

func (f *Select) eq(other Func) bool {
	o, ok := other.(*Select)
	return ok && f.N == o.N && f.Struct == o.Struct && f.Field == o.Field
}

func (f *Switch) arity() int { return len(f.T.Parms) }

func (f *Switch) parm(i int) TypePattern {
	return makeTypePattern(f.typeParms, f.T.Parms[i])
}

func (f *Switch) ret() TypePattern {
	return makeTypePattern(f.typeParms, f.T.Ret)
}

func (f *Switch) sub(parms []*TypeParm, bind map[*TypeParm]Type) (Func, *CandidateError) {
	copy := *f
	copy.typeParms = appendTypeParmsToCopy(copy.typeParms, parms)

	// If this is not the first time (first determined by len(copy.Cases)>0)
	// we are substituting the union parameter (type parameter 1),
	// the only thing to do is substitute the parameter and return types.
	// All the complex work is either already done or to be done
	// when the union itself is substituted.
	typ, ok := bind[copy.typeParms[1]]
	if !ok || len(copy.Cases) > 0 {
		copy.T = subType(bind, copy.T).(*FuncType)
		return &copy, nil
	}

	if copy.Union, ok = valueType(literalType(typ)).(*UnionType); !ok {
		return f, &CandidateError{
			Candidate: f,
			Msg:       fmt.Sprintf("parameter 0: %s is not a union type", typ),
		}
	}
	copy.T.Parms[0] = refLiteral(copy.Union)
	seen := make(map[*CaseDef]bool)
	hasDefault := false
	copy.Cases = nil
	for _, name := range copy.Names {
		if name == "_?" || name == "_:" {
			hasDefault = true
			copy.Cases = append(copy.Cases, nil)
			continue
		}
		c := findCase(name, copy.Union)
		if c == nil {
			return f, &CandidateError{
				Candidate: f,
				Msg:       fmt.Sprintf("%s has no case %s", typ, name),
			}
		}
		if seen[c] {
			// Switch functions only exist for non-duplicated cases.
			return f, &CandidateError{
				Candidate: f,
				Msg:       fmt.Sprintf("duplicate case %s", name),
			}
		}
		seen[c] = true
		copy.Cases = append(copy.Cases, c)
	}
	complete := true
	if !hasDefault {
		for i := range copy.Union.Cases {
			if !seen[&copy.Union.Cases[i]] {
				complete = false
				break
			}
		}
	}

	if !complete {
		copy.T.Ret = _empty
	} else if tv, ok := copy.T.Ret.(*TypeVar); ok && tv.Def == copy.typeParms[0] {
		// Otherwise, if the return type has not been substituted yet
		// If any of the parameters have already been substituted,
		// check whether we can determine the return type.
		// Take the result type of the first function type parameter (if any).
		// Below, we will report errors if any other substituted parameters
		// cannot implicitly convert to have this result type.
		for _, p := range copy.T.Parms[1:] {
			if fun, ok := p.(*FuncType); ok {
				copy.T.Ret = fun.Ret
				break
			}
		}
	}

	// For each parameter we expect it to be a function type
	// possibly with a parameter (if the corresponding case is typed)
	// and with a result matching copy.T.Ret.
	//
	// Some parameters may have already been substituted;
	// if so, they must be implicitly convertible to the desired type.
	for i, c := range copy.Cases {
		var parmType *FuncType
		switch {
		case c == nil:
			parmType = &FuncType{Ret: copy.T.Ret, L: copy.Union.L}
		case c.Type == nil:
			parmType = &FuncType{Ret: copy.T.Ret, L: c.L}
		default:
			parmType = &FuncType{Parms: []Type{c.Type}, Ret: copy.T.Ret, L: c.L}
		}
		if tv, ok := copy.T.Parms[1+i].(*TypeVar); !ok || tv.Def != copy.typeParms[2+i] {
			// It has already ben substituted, so we need to check it converts.
			srcPat := makeTypePattern(copy.typeParms, copy.T.Parms[1+i])
			dstPat := makeTypePattern(copy.typeParms, parmType)
			if _, err := convertType(srcPat, dstPat, implicit); err != nil {
				return f, &CandidateError{
					Candidate: f,
					Msg:       fmt.Sprintf("parameter %d", i),
					Cause:     err,
				}
			}
		}
		copy.T.Parms[1+i] = parmType
	}
	return &copy, nil
}

func eqCase(a, b *CaseDef) bool {
	switch {
	case a == nil && b == nil:
		return true
	case a == nil || b == nil:
		return false
	case a.Name != b.Name:
		return false
	case (a.Type == nil) != (b.Type == nil):
		return false
	case a.Type != nil && !eqType(a.Type, b.Type):
		return false
	default:
		return true
	}
}

func (f *Switch) eq(other Func) bool {
	o, ok := other.(*Switch)
	if !ok || !eqType(f.Union, o.Union) || len(f.Cases) != len(o.Cases) || len(f.T.Parms) != len(o.T.Parms) {
		return false
	}
	for i := range f.Cases {
		if !eqCase(f.Cases[i], o.Cases[i]) {
			return false
		}
	}
	for i := range f.T.Parms {
		if !eqType(f.T.Parms[i], o.T.Parms[i]) {
			return false
		}
	}
	return eqType(f.T.Ret, o.T.Ret)
}

func (f *Builtin) arity() int { return len(f.Parms) }

func (f *Builtin) ret() TypePattern {
	return makeTypePattern(f.typeParms, f.Ret)
}

func (f *Builtin) parm(i int) TypePattern {
	return makeTypePattern(f.typeParms, f.Parms[i])
}

func (f *Builtin) sub(parms []*TypeParm, bind map[*TypeParm]Type) (Func, *CandidateError) {
	copy := *f
	copy.typeParms = appendTypeParmsToCopy(copy.typeParms, parms)
	copy.Parms = subTypes(bind, copy.Parms)
	copy.Ret = subType(bind, copy.Ret)
	return &copy, nil
}

func (f *Builtin) eq(other Func) bool {
	o, ok := other.(*Builtin)
	if !ok || f.Op != o.Op || len(f.Parms) != len(o.Parms) {
		return false
	}
	for i := range f.Parms {
		if !eqType(f.Parms[i], o.Parms[i]) {
			return false
		}
	}
	return eqType(f.Ret, o.Ret)
}

func (f *ExprFunc) arity() int                                                  { return len(f.FuncType.Parms) }
func (f *ExprFunc) ret() TypePattern                                            { return pattern(f.FuncType.Ret) }
func (f *ExprFunc) parm(i int) TypePattern                                      { return pattern(f.FuncType.Parms[i]) }
func (f *ExprFunc) sub([]*TypeParm, map[*TypeParm]Type) (Func, *CandidateError) { return f, nil }

func (f *ExprFunc) eq(other Func) bool {
	o, ok := other.(*ExprFunc)
	return ok && eqType(f.FuncType, o.FuncType)
}

// idFunc is like an ExprFunc, but it holds only ids not yet converted to expressions.
// It is used when checking ID calls and ID interface constraints, maintaining the un-converted id
// to be able to mark its use with useID() if selected as the correct overload.
// It is never returned from the type checker.
type idFunc struct {
	id       id
	funcType *FuncType
	l        loc.Loc
}

func (f *idFunc) String() string                                              { return f.id.String() }
func (f *idFunc) buildString(s *stringBuilder)                                { s.WriteString(f.String()) }
func (f *idFunc) arity() int                                                  { return len(f.funcType.Parms) }
func (f *idFunc) ret() TypePattern                                            { return pattern(f.funcType.Ret) }
func (f *idFunc) parm(i int) TypePattern                                      { return pattern(f.funcType.Parms[i]) }
func (f *idFunc) sub([]*TypeParm, map[*TypeParm]Type) (Func, *CandidateError) { return f, nil }

// eq should never be called; it's used to check equality of FuncInst ifaces.
// FuncInst ifaces should never have an idFunc, since it is a temporary,
// bookkeeping type only used inside overload resolution.
func (*idFunc) eq(Func) bool { panic("impossible") }
