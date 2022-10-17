package checker

import (
	"fmt"
	"strings"

	"github.com/eaburns/pea/loc"
)

func (f *FuncDecl) arity() int                  { return len(f.Parms) }
func (f *FuncDecl) ret() typePattern            { return pattern(f.Ret) }
func (f *FuncDecl) parm(i int) typePattern      { return pattern(f.Parms[i]) }
func (f *FuncDecl) sub(map[*TypeParm]Type) note { return nil }

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

func (f *FuncInst) ret() typePattern {
	return typePattern{parms: f.typeParms(), typ: f.T.Ret}
}

func (f *FuncInst) parm(i int) typePattern {
	if f.T.Parms[i] == nil {
		// TODO: A function that failed to check should not be a candidate for overload resolution.
		// Parms[i]==nil indicates that this function failed to check,
		// but we must be calling parm() during overload resolution.
		panic(fmt.Sprintf("impossible: %s\n", f.Name()))
	}
	return typePattern{parms: f.typeParms(), typ: f.T.Parms[i]}
}

func (f *FuncInst) typeParms() []*TypeParm {
	parms := make([]*TypeParm, 0, len(f.Def.TypeParms))
	for i := range f.Def.TypeParms {
		if f.subbed == nil || !f.subbed[i] {
			parms = append(parms, &f.Def.TypeParms[i])
		}
	}
	return parms
}

func (f *FuncInst) sub(sub map[*TypeParm]Type) note {
	if f.subbed == nil && len(f.Def.TypeParms) > 0 {
		f.subbed = make([]bool, len(f.Def.TypeParms))
	}
	for i := range f.Def.TypeParms {
		if _, ok := sub[&f.Def.TypeParms[i]]; ok {
			f.subbed[i] = true
		}
	}
	for i := range f.TypeArgs {
		f.TypeArgs[i] = subType(sub, f.TypeArgs[i])
	}
	for i, decl := range f.IfaceArgs {
		// FuncInst.sub is only called before instIface,
		// so IfaceArgs must be still *FuncDecls.
		f.IfaceArgs[i] = subFuncDecl(sub, decl.(*FuncDecl))
	}
	f.T = subType(sub, f.T).(*FuncType)
	return nil
}

func instFuncConstraints(x scope, l loc.Loc, fun Func) note {
	f, ok := fun.(*FuncInst)
	if !ok {
		return nil
	}
	if recursiveIfaceDepth(x, f.Def) >= 10 || seenIfaceInst(x, f) {
		return newNote("%s: is excluded from the scope", f.Def).setLoc(f.Def)
	}
	x = &ifaceLookup{parent: x, def: f.Def, inst: f}
	var notes []note
	for i := range f.IfaceArgs {
		// Since the function is not yet instantiated, ifaceargs must be *FuncDecl.
		bind, fun, note := findConstraintFunc(x, l, f, i)
		if note != nil {
			notes = append(notes, note)
			continue
		}
		f.IfaceArgs[i] = fun

		if f.subbed == nil && len(f.Def.TypeParms) > 0 {
			f.subbed = make([]bool, len(f.Def.TypeParms))
		}
		for i := range f.Def.TypeParms {
			if _, ok := bind[&f.Def.TypeParms[i]]; ok {
				f.subbed[i] = true
			}
		}
		for i := range f.TypeArgs {
			f.TypeArgs[i] = subType(bind, f.TypeArgs[i])
		}
		// Substitute type variables in the following iface arguments,
		// with bindings determined by matching this argument.
		for j := i + 1; j < len(f.IfaceArgs); j++ {
			f.IfaceArgs[j] = subFuncDecl(bind, f.IfaceArgs[j].(*FuncDecl))
		}
	}
	if len(notes) > 0 {
		note := newNote("%s: failed to instantiate interface", fun).setLoc(fun)
		note.setNotes(notes)
		return note
	}
	return nil
}

// findConstraintFunc returns a function that satisfies funInst.Def.Iface[i] if any,
// along with any type parameter bindings needed to instantiate that function.
func findConstraintFunc(x scope, l loc.Loc, funInst *FuncInst, i int) (map[*TypeParm]Type, Func, note) {
	constraint := funInst.IfaceArgs[i].(*FuncDecl)
	var funcs []Func
	var notFoundNotes []note
	for _, id := range findIDs(x, constraint.Name) {
		switch id := id.(type) {
		case Func:
			funcs = append(funcs, id)
		case *VarDef:
			if t := funcType(id.T); t != nil {
				funcs = append(funcs, &idFunc{id: id, funcType: t, l: l})
			}
		case *ParmDef:
			if t := funcType(id.T); t != nil {
				funcs = append(funcs, &idFunc{id: id, funcType: t, l: l})
			}
		case *LocalDef:
			if t := funcType(id.T); t != nil {
				funcs = append(funcs, &idFunc{id: id, funcType: t, l: l})
			}
		}
		note := newNote("%s (%s) is not a function", id, id.Type()).setLoc(id)
		notFoundNotes = append(notFoundNotes, note)
		continue
	}
	fs, ns := ifaceADLookup(x, constraint)
	funcs = append(funcs, fs...)
	notFoundNotes = append(notFoundNotes, ns...)
	if len(funcs) > 0 {
		markVerbose(notFoundNotes)
	}

	typeParms := make([]*TypeParm, len(funInst.Def.TypeParms))
	for i := range funInst.Def.TypeParms {
		if funInst.subbed == nil || !funInst.subbed[i] {
			typeParms[i] = &funInst.Def.TypeParms[i]
		}
	}
	declPat := typePattern{parms: typeParms, typ: constraint.Type()}

	var n int
	var unifyFails []*unifyFuncFailure
	binds := make([]map[*TypeParm]Type, len(funcs))
	for _, f := range funcs {
		funType := funInst.Def.Iface[i].Type().(*FuncType)
		bind, fail := unifyFunc(x, l, f, funType, declPat)
		if fail != nil {
			unifyFails = append(unifyFails, fail)
			continue
		}
		if note := instFuncConstraints(x, l, f); note != nil {
			fail := &unifyFuncFailure{note: note, parms: f.arity()}
			unifyFails = append(unifyFails, fail)
			continue
		}
		funcs[n] = f
		binds[n] = bind
		n++
	}
	funcs = funcs[:n]
	binds = binds[:n]
	switch {
	case len(funcs) == 0:
		maxParms := -1
		for _, fail := range unifyFails {
			if fail.parms > maxParms {
				maxParms = fail.parms
			}
		}
		for _, fail := range unifyFails {
			if fail.parms < maxParms {
				fail.note.verbose(true)
			}
			notFoundNotes = append(notFoundNotes, fail.note)
		}
		note := newNote("%s: not found", constraint).setLoc(constraint.L)
		note.setNotes(notFoundNotes)
		return nil, nil, note
	case len(funcs) > 1:
		var notes []note
		for _, f := range funcs {
			notes = append(notes, newNote(f.String()).setLoc(f))
		}
		note := newNote("%s: ambiguous", constraint).setLoc(constraint.L)
		note.setNotes(notes)
		return nil, nil, note
	default:
		fun := useFunc(x, l, funcs[0])
		if builtin, ok := fun.(*Builtin); ok && builtin.Op == Return {
			// We need to wrap the return in a block, since it's not a static function.
			block := wrapCallInBlock(fun, _end, l)
			fun = &ExprFunc{Expr: block, FuncType: block.Func}
		}
		return binds[0], fun, nil
	}
}

func ifaceADLookup(x scope, decl *FuncDecl) ([]Func, []note) {
	file := file(x)
	imports := make(map[string]*Import)
	for _, imp := range file.Imports {
		if !imp.Exp {
			imports[imp.Path] = imp
		}
	}

	var funcs []Func
	var notes []note
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
		fs, ns := filterToFuncs(ids, decl.L)
		funcs = append(funcs, fs...)
		notes = append(notes, ns...)
	}

	return funcs, notes
}

type unifyFuncFailure struct {
	note note
	// parms is the number of parameters that successfully unified.
	// -1 means none attempted (arity is wrong)
	// len(Parms) means that all parameters succeeded, but the return failed.
	//
	// This is used to implement non-verbose note truncation.
	// When unifying multiple funcs, notes are only reported
	// for the failures with the max params value.
	parms int
}

// unifyFunc determines whether dst can be called in place of a function of type src.
// That means, that all parameters of src are implicitly convertable to parameters of dst,
// and the return type of dst is implicitly convertable to the return type of src.
// If all of that holds, dst is a candidate function where a function of type src is expected.
// The returned map is non-nil on success; it contains all TypeParam bindings
// in both dst and src when converting types.
//
// If srcOrigin is non-nil, it is used to determine whether any arguments
// or the return type must be reference literals.
func unifyFunc(x scope, l loc.Loc, dst Func, srcOrigin *FuncType, src typePattern) (map[*TypeParm]Type, *unifyFuncFailure) {
	funcType, ok := valueType(literalType(src.typ)).(*FuncType)
	if !ok {
		return nil, &unifyFuncFailure{
			note:  newNote("%s: not a function", src).setLoc(dst),
			parms: -1,
		}
	}
	srcFunPat := src.withType(funcType)
	if dst.arity() != len(funcType.Parms) {
		return nil, &unifyFuncFailure{
			note: newNote("%s: arity mismatch: got %d expected %d",
				dst, dst.arity(), len(funcType.Parms)).setLoc(dst),
			parms: -1,
		}
	}
	bind := make(map[*TypeParm]Type)
	for i := 0; i < dst.arity(); i++ {
		cvt, notes := convert(nil, srcFunPat.parm(i), dst.parm(i), false, &bind)
		if cvt == nil {
			n := newNote("%s: cannot convert parameter %d %s to %s",
				dst, i, srcFunPat.parm(i), dst.parm(i))
			n.setNotes(notes)
			n.setLoc(dst)
			return nil, &unifyFuncFailure{note: n, parms: i}
		}
		if srcOrigin != nil && isRefLiteral(srcOrigin.Parms[i]) && cvt.Kind == Deref {
			n := newNote("%s: parameter %d has type %s, but expected a reference literal %s",
				dst, i, dst.parm(i), srcFunPat.parm(i))
			n.setNotes(notes)
			n.setLoc(dst)
			return nil, &unifyFuncFailure{note: n, parms: i}
		}
		if n := dst.sub(bind); n != nil {
			n1 := newNote("%s: parameter %d type substitution failed", dst, i)
			n1.setNotes([]note{n})
			n.setLoc(dst)
			return nil, &unifyFuncFailure{note: n1, parms: i}
		}
	}
	cvt, notes := convert(nil, dst.ret(), srcFunPat.ret(), false, &bind)
	if cvt == nil {
		n := newNote("%s: cannot convert returned %s to %s",
			dst, dst.ret(), srcFunPat.ret())
		n.setNotes(notes)
		n.setLoc(dst)
		return nil, &unifyFuncFailure{note: n, parms: dst.arity()}
	}
	if srcOrigin != nil && isRefLiteral(srcOrigin.Ret) && cvt.Kind == Ref {
		n := newNote("%s: return has type %s, but expected a reference literal %s",
			dst, dst.ret(), srcFunPat.ret())
		n.setNotes(notes)
		n.setLoc(dst)
		return nil, &unifyFuncFailure{note: n, parms: dst.arity()}
	}
	if n := dst.sub(bind); n != nil {
		n1 := newNote("%s: return type substitution failed", dst)
		n1.setNotes([]note{n})
		n.setLoc(dst)
		return nil, &unifyFuncFailure{note: n1, parms: dst.arity()}
	}
	return bind, nil
}

// captureExprIfaceArgs returns the memoized FuncInst
// or an ExprFunc wrapping a call to the FuncInst
// with non-static IfaceArgs converted to parameters of the FuncInst,
// passed as block captures.
func captureExprIfaceArgs(f *FuncInst) Func {
	caps := captureIfaceArgs(f)
	f = memoizeFuncInst(f)
	if len(caps) == 0 {
		return f
	}

	// Some IfaceArgs were captured. Wrap the call in a block literal,
	// add captures to the block literal for the IfaceArg expressions,
	// and patch the call inside the block literal to pass the captures
	// as tailing arguments to the function.
	block, call := _wrapCallInBlock(f, f.T.Ret, f.T.L)
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
	// Only bother canonicalizing function instances that have non-variable types,
	// since only instances with non-variable types will be needed to emit code.
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

// captureIfaceArgs adds a parameter to the function
// for every IfaceArg that is not a static function,
// replaces the IfaceArg's expression with a load of that parameter,
// and returns a slice of *BlockCaps capturing the replaced expressions.
func captureIfaceArgs(f *FuncInst) []*BlockCap {
	var caps []*BlockCap
	// FuncInst may already have the f.T.Parms expr IfaceArg parms.
	// This happens when canonicaling a FuncInst during substitution.
	// We don't want to double-up on the args here, so chomp off the old ones.
	f.T.Parms = f.T.Parms[:len(f.Def.Parms)]
	for i := range f.IfaceArgs {
		exprFunc, ok := f.IfaceArgs[i].(*ExprFunc)
		if !ok {
			continue
		}
		decl := &f.Def.Iface[i]
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
	for i := range f.IfaceArgs {
		if !f.IfaceArgs[i].eq(o.IfaceArgs[i]) {
			return false
		}
	}
	return true
}

func (*Select) arity() int { return 1 }

func (s *Select) parm(i int) typePattern {
	if i > 0 {
		panic("impossible") // can't have more than 1 argument
	}
	// Technically, there is one selector for every field of every struct type,
	// but we implement it lazily by using _ for the 0th argument,
	// and erroring out in sub() if it's not a struct type.
	if s.TypeParm == nil {
		return pattern(s.Parm)
	}
	return typePattern{parms: []*TypeParm{s.TypeParm}, typ: s.Parm}
}

func (s *Select) ret() typePattern { return pattern(s.Ret) }

func (s *Select) sub(bind map[*TypeParm]Type) note {
	typ, ok := bind[s.TypeParm]
	if !ok {
		return nil
	}
	switch v := valueType(typ); {
	case isStructType(v):
		s.Parm = &RefType{Type: v, L: v.Loc()}
	case isStructRefType(v):
		s.Parm = v
	default:
		return newNote("%s: argument 0 (%s) is not a struct type", s, typ).setLoc(typ)
	}
	s.Struct = valueType(literalType(typ)).(*StructType)
	for i := range s.Struct.Fields {
		if s.Struct.Fields[i].Name == s.N {
			s.Field = &s.Struct.Fields[i]
			break
		}
	}
	if s.Field == nil {
		return newNote("%s: %s has no field %s", s, typ, s.N).setLoc(typ)
	}
	if s.Field.Type == nil {
		panic(fmt.Sprintf("impossible nil field type: %s\n", s.N))
	}
	s.Ret = &RefType{Type: s.Field.Type, L: s.Field.Type.Loc()}
	return nil
}

func (s *Select) eq(other Func) bool {
	o, ok := other.(*Select)
	return ok && s.Struct == o.Struct && s.Field == o.Field &&
		eqType(s.Parm, o.Parm) && eqType(s.Ret, o.Ret)
}

func (s *Switch) arity() int { return len(s.Parms) }

func (s *Switch) parm(i int) typePattern {
	return typePattern{parms: s.TypeParms, typ: s.Parms[i]}
}

func (s *Switch) ret() typePattern {
	return typePattern{parms: s.TypeParms, typ: s.Ret}
}

func (s *Switch) sub(bind map[*TypeParm]Type) note {
	// The caller of sub() may be lazy and leave keep a binding for TypeParm[0]
	// in the bind map across multiple calls of sub().
	// But we only want to substitute the 0th type parameter the first time.
	// We detect the 1st time by looking a s.Cases;
	// it is populated lazily on the 1st substitution,
	// so if it is non-empty, then we have already substituted.
	if typ, ok := bind[s.TypeParms[0]]; ok && len(s.Cases) == 0 {
		// We are substituting the 0th argument, which needs to be a union.

		switch v := valueType(typ); {
		case isUnionType(v):
			s.Parms[0] = &RefType{Type: v, L: v.Loc()}
		case isUnionRefType(v):
			s.Parms[0] = v
		default:
			return newNote("%s: argument 0 (%s) is not a union type", s, typ).setLoc(typ)
		}
		s.Union = valueType(literalType(typ)).(*UnionType)
		seen := make(map[*CaseDef]bool)
		hasDefault := false
		for _, name := range s.Names {
			if name == "_?" {
				hasDefault = true
				s.Cases = append(s.Cases, nil)
				continue
			}
			c := findCase(name, s.Union)
			if c == nil {
				// TODO: should use the location of the case keyword.
				return newNote("%s: %s has no case %s", s, typ, name).setLoc(typ)
			}
			if seen[c] {
				// Switch functions only exist for non-duplicated cases.
				// TODO: should use the location of the case keyword.
				return newNote("%s: duplicate case %s", s, name).setLoc(typ)
			}
			seen[c] = true
			s.Cases = append(s.Cases, c)
		}
		complete := true
		if !hasDefault {
			for i := range s.Union.Cases {
				if !seen[&s.Union.Cases[i]] {
					complete = false
					break
				}
			}
		}
		if !complete {
			s.Ret = _empty
		} else {
			s.Ret = &TypeVar{Name: s.TypeParms[1].Name, Def: s.TypeParms[1]}
		}
		for i, c := range s.Cases {
			switch {
			case c == nil:
				s.Parms[1+i] = &FuncType{Ret: s.Ret, L: s.Union.L}
			case c.Type == nil:
				s.Parms[1+i] = &FuncType{Ret: s.Ret, L: c.L}
			default:
				s.Parms[1+i] = &FuncType{Parms: []Type{c.Type}, Ret: s.Ret, L: c.L}
			}
		}
		return nil
	}

	for i := range s.Parms {
		s.Parms[i] = subType(bind, s.Parms[i])
	}
	s.Ret = subType(bind, s.Ret)
	return nil
}

func (s *Switch) eq(other Func) bool {
	o, ok := other.(*Switch)
	if !ok || s.Union != o.Union || len(s.Cases) != len(o.Cases) || len(s.Parms) != len(o.Parms) {
		return false
	}
	for i := range s.Cases {
		if s.Cases[i] != o.Cases[i] {
			return false
		}
	}
	for i := range s.Parms {
		if !eqType(s.Parms[i], o.Parms[i]) {
			return false
		}
	}
	return eqType(s.Ret, o.Ret)
}

func (b *Builtin) arity() int { return len(b.Parms) }

func (b *Builtin) ret() typePattern {
	p := pattern(b.Ret)
	if b.TypeParm != nil {
		p.parms = []*TypeParm{b.TypeParm}
	}
	return p
}

func (b *Builtin) parm(i int) typePattern {
	p := pattern(b.Parms[i])
	if b.TypeParm != nil {
		p.parms = []*TypeParm{b.TypeParm}
	}
	return p
}

var intTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, UintRef, Uint, Uint8, Uint16, Uint32, Uint64}
var numTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, UintRef, Uint, Uint8, Uint16, Uint32, Uint64, Float32, Float64}

func (b *Builtin) sub(bind map[*TypeParm]Type) note {
	typ, ok := bind[b.TypeParm]
	if !ok {
		return nil
	}
	switch b.Op {
	case BitNot, BitXor, BitAnd, BitOr, LeftShift, RightShift:
		// TODO: Disallow bit-wise operands by default on defined integer types.
		if !allowed(intTypes, typ) {
			return newNote("%s: does not support type %s", b, typ)
		}
		bind[b.TypeParm] = valueType(typ)
	case Negate, Minus, Plus, Times, Divide, Modulus, Eq, Neq, Cmp:
		// TODO: Disallow arithmetic operands by default on defined numeric types.
		if !allowed(numTypes, typ) {
			return newNote("%s: does not support type %s", b, typ)
		}
		bind[b.TypeParm] = valueType(typ)
	case Index:
		// TODO: Disallow valueType for Index;
		// it should just be defined on [T] and string.
		valType := valueType(typ)
		if isArrayType(valType) {
			b.Parms[0] = valType
			b.Ret = refLiteral(literalType(valType).(*ArrayType).ElemType)
		} else if isStringType(valType) {
			b.Parms[0] = valType
			b.Ret = _uint8
		} else {
			return newNote("%s: %s is not an array or string", b, typ)
		}
	case Slice:
		// TODO: Disallow valueType for Slice;
		// it should just be defined on [T] and string.
		valType := valueType(typ)
		if !isArrayType(valType) && !isStringType(valType) {
			return newNote("%s: %s is not an array or string", b, typ)
		}
		b.Parms[0] = valType
		b.Ret = valType
		return nil
	case Length:
		// TODO: Disallow valueType for Length;
		// it should just be defined on [T] and string.
		valType := valueType(typ)
		if !isArrayType(valType) && !isStringType(valType) {
			return newNote("%s: %s is not an array or string", b, typ)
		}
		b.Parms[0] = valType
		return nil
	}
	for i := range b.Parms {
		b.Parms[i] = subType(bind, b.Parms[i])
	}
	b.Ret = subType(bind, b.Ret)
	return nil
}

func allowed(allowedTypes []BasicTypeKind, typ Type) bool {
	t, ok := valueType(basicType(typ)).(*BasicType)
	if !ok {
		return false
	}
	for _, k := range allowedTypes {
		if k == t.Kind {
			return true
		}
	}
	return false
}

func (b *Builtin) eq(other Func) bool {
	o, ok := other.(*Builtin)
	if !ok || b.Op != o.Op || len(b.Parms) != len(o.Parms) {
		return false
	}
	for i := range b.Parms {
		if !eqType(b.Parms[i], o.Parms[i]) {
			return false
		}
	}
	return eqType(b.Ret, o.Ret)
}

func (e *ExprFunc) arity() int                  { return len(e.FuncType.Parms) }
func (e *ExprFunc) ret() typePattern            { return pattern(e.FuncType.Ret) }
func (e *ExprFunc) parm(i int) typePattern      { return pattern(e.FuncType.Parms[i]) }
func (e *ExprFunc) sub(map[*TypeParm]Type) note { return nil }

func (e *ExprFunc) eq(other Func) bool {
	o, ok := other.(*ExprFunc)
	return ok && eqType(e.FuncType, o.FuncType)
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

func (i *idFunc) String() string { return i.id.String() }

func (i *idFunc) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(i.String())
	return s
}
func (id *idFunc) arity() int                  { return len(id.funcType.Parms) }
func (id *idFunc) ret() typePattern            { return pattern(id.funcType.Ret) }
func (id *idFunc) parm(i int) typePattern      { return pattern(id.funcType.Parms[i]) }
func (id *idFunc) sub(map[*TypeParm]Type) note { return nil }

// eq should never be called; it's used to check equality of FuncInst ifaces.
// FuncInst ifaces should never have an idFunc, since it is a temporary,
// bookkeeping type only used inside overload resolution.
func (*idFunc) eq(Func) bool { panic("impossible") }
