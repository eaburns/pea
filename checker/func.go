package checker

import (
	"fmt"
	"strings"

	"github.com/eaburns/pea/loc"
)

func (f *FuncDecl) arity() int                  { return len(f.Parms) }
func (f *FuncDecl) ret() typePattern            { return typePattern{typ: f.Ret} }
func (f *FuncDecl) parm(i int) typePattern      { return typePattern{typ: f.Parms[i]} }
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

func instIface(x scope, l loc.Loc, fun Func) note {
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
		fun, note := findIfaceFunc(x, l, f.Def, f.IfaceArgs[i].(*FuncDecl))
		if note != nil {
			notes = append(notes, note)
		} else {
			f.IfaceArgs[i] = fun
		}
	}
	if len(notes) > 0 {
		note := newNote("%s: failed to instantiate interface", fun).setLoc(fun)
		note.setNotes(notes)
		return note
	}
	return nil
}

func findIfaceFunc(x scope, l loc.Loc, funDef *FuncDef, decl *FuncDecl) (Func, note) {
	ids := findIDs(x, decl.Name)
	var notFoundNotes []note
	var funcs []Func
	for _, id := range ids {
		switch id.(type) {
		case *Builtin:
		case *FuncInst:
		case *FuncDecl:
		case *Switch:
		case *Select:
		default:
			// TODO: relax the constraint that a funcdecl be a static function.
			notFoundNotes = append(notFoundNotes, newNote("%s: not a static function", id).setLoc(id))
			continue
		}
		funcs = append(funcs, id.(Func))
	}

	fs, ns := ifaceADLookup(x, decl)
	funcs = append(funcs, fs...)
	notFoundNotes = append(notFoundNotes, ns...)
	if len(funcs) > 0 {
		markVerbose(notFoundNotes)
	}

	var n int
	var unifyFails []*unifyFuncFailure
	for _, f := range funcs {
		if fail := unifyFunc(x, l, f, decl.Type()); fail != nil {
			unifyFails = append(unifyFails, fail)
			continue
		}
		funcs[n] = f
		n++
	}
	funcs = funcs[:n]
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
		note := newNote("%s: not found", decl).setLoc(decl.L)
		note.setNotes(notFoundNotes)
		return nil, note
	case len(funcs) > 1:
		var notes []note
		for _, f := range funcs {
			notes = append(notes, newNote(f.String()).setLoc(f))
		}
		note := newNote("%s: ambiguous", decl).setLoc(decl.L)
		note.setNotes(notes)
		return nil, note
	default:
		fun := funcs[0]
		if f, ok := fun.(*FuncInst); ok {
			useFuncInst(x, l, funDef, decl, f.Def)
			fun = canonicalFuncInst(f)
		}
		return fun, nil
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

func unifyFunc(x scope, l loc.Loc, f Func, typ Type) *unifyFuncFailure {
	funcType, ok := valueType(literalType(typ)).(*FuncType)
	if !ok {
		return &unifyFuncFailure{
			note:  newNote("%s: not a function", typ).setLoc(f),
			parms: -1,
		}
	}
	if f.arity() != len(funcType.Parms) {
		return &unifyFuncFailure{
			note:  newNote("%s: parameter mismatch", f).setLoc(f),
			parms: -1,
		}
	}
	for i := 0; i < f.arity(); i++ {
		p := funcType.Parms[i]
		// For the moment, we don't bother trying to unify if the type is already grounded.
		// The difference is just in who reports the error: unify or converson.
		// TODO: always unify types and change the expected error in tests.
		if pat := f.parm(i); !pat.isGroundType() {
			bind := unify(pat, p)
			if bind == nil {
				n := newNote("%s: cannot unify argument %d: %s and %s", f, i, pat, p).setLoc(p)
				return &unifyFuncFailure{note: n, parms: i}
			}
			if n := f.sub(bind); n != nil {
				return &unifyFuncFailure{note: n, parms: i}
			}
		}
		t := f.parm(i).groundType()
		if !canImplicitConvert(p, t) {
			note := newNote("%s: cannot convert argument %s to %s", f, p, t).setLoc(t)
			return &unifyFuncFailure{note: note, parms: i}
		}
	}

	// Some implementations of unifyRet (Builtin for example)
	// assume that the parameters were unified first.
	// So make sure to do unifyRet after unifyParm.
	// For the moment, we don't bother trying to unify if the type is already grounded.
	// The difference is just in who reports the error: unify or converson.
	// TODO: always unify types and change the expected error in tests.
	if pat := f.ret(); !pat.isGroundType() {
		bind := unify(pat, funcType.Ret)
		if bind == nil {
			n := newNote("%s: cannot unify return: %s and %s", f, pat, funcType.Ret).setLoc(funcType.Ret)
			return &unifyFuncFailure{note: n, parms: f.arity()}
		}
		if n := f.sub(bind); n != nil {
			return &unifyFuncFailure{note: n, parms: f.arity()}
		}
	}
	t := f.ret().groundType()
	// Allowing any implicit convert here can lead to normally illegal
	// reference conversions. Consider:
	/*
		func foo(s S) &S : [](S, int, int)S {
			return: &S :: s[5, 6]
		}

		func main() {
			str_ref := &string :: "",
			foo(str_ref)
		}
	*/
	// The when substituting &string for S,
	// the iface argument would be the built-in
	// [](string, int, int)string.
	// The return of s[5, 6] is a defer of &string, type string.
	// We cannot convert string to &S=&&string.
	// to a variable and returning that variable.
	// These are fixed during substitution.
	// Calls are substituted to be a call to a block literal
	// that makes the underlying call, assigning its result.
	// Then the assigned variable is returned with conversion
	// to the iface return type.
	// This extra variable allows the additional reference to be added.
	if !canImplicitConvert(t, funcType.Ret) {
		note := newNote("%s: cannot convert returned %s to %s",
			f, t, funcType.Ret).setLoc(t)
		return &unifyFuncFailure{note: note, parms: f.arity()}
	}

	if note := instIface(x, l, f); note != nil {
		return &unifyFuncFailure{note: note, parms: f.arity()}
	}
	return nil
}

func canonicalFuncInst(f *FuncInst) *FuncInst {
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
		return typePattern{typ: s.Parm}
	}
	return typePattern{parms: []*TypeParm{s.TypeParm}, typ: s.Parm}
}

func (s *Select) ret() typePattern {
	return typePattern{typ: s.Ret}
}

func (s *Select) sub(bind map[*TypeParm]Type) note {
	typ, ok := bind[s.TypeParm]
	if !ok {
		panic("impossible")
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
	if typ, ok := bind[s.TypeParms[0]]; ok {
		// We are substituting the 0th argument, which must be a union.

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
	pat := typePattern{typ: b.Ret}
	if b.TypeParm != nil {
		pat.parms = []*TypeParm{b.TypeParm}
	}
	return pat
}

func (b *Builtin) parm(i int) typePattern {
	pat := typePattern{typ: b.Parms[i]}
	if b.TypeParm != nil {
		pat.parms = []*TypeParm{b.TypeParm}
	}
	return pat
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
	case Negate, Minus, Plus, Times, Divide, Modulus, Eq,
		Neq, Less, LessEq, Greater, GreaterEq:
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
			b.Ret = refType(literalType(valType).(*ArrayType).ElemType)
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
func (e *ExprFunc) ret() typePattern            { return typePattern{typ: e.FuncType.Ret} }
func (e *ExprFunc) parm(i int) typePattern      { return typePattern{typ: e.FuncType.Parms[i]} }
func (e *ExprFunc) sub(map[*TypeParm]Type) note { return nil }

// eq returns whether other is an *ExprFunc.
// As far as Func.eq is concerned, all *ExprFuncs are the same,
// because their underlying representation must be the same:
// a tuple <&capture_block, &static_function>.
func (e *ExprFunc) eq(other Func) bool { _, ok := other.(*ExprFunc); return ok }

// idFunc is like an ExprFunc, but it holds only ids not yet converted to expressions.
// It is used when checking ID calls in order to maintain the un-converted id
// in order to be able to mark its use with useID() if selected as the correct overload.
// An idFunc will only exist inside checkIDCall, and is never returned from checkIDCall.
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
func (id *idFunc) ret() typePattern            { return typePattern{typ: id.funcType.Ret} }
func (id *idFunc) parm(i int) typePattern      { return typePattern{typ: id.funcType.Parms[i]} }
func (id *idFunc) sub(map[*TypeParm]Type) note { return nil }

// eq should never be called; it's used to check equality of FuncInst ifaces.
// FuncInst ifaces should never have an idFunc, since it is a temporary,
// bookkeeping type only used inside overload resolution.
func (*idFunc) eq(Func) bool { panic("impossible") }
