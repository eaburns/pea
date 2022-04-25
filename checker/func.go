package checker

import (
	"fmt"
	"strings"

	"github.com/eaburns/pea/loc"
)

func (f *FuncDecl) arity() int             { return len(f.Parms) }
func (f *FuncDecl) groundRet() Type        { return f.Ret }
func (*FuncDecl) unifyRet(Type) note       { return nil }
func (f *FuncDecl) groundParm(i int) Type  { return f.Parms[i] }
func (*FuncDecl) unifyParm(int, Type) note { return nil }

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

func (f *FuncInst) groundParm(i int) Type {
	if f.T.Parms[i] == nil {
		panic(fmt.Sprintf("impossible: %s\n", f.Name()))
	}
	if !isGroundType(f.typeParmMap(), f.T.Parms[i]) {
		return nil
	}
	return f.T.Parms[i]
}

func (f *FuncInst) unifyParm(i int, typ Type) note {
	if f.T.Parms[i] == nil {
		// TODO: we shouldn't call unifyParm on functions that failed to check.
		// This indicates that the function failed to check,
		// because one of its parameters has a unknown type.
		return newNote("%s: bad parameter type", f)
	}
	parmMap := f.typeParmMap()
	var parms []*TypeParm
	for p := range parmMap {
		parms = append(parms, p)
	}
	if isGroundType(parmMap, f.T.Parms[i]) {
		return nil
	}
	bind := unify(typePattern{parms: parms, typ: f.T.Parms[i]}, typ)
	if bind == nil {
		return newNote("%s: cannot unify argument %d: %s and %s", f, i, f.T.Parms[i], typ).setLoc(typ)
	}
	f.sub(bind)
	return nil
}

func (f *FuncInst) groundRet() Type {
	if !isGroundType(f.typeParmMap(), f.T.Ret) {
		return nil
	}
	return f.T.Ret
}

func (f *FuncInst) unifyRet(typ Type) note {
	parmMap := f.typeParmMap()
	if isGroundType(parmMap, f.T.Ret) {
		return nil
	}
	var parms []*TypeParm
	for p := range parmMap {
		parms = append(parms, p)
	}
	bind := unify(typePattern{parms: parms, typ: f.T.Ret}, typ)
	if bind == nil {
		return newNote("%s: cannot unify return: %s and %s", f, f.T.Ret, typ).setLoc(typ)
	}
	f.sub(bind)
	return nil
}

func (f *FuncInst) sub(sub map[*TypeParm]Type) {
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
}

func (f *FuncInst) typeParmMap() map[*TypeParm]bool {
	parmMap := make(map[*TypeParm]bool, len(f.Def.TypeParms))
	for i := range f.Def.TypeParms {
		parmMap[&f.Def.TypeParms[i]] = f.subbed == nil || !f.subbed[i]
	}
	return parmMap
}

func isGroundType(parms map[*TypeParm]bool, typ Type) bool {
	switch typ := typ.(type) {
	case *DefType:
		for _, arg := range typ.Args {
			if !isGroundType(parms, arg) {
				return false
			}
		}
		return true
	case *RefType:
		return isGroundType(parms, typ.Type)
	case *ArrayType:
		return isGroundType(parms, typ.ElemType)
	case *StructType:
		for i := range typ.Fields {
			if !isGroundType(parms, typ.Fields[i].Type) {
				return false
			}
		}
		return true
	case *UnionType:
		for i := range typ.Cases {
			if typ.Cases[i].Type == nil {
				continue
			}
			if !isGroundType(parms, typ.Cases[i].Type) {
				return false
			}
		}
		return true
	case *FuncType:
		for i := range typ.Parms {
			if !isGroundType(parms, typ.Parms[i]) {
				return false
			}
		}
		return isGroundType(parms, typ.Ret)
	case *BasicType:
		return true
	case *TypeVar:
		return !parms[typ.Def]
	default:
		panic(fmt.Sprintf("impossible Type type: %T", typ))
	}
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
		if note := f.unifyParm(i, p); note != nil {
			return &unifyFuncFailure{note: note, parms: i}
		}
		if t := f.groundParm(i); !canImplicitConvert(p, t) {
			note := newNote("%s: cannot convert argument %s to %s", f, p, t).setLoc(t)
			return &unifyFuncFailure{note: note, parms: i}
		}
	}

	// Some implementations of unifyRet (Builtin for example)
	// assume that the parameters were unified first.
	// So make sure to do unifyRet after unifyParm.
	if note := f.unifyRet(funcType.Ret); note != nil {
		return &unifyFuncFailure{note: note, parms: f.arity()}
	}
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
	if t := f.groundRet(); !canImplicitConvert(t, funcType.Ret) {
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

func (s *Select) groundParm(int) Type { return s.Parm }

func (s *Select) unifyParm(i int, typ Type) note {
	if i > 0 {
		panic("impossible") // can't have more than 1 argument
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
	var f *FieldDef
	for i = range s.Struct.Fields {
		if s.Struct.Fields[i].Name == s.Field.Name {
			f = &s.Struct.Fields[i]
			break
		}
	}
	if f == nil {
		return newNote("%s: %s has no field %s", s, typ, s.Field.Name).setLoc(typ)
	}
	if f.Type == nil {
		panic(fmt.Sprintf("impossible nil field type: %s\n", f.Name))
	}
	s.Field = f
	s.Ret = &RefType{Type: f.Type, L: f.Type.Loc()}
	return nil
}

func (s *Select) eq(other Func) bool {
	o, ok := other.(*Select)
	return ok && s.Struct == o.Struct && s.Field == o.Field &&
		eqType(s.Parm, o.Parm) && eqType(s.Ret, o.Ret)
}

func (s *Select) groundRet() Type { return s.Ret }

func (s *Select) unifyRet(typ Type) note { return nil }

func (s *Switch) arity() int { return len(s.Parms) }

func (s *Switch) groundParm(i int) Type { return s.Parms[i] }

func (s *Switch) unifyParm(i int, typ Type) note {
	switch {
	case i == 0:
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
		for i := range s.Cases {
			name := s.Cases[i].Name
			if name == "_?" {
				hasDefault = true
				s.Cases[i] = nil
				continue
			}
			c := findCase(name, s.Union)
			if c == nil {
				return newNote("%s: %s has no case %s", s, typ, name).setLoc(typ)
			}
			if seen[c] {
				// Switch functions only exist for non-duplicated cases.
				s.Ret = nil
				for i := range s.Parms {
					s.Parms[i] = nil
				}
				return nil
			}
			seen[c] = true
			s.Cases[i] = c
		}
		if hasDefault {
			break
		}
		for i := range s.Union.Cases {
			if seen[&s.Union.Cases[i]] {
				continue
			}
			// If not all cases are convered, the return is the empty struct.
			// In this case we know the argument types, so we ground.
			// In the case that the return is not [.], we don't know the argument
			// return types until after checking the 1st argument,
			// so we don't ground parameters yet in that case.
			s.Ret = &StructType{}
			goto ground_parms
		}
	default:
		if s.Parms[i] != nil {
			break
		}
		f, ok := valueType(literalType(typ)).(*FuncType)
		if !ok {
			return newNote("%s: argument %d (%s) is not a function type", s, i, typ)
		}
		if s.Ret == nil {
			s.Ret = f.Ret
			goto ground_parms
		}
	}
	return nil
ground_parms:
	for j, c := range s.Cases {
		if c == nil {
			s.Parms[j+1] = &FuncType{Ret: s.Ret, L: s.Union.L}
			continue
		}
		f := &FuncType{Ret: s.Ret, L: c.L}
		if c.Type != nil {
			f.Parms = []Type{c.Type}
		}
		s.Parms[j+1] = f
	}
	return nil
}

func (s *Switch) groundRet() Type { return s.Ret }

func (s *Switch) unifyRet(typ Type) note { return nil }

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

func (b *Builtin) groundParm(i int) Type { return b.Parms[i] }

func (b *Builtin) unifyParm(i int, typ Type) note {
	switch b.Op {
	case Assign:
		switch r, ok := typ.(*RefType); {
		case i != 0:
			return nil
		case ok:
			b.Parms[0] = typ
			b.Parms[1] = r.Type
			return nil
		default:
			b.Parms[0] = &RefType{Type: typ, L: typ.Loc()}
			b.Parms[1] = typ
			return nil
		}

	case NewArray:
		if i != 1 {
			return nil
		}
		b.Parms[1] = typ
		b.Ret = &ArrayType{ElemType: typ}
		return nil

	case BitNot, BitXor, BitAnd, BitOr, LeftShift, RightShift:
		return unifyBuiltin(intTypes, b, typ)

	case Negate, Minus, Plus, Times, Divide, Modulus, Eq,
		Neq, Less, LessEq, Greater, GreaterEq:
		return unifyBuiltin(numTypes, b, typ)

	case Index:
		switch v := valueType(typ); {
		case i != 0:
			return nil
		case isArrayType(v):
			b.Parms[0] = v
			b.Ret = &RefType{Type: literalType(v).(*ArrayType).ElemType}
			return nil
		case isStringType(v):
			b.Parms[0] = v
			b.Ret = basic(Uint8)
			return nil
		default:
			return newNote("%s: argument 0 (%s) is not an array or string", b, typ)
		}

	case Slice:
		switch v := valueType(typ); {
		case i != 0:
			return nil
		case isArrayType(v) || isStringType(v):
			b.Parms[0] = v
			b.Ret = v
			return nil
		default:
			return newNote("%s: argument 0 (%s) is not an array or string", b, typ)
		}

	case Length:
		switch v := valueType(typ); {
		case i != 0:
			return nil
		case isArrayType(v) || isStringType(v):
			b.Parms[0] = v
			return nil
		default:
			return newNote("%s: argument 0 (%s) is not an array or string", b, typ)
		}

	case Return, Panic, Print:
		return nil

	default:
		panic("impossible op type")
	}
}

func (b *Builtin) groundRet() Type { return b.Ret }

func (b *Builtin) unifyRet(typ Type) note { return nil }

var intTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, UintRef, Uint, Uint8, Uint16, Uint32, Uint64}
var numTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, UintRef, Uint, Uint8, Uint16, Uint32, Uint64, Float32, Float64}

func unifyBuiltin(allowedTypes []BasicTypeKind, b *Builtin, typ Type) note {
	ground := true
	for _, t := range append(b.Parms, b.Ret) {
		if t == nil {
			ground = false
			break
		}
	}
	if ground {
		return nil
	}
	allowed := false

	if t, ok := valueType(basicType(typ)).(*BasicType); ok {
		for _, k := range allowedTypes {
			if k == t.Kind {
				allowed = true
				break
			}
		}
	}
	if !allowed {
		return newNote("%s: does not support type %s", b, typ)
	}
	t := valueType(typ)
	for i := range b.Parms {
		if b.Parms[i] == nil {
			b.Parms[i] = t
		}
	}
	if b.Ret == nil {
		b.Ret = t
	}
	return nil
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

func (e *ExprFunc) arity() int                   { return len(e.FuncType.Parms) }
func (e *ExprFunc) groundParm(i int) Type        { return e.FuncType.Parms[i] }
func (e *ExprFunc) unifyParm(i int, _ Type) note { return nil }
func (e *ExprFunc) groundRet() Type              { return e.FuncType.Ret }
func (e *ExprFunc) unifyRet(Type) note           { return nil }

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
func (id *idFunc) arity() int                   { return len(id.funcType.Parms) }
func (id *idFunc) groundParm(i int) Type        { return id.funcType.Parms[i] }
func (id *idFunc) unifyParm(i int, _ Type) note { return nil }
func (id *idFunc) groundRet() Type              { return id.funcType.Ret }
func (id *idFunc) unifyRet(Type) note           { return nil }

// eq should never be called; it's used to check equality of FuncInst ifaces.
// FuncInst ifaces should never have an idFunc, since it is a temporary,
// bookkeeping type only used inside overload resolution.
func (*idFunc) eq(Func) bool { panic("impossible") }
