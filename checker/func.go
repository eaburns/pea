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
	parms := f.typeParmMap()
	if isGroundType(parms, f.T.Parms[i]) {
		return nil
	}
	bind := unify(parms, f.T.Parms[i], typ)
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
	parms := f.typeParmMap()
	if isGroundType(parms, f.T.Ret) {
		return nil
	}
	bind := unify(parms, f.T.Ret, typ)
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

// unify unifies typ with a function parameter type pat
// and returns the binding from type variables to types.
// parms are the function's type parameters.
//
// unify is related to unifyStrict, but allows an implicit conversion.
//
// typ unifies with pat if…
// 	* pat is a variable that is a free parameter, in which case pat=typ is bound; or
// 	* pat is a type literal and typ's literal form unifies with pat; or
// 	* typ is a type literal and pat's literal form unifies with pat; or
// 	* pat and typ are references, and their referred types strictly unify; or
// 	* pat is a reference, typ is not, and pat's referred type strictly unifies with typ; or
// 	* typ is a reference, pat is not, and typ's referred type strictly unifies with pat; or
// 	* typ strictly unifies with pat.
func unify(parms map[*TypeParm]bool, pat, typ Type) map[*TypeParm]Type {
	bind := make(map[*TypeParm]Type)
	if v, ok := pat.(*TypeVar); ok && parms[v.Def] {
		bind[v.Def] = typ
		return bind
	}

	if isLiteralType(pat) {
		if lit := literalType(typ); lit != nil {
			typ = lit
		}
	} else if isLiteralType(typ) {
		if lit := literalType(pat); lit != nil {
			pat = lit
		}
	}
	var ok bool
	switch {
	case isRefType(pat) && isRefType(typ):
		ok = unifyStrict(parms, bind, pat.(*RefType).Type, typ.(*RefType).Type)
	case isRefType(pat):
		ok = unifyStrict(parms, bind, pat.(*RefType).Type, typ)
	case isRefType(typ):
		ok = unifyStrict(parms, bind, pat, typ.(*RefType).Type)
	default:
		ok = unifyStrict(parms, bind, pat, typ)
	}
	if !ok {
		return nil
	}
	return bind
}

// unifyStrict strictly unifies typ with another type, pat
// and returns the binding from type variables to types.
// parms are the free type parameters.
//
// typ strictly unifies with pat…
// 	* if pat is a def type and typ is a def type and
// 		- they both have the same type definition, and
// 		- each argument of typ strictly unifies with the corresponding of pat;
// 	* or if pat is a ref type and typ is a ref type and
// 		the referred type of typ strictly unifies with the referred type of pat;
// 	* or if pat is an array type and typ is an array type and
// 		the element type of typ strictly unifies with the element type of pat;
// 	* or if pat is a struct type and typ is a struct type and
// 		- pat and typ have the same number of fields,
// 		- each field of typ has the same name as the corresponding field of pat, and
// 		- the type of each field of typ strictly unifies with the type of the corresponding field of pat,
// 	* or if pat is a union type and typ is a union type and,
// 		- pat and typ have the same number of cases,
// 		- each case of typ has the same name as the corresponding case of pat, and
// 		- each case of typ either is untyped and so is the corresponding case of pat
// 			or is typed and so is the corresponding case of pat and
// 			the type of the case of typ strictly unifies with that of pat;
// 	* or if pat is a function typ and typ is a function type and,
// 		- they have the same number of parameters,
// 		- each parameter type of typ strictly unifies with the corresponding parameter type of pat, and
// 		- the return type of typ strictly unifies with the return type of pat;
// 	* or pat is a type variable that is a free parameter,
// 		in which case the unification contains a binding from pat=typ,
// 	* otherwise pat and typ are equal.
// It is an error for multiple bindings to the same type variable to have differing types.
func unifyStrict(parms map[*TypeParm]bool, bind map[*TypeParm]Type, pat, typ Type) bool {
	switch pat := pat.(type) {
	case *DefType:
		typ, ok := typ.(*DefType)
		if !ok || pat.Def != typ.Def {
			return false
		}
		for i := range pat.Args {
			if !unifyStrict(parms, bind, pat.Args[i], typ.Args[i]) {
				return false
			}
		}
		return true
	case *RefType:
		typ, ok := typ.(*RefType)
		if !ok {
			return false
		}
		return unifyStrict(parms, bind, pat.Type, typ.Type)
	case *ArrayType:
		typ, ok := typ.(*ArrayType)
		if !ok {
			return false
		}
		return unifyStrict(parms, bind, pat.ElemType, typ.ElemType)
	case *StructType:
		typ, ok := typ.(*StructType)
		if !ok || len(pat.Fields) != len(typ.Fields) {
			return false
		}
		for i := range pat.Fields {
			patFieldType := pat.Fields[i].Type
			typFieldType := typ.Fields[i].Type
			if !unifyStrict(parms, bind, patFieldType, typFieldType) {
				return false
			}
		}
		return true
	case *UnionType:
		typ, ok := typ.(*UnionType)
		if !ok || len(pat.Cases) != len(typ.Cases) {
			return false
		}
		for i := range pat.Cases {
			patCaseType := pat.Cases[i].Type
			typCaseType := typ.Cases[i].Type
			if patCaseType == nil {
				if typCaseType != nil {
					return false
				}
				continue
			}
			if !unifyStrict(parms, bind, patCaseType, typCaseType) {
				return false
			}
		}
		return true
	case *FuncType:
		typ, ok := typ.(*FuncType)
		if !ok || len(pat.Parms) != len(typ.Parms) {
			return false
		}
		for i := range pat.Parms {
			if !unifyStrict(parms, bind, pat.Parms[i], typ.Parms[i]) {
				return false
			}
		}
		return unifyStrict(parms, bind, pat.Ret, typ.Ret)
	case *BasicType:
		if !eqType(pat, typ) {
			return false
		}
		return true
	case *TypeVar:
		if !parms[pat.Def] {
			return eqType(pat, typ)
		}
		prev, ok := bind[pat.Def]
		if !ok {
			bind[pat.Def] = typ
			return true
		}
		return eqType(prev, typ)
	default:
		panic(fmt.Sprintf("impossible Type type: %T", pat))
	}
}

func instIface(x scope, l loc.Loc, fun Func) note {
	f, ok := fun.(*FuncInst)
	if !ok {
		return nil
	}
	// Exclude notes: if instantiation fails,
	// these notes explain functions excluded from the scope.
	// But if instantiation is successful these should be ignored.
	var excludeNotes []note
	x = &excludeFunc{parent: x, def: f.Def, notes: &excludeNotes}
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
		notes = append(notes, excludeNotes...)
		note := newNote("%s: failed to instantiate interface", fun).setLoc(fun)
		note.setNotes(notes)
		return note
	}
	return nil
}

func findIfaceFunc(x scope, l loc.Loc, funDef *FuncDef, decl *FuncDecl) (Func, note) {
	var notFoundNotes []note
	var funcs []Func
	for _, id := range x.find(decl.Name) {
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
	var n int
	for _, f := range funcs {
		if note := unifyFunc(x, l, f, decl.Type()); note != nil {
			notFoundNotes = append(notFoundNotes, note)
			continue
		}
		funcs[n] = f
		n++
	}
	funcs = funcs[:n]
	switch {
	case len(funcs) == 0:
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
			x.useFunc(l, funDef, decl, f.Def)
			fun = canonicalFuncInst(f)
		}
		return fun, nil
	}
}

func unifyFunc(x scope, l loc.Loc, f Func, typ Type) note {
	funcType, ok := valueType(literalType(typ)).(*FuncType)
	if !ok {
		return newNote("%s: not a function", f).setLoc(f)
	}
	if f.arity() != len(funcType.Parms) {
		return newNote("%s: parameter mismatch", f).setLoc(f)
	}
	if note := f.unifyRet(funcType.Ret); note != nil {
		return note
	}
	r := funcType.Ret
	if t := f.groundRet(); !canImplicitConvert(t, r) {
		return newNote("%s: cannot convert returned %s to %s", f, t, r).setLoc(t)
	}
	for i := 0; i < f.arity(); i++ {
		p := funcType.Parms[i]
		if note := f.unifyParm(i, p); note != nil {
			return note
		}
		if t := f.groundParm(i); !canImplicitConvert(p, t) {
			return newNote("%s: cannot convert argument %s to %s", f, p, t).setLoc(t)
		}
	}
	return instIface(x, l, f)
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
		for i := range s.Cases {
			name := s.Cases[i].Name
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
		for i := range s.Union.Cases {
			if seen[&s.Union.Cases[i]] {
				continue
			}
			// If not all cases are convered, the return is the empty struct.
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

var intTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64}
var numTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Float32, Float64}

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
