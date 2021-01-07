package checker

import "fmt"

func (f *FuncInst) arity() int { return len(f.T.Parms) }

func (f *FuncInst) groundRet() Type {
	if !isGroundType(f.typeParmMap(), f.T.Ret) {
		return nil
	}
	return f.T.Ret
}

func (f *FuncInst) unifyRet(typ Type) *note {
	parms := f.typeParmMap()
	if isGroundType(parms, f.T.Ret) {
		return nil
	}
	bind := unify(parms, f.T.Ret, typ)
	if bind == nil {
		return &note{
			msg: fmt.Sprintf("%s: cannot unify return: %s and %s", f, f.T.Ret, typ),
			loc: typ.Loc(),
		}
	}
	f.sub(bind)
	return nil
}

func (f *FuncInst) groundParm(i int) Type {
	if !isGroundType(f.typeParmMap(), f.T.Parms[i]) {
		return nil
	}
	return f.T.Parms[i]
}

func (f *FuncInst) unifyParm(i int, typ Type) *note {
	parms := f.typeParmMap()
	if isGroundType(parms, f.T.Parms[i]) {
		return nil
	}
	bind := unify(parms, f.T.Parms[i], typ)
	if bind == nil {
		return &note{
			msg: fmt.Sprintf("%s: cannot argument %d: %s and %s", f, i, f.T.Parms[i], typ),
			loc: typ.Loc(),
		}
	}
	f.sub(bind)
	return nil
}

func (f *FuncInst) sub(sub map[*TypeParm]Type) {
	for i := range f.TypeArgs {
		f.TypeArgs[i] = subType(sub, f.TypeArgs[i])
	}
	if len(f.IfaceArgs) > 0 {
		panic("unimplemented")
	}
	f.T = subType(sub, f.T).(*FuncType)
}

func (f *FuncInst) typeParmMap() map[*TypeParm]bool {
	parmMap := make(map[*TypeParm]bool, len(f.Def.TypeParms))
	for i := range f.Def.TypeParms {
		parmMap[&f.Def.TypeParms[i]] = true
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

func (*Select) arity() int { return 1 }

func (s *Select) groundRet() Type { return s.R }

func (s *Select) unifyRet(typ Type) *note {
	s.R = typ
	return nil
}

func (s *Select) groundParm(int) Type { return s.P }

func (s *Select) unifyParm(i int, typ Type) *note {
	if i > 0 {
		panic("impossible") // can't have more than 1 argument
	}
	v, _ := valueType(literal(typ))
	structType, ok := v.(*StructType)
	if !ok {
		return &note{
			msg: fmt.Sprintf("%s: %s is not a struct type", s, typ),
			loc: typ.Loc(),
		}
	}
	var f *FieldDef
	for i = range structType.Fields {
		if structType.Fields[i].Name == s.Field.Name {
			f = &structType.Fields[i]
			break
		}
	}
	if f == nil {
		return &note{
			msg: fmt.Sprintf("%s: %s has no field %s", s, typ, s.Field.Name),
			loc: typ.Loc(),
		}
	}
	if s.R != nil && !canConvertReturn(f.Type, s.R) {
		return &note{
			msg: fmt.Sprintf("%s: cannot convert %s field %s (%s) to %s",
				s, typ, f.Name, f.Type, s.R),
			loc: typ.Loc(),
		}
	}
	s.Struct = structType
	s.Field = f
	s.P = &RefType{Type: structType, L: structType.L}
	s.R = &RefType{Type: f.Type, L: f.Type.Loc()}
	return nil
}

func (s *Switch) arity() int { return len(s.Ps) }

func (s *Switch) groundRet() Type { return s.R }

func (s *Switch) unifyRet(typ Type) *note {
	s.R = typ
	return nil
}

func (s *Switch) groundParm(i int) Type { return s.Ps[i] }

func (s *Switch) unifyParm(i int, typ Type) *note {
	switch {
	case i == 0:
		var ok bool
		v, _ := valueType(literal(typ))
		if s.Union, ok = v.(*UnionType); !ok {
			return &note{
				msg: fmt.Sprintf("%s: %s is not a union type", s, typ),
				loc: typ.Loc(),
			}
		}
		s.Ps[0] = &RefType{Type: s.Union, L: s.Union.L}
		seen := make(map[*CaseDef]bool)
		for i := range s.Cases {
			name := s.Cases[i].Name
			c := findCase(name, s.Union)
			if c == nil {
				return &note{
					msg: fmt.Sprintf("%s: %s has no case %s", s, typ, name),
					loc: typ.Loc(),
				}
			}
			if seen[c] {
				// Switch functions only exist for non-duplicated cases.
				s.R = nil
				for i := range s.Ps {
					s.Ps[i] = nil
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
			// Make sure that the empty struct is convertible, if needed.
			want := s.R
			s.R = &StructType{}
			t, ok := want.(*StructType)
			if want == nil || ok && len(t.Fields) == 0 {
				// It's OK. We either didn't have any expectation for the return,
				// or we wanted an empty struct anyway.
				break
			}
			return &note{
				msg: fmt.Sprintf("%s: cannot convert returned %s to %s", s, s.R, want),
			}
		}
		if s.R != nil {
			goto ground_parms
		}
	default:
		if s.Ps[i] != nil {
			break
		}

		v, _ := valueType(typ)
		f, ok := literal(v).(*FuncType)
		if !ok {
			return &note{
				msg: fmt.Sprintf("%s: argument %d (%s) is not a function type", s, i, typ),
			}
		}
		s.R = f.Ret
		goto ground_parms
	}
	return nil

ground_parms:
	for j, c := range s.Cases {
		f := &FuncType{Ret: s.R, L: c.L}
		if c.Type != nil {
			f.Parms = []Type{c.Type}
		}
		s.Ps[j+1] = f
	}
	return nil
}

func (b *Builtin) arity() int { return len(b.Ps) }

func (b *Builtin) groundRet() Type { return b.R }

func (b *Builtin) unifyRet(typ Type) *note {
	switch b.Op {
	case Assign:
		b.Ps[0] = &RefType{Type: typ}
		b.Ps[1] = typ
		b.R = typ
		return nil
	case NewArray:
		t, note := arrayType(b, typ)
		if note != nil {
			return note
		}
		b.Ps[1] = t.ElemType
		b.R = t
		return nil
	case BitNot, BitXor, BitAnd, BitOr, LeftShift, RightShift:
		return unifyBuiltin(intTypes, b, typ)
	case Negate, Minus, Plus, Times, Divide, Modulus:
		return unifyBuiltin(numTypes, b, typ)
	case Index:
		if b.R == nil {
			b.Ps[0] = &ArrayType{ElemType: typ}
			b.R = &RefType{Type: typ}
		}
		return nil
	case Slice:
		if b.R == nil {
			t, note := arrayType(b, typ)
			if note != nil {
				return note
			}
			b.Ps[0] = t
			b.R = t
		}
		return nil
	case Eq, Neq, Less, LessEq, Greater, GreaterEq, NumConvert, StrConvert, Length, Panic, Print:
		return nil
	default:
		panic("impossible op type")
	}
}

func (b *Builtin) groundParm(i int) Type { return b.Ps[i] }

func (b *Builtin) unifyParm(i int, typ Type) *note {
	switch b.Op {
	case Assign:
		if b.Ps[i] != nil {
			return nil
		}
		if r, ok := typ.(*RefType); ok {
			b.Ps[0] = typ
			b.Ps[1] = r.Type
			b.R = r.Type
		} else {
			b.Ps[0] = &RefType{Type: typ, L: typ.Loc()}
			b.Ps[1] = typ
			b.R = typ
		}
		return nil
	case NewArray:
		if i == 1 && b.Ps[1] == nil {
			b.Ps[1] = typ
			b.R = &ArrayType{ElemType: typ}
		}
		return nil
	case BitNot, BitXor, BitAnd, BitOr, LeftShift, RightShift:
		return unifyBuiltin(intTypes, b, typ)
	case Negate, Minus, Plus, Times, Divide, Modulus, Eq, Neq, Less, LessEq, Greater, GreaterEq, NumConvert:
		return unifyBuiltin(numTypes, b, typ)
	case Index:
		if i == 0 && b.Ps[0] == nil {
			t, note := arrayType(b, typ)
			if note != nil {
				return note
			}
			b.Ps[0] = t
			b.R = &RefType{Type: t.ElemType}
		}
		return nil
	case Slice:
		if i == 0 && b.Ps[0] == nil {
			t, note := arrayType(b, typ)
			if note != nil {
				return note
			}
			b.Ps[0] = t
			b.R = t
		}
		return nil
	case Length:
		if i == 0 {
			t, note := arrayType(b, typ)
			if note != nil {
				return note
			}
			b.Ps[0] = t
		}
		return nil
	case StrConvert, Panic, Print:
		return nil
	default:
		panic("impossible op type")
	}
}

func arrayType(b *Builtin, typ Type) (*ArrayType, *note) {
	v, _ := valueType(literal(typ))
	t, ok := v.(*ArrayType)
	if !ok {
		return nil, &note{
			msg: fmt.Sprintf("%s: return type %s is not an array type", b, typ),
		}
	}
	return t, nil
}

var intTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64}
var numTypes = []BasicTypeKind{Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Float32, Float64}

func unifyBuiltin(allowedTypes []BasicTypeKind, b *Builtin, typ Type) *note {
	ground := true
	for _, t := range append(b.Ps, b.R) {
		if t == nil {
			ground = false
			break
		}
	}
	if ground {
		return nil
	}
	allowed := false
	valType, _ := valueType(literal(typ))
	t, ok := valType.(*BasicType)
	if ok {
		for _, k := range allowedTypes {
			if k == t.Kind {
				allowed = true
				break
			}
		}
	}
	if !allowed {
		return &note{msg: fmt.Sprintf("%s: does not support type %s", b, typ)}
	}
	for i := range b.Ps {
		if b.Ps[i] == nil {
			b.Ps[i] = t
		}
	}
	if b.R == nil {
		b.R = t
	}
	return nil
}

func (e *ExprFunc) arity() int { return len(e.FuncType.Parms) }

func (e *ExprFunc) groundRet() Type { return e.FuncType.Ret }

func (e *ExprFunc) unifyRet(Type) *note { return nil }

func (e *ExprFunc) groundParm(i int) Type { return e.FuncType.Parms[i] }

func (e *ExprFunc) unifyParm(i int, _ Type) *note { return nil }

func unify(parms map[*TypeParm]bool, pat, typ Type) map[*TypeParm]Type {
	if eq(literal(pat), pat) || eq(literal(typ), typ) {
		pat = literal(pat)
		typ = literal(typ)
	}
	patVal, _ := valueType(pat)
	typVal, _ := valueType(typ)
	bind := make(map[*TypeParm]Type)
	if !unifyStrict(parms, bind, patVal, typVal) {
		return nil
	}
	return bind
}

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
		if !eq(pat, typ) {
			return false
		}
		return true
	case *TypeVar:
		if !parms[pat.Def] {
			return eq(pat, typ)
		}
		prev, ok := bind[pat.Def]
		if !ok {
			bind[pat.Def] = typ
			return true
		}
		return eq(prev, typ)
	default:
		panic(fmt.Sprintf("impossible Type type: %T", pat))
	}
}
