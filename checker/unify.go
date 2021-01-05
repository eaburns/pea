package checker

import "fmt"

func (f *FuncInst) unifyRet(typ Type) (bool, *note) {
	panic("unimplemented")
}

func (f *FuncInst) unifyParm(i int, typ Type) (bool, *note) {
	panic("unimplemented")
}

func (s *Select) unifyRet(typ Type) (bool, *note) {
	s.R = typ
	return true, nil
}

func (s *Select) unifyParm(i int, typ Type) (bool, *note) {
	if i > 0 {
		panic("impossible") // can't have more than 1 argument
	}
	v, _ := valueType(literal(typ))
	structType, ok := v.(*StructType)
	if !ok {
		return false, &note{
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
		return false, &note{
			msg: fmt.Sprintf("%s: %s has no field %s", s, typ, s.Field.Name),
			loc: typ.Loc(),
		}
	}
	if isGround(s.Ret()) && !canConvertReturn(f.Type, s.Field.Type) {
		return false, &note{
			msg: fmt.Sprintf("%s: cannot convert %s field %s (%s) to %s",
				s, typ, f.Name, f.Type, s.Field.Type),
			loc: typ.Loc(),
		}
	}
	s.Struct = structType
	s.Field = f
	s.P = &RefType{Type: structType, L: structType.L}
	s.R = &RefType{Type: f.Type, L: f.Type.Loc()}
	return true, nil
}

func (s *Switch) unifyRet(typ Type) (bool, *note) {
	if s.R != nil {
		s.R = typ
	}
	return true, nil
}

func (s *Switch) unifyParm(i int, typ Type) (bool, *note) {
	switch {
	case i == 0:
		var ok bool
		v, _ := valueType(literal(typ))
		if s.Union, ok = v.(*UnionType); !ok {
			return false, &note{
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
				return false, &note{
					msg: fmt.Sprintf("%s: %s has no case %s", s, typ, name),
					loc: typ.Loc(),
				}
			}
			if seen[c] {
				// Switch functions only exist for non-duplicated cases.
				return false, nil
			}
			seen[c] = true
			s.Cases[i] = c
		}
		for i := range s.Union.Cases {
			if !seen[&s.Union.Cases[i]] {
				// If not all cases are convered, the return is the empty struct.
				s.R = &StructType{}
				break
			}
		}
		for i, c := range s.Cases {
			f := &FuncType{Ret: s.R, L: c.L}
			if c.Type != nil {
				f.Parms = []Type{c.Type}
			}
			s.Ps[i+1] = f
		}
	case i > 0:
		r, ok := s.R.(*TypeVar)
		if !ok || r.Name != "_" {
			return true, nil
		}
		f, ok := typ.(*FuncType)
		if !ok {
			return false, &note{
				msg: fmt.Sprintf("%s: argument %d (%s) is not a function type", s, i, typ),
			}
		}
		s.R = f.Ret
		for j := 1; j < len(s.Ps); j++ {
			s.Ps[j].(*FuncType).Ret = s.R
		}
		return true, nil

	}
	return true, nil
}

func (b *Builtin) unifyRet(typ Type) (bool, *note) {
	switch b.Op {
	case Assign:
		b.Ps[0] = &RefType{Type: typ}
		b.Ps[1] = typ
		b.R = typ
		return true, nil
	case NewArray:
		t, note := arrayType(b, typ)
		if note != nil {
			return false, note
		}
		b.Ps[1] = t.ElemType
		b.R = t
		return true, nil
	case BitNot, BitXor, BitAnd, BitOr, LeftShift, RightShift:
		return unifyBuiltin(intTypes, b, typ)
	case Negate, Minus, Plus, Times, Divide, Modulus:
		return unifyBuiltin(numTypes, b, typ)
	case Index:
		if v, ok := b.R.(*TypeVar); ok && v.Name == "_" {
			b.Ps[0] = &ArrayType{ElemType: typ}
			b.R = &RefType{Type: typ}
		}
		return true, nil
	case Slice:
		if v, ok := b.R.(*TypeVar); ok && v.Name == "_" {
			t, note := arrayType(b, typ)
			if note != nil {
				return false, note
			}
			b.Ps[0] = t
			b.R = t
		}
		return true, nil
	case Eq, Neq, Less, LessEq, Greater, GreaterEq, NumConvert, StrConvert, Length, Panic, Print:
		return true, nil
	default:
		panic("impossible op type")
	}
	panic("unimplemented")
}

func (b *Builtin) unifyParm(i int, typ Type) (bool, *note) {
	switch b.Op {
	case Assign:
		if i == 1 {
			return true, nil
		}
		if t, ok := b.Ps[0].(*TypeVar); !ok || t.Name != "_" {
			return true, nil // already grounded
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
		return true, nil
	case NewArray:
		if i == 0 {
			return true, nil
		}
		if t, ok := b.Ps[1].(*TypeVar); ok && t.Name == "_" {
			b.Ps[1] = typ
			b.R = &ArrayType{ElemType: typ}
		}
		return true, nil
	case BitNot, BitXor, BitAnd, BitOr, LeftShift, RightShift:
		return unifyBuiltin(intTypes, b, typ)
	case Negate, Minus, Plus, Times, Divide, Modulus, Eq, Neq, Less, LessEq, Greater, GreaterEq, NumConvert:
		return unifyBuiltin(numTypes, b, typ)
	case StrConvert:
		return true, nil
	case Index:
		if i != 0 {
			return true, nil
		}
		if t, ok := b.Ps[0].(*TypeVar); !ok || t.Name != "_" {
			return true, nil // already grounded
		}
		t, note := arrayType(b, typ)
		if note != nil {
			return false, note
		}
		b.Ps[0] = t
		b.R = &RefType{Type: t.ElemType}
		return true, nil
	case Slice:
		if i != 0 {
			return true, nil
		}
		if t, ok := b.Ps[0].(*TypeVar); !ok || t.Name != "_" {
			return true, nil // already grounded
		}
		t, note := arrayType(b, typ)
		if note != nil {
			return false, note
		}
		b.Ps[0] = t
		b.R = t
		return true, nil
	case Length:
		if i != 0 {
			return true, nil
		}
		t, note := arrayType(b, typ)
		if note != nil {
			return false, note
		}
		b.Ps[0] = t
		return true, nil
	case Panic, Print:
		return true, nil
	default:
		panic("impossible op type")
	}
	panic("unimplemented")
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

func unifyBuiltin(allowedTypes []BasicTypeKind, b *Builtin, typ Type) (bool, *note) {
	ground := true
	for _, t := range append(b.Ps, b.R) {
		if v, ok := t.(*TypeVar); ok && v.Name == "_" {
			ground = false
			break
		}
	}
	if ground {
		return true, nil
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
		return false, &note{
			msg: fmt.Sprintf("%s: does not support type %s", b, typ),
		}
	}
	for i := range b.Ps {
		if v, ok := b.Ps[i].(*TypeVar); ok && v.Name == "_" {
			b.Ps[i] = t
		}
	}
	if v, ok := b.R.(*TypeVar); ok && v.Name == "_" {
		b.R = t
	}
	return true, nil
}

func (*ExprFunc) unifyRet(typ Type) (bool, *note) {
	panic("impossible") // ExprFunc can't have an ungrounded return.
}

func (*ExprFunc) unifyParm(i int, typ Type) (bool, *note) {
	panic("impossible") // ExprFunc can't have an ungrounded parms.
}
