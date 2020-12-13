package checker

func (r *RefType) eq(other Type) bool {
	o, ok := other.(*RefType)
	return ok && r.Type.eq(o.Type)
}

func (n *NamedType) eq(other Type) bool {
	o, ok := other.(*NamedType)
	if !ok || len(o.Args) != len(n.Args) || o.Def != n.Def {
		return false
	}
	for i, nArg := range n.Args {
		oArg := o.Args[i]
		if !nArg.eq(oArg) {
			return false
		}
	}
	return true
}

func (a *ArrayType) eq(other Type) bool {
	o, ok := other.(*ArrayType)
	return ok && a.ElemType.eq(o.ElemType)
}

func (s *StructType) eq(other Type) bool {
	o, ok := other.(*StructType)
	if !ok || len(o.Fields) != len(s.Fields) {
		return false
	}
	for i := range s.Fields {
		sField := &s.Fields[i]
		oField := &o.Fields[i]
		if sField.Name != oField.Name ||
			!sField.Type.eq(oField.Type) {
			return false
		}
	}
	return true
}

func (u *UnionType) eq(other Type) bool {
	o, ok := other.(*UnionType)
	if !ok || len(o.Cases) != len(u.Cases) {
		return false
	}
	for i := range u.Cases {
		uCase := &u.Cases[i]
		oCase := &o.Cases[i]
		if uCase.Name != oCase.Name ||
			(uCase.Type == nil) != (oCase.Type == nil) ||
			(uCase.Type != nil && !uCase.Type.eq(oCase.Type)) {
			return false
		}
	}
	return true
}

func (f *FuncType) eq(other Type) bool {
	o, ok := other.(*FuncType)
	if !ok || len(o.Parms) != len(f.Parms) || (o.Ret == nil) != (f.Ret == nil) {
		return false
	}
	for i, fParm := range f.Parms {
		oParm := o.Parms[i]
		if !fParm.eq(oParm) {
			return false
		}
	}
	return f.Ret == nil || f.Ret.eq(o.Ret)
}

func (b *BasicType) eq(other Type) bool {
	o, ok := other.(*BasicType)
	return ok && b.Kind == o.Kind
}

func (t *TypeVar) eq(other Type) bool {
	o, ok := other.(*TypeVar)
	return ok && t.Def == o.Def
}
