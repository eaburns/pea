package checker

import "github.com/eaburns/pea/loc"

type scope interface {
	find(key string) []Def
	findType(args []Type, name string, l loc.Loc) Type
}

func (m *Mod) find(key string) []Def {
	return findInDefs(m.Defs, key)
}

func (i *Import) find(key string) []Def {
	return findInDefs(i.Defs, key)
}

func (f *File) find(key string) []Def {
	ds := f.Mod.find(key)
	for _, i := range f.Imports {
		if i.Exp {
			ds = append(ds, i.find(key)...)
		}
	}
	return ds
}

func (t *TypeDef) find(key string) []Def {
	return t.File.find(key)
}

func (f *FuncDef) find(key string) []Def {
	// TODO: lookup parms and iface decls.
	return f.File.find(key)
}

func findInDefs(defs []Def, key string) []Def {
	var matches []Def
	for _, def := range defs {
		if def.key() == key {
			matches = append(matches, def)
		}
	}
	return matches
}

func (i *Import) key() string { return i.Name }
func (v *VarDef) key() string { return v.Name }
func (*TypeDef) key() string  { panic("impossible") }

// TODO: FuncDef key should allow keywords specified in any order.
func (f *FuncDef) key() string { return f.Name }

func findBuiltInType(args []Type, name string, l loc.Loc) Type {
	if len(args) > 0 {
		return nil
	}
	switch name {
	case "bool":
		return &BasicType{Kind: Bool, L: l}
	case "int":
		return &BasicType{Kind: Int, L: l}
	case "int8":
		return &BasicType{Kind: Int8, L: l}
	case "int16":
		return &BasicType{Kind: Int16, L: l}
	case "int32":
		return &BasicType{Kind: Int32, L: l}
	case "int64":
		return &BasicType{Kind: Int64, L: l}
	case "uint":
		return &BasicType{Kind: Uint, L: l}
	case "uint8":
		return &BasicType{Kind: Uint8, L: l}
	case "uint16":
		return &BasicType{Kind: Uint16, L: l}
	case "uint32":
		return &BasicType{Kind: Uint32, L: l}
	case "uint64":
		return &BasicType{Kind: Uint64, L: l}
	case "float32":
		return &BasicType{Kind: Float32, L: l}
	case "float64":
		return &BasicType{Kind: Float64, L: l}
	case "string":
		return &BasicType{Kind: String, L: l}
	}
	return nil
}

func (m *Mod) findType(args []Type, name string, l loc.Loc) Type {
	if t := findTypeInDefs(m.Defs, args, name, l); t != nil {
		return t
	}
	return findBuiltInType(args, name, l)
}

func (i *Import) findType(args []Type, name string, l loc.Loc) Type {
	if t := findTypeInDefs(i.Defs, args, name, l); t != nil {
		return t
	}
	return nil
}

func (f *File) findType(args []Type, name string, l loc.Loc) Type {
	return f.Mod.findType(args, name, l)
}

func (t *TypeDef) findType(args []Type, name string, l loc.Loc) Type {
	if typ := findTypeVar(t.Parms, args, name, l); typ != nil {
		return typ
	}
	return t.File.findType(args, name, l)
}

func (f *FuncDef) findType(args []Type, name string, l loc.Loc) Type {
	if typ := findTypeVar(f.TypeParms, args, name, l); typ != nil {
		return typ
	}
	return f.File.findType(args, name, l)
}

func findTypeVar(parms []TypeParm, args []Type, name string, l loc.Loc) *TypeVar {
	if len(args) != 0 {
		return nil
	}
	for i := range parms {
		if parms[i].Name == name {
			return &TypeVar{Name: name, Def: &parms[i], L: l}
		}
	}
	return nil
}

func findTypeInDefs(defs []Def, args []Type, name string, l loc.Loc) Type {
	for _, def := range defs {
		d, ok := def.(*TypeDef)
		if !ok || d.Name != name || len(d.Parms) != len(args) {
			continue
		}
		return &NamedType{
			Name: name,
			Args: args,
			Def:  d,
			L:    l,
		}
	}
	return nil
}
