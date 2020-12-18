package checker

import (
	"github.com/eaburns/pea/loc"
)

type scope interface {
	findMod(name string) *Import
	findType(args []Type, name string, l loc.Loc) Type
}

func (*Mod) findMod(string) *Import    { return nil }
func (*Import) findMod(string) *Import { return nil }

func (f *File) findMod(name string) *Import {
	for _, imp := range f.Imports {
		if imp.Name == name {
			return imp
		}
	}
	return f.Mod.findMod(name)
}

func (v *VarDef) findMod(name string) *Import {
	return v.File.findMod(name)
}

func (t *TypeDef) findMod(name string) *Import {
	return t.File.findMod(name)
}

func (f *FuncDef) findMod(name string) *Import {
	return f.File.findMod(name)
}

func (t *TestDef) findMod(name string) *Import {
	return t.File.findMod(name)
}

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

func (t *TestDef) findType(args []Type, name string, l loc.Loc) Type {
	return t.File.findType(args, name, l)
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
