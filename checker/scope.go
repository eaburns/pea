package checker

import (
	"strings"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
)

type scope interface {
	find(name string) []id
	findMod(name string) *Import
	findType(args []Type, name string, l loc.Loc) Type
}

type id interface {
	Type() Type
}

type blockLitScope struct {
	parent scope
	*BlockLit
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

func (b *blockLitScope) findMod(name string) *Import {
	return b.parent.findMod(name)
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

func (v *VarDef) findType(args []Type, name string, l loc.Loc) Type {
	return v.File.findType(args, name, l)
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

func (b *blockLitScope) findType(args []Type, name string, l loc.Loc) Type {
	return b.parent.findType(args, name, l)
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
		return &DefType{Name: name, Args: args, Def: d, L: l}
	}
	return nil
}

func (m *Mod) find(name string) []id {
	if name == "_" {
		return nil
	}
	ids := findInDefs(m.Defs, name)
	if strings.HasPrefix(name, ".") {
		// Add a template select type to be filled in with concrete types
		// or rejected when its 0th parameter is unified.
		ids = append(ids, &Select{
			Field: &FieldDef{Name: name},
			P:     uniqueTypeVar(),
			R:     uniqueTypeVar(),
		})
	}
	if strings.HasPrefix(name, "?") {
		// Add two template switches to be filled with concrete types
		// or rejected when their return or 0th parameter is unified.
		caseNames := splitCaseNames(name)
		n := len(caseNames)
		switches := [2]Switch{
			{Ps: make([]Type, n+1), Cases: make([]*CaseDef, n)},
			{Ps: make([]Type, n+1), R: uniqueTypeVar(), Cases: make([]*CaseDef, n)},
		}
		for i := 0; i < 2; i++ {
			for j, caseName := range caseNames {
				switches[i].Ps[j] = uniqueTypeVar()
				switches[i].Cases[j] = &CaseDef{Name: caseName}
			}
			switches[i].Ps[n] = uniqueTypeVar()
		}
		ids = append(ids, &switches[0], &switches[1])
	}
	return ids
}

func splitCaseNames(str string) []string {
	var i int
	var names []string
	for i < len(str) {
		r, w := utf8.DecodeRuneInString(str[i:])
		if r == '?' && i > 0 {
			names = append(names, str[:i])
			str = str[i:]
			i = w
			continue
		}
		i += w
	}
	return append(names, str)
}

func uniqueTypeVar() *TypeVar {
	return &TypeVar{Name: "_", Def: &TypeParm{Name: "_"}}
}

func (i *Import) find(name string) []id { return findInDefs(i.Defs, name) }

func (f *File) find(name string) []id { return f.Mod.find(name) }

func (v *VarDef) find(name string) []id { return v.File.find(name) }

func (t *TypeDef) find(name string) []id { panic("impossible") }

func (f *FuncDef) find(name string) []id {
	if name == "_" {
		return nil
	}
	for i := range f.Locals {
		if f.Locals[i].Name == name {
			return []id{&f.Locals[i]}
		}
	}
	for i := range f.Parms {
		if f.Parms[i].Name == name {
			return []id{&f.Parms[i]}
		}
	}
	return f.File.find(name)
}

func (t *TestDef) find(name string) []id {
	// TODO: tests need locals
	return t.File.find(name)
}

func (b *blockLitScope) find(name string) []id {
	if name == "_" {
		return nil
	}
	for i := range b.Locals {
		if b.Locals[i].Name == name {
			return []id{&b.Locals[i]}
		}
	}
	for i := range b.Parms {
		if b.Parms[i].Name == name {
			return []id{&b.Parms[i]}
		}
	}
	for i := range b.Caps {
		if b.Caps[i].Name == name {
			return []id{&b.Caps[i]}
		}
	}
	return b.parent.find(name)
}

func findInDefs(defs []Def, name string) []id {
	if name == "_" {
		return nil
	}
	var ids []id
	for _, def := range defs {
		switch def := def.(type) {
		case *VarDef:
			if def.Name == name {
				ids = append(ids, def)
			}
		case *FuncDef:
			if def.Name == name {
				ids = append(ids, def)
			}
		}
	}
	return ids
}
