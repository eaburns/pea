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
		sw := Switch{
			Cases: make([]*CaseDef, n),
			Ps:    make([]Type, n+1),
			R:     uniqueTypeVar(),
		}
		for i, caseName := range caseNames {
			sw.Ps[i] = uniqueTypeVar()
			sw.Cases[i] = &CaseDef{Name: caseName}
		}
		sw.Ps[n] = uniqueTypeVar()
		ids = append(ids, &sw)
	}
	for _, binfo := range builtins {
		if binfo.name != name {
			continue
		}
		b := &Builtin{Op: binfo.op}
		for _, p := range binfo.parms {
			if p == nil {
				p = uniqueTypeVar()
			}
			b.Ps = append(b.Ps, p)
		}
		b.R = binfo.ret
		if b.R == nil {
			b.R = uniqueTypeVar()
		}
		ids = append(ids, b)
	}
	return ids
}

type builtin struct {
	name  string
	op    Op
	parms []Type // nil is _
	ret   Type   // nil is _
}

var builtins = []builtin{
	{name: ":=", op: Assign, parms: []Type{nil, nil}, ret: nil},
	{name: "new", op: NewArray, parms: []Type{intType(), nil}, ret: nil},
	{name: "^", op: BitNot, parms: []Type{nil}, ret: nil},
	{name: "^", op: BitXor, parms: []Type{nil, nil}, ret: nil},
	{name: "&", op: BitAnd, parms: []Type{nil, nil}, ret: nil},
	{name: "|", op: BitOr, parms: []Type{nil, nil}, ret: nil},
	{name: "<<", op: LeftShift, parms: []Type{nil, intType()}, ret: nil},
	{name: ">>", op: RightShift, parms: []Type{nil, intType()}, ret: nil},
	{name: "-", op: Negate, parms: []Type{nil}, ret: nil},
	{name: "-", op: Minus, parms: []Type{nil, nil}, ret: nil},
	{name: "+", op: Plus, parms: []Type{nil, nil}, ret: nil},
	{name: "*", op: Times, parms: []Type{nil, nil}, ret: nil},
	{name: "/", op: Divide, parms: []Type{nil, nil}, ret: nil},
	{name: "%", op: Modulus, parms: []Type{nil, nil}, ret: nil},
	{name: "=", op: Eq, parms: []Type{nil, nil}, ret: boolType()},
	{name: "!=", op: Neq, parms: []Type{nil, nil}, ret: boolType()},
	{name: "<", op: Less, parms: []Type{nil, nil}, ret: boolType()},
	{name: "<=", op: LessEq, parms: []Type{nil, nil}, ret: boolType()},
	{name: ">", op: Greater, parms: []Type{nil, nil}, ret: boolType()},
	{name: ">=", op: GreaterEq, parms: []Type{nil, nil}, ret: boolType()},
	{name: "int", op: NumConvert, parms: []Type{nil}},
	{name: "int8", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "int16", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "int32", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "int64", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "uint", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "uint8", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "uint16", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "uint32", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "uint64", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "float32", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "float64", op: NumConvert, parms: []Type{nil}, ret: nil},
	{name: "string", op: StrConvert, parms: []Type{byteArray()}, ret: stringType()},
	{name: "[]", op: Index, parms: []Type{nil, intType()}},
	{name: "[]", op: Slice, parms: []Type{nil, intType(), intType()}},
	{name: ".length", op: Length, parms: []Type{nil}, ret: intType()},
	{name: "panic", op: Panic, parms: []Type{stringType()}, ret: &StructType{}},
	{name: "print", op: Print, parms: []Type{stringType()}, ret: &StructType{}},
}

func boolType() Type   { return &BasicType{Kind: Bool} }
func intType() Type    { return &BasicType{Kind: Int} }
func stringType() Type { return &BasicType{Kind: String} }
func byteArray() Type  { return &ArrayType{ElemType: &BasicType{Kind: Uint8}} }

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
