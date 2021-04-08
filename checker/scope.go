package checker

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
)

type scope interface {
	find(name string) []id
	findMod(name string) *Import
	findType(args []Type, name string, l loc.Loc) []Type
	capture(id) id
	useVar(loc.Loc, *VarDef)
	useFunc(loc.Loc, *FuncDef, *FuncDecl, *FuncDef)
	newLocal(string, Type, loc.Loc) *LocalDef
}

type id interface {
	String() string
	Type() Type
}

type blockLitScope struct {
	parent scope
	*BlockLit
}

type excludeFunc struct {
	parent scope
	def    *FuncDef
	notes  *[]note
}

type localScope struct {
	parent scope
	*LocalDef
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

func (v *VarDef) findMod(name string) *Import        { return v.File.findMod(name) }
func (t *TypeDef) findMod(name string) *Import       { return t.File.findMod(name) }
func (f *FuncDef) findMod(name string) *Import       { return f.File.findMod(name) }
func (t *TestDef) findMod(name string) *Import       { return t.File.findMod(name) }
func (b *blockLitScope) findMod(name string) *Import { return b.parent.findMod(name) }
func (e *excludeFunc) findMod(name string) *Import   { return e.parent.findMod(name) }
func (o *localScope) findMod(name string) *Import    { return o.parent.findMod(name) }

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

func (m *Mod) findType(args []Type, name string, l loc.Loc) []Type {
	if t := findTypeInDefs(m.Defs, args, name, l); t != nil {
		return []Type{t}
	}
	if t := findBuiltInType(args, name, l); t != nil {
		return []Type{t}
	}
	return nil
}

func (i *Import) findType(args []Type, name string, l loc.Loc) []Type {
	if t := findTypeInDefs(i.Defs, args, name, l); t != nil {
		return []Type{t}
	}
	return nil
}

func (f *File) findType(args []Type, name string, l loc.Loc) []Type {
	if t := f.Mod.findType(args, name, l); len(t) > 0 {
		return t
	}
	var types []Type
	for _, imp := range f.Imports {
		if imp.Exp {
			types = append(types, imp.findType(args, name, l)...)
		}
	}
	return types
}

func (v *VarDef) findType(args []Type, name string, l loc.Loc) []Type {
	return v.File.findType(args, name, l)
}

func (t *TypeDef) findType(args []Type, name string, l loc.Loc) []Type {
	if typ := findTypeVar(t.Parms, args, name, l); typ != nil {
		return []Type{typ}
	}
	return t.File.findType(args, name, l)
}

func (f *FuncDef) findType(args []Type, name string, l loc.Loc) []Type {
	if typ := findTypeVar(f.TypeParms, args, name, l); typ != nil {
		return []Type{typ}
	}
	return f.File.findType(args, name, l)
}

func (t *TestDef) findType(args []Type, name string, l loc.Loc) []Type {
	return t.File.findType(args, name, l)
}

func (b *blockLitScope) findType(args []Type, name string, l loc.Loc) []Type {
	return b.parent.findType(args, name, l)
}

func (e *excludeFunc) findType(args []Type, name string, l loc.Loc) []Type {
	return e.parent.findType(args, name, l)
}

func (o *localScope) findType(args []Type, name string, l loc.Loc) []Type {
	return o.parent.findType(args, name, l)
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
		})
	}
	if strings.HasSuffix(name, "?") {
		// Add two template switches to be filled with concrete types
		// or rejected when their return or 0th parameter is unified.
		caseNames := splitCaseNames(name)
		n := len(caseNames)
		sw := Switch{
			Cases: make([]*CaseDef, n),
			Parms: make([]Type, n+1),
		}
		for i, caseName := range caseNames {
			sw.Cases[i] = &CaseDef{Name: caseName}
		}
		ids = append(ids, &sw)
	}
	for _, binfo := range builtins {
		if binfo.name != name {
			continue
		}
		b := &Builtin{Op: binfo.op}
		for _, p := range binfo.parms {
			b.Parms = append(b.Parms, p)
		}
		b.Ret = binfo.ret
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
	{name: "new", op: NewArray, parms: []Type{basic(Int), nil}, ret: nil},
	{name: "^", op: BitNot, parms: []Type{nil}, ret: nil},
	{name: "^", op: BitXor, parms: []Type{nil, nil}, ret: nil},
	{name: "&", op: BitAnd, parms: []Type{nil, nil}, ret: nil},
	{name: "|", op: BitOr, parms: []Type{nil, nil}, ret: nil},
	{name: "<<", op: LeftShift, parms: []Type{nil, basic(Int)}, ret: nil},
	{name: ">>", op: RightShift, parms: []Type{nil, basic(Int)}, ret: nil},
	{name: "-", op: Negate, parms: []Type{nil}, ret: nil},
	{name: "-", op: Minus, parms: []Type{nil, nil}, ret: nil},
	{name: "+", op: Plus, parms: []Type{nil, nil}, ret: nil},
	{name: "*", op: Times, parms: []Type{nil, nil}, ret: nil},
	{name: "/", op: Divide, parms: []Type{nil, nil}, ret: nil},
	{name: "%", op: Modulus, parms: []Type{nil, nil}, ret: nil},
	{name: "=", op: Eq, parms: []Type{nil, nil}, ret: basic(Bool)},
	{name: "!=", op: Neq, parms: []Type{nil, nil}, ret: basic(Bool)},
	{name: "<", op: Less, parms: []Type{nil, nil}, ret: basic(Bool)},
	{name: "<=", op: LessEq, parms: []Type{nil, nil}, ret: basic(Bool)},
	{name: ">", op: Greater, parms: []Type{nil, nil}, ret: basic(Bool)},
	{name: ">=", op: GreaterEq, parms: []Type{nil, nil}, ret: basic(Bool)},
	{name: "[]", op: Index, parms: []Type{nil, basic(Int)}, ret: nil},
	{name: "[]", op: Slice, parms: []Type{nil, basic(Int), basic(Int)}, ret: nil},
	{name: ".length", op: Length, parms: []Type{nil}, ret: basic(Int)},
	{name: "panic", op: Panic, parms: []Type{basic(String)}, ret: &StructType{}},
	{name: "print", op: Print, parms: []Type{basic(String)}, ret: &StructType{}},
}

func basic(k BasicTypeKind) Type { return &BasicType{Kind: k} }
func byteArray() Type            { return &ArrayType{ElemType: basic(Uint8)} }

func splitCaseNames(str string) []string {
	var i int
	var names []string
	for i < len(str) {
		r, w := utf8.DecodeRuneInString(str[i:])
		i += w
		if r == '?' {
			names = append(names, str[:i])
			str = str[i:]
			i = 0
		}
	}
	return names
}

func (i *Import) find(name string) []id { return findInDefs(i.Defs, name) }

func (f *File) find(name string) []id {
	ids := f.Mod.find(name)
	for _, imp := range f.Imports {
		if imp.Exp {
			ids = append(ids, imp.find(name)...)
		}
	}
	return ids
}

func (v *VarDef) find(name string) []id { return v.File.find(name) }

func (t *TypeDef) find(name string) []id { panic("impossible") }

func (f *FuncDef) find(name string) []id {
	if name == "_" {
		return nil
	}
	for i := range f.Parms {
		if f.Parms[i].Name == name {
			return []id{&f.Parms[i]}
		}
	}
	ids := f.File.find(name)
	for i := range f.Iface {
		if f.Iface[i].Name == name {
			ids = append(ids, &f.Iface[i])
		}
	}
	if name == "return:" {
		ids = append(ids, &Builtin{
			Op:    Return,
			Parms: []Type{f.Ret},
			Ret:   &StructType{L: f.L},
		})
	}
	if name == "return" && isEmptyStruct(f.Ret) {
		ids = append(ids, &Builtin{
			Op:  Return,
			Ret: &StructType{L: f.L},
		})
	}
	return ids
}

func (t *TestDef) find(name string) []id {
	return t.File.find(name)
}

func (b *blockLitScope) find(name string) []id {
	if name == "_" {
		return nil
	}
	for i := range b.Parms {
		if b.Parms[i].Name == name {
			return []id{&b.Parms[i]}
		}
	}
	return b.parent.find(name)
}

func (e *excludeFunc) find(name string) []id {
	ids := e.parent.find(name)
	var n int
	for _, id := range ids {
		f, ok := id.(*FuncInst)
		if ok && f.Def == e.def {
			*e.notes = append(*e.notes, newNote("%s: is excluded from the scope", f).setLoc(f.Def))
			continue
		}
		ids[n] = id
		n++
	}
	return ids[:n]
}

func (o *localScope) find(name string) []id {
	ids := o.parent.find(name)
	if o.Name == name {
		ids = append(ids, o.LocalDef)
	}
	return ids
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
			if def.Name != name {
				continue
			}
			var typeArgs []Type
			for i := range def.TypeParms {
				typeArgs = append(typeArgs, &TypeVar{
					Name: def.TypeParms[i].Name,
					Def:  &def.TypeParms[i],
					L:    def.TypeParms[i].L,
				})
			}
			ids = append(ids, newFuncInst(def, typeArgs, def.L))
		}
	}
	return ids
}

func newFuncInst(def *FuncDef, typeArgs []Type, l loc.Loc) *FuncInst {
	sub := make(map[*TypeParm]Type)
	for i := range def.TypeParms {
		sub[&def.TypeParms[i]] = typeArgs[i]
	}
	var parms []Type
	for _, p := range def.Parms {
		parms = append(parms, subType(sub, p.T))
	}
	ret := subType(sub, def.Ret)
	var ifaceArgs []Func
	for i := range def.Iface {
		ifaceArgs = append(ifaceArgs, subFuncDecl(sub, &def.Iface[i]))
	}
	typ := &FuncType{Parms: parms, Ret: ret, L: l}
	inst := &FuncInst{
		TypeArgs:  typeArgs,
		IfaceArgs: ifaceArgs,
		T:         typ,
		Def:       def,
	}
	return inst
}

func subFuncDecl(sub map[*TypeParm]Type, decl *FuncDecl) *FuncDecl {
	var parms []Type
	for _, p := range decl.Parms {
		parms = append(parms, subType(sub, p))
	}
	return &FuncDecl{
		Name:  decl.Name,
		Parms: parms,
		Ret:   subType(sub, decl.Ret),
		L:     decl.L,
	}
}

func (*Mod) capture(id id) id       { return id }
func (*Import) capture(id id) id    { return id }
func (f *File) capture(id id) id    { return id }
func (v *VarDef) capture(id id) id  { return id }
func (t *TypeDef) capture(id id) id { return id }
func (f *FuncDef) capture(id id) id { return id }
func (t *TestDef) capture(id id) id { panic("impossible") }

func (b *blockLitScope) capture(id id) id {
	switch id := id.(type) {
	case *ParmDef:
		for i := range b.Parms {
			if id == &b.Parms[i] {
				return id
			}
		}
	case *LocalDef:
		for _, l := range b.Locals {
			if id == l {
				return id
			}
		}
	case *BlockCap:
		// Since find() never returns a *BlockCap, we can never hit this case.
		panic("impossible")
	default:
		return id
	}

	switch id := b.parent.capture(id).(type) {
	case *ParmDef:
		for _, c := range b.Caps {
			if c.Parm == id {
				return c
			}
		}
		c := &BlockCap{Name: id.Name, T: id.T, L: id.L, Parm: id}
		b.Caps = append(b.Caps, c)
		return c
	case *LocalDef:
		for _, c := range b.Caps {
			if c.Local == id {
				return c
			}
		}
		c := &BlockCap{Name: id.Name, T: id.T, L: id.L, Local: id}
		b.Caps = append(b.Caps, c)
		return c
	case *BlockCap:
		for _, c := range b.Caps {
			if c.Cap == id {
				return c
			}
		}
		c := &BlockCap{Name: id.Name, T: id.T, L: id.L, Cap: id}
		b.Caps = append(b.Caps, c)
		return c
	default:
		panic(fmt.Sprintf("impossible capture type: %T", id))
	}
}

func (e *excludeFunc) capture(id id) id { return e.parent.capture(id) }

func (o *localScope) capture(id id) id {
	if l, ok := id.(*LocalDef); ok && l == o.LocalDef {
		return o.LocalDef
	}
	return o.parent.capture(id)
}

func (*Mod) newLocal(string, Type, loc.Loc) *LocalDef                 { return nil }
func (*Import) newLocal(string, Type, loc.Loc) *LocalDef              { return nil }
func (f *File) newLocal(string, Type, loc.Loc) *LocalDef              { return nil }
func (v *VarDef) newLocal(name string, typ Type, l loc.Loc) *LocalDef { return nil }
func (t *TypeDef) newLocal(string, Type, loc.Loc) *LocalDef           { return nil }

func (f *FuncDef) newLocal(name string, typ Type, l loc.Loc) *LocalDef {
	local := &LocalDef{Name: name, T: typ, L: l}
	f.Locals = append(f.Locals, local)
	return local
}

func (t *TestDef) newLocal(name string, typ Type, l loc.Loc) *LocalDef {
	local := &LocalDef{Name: name, T: typ, L: l}
	t.Locals = append(t.Locals, local)
	return local
}

func (b *blockLitScope) newLocal(name string, typ Type, l loc.Loc) *LocalDef {
	local := &LocalDef{Name: name, T: typ, L: l}
	b.Locals = append(b.Locals, local)
	return local
}

func (e *excludeFunc) newLocal(name string, typ Type, l loc.Loc) *LocalDef {
	return e.parent.newLocal(name, typ, l)
}

func (o *localScope) newLocal(name string, typ Type, l loc.Loc) *LocalDef {
	return o.parent.newLocal(name, typ, l)
}

func (*Mod) useVar(loc.Loc, *VarDef)    {}
func (*Import) useVar(loc.Loc, *VarDef) {}
func (*File) useVar(loc.Loc, *VarDef)   {}

func (v *VarDef) useVar(l loc.Loc, used *VarDef) {
	for _, u := range v.usedVars {
		if u.Var == used {
			return
		}
	}
	v.usedVars = append(v.usedVars, varUse{Var: used, L: l})
}

func (t *TypeDef) useVar(loc.Loc, *VarDef) {}

func (f *FuncDef) useVar(l loc.Loc, used *VarDef) {
	for _, u := range f.usedVars {
		if u.Var == used {
			return
		}
	}
	f.usedVars = append(f.usedVars, varUse{Var: used, L: l})
}

func (t *TestDef) useVar(loc.Loc, *VarDef)           {}
func (b *blockLitScope) useVar(l loc.Loc, v *VarDef) { b.parent.useVar(l, v) }
func (e *excludeFunc) useVar(l loc.Loc, v *VarDef)   { e.parent.useVar(l, v) }
func (o *localScope) useVar(l loc.Loc, v *VarDef)    { o.parent.useVar(l, v) }

func (*Mod) useFunc(loc.Loc, *FuncDef, *FuncDecl, *FuncDef)    {}
func (*Import) useFunc(loc.Loc, *FuncDef, *FuncDecl, *FuncDef) {}
func (*File) useFunc(loc.Loc, *FuncDef, *FuncDecl, *FuncDef)   {}

func (v *VarDef) useFunc(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	for _, c := range v.usedFuncs {
		if c.Func == def && c.Parm == parm && c.Arg == arg {
			return
		}
	}
	v.usedFuncs = append(v.usedFuncs, funcUse{L: l, Func: def, Parm: parm, Arg: arg})
}

func (t *TypeDef) useFunc(loc.Loc, *FuncDef, *FuncDecl, *FuncDef) {}

func (f *FuncDef) useFunc(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	for _, c := range f.usedFuncs {
		if c.Func == def && c.Parm == parm && c.Arg == arg {
			return
		}
	}
	f.usedFuncs = append(f.usedFuncs, funcUse{L: l, Func: def, Parm: parm, Arg: arg})
}

func (t *TestDef) useFunc(loc.Loc, *FuncDef, *FuncDecl, *FuncDef) {}

func (b *blockLitScope) useFunc(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	b.parent.useFunc(l, def, parm, arg)
}

func (e *excludeFunc) useFunc(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	e.parent.useFunc(l, def, parm, arg)
}

func (o *localScope) useFunc(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	o.parent.useFunc(l, def, parm, arg)
}
