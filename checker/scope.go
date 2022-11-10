package checker

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
)

type scope interface {
	up() scope
}

type blockLitScope struct {
	parent scope
	*BlockLit
}

// ifaceLookup tracks the number of times
// def is used during its own interface satisfaction.
// A function can take part in satisfying its own interface,
// but only to a limited recursion depth.
type ifaceLookup struct {
	parent scope
	def    *FuncDef
	inst   *FuncInst
}

type localScope struct {
	parent scope
	*LocalDef
}

// addedImportScope adds the IDs of the given Import
// to the current scope, even if the *Import is not Capital.
type addedImportScope struct {
	parent scope
	Import *Import
}

func (*Mod) up() scope                { return nil }
func (*Import) up() scope             { return nil }
func (f *File) up() scope             { return f.Mod }
func (v *VarDef) up() scope           { return v.File }
func (t *TypeDef) up() scope          { return t.File }
func (d *IfaceDef) up() scope         { return d.File }
func (f *FuncDef) up() scope          { return f.File }
func (t *TestDef) up() scope          { return t.File }
func (b *blockLitScope) up() scope    { return b.parent }
func (e *ifaceLookup) up() scope      { return e.parent }
func (o *localScope) up() scope       { return o.parent }
func (o *addedImportScope) up() scope { return o.parent }

func file(x scope) *File {
	for x != nil {
		if f, ok := x.(*File); ok {
			return f
		}
		x = x.up()
	}
	return nil
}

func findImport(x scope, modName string) *Import {
	f := file(x)
	if f == nil {
		return nil
	}
	for _, imp := range f.Imports {
		if imp.Name == modName {
			return imp
		}
	}
	return nil
}

// addImportScope returns a scope containing imp's definitions.
// If the definitions are already in-scope, they are not re-added.
func addImportScope(x0 scope, imp *Import) scope {
	if imp.Exp {
		// If this is already a Capital import, it is already added.
		// We do not need to re-add it.
		return x0
	}
	x := x0
	for x != nil {
		if ais, ok := x.(*addedImportScope); ok && ais.Import == imp {
			return x0
		}
		x = x.up()
	}
	return &addedImportScope{parent: x0, Import: imp}
}

// isModuleInScope returns whether the current scope
// already contains the definitions from the given module path.
func isModuleInScope(x scope, path string) bool {
	for x != nil {
		if m, ok := x.(*Mod); ok && m.Path == path {
			return true
		}
		if f, ok := x.(*File); ok {
			for _, imp := range f.Imports {
				if imp.Exp && imp.Path == path {
					return true
				}
			}
		}
		if ais, ok := x.(*addedImportScope); ok && ais.Import.Path == path {
			return true
		}
		x = x.up()
	}
	return false
}

func recursiveIfaceDepth(x scope, d *FuncDef) int {
	n := 0
	for x != nil {
		if f, ok := x.(*ifaceLookup); ok && f.def == d {
			n++
		}
		x = x.up()
	}
	return n
}

// seenIfaceInst returns whether inst is already
// under consideration for iface instantiation.
// If it is, re-considering it will simply be a loop.
// Without otherwise checking, the loop would
// be unrolled the max recursiveIfaceDepth,
// which can create exponential behavior.
// Instead, we find these obvious loops
// and cut them off immediately.
func seenIfaceInst(x scope, inst *FuncInst) bool {
	for x != nil {
		if f, ok := x.(*ifaceLookup); ok && f.inst.eq(inst) {
			return true
		}
		x = x.up()
	}
	return false
}

func findType(x0 scope, args []Type, name string, l loc.Loc) []Type {
	x := x0
	var types []Type
	for x != nil {
		if ft, ok := x.(interface {
			findType([]Type, string, loc.Loc) []Type
		}); ok {
			types = append(types, ft.findType(args, name, l)...)
		}
		x = x.up()
	}
	if len(types) > 0 {
		return types
	}

	// Types in the current module shadow Capital Imported types.
	// If there was no type in this module, check Capital Imported types.
	f := file(x0)
	if f == nil {
		return nil
	}
	for _, imp := range f.Imports {
		if imp.Exp {
			types = append(types, imp.findType(args, name, l)...)
		}
	}
	return types
}

func (m *Mod) findType(args []Type, name string, l loc.Loc) []Type {
	if t := findTypeInDefs(m.Defs, args, name, false, l); t != nil {
		return []Type{t}
	}
	if t := findBuiltInType(args, name, l); t != nil {
		return []Type{t}
	}
	return nil
}

func findBuiltInType(args []Type, name string, l loc.Loc) Type {
	if len(args) > 0 {
		return nil
	}
	switch name {
	case "!":
		return &BasicType{Kind: End, L: l}
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
	case "uintref":
		return &BasicType{Kind: UintRef, L: l}
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

func (i *Import) findType(args []Type, name string, l loc.Loc) []Type {
	if t := findTypeInDefs(i.Defs, args, name, true, l); t != nil {
		return []Type{t}
	}
	return nil
}

func findTypeInDefs(defs []Def, args []Type, name string, exportedOnly bool, l loc.Loc) Type {
	for _, def := range defs {
		d, ok := def.(*TypeDef)
		if !ok || d.Name != name || len(d.Parms) != len(args) || exportedOnly && !d.Exp {
			continue
		}
		return &DefType{Name: name, Args: args, Def: d, L: l}
	}
	return nil
}

func (t *TypeDef) findType(args []Type, name string, l loc.Loc) []Type {
	if typ := findTypeVar(t.Parms, args, name, l); typ != nil {
		return []Type{typ}
	}
	return nil
}

func (t *IfaceDef) findType(args []Type, name string, l loc.Loc) []Type {
	if typ := findTypeVar(t.Parms, args, name, l); typ != nil {
		return []Type{typ}
	}
	return nil
}

func (f *FuncDef) findType(args []Type, name string, l loc.Loc) []Type {
	if typ := findTypeVar(f.TypeParms, args, name, l); typ != nil {
		return []Type{typ}
	}
	return nil
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

func findIfaceDef(x0 scope, arity int, name string, l loc.Loc) []*IfaceDef {
	x := x0
	var ifaces []*IfaceDef
	for x != nil {
		if fi, ok := x.(interface {
			findIfaceDef(int, string, loc.Loc) *IfaceDef
		}); ok {
			iface := fi.findIfaceDef(arity, name, l)
			if iface != nil {
				ifaces = append(ifaces, iface)
			}
		}
		x = x.up()
	}
	if len(ifaces) > 0 {
		return ifaces
	}

	// IfaceDefs in the current module shadow Capital Imported IfaceDefs.
	// If there was no IfaceDef in this module, check Capital Imported ones.
	f := file(x0)
	if f == nil {
		return nil
	}
	for _, imp := range f.Imports {
		if imp.Exp {
			iface := imp.findIfaceDef(arity, name, l)
			if iface != nil {
				ifaces = append(ifaces, iface)
			}
		}
	}
	return ifaces
}

func (m *Mod) findIfaceDef(arity int, name string, l loc.Loc) *IfaceDef {
	if iface := findIfaceDefInDefs(m.Defs, arity, name, false, l); iface != nil {
		return iface
	}
	return nil
}

func (i *Import) findIfaceDef(arity int, name string, l loc.Loc) *IfaceDef {
	if iface := findIfaceDefInDefs(i.Defs, arity, name, true, l); iface != nil {
		return iface
	}
	return nil
}

func findIfaceDefInDefs(defs []Def, arity int, name string, exportedOnly bool, l loc.Loc) *IfaceDef {
	for _, def := range defs {
		d, ok := def.(*IfaceDef)
		if ok && d.Name == name && len(d.Parms) == arity && (!exportedOnly || d.Exp) {
			return d
		}
	}
	return nil
}

type id interface {
	String() string
	Type() Type
}

func findIDs(x scope, name string) []id {
	if name == "_" {
		return nil
	}
	if x == nil {
		return nil
	}
	if fi, ok := x.(interface{ findIDs(string) []id }); ok {
		return fi.findIDs(name)
	}
	return findIDs(x.up(), name)
}

func (m *Mod) findIDs(name string) []id {
	ids := findInDefs(m.Defs, name, false)
	if strings.HasPrefix(name, ".") {
		parms, vars := newAnyParmsAndTypeVars(2)
		ids = append(ids, &Select{
			N:         name,
			typeParms: parms,
			T: &FuncType{
				Parms: []Type{refLiteral(vars[0])},
				Ret:   refLiteral(vars[1]),
			},
		})
	}
	if strings.HasSuffix(name, "?") {
		names := splitCaseNames(name)
		defaultCases := 0
		for _, n := range names {
			if n == "_?" {
				defaultCases++
			}
		}
		// Multiple default cases are not supported.
		if defaultCases <= 1 {
			// One for each case, one for the union, and one for the return.
			parms, vars := newAnyParmsAndTypeVars(len(names) + 2)
			parmTypes := make([]Type, len(vars)-1)
			for i, v := range vars[1:] {
				parmTypes[i] = v
			}
			sw := Switch{
				N:         name,
				Names:     names,
				T:         &FuncType{Parms: parmTypes, Ret: vars[0]},
				typeParms: parms,
			}
			ids = append(ids, &sw)
		}
	}
	for i := range builtins {
		if builtins[i].N == name {
			b := builtins[i]
			b.Parms = append([]Type{}, b.Parms...) // copy
			if b.TypeParm != nil {
				// create a unique type parameter to replaced _P.
				b.TypeParm = &TypeParm{Name: "T"}
				bind := map[*TypeParm]Type{
					_P: &TypeVar{Name: b.TypeParm.Name, Def: b.TypeParm},
				}
				for i := range b.Parms {
					b.Parms[i] = subType(bind, b.Parms[i])
				}
				b.Ret = subType(bind, b.Ret)
			}
			ids = append(ids, &b)
		}
	}
	return ids
}

func newAnyParmsAndTypeVars(n int) ([]*TypeParm, []*TypeVar) {
	var parms []*TypeParm
	var vars []*TypeVar
	for i := 0; i < n; i++ {
		n := fmt.Sprintf("T%d", i)
		p := &TypeParm{Name: n}
		v := &TypeVar{Name: n, Def: p}
		parms = append(parms, p)
		vars = append(vars, v)
	}
	return parms, vars
}

var (
	// _P is a placeholder; before returning a Builtin from the scope,
	// it is always replaced with a brand new TypeParam.
	_P       = &TypeParm{Name: "T"}
	_T       = &TypeVar{Name: _P.Name, Def: _P}
	_end     = basic(End)
	_int     = basic(Int)
	_int8    = basic(Int8)
	_int16   = basic(Int16)
	_int32   = basic(Int32)
	_int64   = basic(Int64)
	_uint    = basic(Uint)
	_uint8   = basic(Uint8)
	_uint16  = basic(Uint16)
	_uint32  = basic(Uint32)
	_uint64  = basic(Uint64)
	_uintref = basic(UintRef)
	_float32 = basic(Float32)
	_float64 = basic(Float64)
	_bool    = &UnionType{
		Cases: []CaseDef{
			{Name: "false?"},
			{Name: "true?"},
		},
	}
	_ordering = &UnionType{
		Cases: []CaseDef{
			{Name: "less?"},
			{Name: "equal?"},
			{Name: "greater?"},
		},
	}
	_partialOrdering = &UnionType{
		Cases: []CaseDef{
			{Name: "less?"},
			{Name: "equal?"},
			{Name: "greater?"},
			{Name: "none?"},
		},
	}
	_string = basic(String)
	_empty  = &StructType{}

	builtins = []Builtin{
		{N: ":=", Op: Assign, TypeParm: _P, Parms: []Type{refLiteral(_T), _T}, Ret: _empty},
		{N: "new", Op: NewArray, TypeParm: _P, Parms: []Type{_int, _T}, Ret: arrayLiteral(_T)},

		{N: "^", Op: BitNot, Parms: []Type{_int}, Ret: _int},
		{N: "^", Op: BitXor, Parms: []Type{_int, _int}, Ret: _int},
		{N: "&", Op: BitAnd, Parms: []Type{_int, _int}, Ret: _int},
		{N: "|", Op: BitOr, Parms: []Type{_int, _int}, Ret: _int},
		{N: "<<", Op: LeftShift, Parms: []Type{_int, _int}, Ret: _int},
		{N: ">>", Op: RightShift, Parms: []Type{_int, _int}, Ret: _int},

		{N: "^", Op: BitNot, Parms: []Type{_int8}, Ret: _int8},
		{N: "^", Op: BitXor, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "&", Op: BitAnd, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "|", Op: BitOr, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "<<", Op: LeftShift, Parms: []Type{_int8, _int}, Ret: _int8},
		{N: ">>", Op: RightShift, Parms: []Type{_int8, _int}, Ret: _int8},

		{N: "^", Op: BitNot, Parms: []Type{_int16}, Ret: _int16},
		{N: "^", Op: BitXor, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "&", Op: BitAnd, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "|", Op: BitOr, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "<<", Op: LeftShift, Parms: []Type{_int16, _int}, Ret: _int16},
		{N: ">>", Op: RightShift, Parms: []Type{_int16, _int}, Ret: _int16},

		{N: "^", Op: BitNot, Parms: []Type{_int32}, Ret: _int32},
		{N: "^", Op: BitXor, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "&", Op: BitAnd, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "|", Op: BitOr, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "<<", Op: LeftShift, Parms: []Type{_int32, _int}, Ret: _int32},
		{N: ">>", Op: RightShift, Parms: []Type{_int32, _int}, Ret: _int32},

		{N: "^", Op: BitNot, Parms: []Type{_int64}, Ret: _int64},
		{N: "^", Op: BitXor, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "&", Op: BitAnd, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "|", Op: BitOr, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "<<", Op: LeftShift, Parms: []Type{_int64, _int}, Ret: _int64},
		{N: ">>", Op: RightShift, Parms: []Type{_int64, _int}, Ret: _int64},

		{N: "^", Op: BitNot, Parms: []Type{_uint}, Ret: _uint},
		{N: "^", Op: BitXor, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "&", Op: BitAnd, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "|", Op: BitOr, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "<<", Op: LeftShift, Parms: []Type{_uint, _int}, Ret: _uint},
		{N: ">>", Op: RightShift, Parms: []Type{_uint, _int}, Ret: _uint},

		{N: "^", Op: BitNot, Parms: []Type{_uint8}, Ret: _uint8},
		{N: "^", Op: BitXor, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "&", Op: BitAnd, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "|", Op: BitOr, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "<<", Op: LeftShift, Parms: []Type{_uint8, _int}, Ret: _uint8},
		{N: ">>", Op: RightShift, Parms: []Type{_uint8, _int}, Ret: _uint8},

		{N: "^", Op: BitNot, Parms: []Type{_uint16}, Ret: _uint16},
		{N: "^", Op: BitXor, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "&", Op: BitAnd, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "|", Op: BitOr, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "<<", Op: LeftShift, Parms: []Type{_uint16, _int}, Ret: _uint16},
		{N: ">>", Op: RightShift, Parms: []Type{_uint16, _int}, Ret: _uint16},

		{N: "^", Op: BitNot, Parms: []Type{_uint32}, Ret: _uint32},
		{N: "^", Op: BitXor, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "&", Op: BitAnd, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "|", Op: BitOr, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "<<", Op: LeftShift, Parms: []Type{_uint32, _int}, Ret: _uint32},
		{N: ">>", Op: RightShift, Parms: []Type{_uint32, _int}, Ret: _uint32},

		{N: "^", Op: BitNot, Parms: []Type{_uint64}, Ret: _uint64},
		{N: "^", Op: BitXor, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "&", Op: BitAnd, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "|", Op: BitOr, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "<<", Op: LeftShift, Parms: []Type{_uint64, _int}, Ret: _uint64},
		{N: ">>", Op: RightShift, Parms: []Type{_uint64, _int}, Ret: _uint64},

		{N: "-", Op: Negate, Parms: []Type{_int}, Ret: _int},
		{N: "-", Op: Minus, Parms: []Type{_int, _int}, Ret: _int},
		{N: "+", Op: Plus, Parms: []Type{_int, _int}, Ret: _int},
		{N: "*", Op: Times, Parms: []Type{_int, _int}, Ret: _int},
		{N: "/", Op: Divide, Parms: []Type{_int, _int}, Ret: _int},
		{N: "%", Op: Modulus, Parms: []Type{_int, _int}, Ret: _int},

		{N: "-", Op: Negate, Parms: []Type{_int8}, Ret: _int8},
		{N: "-", Op: Minus, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "+", Op: Plus, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "*", Op: Times, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "/", Op: Divide, Parms: []Type{_int8, _int8}, Ret: _int8},
		{N: "%", Op: Modulus, Parms: []Type{_int8, _int8}, Ret: _int8},

		{N: "-", Op: Negate, Parms: []Type{_int16}, Ret: _int16},
		{N: "-", Op: Minus, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "+", Op: Plus, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "*", Op: Times, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "/", Op: Divide, Parms: []Type{_int16, _int16}, Ret: _int16},
		{N: "%", Op: Modulus, Parms: []Type{_int16, _int16}, Ret: _int16},

		{N: "-", Op: Negate, Parms: []Type{_int32}, Ret: _int32},
		{N: "-", Op: Minus, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "+", Op: Plus, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "*", Op: Times, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "/", Op: Divide, Parms: []Type{_int32, _int32}, Ret: _int32},
		{N: "%", Op: Modulus, Parms: []Type{_int32, _int32}, Ret: _int32},

		{N: "-", Op: Negate, Parms: []Type{_int64}, Ret: _int64},
		{N: "-", Op: Minus, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "+", Op: Plus, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "*", Op: Times, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "/", Op: Divide, Parms: []Type{_int64, _int64}, Ret: _int64},
		{N: "%", Op: Modulus, Parms: []Type{_int64, _int64}, Ret: _int64},

		{N: "-", Op: Negate, Parms: []Type{_uint}, Ret: _uint},
		{N: "-", Op: Minus, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "+", Op: Plus, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "*", Op: Times, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "/", Op: Divide, Parms: []Type{_uint, _uint}, Ret: _uint},
		{N: "%", Op: Modulus, Parms: []Type{_uint, _uint}, Ret: _uint},

		{N: "-", Op: Negate, Parms: []Type{_uint8}, Ret: _uint8},
		{N: "-", Op: Minus, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "+", Op: Plus, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "*", Op: Times, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "/", Op: Divide, Parms: []Type{_uint8, _uint8}, Ret: _uint8},
		{N: "%", Op: Modulus, Parms: []Type{_uint8, _uint8}, Ret: _uint8},

		{N: "-", Op: Negate, Parms: []Type{_uint16}, Ret: _uint16},
		{N: "-", Op: Minus, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "+", Op: Plus, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "*", Op: Times, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "/", Op: Divide, Parms: []Type{_uint16, _uint16}, Ret: _uint16},
		{N: "%", Op: Modulus, Parms: []Type{_uint16, _uint16}, Ret: _uint16},

		{N: "-", Op: Negate, Parms: []Type{_uint32}, Ret: _uint32},
		{N: "-", Op: Minus, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "+", Op: Plus, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "*", Op: Times, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "/", Op: Divide, Parms: []Type{_uint32, _uint32}, Ret: _uint32},
		{N: "%", Op: Modulus, Parms: []Type{_uint32, _uint32}, Ret: _uint32},

		{N: "-", Op: Negate, Parms: []Type{_uint64}, Ret: _uint64},
		{N: "-", Op: Minus, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "+", Op: Plus, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "*", Op: Times, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "/", Op: Divide, Parms: []Type{_uint64, _uint64}, Ret: _uint64},
		{N: "%", Op: Modulus, Parms: []Type{_uint64, _uint64}, Ret: _uint64},

		{N: "-", Op: Negate, Parms: []Type{_float32}, Ret: _float32},
		{N: "-", Op: Minus, Parms: []Type{_float32, _float32}, Ret: _float32},
		{N: "+", Op: Plus, Parms: []Type{_float32, _float32}, Ret: _float32},
		{N: "*", Op: Times, Parms: []Type{_float32, _float32}, Ret: _float32},
		{N: "/", Op: Divide, Parms: []Type{_float32, _float32}, Ret: _float32},
		{N: "%", Op: Modulus, Parms: []Type{_float32, _float32}, Ret: _float32},

		{N: "-", Op: Negate, Parms: []Type{_float64}, Ret: _float64},
		{N: "-", Op: Minus, Parms: []Type{_float64, _float64}, Ret: _float64},
		{N: "+", Op: Plus, Parms: []Type{_float64, _float64}, Ret: _float64},
		{N: "*", Op: Times, Parms: []Type{_float64, _float64}, Ret: _float64},
		{N: "/", Op: Divide, Parms: []Type{_float64, _float64}, Ret: _float64},
		{N: "%", Op: Modulus, Parms: []Type{_float64, _float64}, Ret: _float64},

		{N: "=", Op: Eq, Parms: []Type{_int, _int}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_int, _int}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_int8, _int8}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_int8, _int8}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_int16, _int16}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_int16, _int16}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_int32, _int32}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_int32, _int32}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_int64, _int64}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_int64, _int64}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_uint, _uint}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_uint, _uint}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_uint8, _uint8}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_uint8, _uint8}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_uint16, _uint16}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_uint16, _uint16}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_uint32, _uint32}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_uint32, _uint32}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_uint64, _uint64}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_uint64, _uint64}, Ret: _ordering},

		{N: "=", Op: Eq, Parms: []Type{_uintref, _uintref}, Ret: _bool},
		// uintref has no <=> operator.

		{N: "=", Op: Eq, Parms: []Type{_float32, _float32}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_float32, _float32}, Ret: _partialOrdering},

		{N: "=", Op: Eq, Parms: []Type{_float64, _float64}, Ret: _bool},
		{N: "<=>", Op: Cmp, Parms: []Type{_float64, _float64}, Ret: _partialOrdering},

		{N: "[]", Op: Index, TypeParm: _P, Parms: []Type{arrayLiteral(_T), _int}, Ret: refLiteral(_T)},
		{N: "[]", Op: Slice, TypeParm: _P, Parms: []Type{arrayLiteral(_T), _int, _int}, Ret: arrayLiteral(_T)},
		{N: "[]", Op: Index, Parms: []Type{_string, _int}, Ret: _uint8},
		{N: "[]", Op: Slice, Parms: []Type{_string, _int, _int}, Ret: _string},
		{N: ".length", Op: Length, TypeParm: _P, Parms: []Type{arrayLiteral(_T)}, Ret: _int},
		{N: ".length", Op: Length, Parms: []Type{_string}, Ret: _int},

		{N: "panic", Op: Panic, Parms: []Type{_string}, Ret: _end},
		{N: "print", Op: Print, Parms: []Type{_string}, Ret: _empty},
	}
)

func basic(k BasicTypeKind) Type { return &BasicType{Kind: k} }

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

func (i *Import) findIDs(name string) []id {
	return findInDefs(i.Defs, name, true)
}

func (f *File) findIDs(name string) []id {
	ids := findIDs(f.Mod, name)
	for _, imp := range f.Imports {
		if imp.Exp {
			ids = append(ids, findIDs(imp, name)...)
		}
	}
	return ids
}

func (f *FuncDef) findIDs(name string) []id {
	for i := range f.Parms {
		if f.Parms[i].Name == name {
			return []id{&f.Parms[i]}
		}
	}
	ids := findIDs(f.File, name)
nextIface:
	for i := range f.Iface {
		iface := &f.Iface[i]
		if iface.Name != name {
			continue
		}

		// If there was an error checking this interface, don't return it.
		if iface.Ret == nil {
			continue
		}
		for _, p := range iface.Parms {
			if p == nil {
				continue nextIface
			}
		}

		ids = append(ids, iface)
	}
	if name == "return:" {
		ids = append(ids, &Builtin{
			Op:    Return,
			Parms: []Type{f.Ret},
			Ret:   &BasicType{Kind: End, L: f.L},
		})
	}
	if name == "return" && isEmptyStruct(f.Ret) {
		ids = append(ids, &Builtin{
			Op:  Return,
			Ret: &BasicType{Kind: End, L: f.L},
		})
	}
	return ids
}

func (b *blockLitScope) findIDs(name string) []id {
	if name == "_" {
		return nil
	}
	for i := range b.Parms {
		if b.Parms[i].Name == name {
			return []id{&b.Parms[i]}
		}
	}
	return findIDs(b.parent, name)
}

func (o *localScope) findIDs(name string) []id {
	if o.Name == name {
		return []id{o.LocalDef}
	}
	return findIDs(o.parent, name)
}

func (x *addedImportScope) findIDs(name string) []id {
	ids := findIDs(x.parent, name)
	return append(ids, findInDefs(x.Import.Defs, name, true)...)
}

func findInDefs(defs []Def, name string, exportedOnly bool) []id {
	var ids []id
nextDef:
	for _, def := range defs {
		switch def := def.(type) {
		case *VarDef:
			if exportedOnly && !def.Exp {
				continue
			}
			if def.Name == name {
				ids = append(ids, def)
			}
		case *FuncDef:
			if exportedOnly && !def.Exp {
				continue
			}
			if def.Name != name {
				continue
			}

			// If there was an error checking this function's signature,
			// Don't return it.
			if def.Ret == nil {
				continue
			}
			for _, p := range def.Parms {
				if p.T == nil {
					continue nextDef
				}
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

func capture(x scope, i id) id {
	if x == nil {
		return i
	}
	if c, ok := x.(interface{ capture(id) id }); ok {
		return c.capture(i)
	}
	return capture(x.up(), i)
}

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

	switch id := capture(b.parent, id).(type) {
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

func (o *localScope) capture(id id) id {
	if l, ok := id.(*LocalDef); ok && l == o.LocalDef {
		return o.LocalDef
	}
	return capture(o.parent, id)
}

func newLocal(x scope, name string, typ Type, l loc.Loc) *LocalDef {
	for x != nil {
		if nl, ok := x.(interface {
			newLocal(string, Type, loc.Loc) *LocalDef
		}); ok {
			return nl.newLocal(name, typ, l)
		}
		x = x.up()
	}
	return nil
}

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

func useVar(x scope, l loc.Loc, v *VarDef) {
	for x != nil {
		if uv, ok := x.(interface {
			useVar(loc.Loc, *VarDef)
		}); ok {
			uv.useVar(l, v)
			return
		}
		x = x.up()
	}
}

func (v *VarDef) useVar(l loc.Loc, used *VarDef) {
	for _, u := range v.usedVars {
		if u.Var == used {
			return
		}
	}
	v.usedVars = append(v.usedVars, varUse{Var: used, L: l})
}

func (f *FuncDef) useVar(l loc.Loc, used *VarDef) {
	for _, u := range f.usedVars {
		if u.Var == used {
			return
		}
	}
	f.usedVars = append(f.usedVars, varUse{Var: used, L: l})
}

func useFuncInst(x scope, l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	for x != nil {
		if uf, ok := x.(interface {
			useFuncInst(loc.Loc, *FuncDef, *FuncDecl, *FuncDef)
		}); ok {
			uf.useFuncInst(l, def, parm, arg)
			return
		}
		x = x.up()
	}
}

func (v *VarDef) useFuncInst(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	for _, c := range v.usedFuncs {
		if c.Func == def && c.Parm == parm && c.Arg == arg {
			return
		}
	}
	v.usedFuncs = append(v.usedFuncs, funcUse{L: l, Func: def, Parm: parm, Arg: arg})
}

func (f *FuncDef) useFuncInst(l loc.Loc, def *FuncDef, parm *FuncDecl, arg *FuncDef) {
	for _, c := range f.usedFuncs {
		if c.Func == def && c.Parm == parm && c.Arg == arg {
			return
		}
	}
	f.usedFuncs = append(f.usedFuncs, funcUse{L: l, Func: def, Parm: parm, Arg: arg})
}
