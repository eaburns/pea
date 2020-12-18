package checker

import (
	"math/big"
	"path/filepath"
	"strings"

	"github.com/eaburns/pea/loc"
)

type Mod struct {
	Path     string
	Imported bool
	Files    []*File
	Defs     []Def
}

func (m *Mod) Name() string {
	return filepath.Base(m.Path)
}

type File struct {
	path    string
	nls     []int
	len     int
	Mod     *Mod
	Imports []*Import
}

func (f *File) Path() string    { return f.path }
func (f *File) NewLines() []int { return f.nls }
func (f *File) Len() int        { return f.len }

type Def interface {
}

type Import struct {
	Name string
	Path string
	Defs []Def
	Exp  bool
	L    loc.Loc
}

type VarDef struct {
	File  *File
	Mod   string
	Name  string
	Type  Type
	Expr  Expr
	Const bool
	Exp   bool
	L     loc.Loc
}

type TypeDef struct {
	File  *File
	Alias bool
	Mod   string
	Name  string
	Parms []TypeParm
	Type  Type
	Exp   bool
	Insts []*TypeInst
	L     loc.Loc
}

type TypeParm struct {
	Name string
	L    loc.Loc
	// Used for String(); we need to store it,
	// since the String() method doesn't
	// otherwise have access to loc.Files.
	location loc.Location
}

type TypeInst struct {
	Args []Type
	Type Type
}

type Type interface {
	// String returns a human-readable string representation
	// appropriate for error messages.
	String() string
	buildString(w *strings.Builder) *strings.Builder

	// eq must not be called on a type before aliases have been resolved.
	eq(Type) bool

	baseType() Type
}

type RefType struct {
	Type Type
	L    loc.Loc
}

func (r *RefType) baseType() Type { return r }

type NamedType struct {
	Name string
	Args []Type
	Def  *TypeDef
	Inst *TypeInst
	L    loc.Loc
}

func (n *NamedType) baseType() Type { return n.Inst.Type.baseType() }

type ArrayType struct {
	ElemType Type
	L        loc.Loc
}

func (a *ArrayType) baseType() Type { return a }

type StructType struct {
	Fields []Field
	L      loc.Loc
}

func (s *StructType) baseType() Type { return s }

type Field struct {
	Name string
	Type Type
	L    loc.Loc
}

type UnionType struct {
	Cases []Case
	L     loc.Loc
}

func (u *UnionType) baseType() Type { return u }

type Case struct {
	Name string
	Type Type // nil for untyped
	L    loc.Loc
}

type FuncType struct {
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

func (f *FuncType) baseType() Type { return f }

type TypeVar struct {
	Name string
	Def  *TypeParm
	L    loc.Loc
}

func (t *TypeVar) baseType() Type { return t }

const (
	Bool BasicTypeKind = iota + 1
	Int
	Int8
	Int16
	Int32
	Int64
	Uint
	Uint8
	Uint16
	Uint32
	Uint64
	Float32
	Float64
	String
)

type BasicTypeKind int

type BasicType struct {
	Kind BasicTypeKind
	L    loc.Loc
}

func (b *BasicType) baseType() Type { return b }

type FuncDef struct {
	File      *File
	Mod       string
	Name      string
	TypeParms []TypeParm
	Parms     []FuncParm
	Locals    []FuncLocal
	Ret       Type // nil if no return
	Iface     []FuncDecl
	Exprs     []Expr // nil if unspecified, non-nil, len()==0 if empty
	Exp       bool
	Insts     []*FuncInst
	L         loc.Loc
}

type FuncParm struct {
	Name string
	Type Type
	Init Expr
	L    loc.Loc
}

type FuncLocal struct {
	Name string
	Type Type
	L    loc.Loc
}

type FuncDecl struct {
	Name  string
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type FuncInst struct {
	Ps  []Type
	R   Type
	Def *FuncDef
}

func (f *FuncInst) Parms() []Type { return f.Ps }
func (f *FuncInst) Ret() Type     { return f.R }

type TestDef struct {
	File  *File
	Mod   string
	Name  string
	Exprs []Expr
	L     loc.Loc
}

type Expr interface {
	Type() Type
	Loc() loc.Loc
}

type Call struct {
	Fun  Callable
	Args []Expr
	T    Type
	L    loc.Loc
}

func (c *Call) Type() Type   { return c.T }
func (c *Call) Loc() loc.Loc { return c.L }

type Callable interface {
	Parms() []Type
	Ret() Type // returns nil if no return
	String() string
}

type Select struct {
	Type   Type
	Struct *StructType
	Field  *Field
}

func (s *Select) Parms() []Type { return []Type{s.Type} }
func (s *Select) Ret() Type     { return s.Field.Type }

type Switch struct {
	Type  Type
	Union *UnionType
	Cases []*Case
	R     Type // inferred return type; nil if no return
}

func (s *Switch) Parms() []Type {
	var parms []Type
	for _, cas := range s.Cases {
		parm := &FuncType{Parms: nil, Ret: s.R, L: cas.L}
		if cas.Type != nil {
			parm.Parms = []Type{cas.Type}
		}
		parms = append(parms, parm)
	}
	return parms
}

func (s *Switch) Ret() Type { return s.R }

type Op int

const (
	Assign Op = iota + 1
	NewArray
	BitNot
	BitXor
	BitAnd
	BitOr
	LeftShift
	RightShift
	Negate
	Minus
	Plus
	Times
	Divide
	Modulus
	Eq
	Neq
	Less
	LessEq
	Greater
	GreaterEq
	NumConvert
	StrConvert // string([int8])
	Index
	Slice
	Length
	Panic

	Print
)

type Builtin struct {
	Op Op
	Ps []Type
	R  Type // nil if no return
}

func (b *Builtin) Parms() []Type { return b.Ps }
func (b *Builtin) Ret() Type     { return b.R }

type Ref struct {
	Expr Expr
	T    Type
	L    loc.Loc
}

func (r *Ref) Type() Type   { return r.T }
func (r *Ref) Loc() loc.Loc { return r.L }

type Deref struct {
	Expr Expr
	T    Type
	L    loc.Loc
}

func (d *Deref) Type() Type   { return d.T }
func (d *Deref) Loc() loc.Loc { return d.L }

type Var struct {
	// Global, Local, Parm, and Cap are mutually exclusive;
	// exactly one of them is non-nil.
	Global *VarDef
	Local  *FuncLocal
	Parm   *FuncParm
	Cap    *BlockCap
	T      Type
	L      loc.Loc
}

func (v *Var) Type() Type   { return v.T }
func (v *Var) Loc() loc.Loc { return v.L }

type ArrayLit struct {
	Array *ArrayType
	Elems []Expr
	T     Type
	L     loc.Loc
}

func (a *ArrayLit) Type() Type   { return a.T }
func (a *ArrayLit) Loc() loc.Loc { return a.L }

type StructLit struct {
	Struct *StructType
	Fields []Expr
	T      Type
	L      loc.Loc
}

func (s *StructLit) Type() Type   { return s.T }
func (s *StructLit) Loc() loc.Loc { return s.L }

type UnionLit struct {
	Union *UnionType
	Case  *Case
	Val   Expr // nil for type-less case
	T     Type
	L     loc.Loc
}

func (u *UnionLit) Type() Type   { return u.T }
func (u *UnionLit) Loc() loc.Loc { return u.L }

type BlockLit struct {
	Caps   []BlockCap
	Parms  []FuncParm
	Locals []FuncLocal
	Ret    Type // result type of the block
	Exprs  []Expr
	T      Type
	L      loc.Loc
}

func (b *BlockLit) Type() Type   { return b.T }
func (b *BlockLit) Loc() loc.Loc { return b.L }

type BlockCap struct {
	Name string
	T    Type
	L    loc.Loc

	// Parm, Local, and Cap are mutually exclusive.
	Parm  *FuncParm
	Local *FuncLocal
	Cap   *BlockCap
}

type StrLit struct {
	Text string
	T    Type
	L    loc.Loc
}

func (s *StrLit) Type() Type   { return s.T }
func (s *StrLit) Loc() loc.Loc { return s.L }

type IntLit struct {
	Text string
	Val  big.Int
	T    Type
	L    loc.Loc
}

func (i *IntLit) Type() Type   { return i.T }
func (i *IntLit) Loc() loc.Loc { return i.L }

type FloatLit struct {
	Text string
	Val  big.Float
	T    Type
	L    loc.Loc
}

func (f *FloatLit) Type() Type   { return f.T }
func (f *FloatLit) Loc() loc.Loc { return f.L }
