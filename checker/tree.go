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
	FilePath string
	Nls      []int
	Length   int
	Mod      *Mod
	Imports  []*Import
}

func (f *File) Path() string    { return f.FilePath }
func (f *File) NewLines() []int { return f.Nls }
func (f *File) Len() int        { return f.Length }

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
	T     Type
	Expr  Expr
	Const bool
	Exp   bool
	L     loc.Loc
}

func (v *VarDef) Type() Type { return v.T }

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

	Loc() loc.Loc
}

type RefType struct {
	Type Type
	L    loc.Loc
}

func (r *RefType) Loc() loc.Loc { return r.L }

type DefType struct {
	Name string
	Args []Type
	Def  *TypeDef
	Inst *TypeInst
	L    loc.Loc
}

func (d *DefType) Loc() loc.Loc { return d.L }

type ArrayType struct {
	ElemType Type
	L        loc.Loc
}

func (a *ArrayType) Loc() loc.Loc { return a.L }

type StructType struct {
	Fields []FieldDef
	L      loc.Loc
}

func (s *StructType) Loc() loc.Loc { return s.L }

type FieldDef struct {
	Name string
	Type Type
	L    loc.Loc
}

type UnionType struct {
	Cases []CaseDef
	L     loc.Loc
}

func (u *UnionType) Loc() loc.Loc { return u.L }

type CaseDef struct {
	Name string
	Type Type // nil for untyped
	L    loc.Loc
}

type FuncType struct {
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

func (f *FuncType) Loc() loc.Loc { return f.L }

type TypeVar struct {
	Name string
	Def  *TypeParm
	L    loc.Loc
}

func (t *TypeVar) Loc() loc.Loc { return t.L }

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

func (b *BasicType) Loc() loc.Loc { return b.L }

type TestDef struct {
	File  *File
	Mod   string
	Name  string
	Exprs []Expr
	L     loc.Loc
}

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
	T         Type
	L         loc.Loc
}

func (f *FuncDef) Type() Type { return f.T }

type FuncParm struct {
	Name string
	T    Type
	Init Expr
	L    loc.Loc
}

func (f *FuncParm) Type() Type { return f.T }

type FuncLocal struct {
	Name string
	T    Type
	L    loc.Loc
}

func (f *FuncLocal) Type() Type { return f.T }

type FuncDecl struct {
	Name  string
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type Func interface {
	String() string
	Parms() []Type
	Ret() Type

	unifyRet(Type) (bool, *note)
	unifyParm(int, Type) (bool, *note)
	buildString(*strings.Builder) *strings.Builder
}

type FuncInst struct {
	T   *FuncType
	Def *FuncDef
}

func (f *FuncInst) Loc() loc.Loc  { return f.Def.L }
func (f *FuncInst) Type() Type    { return f.T }
func (f *FuncInst) Parms() []Type { return f.T.Parms }
func (f *FuncInst) Ret() Type     { return f.T.Ret }

type Select struct {
	// Struct is the struct type and the type of the 0th parameter.
	// During overload resolution, Struct is nil until parm 0 is unified.
	Struct *StructType
	Field  *FieldDef
	P      Type
	R      Type
}

func (s *Select) Parms() []Type { return []Type{s.P} }

func (s *Select) Ret() Type { return s.R }

func (s *Select) Type() Type { return &FuncType{Parms: []Type{s.P}, Ret: s.R} }

type Switch struct {
	Union *UnionType
	Cases []*CaseDef
	Ps    []Type
	R     Type // inferred return type; nil if no return
}

func (s *Switch) Parms() []Type { return s.Ps }
func (s *Switch) Ret() Type     { return s.R }

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

type ExprFunc struct {
	Expr
	FuncType *FuncType
}

func (e *ExprFunc) Parms() []Type { return e.FuncType.Parms }
func (e *ExprFunc) Ret() Type     { return e.FuncType.Ret }

type Expr interface {
	String() string
	Type() Type
	Loc() loc.Loc

	buildString(*strings.Builder) *strings.Builder
}

type Call struct {
	Func Func
	Args []Expr
	T    Type
	L    loc.Loc
}

func (c *Call) Type() Type   { return c.T }
func (c *Call) Loc() loc.Loc { return c.L }

type Deref struct {
	Expr Expr
	T    Type
	L    loc.Loc
}

func (d *Deref) Type() Type   { return d.T }
func (d *Deref) Loc() loc.Loc { return d.L }

type Var struct {
	Def *VarDef
	T   Type
	L   loc.Loc
}

func (v *Var) Type() Type   { return v.T }
func (v *Var) Loc() loc.Loc { return v.L }

type Local struct {
	Def *FuncLocal
	T   Type
	L   loc.Loc
}

func (l *Local) Type() Type   { return l.T }
func (l *Local) Loc() loc.Loc { return l.L }

type Parm struct {
	Def *FuncParm
	T   Type
	L   loc.Loc
}

func (p *Parm) Type() Type   { return p.T }
func (p *Parm) Loc() loc.Loc { return p.L }

type Cap struct {
	Def *BlockCap
	T   Type
	L   loc.Loc
}

func (c *Cap) Type() Type   { return c.T }
func (c *Cap) Loc() loc.Loc { return c.L }

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
	Case  *CaseDef
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

func (b *BlockCap) Type() Type { return b.T }

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
