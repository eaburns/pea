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
	// Defs contains all module-level definitions.
	// VarDefs will appear in topoligical order
	// with VarDefs appearing always after those
	// on which they have initialization dependencies.
	Defs []Def

	// Deps containts module paths of all transitive dependencies
	// in topological with dependencies appearing before their dependents.
	Deps []string

	toSub []*FuncInst
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

	usedVars  []varUse
	usedFuncs []funcUse
}

type varUse struct {
	L   loc.Loc
	Var *VarDef
}

type funcUse struct {
	L    loc.Loc
	Func *FuncDef
	// Parm is an iface parameter of Func or nil.
	// If Parm is non-nil, this is marking a use of Arg
	// as the parameter to a call to Func.
	// Func's use itself is tracked as a separate funcUse
	// with Parm==nil and Arg == nil.
	Parm *FuncDecl
	// Arg is the arg to Parm if it is non-nil.
	Arg *FuncDef
}

func (v *VarDef) Type() Type   { return v.T }
func (v *VarDef) Loc() loc.Loc { return v.L }

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
	Loc() loc.Loc

	buildString(w *strings.Builder) *strings.Builder
}

type DefType struct {
	Name string
	Args []Type
	Def  *TypeDef
	Inst *TypeInst
	L    loc.Loc
}

func (d *DefType) Loc() loc.Loc { return d.L }

type RefType struct {
	Type Type
	L    loc.Loc
}

func (r *RefType) Loc() loc.Loc { return r.L }

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
	Ret   Type
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

	noBasicTypeKind
)

type BasicTypeKind int

type BasicType struct {
	Kind BasicTypeKind
	L    loc.Loc
}

func (b *BasicType) Loc() loc.Loc { return b.L }

type TestDef struct {
	File   *File
	Mod    string
	Name   string
	Locals []*LocalDef
	Exprs  []Expr
	L      loc.Loc
}

type FuncDef struct {
	File      *File
	Mod       string
	Name      string
	TypeParms []TypeParm
	Parms     []ParmDef
	Locals    []*LocalDef
	Ret       Type
	Iface     []FuncDecl
	Exprs     []Expr // nil if unspecified, non-nil, len()==0 if empty
	Exp       bool
	Insts     []*FuncInst
	L         loc.Loc

	usedVars  []varUse
	usedFuncs []funcUse
}

func (f *FuncDef) Loc() loc.Loc { return f.L }

type ParmDef struct {
	Name string
	T    Type
	L    loc.Loc
}

func (f *ParmDef) Type() Type   { return f.T }
func (f *ParmDef) Loc() loc.Loc { return f.L }

type LocalDef struct {
	Name string
	T    Type
	L    loc.Loc

	used bool
}

func (f *LocalDef) Type() Type   { return f.T }
func (f *LocalDef) Loc() loc.Loc { return f.L }

type FuncDecl struct {
	Name  string
	Parms []Type
	Ret   Type
	L     loc.Loc
}

func (f *FuncDecl) Loc() loc.Loc { return f.L }

func (f *FuncDecl) Type() Type {
	return &FuncType{Parms: f.Parms, Ret: f.Ret, L: f.L}
}

type Func interface {
	String() string

	arity() int
	groundRet() Type // nil if not grounded
	unifyRet(Type) note
	groundParm(int) Type // nil if not grounded
	unifyParm(int, Type) note
	buildString(*strings.Builder) *strings.Builder
	eq(Func) bool
}

type FuncInst struct {
	TypeArgs  []Type
	IfaceArgs []Func
	Def       *FuncDef
	T         *FuncType

	// subbed parallels TypeParms.
	// It indicates which TypeParms
	// have been substituted.
	// This is used to determine grounded types
	// with respect to a FuncInst.
	// It's not enough to look at whether
	// the type has a TypeParm TypeVar in it,
	// since the TypeVar may be substituted
	// for an instance of itself in a recursive call;
	// this is still "grounded".
	subbed []bool

	// The following fields are populated by subFuncInst.

	Parms  []*ParmDef
	Locals []*LocalDef
	Exprs  []Expr
}

func (f *FuncInst) Loc() loc.Loc { return f.Def.L }
func (f *FuncInst) Type() Type   { return f.T }

type Select struct {
	// Struct is the struct type and the type of the 0th parameter.
	// During overload resolution, Struct is nil until parm 0 is unified.
	Struct *StructType
	Field  *FieldDef
	Parm   Type
	Ret    Type
}

func (s *Select) Type() Type { return &FuncType{Parms: []Type{s.Parm}, Ret: s.Ret} }

type Switch struct {
	Union *UnionType
	Cases []*CaseDef
	Parms []Type
	Ret   Type // inferred return type
}

func (s *Switch) Type() Type { return &FuncType{Parms: s.Parms, Ret: s.Ret} }

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
	Index
	Slice
	Length
	Return
	Panic

	Print
)

type Builtin struct {
	Op    Op
	Parms []Type
	Ret   Type
}

func (b *Builtin) Type() Type { return &FuncType{Parms: b.Parms, Ret: b.Ret} }

type ExprFunc struct {
	Expr
	FuncType *FuncType
}

type Expr interface {
	String() string
	Type() Type
	Loc() loc.Loc

	buildString(*strings.Builder) *strings.Builder
	// subExpr returns a copy of the Expr
	// with TypeVars and FuncDecls substituted.
	// The return is always a copy.
	subExpr(bindings) Expr
}

type Call struct {
	Func Func
	Args []Expr
	T    Type
	L    loc.Loc
}

func (c *Call) Type() Type   { return c.T }
func (c *Call) Loc() loc.Loc { return c.L }

type ConvertKind int

const (
	// Noop is a no-op conversion.
	// It is used to track explicit conversion nodes,
	// but nothing is actually converted.
	Noop ConvertKind = iota

	// Drop is a conversion to an empty struct.
	// The value is "dropped".
	Drop

	// Deref is a dereference conversion.
	Deref

	// StrConvert is a conversion from [int8] to string.
	StrConvert

	// NumConvert is a numeric conversion.
	NumConvert
)

type Convert struct {
	Kind     ConvertKind
	Explicit bool
	Expr     Expr
	T        Type
	L        loc.Loc
}

func (d *Convert) Type() Type   { return d.T }
func (d *Convert) Loc() loc.Loc { return d.L }

type Var struct {
	Def *VarDef
	T   Type
	L   loc.Loc
}

func (v *Var) Type() Type   { return v.T }
func (v *Var) Loc() loc.Loc { return v.L }

type Local struct {
	Def *LocalDef
	T   Type
	L   loc.Loc
}

func (l *Local) Type() Type   { return l.T }
func (l *Local) Loc() loc.Loc { return l.L }

type Parm struct {
	Def *ParmDef
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
	Caps   []*BlockCap
	Parms  []ParmDef
	Locals []*LocalDef
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
	Parm  *ParmDef
	Local *LocalDef
	Cap   *BlockCap
}

func (b *BlockCap) Type() Type   { return b.T }
func (b *BlockCap) Loc() loc.Loc { return b.L }

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
