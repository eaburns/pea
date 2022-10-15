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
	Loc() loc.Loc
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
	File   *File
	Alias  bool
	Mod    string
	Name   string
	Parms  []TypeParm
	Type   Type
	Exp    bool
	Opaque bool
	Insts  []*TypeInst
	L      loc.Loc
}

func (d *TypeDef) Loc() loc.Loc { return d.L }

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
	End BasicTypeKind = iota + 1
	Bool
	Ordering
	Int
	Int8
	Int16
	Int32
	Int64
	UintRef
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

type IfaceDef struct {
	File  *File
	Mod   string
	Name  string
	Parms []TypeParm
	// Iface and Alias are mutually exclusive.
	// Iface elements are of type *FuncDecl or *IfaceInst.
	Iface  []interface{}
	Alias  *IfaceInst
	Exp    bool
	Opaque bool

	Funcs []FuncDecl
	Insts []*IfaceInst
	L     loc.Loc
}

func (d *IfaceDef) Loc() loc.Loc { return d.L }

type IfaceInst struct {
	Def   *IfaceDef
	Args  []Type
	Funcs []FuncDecl
}

type TestDef struct {
	File   *File
	Mod    string
	Name   string
	Locals []*LocalDef
	Exprs  []Expr
	L      loc.Loc
}

func (d *TestDef) Loc() loc.Loc { return d.L }

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
	buildString(*strings.Builder) *strings.Builder
	eq(Func) bool
	arity() int
	// ret() and parm() return the return type pattern and parameter type patterns respectively.
	// An implementation may assume that for i>0, parm(i) is not called until after parm(i-1)
	// is called and a subsequent sub() call substitutes any type variables from parm(i-1);
	// similarly ret() is not called until after parm(arity()-1).
	ret() typePattern
	parm(int) typePattern
	sub(map[*TypeParm]Type) note
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
	// N is the selected field name.
	N        string
	TypeParm *TypeParm
	Parm     Type
	// Ret is nil until after a successful call to Select.sub().
	Ret Type

	// Struct is the struct type and the type of the 0th parameter.
	// It is nil until parm 0 is successfully substituted.
	Struct *StructType
	// Field is a pointer to the selected field in Struct.
	// It is nil until parm0 is successfully substituted.
	Field *FieldDef
}

func (s *Select) Type() Type { return &FuncType{Parms: []Type{s.Parm}, Ret: s.Ret} }

type Switch struct {
	// N is the name of the switch function.
	N string
	// Names is the names of the cases; N split by ?.
	Names []string
	// TypeParms are the type parameters.
	//
	// TypeParms[0] is the fake type parameter representing the union type.
	// By the spec, there are Switch functions defined directly on every union type,
	// but we represent this lazily by using a type parameter for it,
	// and returning an error from Switch.sub() if that parameter is substituted with a non-union.
	//
	// TypeParms[1] is the type parameter of the return type.
	TypeParms []*TypeParm
	// Parms is the correct length, but:
	// before Switch.sub(), Parms[0] is a type variable of TypeParms[0],
	// and the remaining elements are nil, and
	// after Switch.sub() the elements are the correct type for the switch.
	Parms []Type
	// Ret is nil until after Switch.sub().
	Ret Type

	Union *UnionType
	// Cases[i] is nil for the default _? case.
	Cases []*CaseDef
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
	Cmp
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
	N        string
	Op       Op
	TypeParm *TypeParm
	Parms    []Type
	Ret      Type
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

	// Ref is a reference conversion.
	Ref

	// StrConvert is a conversion from [int8] to string.
	StrConvert

	// NumConvert is a numeric conversion.
	NumConvert

	// UnionConvert converts a union value
	// into a value of a superset union type.
	UnionConvert

	// funcConvert converts a function value
	// to a function with compatible parameter
	// and return types.
	//
	// This is never returned from checker.
	// Instead, it is implemented by
	// creating a new block literal to wrap
	// a call to the converted function.
	funcConvert
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
	Func   *FuncType
	T      Type
	L      loc.Loc
}

func (b *BlockLit) Type() Type   { return b.T }
func (b *BlockLit) Loc() loc.Loc { return b.L }

type BlockCap struct {
	Name string
	T    Type
	L    loc.Loc

	// The following are mutually exclusive.
	//
	// TODO: all of these can be implemented as Expr.
	Parm  *ParmDef
	Local *LocalDef
	Cap   *BlockCap
	// If non-nil, Expr is evaluated when creating the block literal;
	// its value is assigned to a variable, and that variable is captured.
	// Note that since the variable is captured and not the Expr value itself,
	// the type of this BlockCap is a reference to Expr.Type().
	Expr Expr
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
