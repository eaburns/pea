package parser

import "github.com/eaburns/pea/loc"

type File struct {
	Imports []*Import
	Defs    []Def

	P      string
	NLs    []int
	Length int
}

func (f *File) Path() string    { return f.P }
func (f *File) NewLines() []int { return f.NLs }
func (f *File) Len() int        { return f.Length }

type Import struct {
	Exp  bool
	Name *Id // nil if unspecified
	Path string
	L    loc.Loc
}

type Def interface{}

type VarDef struct {
	Exp   bool
	Const bool
	Name  Id
	Type  Type // nil if unspecified
	Expr  Expr // nil if unspecified
	L     loc.Loc
}

type TypeDef struct {
	Exp       bool
	Alias     bool
	TypeParms []TypeVar
	Name      Id
	Type      Type // nil if unspecified
	L         loc.Loc
}

type Type interface{}

type RefType struct {
	Type Type
	L    loc.Loc
}

type NamedType struct {
	Args []Type
	Mod  *Id // nil if unspecified
	Name Id
	L    loc.Loc
}

type ArrayType struct {
	ElemType Type
	L        loc.Loc
}

type StructType struct {
	Fields []FieldDef
	L      loc.Loc
}

type FieldDef struct {
	Name Id
	Type Type
	L    loc.Loc
}

type UnionType struct {
	Cases []CaseDef
	L     loc.Loc
}

type CaseDef struct {
	Name Id
	Type Type // nil if an un-typed case
	L    loc.Loc
}

type FuncType struct {
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type FuncDef struct {
	Exp   bool
	Name  Id
	Parms []FuncParm
	Ret   Type // nil if no return
	Iface []FuncDecl
	Exprs []Expr // nil if unspecified, non-nil, len()==0 if empty
	L     loc.Loc
}

type FuncParm struct {
	Name Id
	Type Type
	L    loc.Loc
}

type FuncDecl struct {
	Name  Id
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type TestDef struct {
	Name  Id
	Exprs []Expr
	L     loc.Loc
}

type Expr interface {
	Loc() loc.Loc
}

type Call struct {
	Fun  Expr
	Args []Expr
	L    loc.Loc
}

func (c *Call) Loc() loc.Loc { return c.L }

type Convert struct {
	Type Type
	Expr Expr
	L    loc.Loc
}

func (c *Convert) Loc() loc.Loc { return c.L }

type SubExpr struct {
	Expr
	L loc.Loc
}

func (s *SubExpr) Loc() loc.Loc { return s.L }

type ModSel struct {
	Mod  Id
	Name Id
	L    loc.Loc
}

func (m *ModSel) Loc() loc.Loc { return m.L }

type ArrayLit struct {
	Exprs []Expr
	L     loc.Loc
}

func (a *ArrayLit) Loc() loc.Loc { return a.L }

type StructLit struct {
	FieldVals []FieldVal
	L         loc.Loc
}

func (s *StructLit) Loc() loc.Loc { return s.L }

type FieldVal struct {
	Name Id
	Val  Expr
	L    loc.Loc
}

type UnionLit struct {
	CaseVal CaseVal
	L       loc.Loc
}

func (u *UnionLit) Loc() loc.Loc { return u.L }

type CaseVal struct {
	Name Id
	Val  Expr // nil if value-less case
	L    loc.Loc
}

type BlockLit struct {
	Parms []FuncParm
	Exprs []Expr
	L     loc.Loc
}

func (b *BlockLit) Loc() loc.Loc { return b.L }

type CharLit struct {
	Rune rune
	L    loc.Loc
}

func (c *CharLit) Loc() loc.Loc { return c.L }

type StrLit struct {
	Raw  bool
	Data string
	L    loc.Loc
}

func (s *StrLit) Loc() loc.Loc { return s.L }

type IntLit struct {
	Text string
	L    loc.Loc
}

func (i *IntLit) Loc() loc.Loc { return i.L }

type FloatLit struct {
	Text string
	L    loc.Loc
}

func (f *FloatLit) Loc() loc.Loc { return f.L }

type TypeVar struct {
	Name string
	L    loc.Loc
}

type Id struct {
	Name string
	L    loc.Loc
}

func (i Id) Loc() loc.Loc { return i.L }
