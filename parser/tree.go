package parser

import "github.com/eaburns/pea/loc"

type File struct {
	Imports []*Import
	Defs    []Def
	// All comments sorted by their start location.
	Comments []Comment

	P      string
	NLs    []int
	Length int
}

func (f *File) Path() string    { return f.P }
func (f *File) NewLines() []int { return f.NLs }
func (f *File) Len() int        { return f.Length }

type Import struct {
	Exp  bool
	Name *Ident // nil if unspecified
	Path string
	L    loc.Loc
}

type Def interface{}

type VarDef struct {
	Exp   bool
	Const bool
	Name  Ident
	Type  Type
	Expr  Expr // nil if unspecified
	L     loc.Loc
}

type TypeDef struct {
	Exp       bool
	Opaque    bool
	Alias     bool
	TypeParms []TypeVar
	Name      Ident
	Type      Type
	L         loc.Loc
}

type Type interface{}

type RefType struct {
	Type Type
	L    loc.Loc
}

type NamedType struct {
	Args []Type
	Mod  *Ident // nil if unspecified
	Name Ident
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
	Name Ident
	Type Type
	L    loc.Loc
}

type UnionType struct {
	Cases []CaseDef
	L     loc.Loc
}

type CaseDef struct {
	Name Ident
	Type Type // nil if an un-typed case
	L    loc.Loc
}

type FuncType struct {
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type IfaceDef struct {
	Exp       bool
	Opaque    bool
	TypeParms []TypeVar
	Name      Ident
	// Iface and Alias are mutually exclusive.
	// Iface is the defintition of a non-alias interface.
	// Each element is either a *FuncDecl
	// or a *NamedType naming an interface.
	Iface []interface{}
	// Alias is the name of an interface that this one aliases.
	Alias *NamedType
	L     loc.Loc
}

type FuncDef struct {
	Exp   bool
	Name  Ident
	Parms []FuncParm
	Ret   Type // nil if no return
	// Constraints is an interface constraint to satisfy
	// after the return type is determined.
	// Each element is either a *FuncDecl
	// or a *NamedType naming an interface.
	Constraints []interface{}
	Exprs       []Expr // nil if unspecified, non-nil, len()==0 if empty
	L           loc.Loc
}

type FuncParm struct {
	Name Ident
	Type Type
	// Constraints is an interface constraint to satisfy
	// after this parameter type is determined.
	// Each element is either a *FuncDecl
	// or a *NamedType naming an interface.
	Constraints []interface{}
	L           loc.Loc
}

type FuncDecl struct {
	Name  Ident
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type TestDef struct {
	Name  Ident
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
	Mod  Ident
	Name Ident
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
	Name Ident
	Val  Expr
	L    loc.Loc
}

type UnionLit struct {
	CaseVal CaseVal
	L       loc.Loc
}

func (u *UnionLit) Loc() loc.Loc { return u.L }

type CaseVal struct {
	Name Ident
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

func (tv TypeVar) Loc() loc.Loc   { return tv.L }
func (tv TypeVar) String() string { return tv.Name }

type Ident struct {
	Name string
	L    loc.Loc
}

func (i Ident) Loc() loc.Loc   { return i.L }
func (i Ident) String() string { return i.Name }

type Comment struct {
	Text string
	L    loc.Loc
}

func (c Comment) Loc() loc.Loc   { return c.L }
func (c Comment) String() string { return c.Text }
