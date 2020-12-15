package checker

import (
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
	key() string
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

	eq(Type) bool
}

type RefType struct {
	Type Type
	L    loc.Loc
}

type NamedType struct {
	Name string
	Args []Type
	Def  *TypeDef
	Inst *TypeInst
	L    loc.Loc
}

type ArrayType struct {
	ElemType Type
	L        loc.Loc
}

type StructType struct {
	Fields []Field
	L      loc.Loc
}

type Field struct {
	Name string
	Type Type
	L    loc.Loc
}

type UnionType struct {
	Cases []Case
	L     loc.Loc
}

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

type TypeVar struct {
	Name string
	Def  *TypeParm
	L    loc.Loc
}

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

type FuncDef struct {
	File      *File
	Mod       string
	Name      string
	TypeParms []TypeParm
	Parms     []FuncParm
	Ret       Type // nil if no return
	Iface     []FuncDecl
	Exprs     []Expr // nil if unspecified, non-nil, len()==0 if empty
	Exp       bool
	L         loc.Loc
}

type FuncParm struct {
	Name string
	Type Type
	Init Expr
	L    loc.Loc
}

type FuncDecl struct {
	Name  string
	Parms []Type
	Ret   Type // nil if no return
	L     loc.Loc
}

type TestDef struct {
	File  *File
	Mod   string
	Name  string
	Exprs []Expr
	L     loc.Loc
}

type Expr interface{}
