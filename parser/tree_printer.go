package parser

import (
	"fmt"
	"io"
	"reflect"
	"strings"

	"github.com/eaburns/pea/loc"
)

type PrintTreeOpt func(*config)

func PrintTreeLocs(fs ...*File) PrintTreeOpt {
	var files loc.Files
	for _, f := range fs {
		files = append(files, f)
	}
	return func(pc *config) { pc.files = files }
}

func (f *File) PrintTree(w io.Writer, opts ...PrintTreeOpt) error {
	return printTree(w, f, opts...)
}

type config struct {
	w     io.Writer
	files loc.Files
	n     int
	ident string
}

type printTreeError struct{ error }

type treePrinter interface {
	printTree(*config)
}

func printTree(w io.Writer, tree treePrinter, opts ...PrintTreeOpt) (err error) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		if e, ok := r.(printTreeError); ok {
			err = e
		} else {
			panic(r)
		}
	}()
	pc := &config{w: w, ident: "  "}
	for _, opt := range opts {
		opt(pc)
	}
	tree.printTree(pc)
	pc.p("\n")
	return err
}

func (f *File) printTree(pc *config) {
	pc.p("File{")
	pc.field("Path", f.P)
	pc.field("Comments", f.Comments)
	pc.field("Imports", f.Imports)
	pc.field("Defs", f.Defs)
	pc.p("\n}")
}

func (i *Import) printTree(pc *config) {
	pc.p("Import{")
	pc.loc(i.L)
	pc.field("Name", i.Name)
	pc.field("Path", i.Path)
	pc.p("\n}")
}

func (v *VarDef) printTree(pc *config) {
	pc.p("VarDef{")
	pc.loc(v.L)
	pc.field("Exp", v.Exp)
	pc.field("Const", v.Const)
	pc.field("Name", v.Name)
	pc.field("Type", v.Type)
	pc.field("Expr", v.Expr)
	pc.p("\n}")
}

func (t *TypeDef) printTree(pc *config) {
	pc.p("TypeDef{")
	pc.loc(t.L)
	pc.field("Exp", t.Exp)
	pc.field("Alias", t.Alias)
	pc.field("TypeParms", t.TypeParms)
	pc.field("Name", t.Name)
	pc.field("Type", t.Type)
	pc.p("\n}")
}

func (t *IfaceDef) printTree(pc *config) {
	pc.p("IfaceDef{")
	pc.loc(t.L)
	pc.field("Exp", t.Exp)
	pc.field("Alias", t.Alias)
	pc.field("TypeParms", t.TypeParms)
	pc.field("Name", t.Name)
	if t.Alias == nil {
		pc.field("Iface", t.Iface)
	} else {
		pc.field("Alias", t.Alias)
	}
	pc.p("\n}")
}

func (r *RefType) printTree(pc *config) {
	pc.p("RefType{")
	pc.loc(r.L)
	pc.field("Type", r.Type)
	pc.p("\n}")
}

func (n *NamedType) printTree(pc *config) {
	pc.p("NamedType{")
	pc.loc(n.L)
	pc.field("Mod", n.Mod)
	pc.field("Name", n.Name)
	pc.field("Args", n.Args)
	pc.p("\n}")
}

func (a *ArrayType) printTree(pc *config) {
	pc.p("ArrayType{")
	pc.loc(a.L)
	pc.field("ElemType", a.ElemType)
	pc.p("\n}")
}

func (s *StructType) printTree(pc *config) {
	pc.p("StructType{")
	pc.loc(s.L)
	pc.field("Fields", s.Fields)
	pc.p("\n}")
}

func (f FieldDef) printTree(pc *config) {
	pc.p("FieldDef{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Type", f.Type)
	pc.p("\n}")
}

func (u *UnionType) printTree(pc *config) {
	pc.p("UnionType{")
	pc.loc(u.L)
	pc.field("Cases", u.Cases)
	pc.p("\n}")
}

func (c CaseDef) printTree(pc *config) {
	pc.p("CaseDef{")
	pc.loc(c.L)
	pc.field("Name", c.Name)
	pc.field("Type", c.Type)
	pc.p("\n}")
}

func (f *FuncType) printTree(pc *config) {
	pc.p("FuncType{")
	pc.loc(f.L)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.p("\n}")
}

func (f *FuncDef) printTree(pc *config) {
	pc.p("FuncDef{")
	pc.loc(f.L)
	pc.field("Expr", f.Exp)
	pc.field("Name", f.Name)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.field("Constraints", f.Constraints)
	pc.field("Exprs", f.Exprs)
	pc.p("\n}")
}

func (f FuncParm) printTree(pc *config) {
	pc.p("FuncParm{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Type", f.Type)
	pc.field("Constraints", f.Constraints)
	pc.p("\n}")
}

func (f FuncDecl) printTree(pc *config) {
	pc.p("FuncDecl{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.p("\n}")
}

func (f *TestDef) printTree(pc *config) {
	pc.p("TestDef{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Exprs", f.Exprs)
	pc.p("\n}")
}

func (c *Call) printTree(pc *config) {
	pc.p("Call{")
	pc.loc(c.L)
	pc.field("Fun", c.Fun)
	pc.field("Args", c.Args)
	pc.p("\n}")
}

func (c *Convert) printTree(pc *config) {
	pc.p("Convert{")
	pc.loc(c.L)
	pc.field("Type", c.Type)
	pc.field("Expr", c.Expr)
	pc.p("\n}")
}

func (s *SubExpr) printTree(pc *config) {
	pc.p("SubExpr{")
	pc.loc(s.L)
	pc.field("Expr", s.Expr)
	pc.p("\n}")
}

func (m *ModSel) printTree(pc *config) {
	pc.p("ModSel{")
	pc.loc(m.L)
	pc.field("Mod", m.Mod)
	pc.field("Name", m.Name)
	pc.p("\n}")
}

func (a *ArrayLit) printTree(pc *config) {
	pc.p("ArrayLit{")
	pc.loc(a.L)
	pc.field("Exprs", a.Exprs)
	pc.p("\n}")
}

func (s *StructLit) printTree(pc *config) {
	pc.p("StructLit{")
	pc.loc(s.L)
	pc.field("FieldVals", s.FieldVals)
	pc.p("\n}")
}
func (f FieldVal) printTree(pc *config) {
	pc.p("FieldVal{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Val", f.Val)
	pc.p("\n}")
}

func (u *UnionLit) printTree(pc *config) {
	pc.p("UnionLit{")
	pc.loc(u.L)
	pc.field("CaseVal", u.CaseVal)
	pc.p("\n}")
}

func (c *CaseVal) printTree(pc *config) {
	pc.p("CaseVal{")
	pc.loc(c.L)
	pc.field("Name", c.Name)
	pc.field("Val", c.Val)
	pc.p("\n}")
}

func (b *BlockLit) printTree(pc *config) {
	pc.p("BlockLit{")
	pc.loc(b.L)
	pc.field("Parms", b.Parms)
	pc.field("Exprs", b.Exprs)
	pc.p("\n}")
}

func (c *CharLit) printTree(pc *config) {
	pc.p("CharLit(%c)", c.Rune)
	pc.loc(c.L)
}

func (s *StrLit) printTree(pc *config) {
	pc.p("StrLit(%q)", s.Data)
	pc.loc(s.L)
}

func (i *IntLit) printTree(pc *config) {
	pc.p("IntLit(%s)", i.Text)
	pc.loc(i.L)
}

func (i *FloatLit) printTree(pc *config) {
	pc.p("FloatLit(%s)", i.Text)
	pc.loc(i.L)
}

func (t TypeVar) printTree(pc *config) {
	pc.p("TypeVar(%s)", t.Name)
	pc.loc(t.L)
}

func (i Ident) printTree(pc *config) {
	pc.p("Id(%s)", i.Name)
	pc.loc(i.L)
}

func (c Comment) printTree(pc *config) {
	pc.p("Comment(%s)", c.Text)
	pc.loc(c.L)
}

func (pc *config) loc(l loc.Loc) {
	if pc.files == nil || (l == loc.Loc{}) {
		return
	}
	pc.p("\t(%s)", pc.files.Location(l))
}

func (pc *config) field(name string, val interface{}) {
	v := reflect.ValueOf(val)
	if val == nil || (v.Kind() == reflect.Ptr || v.Kind() == reflect.Slice || v.Kind() == reflect.Interface) && v.IsNil() {
		return
	}
	pc.n++
	defer func() { pc.n-- }()
	pc.p("\n" + name + ": ")
	if v.Kind() == reflect.Slice {
		pc.slice(val)
		return
	}
	if t, ok := val.(treePrinter); ok {
		t.printTree(pc)
		return
	}
	pc.p("%v", val)
}

func (pc *config) slice(s interface{}) {
	v := reflect.ValueOf(s)
	if v.IsNil() {
		pc.p("nil")
		return
	}
	pc.n++
	pc.p("{")
	for i := 0; i < v.Len(); i++ {
		pc.p("\n")
		v.Index(i).Interface().(treePrinter).printTree(pc)
		pc.p(",")
	}
	pc.n--
	pc.p("\n}")
}

func (pc *config) p(f string, vs ...interface{}) {
	f = strings.ReplaceAll(f, "\n", "\n"+strings.Repeat(pc.ident, pc.n))
	_, err := fmt.Fprintf(pc.w, f, vs...)
	if err != nil {
		panic(printTreeError{err})
	}
}