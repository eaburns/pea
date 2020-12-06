package tree

import (
	"fmt"
	"io"
	"reflect"
	"strings"

	"github.com/eaburns/pea/loc"
)

type PrintOpt func(*config)

func PrintLocs(fs ...*File) PrintOpt {
	var files loc.Files
	for _, f := range fs {
		files = append(files, f)
	}
	return func(pc *config) { pc.files = files }
}

func (f *File) Print(w io.Writer, opts ...PrintOpt) error {
	return print(w, f, opts...)
}

type config struct {
	w     io.Writer
	files loc.Files
	n     int
	ident string
}

type printerError struct{ error }

type printer interface {
	print(*config)
}

func print(w io.Writer, tree printer, opts ...PrintOpt) (err error) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		if e, ok := r.(printerError); ok {
			err = e
		} else {
			panic(r)
		}
	}()
	pc := &config{w: w, ident: "  "}
	for _, opt := range opts {
		opt(pc)
	}
	tree.print(pc)
	pc.p("\n")
	return err
}

func (f *File) print(pc *config) {
	pc.p("File{")
	pc.field("Path", f.P)
	pc.field("Imports", f.Imports)
	pc.field("Defs", f.Defs)
	pc.p("\n}")
}

func (i *Import) print(pc *config) {
	pc.p("Import{")
	pc.loc(i.L)
	pc.field("Name", i.Name)
	pc.field("Path", i.Path)
	pc.p("\n}")
}

func (v *VarDef) print(pc *config) {
	pc.p("VarDef{")
	pc.loc(v.L)
	pc.field("Exp", v.Exp)
	pc.field("Const", v.Const)
	pc.field("Name", v.Name)
	pc.field("Type", v.Type)
	pc.field("Expr", v.Expr)
	pc.p("\n}")
}

func (t *TypeDef) print(pc *config) {
	pc.p("TypeDef{")
	pc.loc(t.L)
	pc.field("Exp", t.Exp)
	pc.field("TypeParms", t.TypeParms)
	pc.field("Name", t.Name)
	pc.field("Type", t.Type)
	pc.p("\n}")
}

func (r *RefType) print(pc *config) {
	pc.p("RefType{")
	pc.loc(r.L)
	pc.field("Type", r.Type)
	pc.p("\n}")
}

func (n *NamedType) print(pc *config) {
	pc.p("NamedType{")
	pc.loc(n.L)
	pc.field("Mod", n.Mod)
	pc.field("Name", n.Name)
	pc.field("Args", n.Args)
	pc.p("\n}")
}

func (s *StructType) print(pc *config) {
	pc.p("StructType{")
	pc.loc(s.L)
	pc.field("Fields", s.Fields)
	pc.p("\n}")
}

func (f Field) print(pc *config) {
	pc.p("Field{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Type", f.Type)
	pc.p("\n}")
}

func (u *UnionType) print(pc *config) {
	pc.p("UnionType{")
	pc.loc(u.L)
	pc.field("Cases", u.Cases)
	pc.p("\n}")
}

func (c Case) print(pc *config) {
	pc.p("Case{")
	pc.loc(c.L)
	pc.field("Name", c.Name)
	pc.field("Type", c.Type)
	pc.p("\n}")
}

func (f *FuncType) print(pc *config) {
	pc.p("FuncType{")
	pc.loc(f.L)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.p("\n}")
}

func (f *FuncDef) print(pc *config) {
	pc.p("FuncDef{")
	pc.loc(f.L)
	pc.field("Expr", f.Exp)
	pc.field("Name", f.Name)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.field("Iface", f.Iface)
	pc.field("Exprs", f.Exprs)
	pc.p("\n}")
}

func (f FuncParm) print(pc *config) {
	pc.p("FuncParm{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Type", f.Type)
	pc.field("Init", f.Init)
	pc.p("\n}")
}

func (f FuncDecl) print(pc *config) {
	pc.p("FuncDecl{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.p("\n}")
}

func (f *TestDef) print(pc *config) {
	pc.p("TestDef{")
	pc.loc(f.L)
	pc.field("Name", f.Name)
	pc.field("Exprs", f.Exprs)
	pc.p("\n}")
}

func (c *Call) print(pc *config) {
	pc.p("Call{")
	pc.loc(c.L)
	pc.field("Fun", c.Fun)
	pc.field("Args", c.Args)
	pc.p("\n}")
}

func (c *Convert) print(pc *config) {
	pc.p("Convert{")
	pc.loc(c.L)
	pc.field("Expr", c.Expr)
	pc.field("Type", c.Type)
	pc.p("\n}")
}

func (s *SubExpr) print(pc *config) {
	pc.p("SubExpr{")
	pc.loc(s.L)
	pc.field("Expr", s.Expr)
	pc.p("\n}")
}

func (a *CompLit) print(pc *config) {
	pc.p("CompLit{")
	pc.loc(a.L)
	pc.field("Exprs", a.Exprs)
	pc.p("\n}")
}

func (b *BlkLit) print(pc *config) {
	pc.p("BlockLit{")
	pc.loc(b.L)
	pc.field("Parms", b.Parms)
	pc.field("Exprs", b.Exprs)
	pc.p("\n}")
}

func (c *CharLit) print(pc *config) {
	pc.p("CharLit(%c)", c.Rune)
	pc.loc(c.L)
}

func (s *StrLit) print(pc *config) {
	pc.p("StrLit(%q)", s.Data)
	pc.loc(s.L)
}

func (i *IntLit) print(pc *config) {
	pc.p("IntLit(%s)", i.Text)
	pc.loc(i.L)
}

func (i *FloatLit) print(pc *config) {
	pc.p("FloatLit(%s)", i.Text)
	pc.loc(i.L)
}

func (t TypeVar) print(pc *config) {
	pc.p("TypeVar(%s)", t.Name)
	pc.loc(t.L)
}

func (i Id) print(pc *config) {
	pc.p("Id(%s)", i.Name)
	pc.loc(i.L)
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
	if t, ok := val.(printer); ok {
		t.print(pc)
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
		v.Index(i).Interface().(printer).print(pc)
		pc.p(",")
	}
	pc.n--
	pc.p("\n}")
}

func (pc *config) p(f string, vs ...interface{}) {
	f = strings.ReplaceAll(f, "\n", "\n"+strings.Repeat(pc.ident, pc.n))
	_, err := fmt.Fprintf(pc.w, f, vs...)
	if err != nil {
		panic(printerError{err})
	}
}
