package checker

import (
	"fmt"
	"io"
	"reflect"
	"strings"

	"github.com/eaburns/pea/loc"
)

type PrintOpt func(*config)

func PrintLocs(files loc.Files) PrintOpt {
	return func(pc *config) { pc.files = files }
}

func (m *Mod) Print(w io.Writer, opts ...PrintOpt) error {
	return print(w, m, opts...)
}

type config struct {
	w              io.Writer
	files          loc.Files
	n              int
	ident          string
	printTypeInsts bool
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
	pc := &config{
		w:              w,
		ident:          "  ",
		printTypeInsts: true,
	}
	for _, opt := range opts {
		opt(pc)
	}
	tree.print(pc)
	pc.p("\n")
	return err
}

func (m *Mod) print(pc *config) {
	pc.p("Mod{")
	pc.field("Path", m.Path)
	pc.field("Files", m.Files)
	pc.field("Defs", m.Defs)
	pc.p("\n}")
}

func (f *File) print(pc *config) {
	pc.p("File{")
	pc.field("Path", f.FilePath)
	pc.field("Imports", f.Imports)
	pc.p("\n}")
}

func (i *Import) print(pc *config) {
	pc.p("Import{")
	pc.loc(i.L)
	pc.field("Name", i.Name)
	pc.field("Path", i.Path)
	pc.field("Exp", i.Exp)
	pc.p("\n}")
}

func (v *VarDef) print(pc *config) {
	pc.p("VarDef{")
	pc.loc(v.L)
	pc.field("Mod", v.Mod)
	pc.field("Name", v.Name)
	pc.field("Const", v.Const)
	pc.field("Type", v.Type)
	pc.field("Expr", v.Expr)
	pc.field("Exp", v.Exp)
	pc.p("\n}")
}

func (t *TypeDef) print(pc *config) {
	pc.p("TypeDef{")
	pc.loc(t.L)
	pc.field("Mod", t.Mod)
	pc.field("Name", t.Name)
	pc.field("Exp", t.Exp)
	pc.field("Parms", t.Parms)
	printTypeInsts := pc.printTypeInsts
	pc.printTypeInsts = false
	pc.field("Type", t.Type)
	pc.field("Insts", t.Insts)
	pc.printTypeInsts = printTypeInsts
	pc.p("\n}")
}

func (t TypeParm) print(pc *config) {
	pc.p("TypeParm(%s)", t.Name)
	pc.loc(t.L)
}

func (t *TypeInst) print(pc *config) {
	pc.p("TypeInst{	<%p>", t)
	pc.field("Args", t.Args)
	pc.field("Type", t.Type)
	pc.p("\n}")
}

func (r *RefType) print(pc *config) {
	pc.p("RefType{")
	pc.loc(r.L)
	pc.field("Type", r.Type)
	pc.p("\n}")
}

func (d *DefType) print(pc *config) {
	if len(d.Args) == 0 && d.Def == nil {
		pc.p("DefType(%s)", d.Name)
		pc.loc(d.L)
		return
	}
	pc.p("DefType{")
	pc.loc(d.L)
	pc.field("Name", d.Name)
	pc.field("Args", d.Args)
	if d.Def != nil {
		pc.field("Def", fmt.Sprintf("{ Mod: %s, Name: %s }", d.Def.Mod, d.Def.Name))
		pc.loc(d.Def.L)
	}
	if pc.printTypeInsts {
		pc.printTypeInsts = false
		pc.field("Inst", d.Inst)
		pc.printTypeInsts = true
	} else {
		pc.field("Inst", fmt.Sprintf("<%p>", d.Inst))
	}
	pc.p("\n}")
}

func (a *ArrayType) print(pc *config) {
	pc.p("ArrayType{")
	pc.loc(a.L)
	pc.field("ElemType", a.ElemType)
	pc.p("\n}")
}

func (s *StructType) print(pc *config) {
	pc.p("StructType{")
	pc.loc(s.L)
	pc.field("Fields", s.Fields)
	pc.p("\n}")
}

func (f FieldDef) print(pc *config) {
	pc.p("FieldDef{")
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

func (c CaseDef) print(pc *config) {
	pc.p("CaseDef{")
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

func (t TypeVar) print(pc *config) {
	if t.Def == nil {
		pc.p("TypeVar(%s)", t.Name)
		pc.loc(t.L)
		return
	}
	pc.p("TypeVar{")
	pc.loc(t.L)
	pc.field("Name", t.Name)
	if t.Def != nil {
		pc.field("Def", pc.files.Location(t.Def.L))
	}
	pc.p("\n}")
}

func (b BasicType) print(pc *config) {
	pc.p("BasicType{%s}", b.Kind)
	pc.loc(b.L)
}

func (f *FuncDef) print(pc *config) {
	pc.p("FuncDef{")
	pc.loc(f.L)
	pc.field("Mod", f.Mod)
	pc.field("Name", f.Name)
	pc.field("TypeParms", f.TypeParms)
	pc.field("Parms", f.Parms)
	pc.field("Ret", f.Ret)
	pc.field("Iface", f.Iface)
	pc.field("Exprs", f.Exprs)
	pc.field("Exp", f.Exp)
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

func (t *TestDef) print(pc *config) {
	pc.p("TestDef{")
	pc.loc(t.L)
	pc.field("Mod", t.Mod)
	pc.field("Name", t.Name)
	pc.field("Exprs", t.Exprs)
	pc.p("\n}")
}

func (c *Call) print(pc *config) {
	pc.p("Call{")
	pc.loc(c.L)
	pc.field("Fun", c.Fun)
	pc.field("Args", c.Args)
	pc.field("Type", c.Type)
	pc.p("\n}")
}

func (s *Select) print(pc *config) {
	pc.p("Select{")
	pc.field("Type", s.Type)
	pc.field("Field", s.Field)
	pc.p("\n}")
}

func (s *Switch) print(pc *config) {
	pc.p("Switch{")
	pc.field("Type", s.Type)
	pc.field("Cases", s.Cases)
	pc.field("Ret", s.Ret)
	pc.p("\n}")
}

func (b *Builtin) print(pc *config) {
	pc.p("Builtin{")
	pc.field("Op", b.Op)
	pc.field("Parms", b.Ps)
	pc.field("Ret", b.R)
	pc.p("\n}")
}

func (d *Deref) print(pc *config) {
	pc.p("Deref{")
	pc.loc(d.L)
	pc.field("Expr", d.Expr)
	pc.field("Type", d.T)
	pc.p("\n}")
}

func (v *Var) print(pc *config) {
	pc.p("Var{")
	pc.loc(v.L)
	pc.field("Global", v.Global)
	pc.field("Local", v.Local)
	pc.field("Parm", v.Parm)
	pc.field("Cap", v.Cap)
	pc.field("Type", v.T)
	pc.p("\n}")
}

func (a *ArrayLit) print(pc *config) {
	pc.p("ArrayLit{")
	pc.loc(a.L)
	pc.field("Elems", a.Elems)
	pc.field("Type", a.T)
	pc.p("\n}")
}

func (s *StructLit) print(pc *config) {
	pc.p("StructLit{")
	pc.loc(s.L)
	pc.field("Fields", s.Fields)
	pc.field("Type", s.T)
	pc.p("\n}")
}

func (u *UnionLit) print(pc *config) {
	pc.p("UnionLit{")
	pc.loc(u.L)
	pc.field("Case", u.Case)
	pc.field("Val", u.Val)
	pc.field("Type", u.T)
	pc.p("\n}")
}

func (b *BlockLit) print(pc *config) {
	pc.p("BlockLit{")
	pc.loc(b.L)
	pc.field("Caps", b.Caps)
	pc.field("Parms", b.Parms)
	pc.field("Locals", b.Locals)
	pc.field("Ret", b.Ret)
	pc.field("Type", b.T)
	pc.field("Exprs", b.Exprs)
	pc.p("\n}")
}

func (b *BlockCap) print(pc *config) {
	pc.p("BlockCap{")
	pc.loc(b.L)
	pc.field("Parm", b.Parm)
	pc.field("Local", b.Local)
	pc.field("Cap", b.Cap)
	pc.field("Type", b.T)
	pc.p("\n}")
}

func (s *StrLit) print(pc *config) {
	pc.p("StrLit{")
	pc.loc(s.L)
	pc.field("Text", s.Text)
	pc.field("Type", s.T)
	pc.p("\n}")
}

func (i *IntLit) print(pc *config) {
	pc.p("IntLit{")
	pc.loc(i.L)
	pc.field("Text", i.Text)
	pc.field("Type", i.T)
	pc.p("\n}")
}

func (f *FloatLit) print(pc *config) {
	pc.p("FloatLit{")
	pc.loc(f.L)
	pc.field("Text", f.Text)
	pc.field("Type", f.T)
	pc.p("\n}")
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
	if s, ok := val.(string); ok && s == "" && name == "Mod" {
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
	if l, ok := val.(loc.Location); ok {
		pc.p("(%v)", l)
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
