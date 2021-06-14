package flowgraph

import (
	"fmt"
	"strings"
)

func (m *Mod) String() string        { return m.buildString(new(strings.Builder)).String() }
func (t *IntType) String() string    { return t.buildString(new(strings.Builder)).String() }
func (t *FloatType) String() string  { return t.buildString(new(strings.Builder)).String() }
func (t *AddrType) String() string   { return t.buildString(new(strings.Builder)).String() }
func (t *ArrayType) String() string  { return t.buildString(new(strings.Builder)).String() }
func (t *FrameType) String() string  { return t.buildString(new(strings.Builder)).String() }
func (t *StructType) String() string { return t.buildString(new(strings.Builder)).String() }
func (t *UnionType) String() string  { return t.buildString(new(strings.Builder)).String() }
func (t *FuncType) String() string   { return t.buildString(new(strings.Builder)).String() }
func (d *StrDef) String() string     { return d.buildString(new(strings.Builder)).String() }
func (d *VarDef) String() string     { return d.buildString(new(strings.Builder)).String() }
func (d *FuncDef) String() string    { return d.buildString(new(strings.Builder)).String() }
func (b *BasicBlock) String() string { return b.buildString(new(strings.Builder)).String() }
func (r *Store) String() string      { return r.buildString(new(strings.Builder)).String() }
func (r *Copy) String() string       { return r.buildString(new(strings.Builder)).String() }
func (r *Call) String() string       { return r.buildString(new(strings.Builder)).String() }
func (r *If) String() string         { return r.buildString(new(strings.Builder)).String() }
func (r *Jump) String() string       { return r.buildString(new(strings.Builder)).String() }
func (r *Return) String() string     { return r.buildString(new(strings.Builder)).String() }
func (v *Frame) String() string      { return v.buildString(new(strings.Builder)).String() }
func (v *Alloc) String() string      { return v.buildString(new(strings.Builder)).String() }
func (v *Load) String() string       { return v.buildString(new(strings.Builder)).String() }
func (v *Func) String() string       { return v.buildString(new(strings.Builder)).String() }
func (v *String) String() string     { return v.buildString(new(strings.Builder)).String() }
func (v *Var) String() string        { return v.buildString(new(strings.Builder)).String() }
func (v *Parm) String() string       { return v.buildString(new(strings.Builder)).String() }
func (v *Field) String() string      { return v.buildString(new(strings.Builder)).String() }
func (v *Case) String() string       { return v.buildString(new(strings.Builder)).String() }
func (v *Index) String() string      { return v.buildString(new(strings.Builder)).String() }
func (v *Slice) String() string      { return v.buildString(new(strings.Builder)).String() }
func (v *Int) String() string        { return v.buildString(new(strings.Builder)).String() }
func (v *Float) String() string      { return v.buildString(new(strings.Builder)).String() }
func (v *Null) String() string       { return v.buildString(new(strings.Builder)).String() }
func (v *Op) String() string         { return v.buildString(new(strings.Builder)).String() }

func (m *Mod) buildString(s *strings.Builder) *strings.Builder {
	for _, t := range m.Types {
		if s.Len() > 0 {
			s.WriteRune('\n')
		}
		s.WriteString("type ")
		t.buildString(s)
		s.WriteRune(' ')
		// Make a copy without a name, so it builds as a literal.
		lit := *t
		lit.Mod = ""
		lit.Name = ""
		lit.Args = nil
		lit.buildString(s)
	}
	for _, t := range m.Strings {
		if s.Len() > 0 {
			s.WriteRune('\n')
		}
		t.buildString(s)
	}
	for _, v := range m.Vars {
		if s.Len() > 0 {
			s.WriteRune('\n')
		}
		v.buildString(s)
	}
	for _, f := range m.Funcs {
		if s.Len() > 0 {
			s.WriteString("\n\n")
		}
		f.buildString(s)
	}
	return s
}

func (t *IntType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *IntType) buildStringRecur(_ map[Type]bool, s *strings.Builder) *strings.Builder {
	if t.Unsigned {
		fmt.Fprintf(s, "uint%d", t.Size)
	} else {
		fmt.Fprintf(s, "int%d", t.Size)
	}
	return s
}

func (t *FloatType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *FloatType) buildStringRecur(_ map[Type]bool, s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "float%d", t.Size)
	return s
}

func (t *AddrType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *AddrType) buildStringRecur(seen map[Type]bool, s *strings.Builder) *strings.Builder {
	s.WriteRune('*')
	t.Elem.buildStringRecur(seen, s)
	return s
}

func (t *ArrayType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *ArrayType) buildStringRecur(seen map[Type]bool, s *strings.Builder) *strings.Builder {
	s.WriteString("[]")
	t.Elem.buildStringRecur(seen, s)
	return s
}

func (t *FrameType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *FrameType) buildStringRecur(_ map[Type]bool, s *strings.Builder) *strings.Builder {
	s.WriteString("<frame>")
	return s
}

func (t *StructType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *StructType) buildStringRecur(seen map[Type]bool, s *strings.Builder) *strings.Builder {
	if t.Name != "" {
		if t.Mod != "" {
			s.WriteString(t.Mod)
			s.WriteRune('.')
		}
		s.WriteString(t.Name)
		if len(t.Args) > 0 {
			// The only recursive types built are named structs.
			// The only way their strings can be recursive
			// is through the argument list; break it here.
			if seen[t] {
				s.WriteString("<…>")
				return s
			}
			seen[t] = true

			s.WriteRune('<')
			for i, a := range t.Args {
				if i > 0 {
					s.WriteString(", ")
				}
				a.buildStringRecur(seen, s)
			}
			s.WriteRune('>')
		}
		return s
	}
	s.WriteString("struct{")
	for i, f := range t.Fields {
		if i > 0 {
			s.WriteString("; ")
		}
		s.WriteString(f.Name)
		s.WriteRune(' ')
		if f.Type == nil {
			s.WriteString("<nil>")
		} else {
			f.Type.buildStringRecur(seen, s)
		}
	}
	s.WriteRune('}')
	return s
}

func (t *UnionType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *UnionType) buildStringRecur(seen map[Type]bool, s *strings.Builder) *strings.Builder {
	s.WriteString("union{")
	for i, f := range t.Cases {
		if i > 0 {
			s.WriteString("; ")
		}
		s.WriteString(f.Name)
		s.WriteRune(' ')
		f.Type.buildStringRecur(seen, s)
	}
	s.WriteRune('}')
	return s
}

func (t *FuncType) buildString(s *strings.Builder) *strings.Builder {
	return t.buildStringRecur(make(map[Type]bool), s)
}

func (t *FuncType) buildStringRecur(seen map[Type]bool, s *strings.Builder) *strings.Builder {
	s.WriteString("func(")
	for i, p := range t.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		p.buildStringRecur(seen, s)
	}
	s.WriteRune(')')
	if !t.Ret.isEmpty() {
		t.Ret.buildStringRecur(seen, s)
	}
	return s
}

func (d *StrDef) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "string%d := %q", d.Num, d.Text)
	return s
}

func (d *VarDef) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("var ")
	s.WriteString(d.Name)
	s.WriteRune(' ')
	d.Type.buildString(s)
	return s
}

func (d *FuncDef) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "func \"%s\"(", d.SourceName)
	for i, p := range d.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(p.Name)
		s.WriteRune(' ')
		p.Type.buildString(s)
	}
	s.WriteRune(')')
	if len(d.Blocks) == 0 {
		return s
	}
	s.WriteString(" {")
	for _, b := range d.Blocks {
		s.WriteRune('\n')
		b.buildString(s)
	}
	s.WriteString("\n}")
	return s
}

func (b *BasicBlock) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "%d:\tin=[", b.Num)
	for i, in := range b.In() {
		if i > 0 {
			s.WriteString(", ")
		}
		fmt.Fprintf(s, "%d", in.Num)
	}
	s.WriteString("], out=[")
	for i, out := range b.Out() {
		if i > 0 {
			s.WriteString(", ")
		}
		fmt.Fprintf(s, "%d", out.Num)
	}
	s.WriteRune(']')
	for _, instr := range b.Instrs {
		if instr.Comment() != "" {
			s.WriteString("\n    // ")
			s.WriteString(instr.Comment())
		}
		s.WriteString("\n    ")
		instr.buildString(s)
	}
	return s
}

func (r *Store) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "store(*x%d, x%d)", r.Dst.Num(), r.Src.Num())
	col.addCol("// store(%s, %s)", r.Dst.Type(), r.Src.Type())
	return s
}

func (r *Copy) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "copy(x%d, x%d)", r.Dst.Num(), r.Src.Num())
	col.addCol("// copy(%s, %s)", r.Dst.Type(), r.Src.Type())
	return s
}

func (r *Call) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "x%d(", r.Func.Num())
	for i, arg := range r.Args {
		if i > 0 {
			s.WriteString(", ")
		}
		fmt.Fprintf(s, "x%d", arg.Num())
	}
	s.WriteRune(')')
	return s
}

func (r *If) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "if x%d %s %d then %d else %d",
		r.Value.Num(), r.Op, r.X, r.Yes.Num, r.No.Num)
	return s
}

func (r *Jump) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "jump %d", r.Dst.Num)
	return s
}

func (r *Return) buildString(s *strings.Builder) *strings.Builder {
	if r.Frame != nil {
		fmt.Fprintf(s, "return to x%d", r.Frame.Num())
		return s
	}
	s.WriteString("return")
	return s
}

func (v *Frame) buildString(s *strings.Builder) *strings.Builder {
	fmt.Fprintf(s, "x%d := <frame>", v.Num())
	return s
}

func (v *Alloc) buildString(s *strings.Builder) *strings.Builder {
	name := "alloc"
	if v.Stack {
		name = "aalloc"
	}
	switch t := v.Type().(type) {
	case *AddrType:
		fmt.Fprintf(s, "x%d := %s(%s)", v.Num(), name, t.Elem)
	case *ArrayType:
		switch {
		case v.Count != nil:
			fmt.Fprintf(s, "x%d := %s(x%d, %s)", v.Num(), name, v.Count.Num(), t.Elem)
		case v.CountImm >= 0:
			fmt.Fprintf(s, "x%d := %s(%d, %s)", v.Num(), name, v.CountImm, t.Elem)
		default:
			panic("bad array alloc: no count")
		}
	default:
		panic(fmt.Sprintf("bad alloc type: %T", v.Type()))
	}
	return s
}

func (v *Load) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := *x%d", v.Num(), v.Addr.Num())
	col.addCol("// %s", v.Type())
	return s
}

func (v *Func) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := &\"%s\"", v.Num(), v.Def.SourceName)
	col.addCol("// %s", v.Type())
	return s
}

func (v *String) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := &%s.string%d", v.Num(), v.Def.Mod.Path, v.Def.Num)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Var) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := &%s.%s", v.Num(), v.Def.Mod, v.Def.Name)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Parm) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := %s", v.Num(), v.Def.Name)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Field) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := &x%d.%s", v.Num(), v.Base.Num(), v.Def.Name)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Case) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := &x%d.%s", v.Num(), v.Base.Num(), v.Def.Name)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Index) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := &x%d[x%d]", v.Num(), v.Base.Num(), v.Index.Num())
	col.addCol("// %s", v.Type())
	return s
}

func (v *Slice) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := x%d[x%d:]", v.Num(), v.Base.Num(), v.Index.Num())
	col.addCol("// %s", v.Type())
	return s
}

func (v *Int) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := %s", v.Num(), v.Text)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Float) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := %s", v.Num(), v.Text)
	col.addCol("// %s", v.Type())
	return s
}

func (v *Null) buildString(s *strings.Builder) *strings.Builder {
	col := newCol(s, "x%d := null", v.Num())
	col.addCol("// %s", v.Type())
	return s
}

func (o OpKind) String() string {
	switch o {
	case BitNot:
		return "^"
	case BitXor:
		return "^"
	case BitAnd:
		return "&"
	case BitOr:
		return "|"
	case LeftShift:
		return "<<"
	case RightShift:
		return ">>"
	case Negate:
		return "-"
	case Minus:
		return "-"
	case Plus:
		return "+"
	case Times:
		return "*"
	case Divide:
		return "/"
	case Modulus:
		return "%"
	case Eq:
		return "=="
	case Neq:
		return "!="
	case Less:
		return "<"
	case LessEq:
		return "<="
	case Greater:
		return ">"
	case GreaterEq:
		return ">="
	default:
		panic(fmt.Sprintf("impossible Op: %d", o))
	}
}

func (v *Op) buildString(s *strings.Builder) *strings.Builder {
	switch {
	case v.Op == NumConvert:
		col := newCol(s, "x%d := %s(x%d)", v.Num(), v.Type(), v.Args[0].Num())
		col.addCol("// %s", v.Type())
	case v.Op == Panic:
		fmt.Fprintf(s, "panic(x%d)", v.Args[0].Num())
	case v.Op == Print:
		fmt.Fprintf(s, "print(x%d)", v.Args[0].Num())
	case len(v.Args) == 1:
		col := newCol(s, "x%d := %sx%d", v.Num(), v.Op, v.Args[0].Num())
		col.addCol("// %s := %s%s", v.Type(), v.Op, v.Args[0].Type())
	case len(v.Args) == 2:
		col := newCol(s, "x%d := x%d %s x%d", v.Num(), v.Args[0].Num(), v.Op, v.Args[1].Num())
		col.addCol("// %s := %s %s %s", v.Type(), v.Args[0].Type(), v.Op, v.Args[1].Type())
	default:
		panic("impossible")
	}
	return s
}

const colWidth = 40

type col struct {
	s     *strings.Builder
	width int
}

func newCol(s *strings.Builder, f string, vs ...interface{}) col {
	start := s.Len()
	fmt.Fprintf(s, f, vs...)
	return col{s: s, width: s.Len() - start}
}

func (c col) addCol(f string, vs ...interface{}) col {
	if pad := colWidth - c.width; pad > 0 {
		c.s.WriteString(strings.Repeat(" ", pad))
	}
	s := fmt.Sprintf(f, vs...)
	if len(s) > colWidth {
		s = s[:colWidth-1] + "…"
	}
	return newCol(c.s, s)
}
