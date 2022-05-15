package checker

import (
	"strconv"
	"strings"
)

func (v *VarDef) String() string   { return v.Name }
func (f *FuncDef) String() string  { return f.Name }
func (f *ParmDef) String() string  { return f.Name }
func (f *LocalDef) String() string { return f.Name }

func (b *BlockCap) String() string {
	switch {
	case b.Parm != nil:
		return b.Parm.String()
	case b.Local != nil:
		return b.Local.String()
	default:
		return b.Cap.String()
	}
}

func (r *RefType) String() string {
	return r.buildString(new(strings.Builder)).String()
}

func (d *DefType) String() string {
	return d.buildString(new(strings.Builder)).String()
}

func (a *ArrayType) String() string {
	return a.buildString(new(strings.Builder)).String()
}

func (s *StructType) String() string {
	return s.buildString(new(strings.Builder)).String()
}

func (u *UnionType) String() string {
	return u.buildString(new(strings.Builder)).String()
}

func (f *FuncType) String() string {
	return f.buildString(new(strings.Builder)).String()
}

func (t *TypeVar) String() string {
	return t.buildString(new(strings.Builder)).String()
}

func (b *BasicType) String() string {
	return b.buildString(new(strings.Builder)).String()
}

func (f *FuncDecl) String() string {
	return f.buildString(new(strings.Builder)).String()
}

func (f *FuncInst) String() string {
	return f.buildString(new(strings.Builder)).String()
}

func (e *Select) String() string {
	return e.buildString(new(strings.Builder)).String()
}

func (w *Switch) String() string {
	return w.buildString(new(strings.Builder)).String()
}

func (b *Builtin) String() string {
	return b.buildString(new(strings.Builder)).String()
}

func (c *Call) String() string {
	return c.buildString(new(strings.Builder)).String()
}

func (c *Convert) String() string {
	return c.buildString(new(strings.Builder)).String()
}

func (v *Var) String() string {
	return v.buildString(new(strings.Builder)).String()
}

func (l *Local) String() string {
	return l.buildString(new(strings.Builder)).String()
}

func (p *Parm) String() string {
	return p.buildString(new(strings.Builder)).String()
}

func (c *Cap) String() string {
	return c.buildString(new(strings.Builder)).String()
}

func (a *ArrayLit) String() string {
	return a.buildString(new(strings.Builder)).String()
}

func (t *StructLit) String() string {
	return t.buildString(new(strings.Builder)).String()
}

func (u *UnionLit) String() string {
	return u.buildString(new(strings.Builder)).String()
}

func (b *BlockLit) String() string {
	return b.buildString(new(strings.Builder)).String()
}

func (t *StrLit) String() string {
	return t.buildString(new(strings.Builder)).String()
}

func (i *IntLit) String() string {
	return i.buildString(new(strings.Builder)).String()
}

func (f *FloatLit) String() string {
	return f.buildString(new(strings.Builder)).String()
}

func (r *RefType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('&')
	r.Type.buildString(w)
	return w
}

func (d *DefType) buildString(w *strings.Builder) *strings.Builder {
	if needParens(d) {
		w.WriteRune('(')
	}
	for i, a := range d.Args {
		if i > 0 {
			w.WriteString(", ")
		}
		a.buildString(w)
	}
	switch {
	case needParens(d):
		w.WriteString(") ")
	case len(d.Args) == 1:
		w.WriteRune(' ')
	}
	if d.Def != nil && d.Def.File.Mod.Imported {
		w.WriteString(d.Def.File.Mod.Name())
		w.WriteRune('#')
	}
	w.WriteString(d.Name)
	return w
}

func needParens(d *DefType) bool {
	switch {
	case len(d.Args) == 0:
		return false
	case len(d.Args) > 1:
		return true
	default:
		_, ok := d.Args[0].(*RefType)
		return ok
	}
}

func (a *ArrayType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('[')
	a.ElemType.buildString(w)
	w.WriteRune(']')
	return w
}

func (s *StructType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('[')
	for i, f := range s.Fields {
		if i > 0 {
			w.WriteString(", ")
		}
		w.WriteString(f.Name)
		w.WriteRune(' ')
		f.Type.buildString(w)
	}
	if len(s.Fields) == 0 {
		w.WriteRune('.')
	}
	w.WriteRune(']')
	return w
}

func (u *UnionType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('[')
	for i, c := range u.Cases {
		if i > 0 {
			w.WriteString(", ")
		}
		w.WriteString(c.Name)
		if c.Type != nil {
			w.WriteRune(' ')
			c.Type.buildString(w)
		}
	}
	w.WriteRune(']')
	return w
}

func (f FuncType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('(')
	for i, p := range f.Parms {
		if i > 0 {
			w.WriteString(", ")
		}
		p.buildString(w)
	}
	w.WriteString("){")
	if f.Ret != nil && !isEmptyStruct(f.Ret) {
		f.Ret.buildString(w)
	}
	w.WriteRune('}')
	return w
}

func (t *TypeVar) buildString(w *strings.Builder) *strings.Builder {
	w.WriteString(t.Name)
	return w
}

func (k BasicTypeKind) String() string {
	switch k {
	case Bool:
		return "bool"
	case Int:
		return "int"
	case Int8:
		return "int8"
	case Int16:
		return "int16"
	case Int32:
		return "int32"
	case Int64:
		return "int64"
	case UintRef:
		return "uintref"
	case Uint:
		return "uint"
	case Uint8:
		return "uint8"
	case Uint16:
		return "uint16"
	case Uint32:
		return "uint32"
	case Uint64:
		return "uint64"
	case Float32:
		return "float32"
	case Float64:
		return "float64"
	case String:
		return "string"
	default:
		panic("impossible")
	}
}

func (b *BasicType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteString(b.Kind.String())
	return w
}

func (f *FuncDecl) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(f.Name)
	s.WriteRune('(')
	for i, p := range f.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		p.buildString(s)
	}
	s.WriteRune(')')
	if f.Ret != nil && !isEmptyStruct(f.Ret) {
		f.Ret.buildString(s)
	}
	return s
}

func (f *FuncInst) buildString(s *strings.Builder) *strings.Builder {
	if f.Def.File.Mod.Imported {
		s.WriteString(f.Def.Mod)
		s.WriteRune('#')
	}
	s.WriteString(f.Def.Name)
	s.WriteRune('(')
	for i, p := range f.T.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		p.buildString(s)
	}
	s.WriteRune(')')
	if f.T.Ret != nil && !isEmptyStruct(f.T.Ret) {
		f.T.Ret.buildString(s)
	}
	return s
}

func (e *Select) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("built-in ")
	s.WriteString(e.N)
	if e.Struct == nil {
		return s
	}
	s.WriteRune('(')
	if e.Parm == nil {
		s.WriteRune('_')
	} else {
		e.Parm.buildString(s)
	}
	s.WriteRune(')')
	if e.Ret == nil {
		s.WriteRune('_')
	} else {
		e.Ret.buildString(s)
	}
	return s
}

func (w *Switch) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("built-in ")
	for _, c := range w.Cases {
		if c == nil {
			s.WriteString("_?")
			continue
		}
		s.WriteString(c.Name)
	}
	if w.Union == nil {
		return s
	}
	s.WriteRune('(')
	for i, p := range w.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		if p == nil {
			s.WriteRune('_')
		} else {
			p.buildString(s)
		}
	}
	s.WriteRune(')')
	if w.Ret != nil && !isEmptyStruct(w.Ret) {
		w.Ret.buildString(s)
	}
	return s
}

func (o Op) String() string {
	switch o {
	case Assign:
		return "Assign"
	case NewArray:
		return "NewArray"
	case BitNot:
		return "BitNot"
	case BitXor:
		return "BitXor"
	case BitAnd:
		return "BitAnd"
	case BitOr:
		return "BitOr"
	case LeftShift:
		return "LeftShift"
	case RightShift:
		return "RightShift"
	case Negate:
		return "Negate"
	case Minus:
		return "Minus"
	case Plus:
		return "Plus"
	case Times:
		return "Times"
	case Divide:
		return "Divide"
	case Modulus:
		return "Modulus"
	case Eq:
		return "Eq"
	case Neq:
		return "Neq"
	case Less:
		return "Less"
	case LessEq:
		return "LessEq"
	case Greater:
		return "Greater"
	case GreaterEq:
		return "GreaterEq"
	case Index:
		return "Index"
	case Slice:
		return "Slice"
	case Length:
		return "Length"
	case Return:
		return "Return"
	case Panic:
		return "Panic"
	case Print:
		return "Print"
	default:
		panic("impossible")
	}
}

func (b *Builtin) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("built-in ")
	s.WriteString(b.name(false))
	s.WriteRune('(')
	for i, p := range b.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		if p == nil {
			s.WriteRune('_')
		} else {
			p.buildString(s)
		}
	}
	s.WriteRune(')')
	if b.Ret != nil && !isEmptyStruct(b.Ret) {
		b.Ret.buildString(s)
	}
	return s
}

func (c *Call) buildString(s *strings.Builder) *strings.Builder {
	if namer, ok := c.Func.(interface{ Name() string }); ok {
		s.WriteString(namer.Name())
	} else {
		c.Func.buildString(s)
	}
	s.WriteRune('(')
	for i, a := range c.Args {
		if i > 0 {
			s.WriteString(", ")
		}
		a.buildString(s)
	}
	s.WriteRune(')')
	return s
}

func (f *FuncInst) Name() string { return f.Def.Name }

func (e *Select) Name() string { return "(" + e.Field.Name + ")" }

func (w *Switch) Name() string {
	var s strings.Builder
	s.WriteRune('(')
	for _, c := range w.Cases {
		s.WriteString(c.Name)
	}
	s.WriteRune(')')
	return s.String()
}

func (b *Builtin) Name() string { return b.name(true) }

func (b *Builtin) name(paren bool) string {
	var s strings.Builder
	if paren {
		s.WriteRune('(')
	}
	switch b.Op {
	case Assign:
		s.WriteString(":=")
	case NewArray:
		s.WriteString("new")
	case BitNot:
		s.WriteRune('^')
	case BitXor:
		s.WriteRune('^')
	case BitAnd:
		s.WriteRune('&')
	case BitOr:
		s.WriteRune('|')
	case LeftShift:
		s.WriteString("<<")
	case RightShift:
		s.WriteString(">>")
	case Negate:
		s.WriteRune('-')
	case Minus:
		s.WriteRune('-')
	case Plus:
		s.WriteRune('+')
	case Times:
		s.WriteRune('*')
	case Divide:
		s.WriteRune('/')
	case Modulus:
		s.WriteRune('%')
	case Eq:
		s.WriteRune('=')
	case Neq:
		s.WriteString("!=")
	case Less:
		s.WriteRune('<')
	case LessEq:
		s.WriteString("<=")
	case Greater:
		s.WriteRune('>')
	case GreaterEq:
		s.WriteString(">=")
	case Index:
		s.WriteString("[]")
	case Slice:
		s.WriteString("[]")
	case Length:
		return ".length"
	case Panic:
		return "panic"
	case Return:
		if len(b.Parms) == 0 {
			return "return"
		} else {
			return "return:"
		}
	case Print:
		return "print"
	default:
		panic("impossible")
	}
	if paren {
		s.WriteRune(')')
	}
	return s.String()
}

func (c ConvertKind) String() string {
	switch c {
	case Noop:
		return "Noop"
	case Drop:
		return "Drop"
	case Deref:
		return "Deref"
	case Ref:
		return "Ref"
	case NumConvert:
		return "NumConvert"
	case StrConvert:
		return "StrConvert"
	case UnionConvert:
		return "UnionConvert"
	default:
		panic("impossible ConvertKind")
	}
}

func (c *Convert) buildString(s *strings.Builder) *strings.Builder {
	if !c.Explicit {
		return c.Expr.buildString(s)
	}
	c.T.buildString(s)
	s.WriteString(" : ")
	c.Expr.buildString(s)
	return s
}

func (v *Var) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(v.Def.Name)
	return s
}

func (l *Local) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(l.Def.Name)
	return s
}

func (p *Parm) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(p.Def.Name)
	return s
}

func (c Cap) buildString(s *strings.Builder) *strings.Builder {
	switch {
	case c.Def.Parm != nil:
		s.WriteString(c.Def.Parm.Name)
	case c.Def.Local != nil:
		s.WriteString(c.Def.Local.Name)
	case c.Def.Cap != nil:
		Cap{Def: c.Def.Cap}.buildString(s)
	}
	return s
}

func (a *ArrayLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteRune('[')
	for i, e := range a.Elems {
		if i > 0 {
			s.WriteString(", ")
		}
		e.buildString(s)
	}
	s.WriteRune(']')
	return s

}

func (t *StructLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteRune('[')
	for i, f := range t.Fields {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(t.Struct.Fields[i].Name)
		s.WriteRune(' ')
		f.buildString(s)
	}
	s.WriteRune(']')
	return s
}

func (u *UnionLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteRune('[')
	s.WriteString(u.Case.Name)
	if u.Val != nil {
		s.WriteRune(' ')
		u.Val.buildString(s)
	}
	s.WriteRune(']')
	return s
}

func (b *BlockLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteRune('(')
	for i, p := range b.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(p.Name)
		if p.T != nil {
			p.T.buildString(s)
		}
	}
	s.WriteString("){…}")
	return s
}

func (t *StrLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(strconv.Quote(t.Text))
	return s
}

func (i *IntLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(i.Text)
	return s
}

func (f *FloatLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(f.Text)
	return s
}
