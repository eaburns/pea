package checker

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"
)

type stringBuilder struct {
	// fullString specifies to use the full name for everything.
	// This is intended for test strings.
	fullString bool

	// typeParms are the current type parameters;
	// their type variables are re-named to ?#,
	// where # is teh index of the TypeParm in the array.
	typeParms []*TypeParm
	// typeParm0Name is the name given to the 0th type parameter.
	// When first named, if there is only one type parameter, its name is ?.
	// For correctness, this choice must be carried through
	// even if typeParms is later appended to.
	typeParm0Name string
	useSubScripts bool

	// elideMod is a module name to elide in type names.
	elideMod string

	builder strings.Builder
}

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

func (d *IfaceDef) String() string {
	var w stringBuilder
	d.buildString(&w)
	return w.builder.String()
}

func (p TypePattern) String() string {
	var w stringBuilder
	p.buildString(&w)
	return w.builder.String()
}

func (r *RefType) String() string {
	var w stringBuilder
	r.buildString(&w)
	return w.builder.String()
}

func (d *DefType) String() string {
	var w stringBuilder
	d.buildString(&w)
	return w.builder.String()
}

func (a *ArrayType) String() string {
	var w stringBuilder
	a.buildString(&w)
	return w.builder.String()
}

func (s *StructType) String() string {
	var w stringBuilder
	s.buildString(&w)
	return w.builder.String()
}

func (u *UnionType) String() string {
	var w stringBuilder
	u.buildString(&w)
	return w.builder.String()
}

func (f *FuncType) String() string {
	var w stringBuilder
	f.buildString(&w)
	return w.builder.String()
}

func (t *TypeVar) String() string {
	var w stringBuilder
	t.buildString(&w)
	return w.builder.String()
}

func (b *BasicType) String() string {
	var w stringBuilder
	b.buildString(&w)
	return w.builder.String()
}

func (f *FuncDecl) String() string {
	var w stringBuilder
	f.buildString(&w)
	return w.builder.String()
}

func (f *FuncInst) String() string {
	var w stringBuilder
	f.buildString(&w)
	return w.builder.String()
}

func (e *Select) String() string {
	var w stringBuilder
	e.buildString(&w)
	return w.builder.String()
}

func (s *Switch) String() string {
	var w stringBuilder
	s.buildString(&w)
	return w.builder.String()
}

func (b *Builtin) String() string {
	var w stringBuilder
	b.buildString(&w)
	return w.builder.String()
}

func (c *Call) String() string {
	var w stringBuilder
	c.buildString(&w)
	return w.builder.String()
}

func (c *Convert) String() string {
	var w stringBuilder
	c.buildString(&w)
	return w.builder.String()
}

func (v *Var) String() string {
	var w stringBuilder
	v.buildString(&w)
	return w.builder.String()
}

func (l *Local) String() string {
	var w stringBuilder
	l.buildString(&w)
	return w.builder.String()
}

func (p *Parm) String() string {
	var w stringBuilder
	p.buildString(&w)
	return w.builder.String()
}

func (c *Cap) String() string {
	var w stringBuilder
	c.buildString(&w)
	return w.builder.String()
}

func (a *ArrayLit) String() string {
	var w stringBuilder
	a.buildString(&w)
	return w.builder.String()
}

func (t *StructLit) String() string {
	var w stringBuilder
	t.buildString(&w)
	return w.builder.String()
}

func (u *UnionLit) String() string {
	var w stringBuilder
	u.buildString(&w)
	return w.builder.String()
}

func (b *BlockLit) String() string {
	var w stringBuilder
	b.buildString(&w)
	return w.builder.String()
}

func (t *StrLit) String() string {
	var w stringBuilder
	t.buildString(&w)
	return w.builder.String()
}

func (i *IntLit) String() string {
	var w stringBuilder
	i.buildString(&w)
	return w.builder.String()
}

func (f *FloatLit) String() string {
	var w stringBuilder
	f.buildString(&w)
	return w.builder.String()
}

func (w *stringBuilder) WriteString(s string) {
	w.builder.WriteString(s)
}

func (w *stringBuilder) WriteRune(r rune) {
	w.builder.WriteRune(r)
}

func (pat TypePattern) buildString(w *stringBuilder) {
	orig := w.typeParms
	defer func() { w.typeParms = orig }()
	w.typeParms = appendTypeParmsToCopy(orig, pat.Parms)
	pat.Type.buildString(w)
}

func (d *IfaceDef) buildString(w *stringBuilder) {
	switch len(d.Parms) {
	case 0:
		break
	case 1:
		w.WriteString(d.Parms[0].Name)
		w.WriteString(" ")
	default:
		w.WriteString("(")
		for i, p := range d.Parms {
			if i > 0 {
				w.WriteString(", ")
			}
			w.WriteString(p.Name)
		}
		w.WriteString(") ")
	}
	if d.File.Mod.Imported && d.Mod != w.elideMod {
		w.WriteString(d.Mod)
		w.WriteRune('#')
	}
	w.WriteString(d.Name)
}

func (r *RefType) buildString(w *stringBuilder) {
	w.WriteRune('&')
	r.Type.buildString(w)
}

func (d *DefType) buildString(w *stringBuilder) {
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
	if d.Def != nil && d.Def.File.Mod.Imported && d.Def.Mod != w.elideMod {
		w.WriteString(d.Def.Mod)
		w.WriteRune('#')
	}
	w.WriteString(d.Name)
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

func (a *ArrayType) buildString(w *stringBuilder) {
	w.WriteRune('[')
	a.ElemType.buildString(w)
	w.WriteRune(']')
}

func (s *StructType) buildString(w *stringBuilder) {
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
}

func (u *UnionType) buildString(w *stringBuilder) {
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
}

func (f FuncType) buildString(w *stringBuilder) {
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
}

func toSub(s string) string {
	s = strings.ReplaceAll(s, "0", "₀")
	s = strings.ReplaceAll(s, "1", "₁")
	s = strings.ReplaceAll(s, "2", "₂")
	s = strings.ReplaceAll(s, "3", "₃")
	s = strings.ReplaceAll(s, "4", "₄")
	s = strings.ReplaceAll(s, "5", "₅")
	s = strings.ReplaceAll(s, "6", "₆")
	s = strings.ReplaceAll(s, "7", "₇")
	s = strings.ReplaceAll(s, "8", "₈")
	s = strings.ReplaceAll(s, "9", "₉")
	return s
}

func (t *TypeVar) buildString(w *stringBuilder) {
	parmIndex := -1
	for i, p := range w.typeParms {
		if t.Def == p {
			parmIndex = i
			break
		}
	}
	switch {
	case parmIndex < 0:
		if t.Def != nil {
			w.WriteString(t.Def.Name)
		} else {
			w.WriteString(t.SourceName)
		}
	case parmIndex == 0:
		switch {
		case w.typeParm0Name == "" && len(w.typeParms) == 1:
			w.typeParm0Name = "?"
		case w.typeParm0Name == "":
			w.typeParm0Name = "?0"
			if w.useSubScripts {
				w.typeParm0Name = toSub(w.typeParm0Name)
			}
		}
		w.WriteString(w.typeParm0Name)
	default:
		s := fmt.Sprintf("?%d", parmIndex)
		if w.useSubScripts {
			s = toSub(s)
		}
		w.WriteString(s)
	}
}

func (k BasicTypeKind) String() string {
	switch k {
	case End:
		return "!"
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
	case Int128:
		return "int128"
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
	case Uint128:
		return "uint128"
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

func (b *BasicType) buildString(w *stringBuilder) {
	w.WriteString(b.Kind.String())
}

func (f *FuncDecl) buildString(s *stringBuilder) {
	s.WriteString(f.Name)
	s.WriteRune('(')
	buildParmsString(f.Parms, s)
	s.WriteRune(')')
	if f.Ret != nil && !isEmptyStruct(f.Ret) {
		f.Ret.buildString(s)
	}
}

func (f *FuncInst) buildString(s *stringBuilder) {
	if f.Def.File.Mod.Imported {
		s.WriteString(f.Def.Mod)
		if !s.fullString {
			s.elideMod = unambiguousMod(f)
		}
		s.WriteRune('#')
	}
	s.WriteString(f.Def.Name)
	s.WriteRune('(')
	buildParmsString(f.T.Parms, s)
	s.WriteRune(')')
	if f.T.Ret != nil && !isEmptyStruct(f.T.Ret) {
		f.T.Ret.buildString(s)
	}
}

func unambiguousMod(f *FuncInst) string {
	if !f.Def.File.Mod.Imported {
		return ""
	}
	thisModDefs := make(map[string]bool)
	otherModDefs := make(map[string]bool)
	for _, typ := range defTypes(f) {
		if typ.Def != nil && typ.Def.Mod == f.Def.Mod {
			thisModDefs[typ.Name] = true
		} else {
			otherModDefs[typ.Name] = true
		}
	}
	for name := range thisModDefs {
		if otherModDefs[name] {
			return ""
		}
	}
	return f.Def.Mod
}

func defTypes(f *FuncInst) []*DefType {
	var defTypes []*DefType
	walkType(f.T, func(t Type) bool {
		if dt, ok := t.(*DefType); ok {
			defTypes = append(defTypes, dt)
		}
		return false
	})
	return defTypes
}

func buildParmsString(parms []Type, s *stringBuilder) {
	argsLen := 0
	var prev Type
	var canCollapse bool
	for _, p := range parms {
		if prev != nil && eqType(prev, p) {
			canCollapse = true
		}
		prev = p
		argsLen += utf8.RuneCountInString(p.String())
	}
	if s.fullString || !canCollapse || argsLen < 20 {
		for i, p := range parms {
			if i > 0 {
				s.WriteString(", ")
			}
			if p == nil {
				s.WriteString("?")
			} else {
				p.buildString(s)
			}
		}
		return
	}
	for i, p := range parms {
		if i < len(parms)-1 && eqType(p, parms[i+1]) {
			s.WriteString("_, ")
			continue
		}
		s.WriteString("_ ")
		if p == nil {
			s.WriteString("?")
		} else {
			p.buildString(s)
		}
		if i < len(parms)-1 {
			s.WriteString(", ")
		}
	}
}

func (e *Select) buildString(s *stringBuilder) {
	s.WriteString("built-in ")
	s.WriteString(e.N)
	if e.Struct == nil {
		return
	}
	s.WriteRune('(')
	refLiteral(e.Struct).buildString(s)
	s.WriteRune(')')
	refLiteral(e.Field.Type).buildString(s)
}

func (w *Switch) buildString(s *stringBuilder) {
	s.WriteString("built-in ")
	s.WriteString(w.N)
	if w.Union == nil {
		return
	}
	s.WriteRune('(')
	buildParmsString(w.T.Parms, s)
	s.WriteRune(')')
	if w.T.Ret != nil && !isEmptyStruct(w.T.Ret) {
		w.T.Ret.buildString(s)
	}
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
	case Cmp:
		return "Cmp"
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

func (b *Builtin) buildString(s *stringBuilder) {
	s.WriteString("built-in ")
	s.WriteString(b.name(false))
	s.WriteRune('(')
	buildParmsString(b.Parms, s)
	s.WriteRune(')')
	if b.Ret != nil && !isEmptyStruct(b.Ret) {
		b.Ret.buildString(s)
	}
}

func (c *Call) buildString(s *stringBuilder) {
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
	case Cmp:
		s.WriteString("<=>")
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

func (c *Convert) buildString(s *stringBuilder) {
	if !c.Explicit {
		c.Expr.buildString(s)
		return
	}
	c.T.buildString(s)
	s.WriteString(" :: ")
	c.Expr.buildString(s)
}

func (v *Var) buildString(s *stringBuilder) {
	s.WriteString(v.Def.Name)
}

func (l *Local) buildString(s *stringBuilder) {
	s.WriteString(l.Def.Name)
}

func (p *Parm) buildString(s *stringBuilder) {
	s.WriteString(p.Def.Name)
}

func (c Cap) buildString(s *stringBuilder) {
	switch {
	case c.Def.Parm != nil:
		s.WriteString(c.Def.Parm.Name)
	case c.Def.Local != nil:
		s.WriteString(c.Def.Local.Name)
	case c.Def.Cap != nil:
		Cap{Def: c.Def.Cap}.buildString(s)
	}
}

func (a *ArrayLit) buildString(s *stringBuilder) {
	s.WriteRune('[')
	for i, e := range a.Elems {
		if i > 0 {
			s.WriteString(", ")
		}
		e.buildString(s)
	}
	s.WriteRune(']')

}

func (t *StructLit) buildString(s *stringBuilder) {
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
}

func (u *UnionLit) buildString(s *stringBuilder) {
	s.WriteRune('[')
	s.WriteString(u.Case.Name)
	if u.Val != nil {
		s.WriteRune(' ')
		u.Val.buildString(s)
	}
	s.WriteRune(']')
}

func (b *BlockLit) buildString(s *stringBuilder) {
	s.WriteRune('(')
	for i, p := range b.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(p.Name)
		if p.T != nil {
			s.WriteString(" ")
			p.T.buildString(s)
		}
	}
	s.WriteString("){…}")
}

func (t *StrLit) buildString(s *stringBuilder) {
	s.WriteString(strconv.Quote(t.Text))
}

func (i *IntLit) buildString(s *stringBuilder) {
	s.WriteString(i.Text)
}

func (f *FloatLit) buildString(s *stringBuilder) {
	s.WriteString(f.Text)
}
