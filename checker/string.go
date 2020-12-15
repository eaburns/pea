package checker

import (
	"strings"
)

func (r *RefType) String() string {
	return r.buildString(new(strings.Builder)).String()
}

func (n *NamedType) String() string {
	return n.buildString(new(strings.Builder)).String()
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

func (r *RefType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('&')
	r.Type.buildString(w)
	return w
}

func (n *NamedType) buildString(w *strings.Builder) *strings.Builder {
	if len(n.Args) > 1 {
		w.WriteRune('(')
	}
	for i, a := range n.Args {
		if i > 0 {
			w.WriteString(", ")
		}
		a.buildString(w)
	}
	switch {
	case len(n.Args) > 1:
		w.WriteString(") ")
	case len(n.Args) == 1:
		w.WriteRune(' ')
	}
	if n.Def != nil && n.Def.File.Mod.Imported {
		w.WriteString(n.Def.File.Mod.Name())
		w.WriteRune('#')
	}
	w.WriteString(n.Name)
	return w
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
		w.WriteString(": ")
		f.Type.buildString(w)
	}
	w.WriteRune(']')
	return w
}

func (u *UnionType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('[')
	if len(u.Cases) == 1 && u.Cases[0].Type == nil {
		w.WriteString("|")
	}
	for i, c := range u.Cases {
		if i > 0 {
			w.WriteString(" | ")
		}
		w.WriteString(c.Name)
		if c.Type != nil {
			w.WriteString(": ")
			c.Type.buildString(w)
		}
	}
	w.WriteRune(']')
	return w
}

func (f *FuncType) buildString(w *strings.Builder) *strings.Builder {
	w.WriteRune('(')
	for i, p := range f.Parms {
		if i > 0 {
			w.WriteString(", ")
		}
		p.buildString(w)
	}
	w.WriteString("){")
	if f.Ret != nil {
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
