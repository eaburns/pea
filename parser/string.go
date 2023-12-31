package parser

import (
	"strings"
)

func (x *RefType) String() string    { return x.buildString(new(strings.Builder)).String() }
func (x *NamedType) String() string  { return x.buildString(new(strings.Builder)).String() }
func (x *ArrayType) String() string  { return x.buildString(new(strings.Builder)).String() }
func (x *StructType) String() string { return x.buildString(new(strings.Builder)).String() }
func (x *UnionType) String() string  { return x.buildString(new(strings.Builder)).String() }
func (x *FuncType) String() string   { return x.buildString(new(strings.Builder)).String() }
func (x *Call) String() string       { return x.buildString(new(strings.Builder)).String() }
func (x *Convert) String() string    { return x.buildString(new(strings.Builder)).String() }
func (x *SubExpr) String() string    { return x.buildString(new(strings.Builder)).String() }
func (x *ModSel) String() string     { return x.buildString(new(strings.Builder)).String() }
func (x *ArrayLit) String() string   { return x.buildString(new(strings.Builder)).String() }
func (x *StructLit) String() string  { return x.buildString(new(strings.Builder)).String() }
func (x *UnionLit) String() string   { return x.buildString(new(strings.Builder)).String() }
func (x *BlockLit) String() string   { return x.buildString(new(strings.Builder)).String() }
func (x *CharLit) String() string    { return x.buildString(new(strings.Builder)).String() }
func (x *StrLit) String() string     { return x.buildString(new(strings.Builder)).String() }
func (x *IntLit) String() string     { return x.buildString(new(strings.Builder)).String() }
func (x *FloatLit) String() string   { return x.buildString(new(strings.Builder)).String() }
func (x TypeVar) String() string     { return x.buildString(new(strings.Builder)).String() }
func (x Ident) String() string       { return x.buildString(new(strings.Builder)).String() }

func (x *RefType) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("&")
	x.Type.buildString(s)
	return s
}

func (x *NamedType) buildString(s *strings.Builder) *strings.Builder {
	switch len(x.Args) {
	case 0:
		break
	case 1:
		if _, ok := x.Args[0].(*RefType); ok {
			s.WriteString("(")
			x.Args[0].buildString(s)
			s.WriteString(") ")
		} else {
			x.Args[0].buildString(s)
			s.WriteString(" ")
		}
	default:
		s.WriteString("(")
		for i, a := range x.Args {
			if i > 0 {
				s.WriteString(", ")
			}
			a.buildString(s)
		}
		s.WriteString(") ")
	}
	if x.Mod != nil {
		s.WriteString(x.Mod.Name)
		s.WriteString("#")
	}
	s.WriteString(x.Name.Name)
	return s
}

func (x *ArrayType) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("[")
	x.ElemType.buildString(s)
	s.WriteString("]")
	return s
}

func (x *StructType) buildString(s *strings.Builder) *strings.Builder {
	if len(x.Fields) == 0 {
		s.WriteString("[.]")
		return s
	}
	s.WriteString("[")
	for i, f := range x.Fields {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(f.Name.Name)
		s.WriteString(" ")
		f.Type.buildString(s)
	}
	s.WriteString("]")
	return s
}

func (x *UnionType) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("[")
	for i, c := range x.Cases {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(c.Name.Name)
		if c.Type != nil {
			s.WriteString(" ")
			c.Type.buildString(s)
		}
	}
	s.WriteString("]")
	return s
}

func (x *FuncType) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("(")
	for i, p := range x.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		p.buildString(s)
	}
	s.WriteString("){")
	if x.Ret != nil {
		x.Ret.buildString(s)
	}
	s.WriteString("}")
	return s
}

func (x *Call) buildString(s *strings.Builder) *strings.Builder {
	var id, mod *Ident
	switch f := x.Fun.(type) {
	case Ident:
		id = &f
	case *ModSel:
		mod = &f.Mod
		id = &f.Name
	}
	switch {
	case id == nil:
		fallthrough
	default:
		x.Fun.buildString(s)
		s.WriteString("(")
		for i, a := range x.Args {
			if i > 0 {
				s.WriteString(", ")
			}
			a.buildString(s)
		}
		s.WriteString(")")

	case id.Name == "[]" && len(x.Args) > 1 && mod == nil:
		x.Args[0].buildString(s)
		s.WriteString("[")
		for i, a := range x.Args[1:] {
			if i > 0 {
				s.WriteString(", ")
			}
			a.buildString(s)
		}
		s.WriteString("]")

	case strings.HasPrefix(id.Name, ".") && len(x.Args) == 1 && mod == nil:
		x.Args[0].buildString(s)
		s.WriteString(id.Name)

	case strings.ContainsRune(id.Name, '?') || strings.ContainsRune(id.Name, ':') && id.Name != ":=":
		args := x.Args
		if len(args) > len(id.Parts) {
			args[0].buildString(s)
			s.WriteString(" ")
			args = args[1:]
		}
		if len(args) != len(id.Parts) {
			panic("impossible")
		}
		for i, a := range args {
			if i == 0 && mod != nil {
				s.WriteString(mod.Name)
				s.WriteString("#")
			}
			if i > 0 {
				s.WriteString(" ")
			}
			s.WriteString(id.Parts[i].Name)
			s.WriteString(" ")
			a.buildString(s)
		}

	case strings.IndexFunc(id.Name, hasOpRune) >= 0:
		switch len(x.Args) {
		case 1:
			if mod != nil {
				s.WriteString(mod.Name)
				s.WriteString("#")
			}
			s.WriteString(id.Name)
			s.WriteString(" ")
			x.Args[0].buildString(s)
		case 2:
			x.Args[0].buildString(s)
			s.WriteString(" ")
			if mod != nil {
				s.WriteString(mod.Name)
				s.WriteString("#")
			}
			s.WriteString(id.Name)
			s.WriteString(" ")
			x.Args[1].buildString(s)
		default:
			panic("impossible")
		}
	}

	return s
}

func hasOpRune(r rune) bool {
	return strings.ContainsRune(`*/%+\-^=!<>&|~@$`, r)
}

func (x *Convert) buildString(s *strings.Builder) *strings.Builder {
	x.Type.buildString(s)
	s.WriteString(" :: ")
	x.Expr.buildString(s)
	return s
}

func (x *SubExpr) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("(")
	x.Expr.buildString(s)
	s.WriteString(")")
	return s
}

func (x *ModSel) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Mod.Name)
	s.WriteString("#")
	s.WriteString(x.Name.Name)
	return s
}

func (x *ArrayLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("[")
	for i, e := range x.Exprs {
		if i > 0 {
			s.WriteString(", ")
		}
		e.buildString(s)
	}
	s.WriteString("]")
	return s
}

func (x *StructLit) buildString(s *strings.Builder) *strings.Builder {
	if len(x.FieldVals) == 0 {
		s.WriteString("[.]")
		return s
	}
	s.WriteString("[")
	for i, f := range x.FieldVals {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(f.Name.Name)
		s.WriteString(" ")
		f.Val.buildString(s)
	}
	s.WriteString("]")
	return s
}

func (x *UnionLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("[")
	s.WriteString(x.CaseVal.Name.Name)
	if x.CaseVal.Val != nil {
		s.WriteString(" ")
		x.CaseVal.Val.buildString(s)
	}
	s.WriteString("]")
	return s
}

func (x *BlockLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString("(")
	for i, p := range x.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(p.Name.Name)
		if p.Type != nil {
			s.WriteString(" ")
			p.Type.buildString(s)
		}
	}
	s.WriteString("){")
	if len(x.Exprs) > 1 {
		s.WriteString("…, ")
	}
	if n := len(x.Exprs); n > 0 {
		x.Exprs[n-1].buildString(s)
	}
	s.WriteString("}")
	return s
}

func (x *CharLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Source)
	return s
}

func (x *StrLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Source)
	return s
}

func (x *IntLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Text)
	return s
}

func (x *FloatLit) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Text)
	return s
}

func (x TypeVar) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Name)
	return s
}

func (x Ident) buildString(s *strings.Builder) *strings.Builder {
	s.WriteString(x.Name)
	return s
}
