package checker

import (
	"io"
	"strings"
)

type exprPrinter struct {
	w      io.Writer
	nl     bool
	indent string
	tparms []*TypeParm
}

// exprAndTypeString returns the result of printExpr as a string.
func exprAndTypeString(tparms []*TypeParm, expr Expr) string {
	var s strings.Builder
	if err := printExpr(&s, tparms, expr); err != nil {
		panic("impossible strings.Builder error")
	}
	return s.String()
}

type printExprError struct{ error }

// printExpr writes a debug representation of the expression to w,
// which includes the expression and the types of all subexpressoins.
//
// If tparms is non-nil, it is a list of type parameters to which
// type variables in the expression or its subexpression types may bind.
func printExpr(w io.Writer, tparms []*TypeParm, expr Expr) (err error) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		if e, ok := r.(printExprError); ok {
			err = e
		} else {
			panic(r)
		}
	}()
	pr := &exprPrinter{w: w, tparms: tparms}
	expr.printExpr(pr)
	pr.writeString("\n")
	return nil
}

func (x *Call) printExpr(pr *exprPrinter) {
	if xx, ok := x.Func.(*ExprFunc); ok {
		pr.writeString("(")
		xx.printExpr(pr)
		if len(x.Args) == 0 {
			pr.writeString(")()")
			return
		}
		pr.writeString(")(")
		printArgs(pr, x.Args)
		pr.writeStringAndType(")", x.T)
		return
	}
	name := x.Func.String()
	if strings.Contains(name, ":=") {
		// Rewrite "build-int :=(&T, T)" to just ":="
		name = ":="
	}
	if strings.IndexFunc(name, isOpRune) >= 0 {
		name = "(" + name + ")"
	}
	if len(x.Args) == 0 {
		pr.writeStringAndType(name+"()", x.T)
		return
	}
	pr.writeString(name + "(")
	printArgs(pr, x.Args)
	pr.writeStringAndType(")", x.T)
}

func printArgs(pr *exprPrinter, args []Expr) {
	if len(args) <= 3 && allLikelySmall(args) {
		for i, xx := range args {
			if i > 0 {
				pr.writeString(", ")
			}
			xx.printExpr(pr)
		}
	} else {
		pr.nest()
		for _, x := range args {
			x.printExpr(pr)
			pr.writeString(",\n")
		}
		pr.unnest()
	}
}

func isOpRune(r rune) bool {
	return strings.ContainsRune(`*/%+\-^=!<>&|~@$`, r)
}

func (x *Convert) printExpr(pr *exprPrinter) {
	if !x.Explicit && x.Kind == Deref {
		// For implicit derefs of vars, parms, and caps,
		// just print the var, parm, or cap with the given derefed type
		// instead of making a separate column for the dereference.
		switch xx := x.Expr.(type) {
		case *Var:
			copy := *xx
			copy.T = x.T
			copy.printExpr(pr)
			return
		case *Parm:
			copy := *xx
			copy.T = x.T
			copy.printExpr(pr)
			return
		case *Local:
			copy := *xx
			copy.T = x.T
			copy.printExpr(pr)
			return
		case *Cap:
			copy := *xx
			copy.T = x.T
			copy.printExpr(pr)
			return
		}
	}
	if x.Explicit {
		pr.writeString(pr.typeString(x.T) + " :: (")
		x.Expr.printExpr(pr)
		pr.writeStringAndType(")", x.T)
	} else {
		pr.writeString("(")
		x.Expr.printExpr(pr)
		pr.writeStringAndType(")", x.T)
	}
}

func (x *Var) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *Local) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *Parm) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *Cap) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *unresolvedID) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *ArrayLit) printExpr(pr *exprPrinter) {
	switch {
	case len(x.Elems) == 0:
		pr.writeStringAndType("[]", x.T)
	case len(x.Elems) <= 3 && allLikelySmall(x.Elems):
		pr.writeString("[")
		for i, xx := range x.Elems {
			if i > 0 {
				pr.writeString(", ")
			}
			xx.printExpr(pr)
		}
		pr.writeStringAndType("]", x.T)
	default:
		pr.writeString("[")
		pr.nest()
		for _, xx := range x.Elems {
			xx.printExpr(pr)
			pr.writeString(",\n")
		}
		pr.unnest()
		pr.writeStringAndType("]", x.T)
	}
}

func (x *StructLit) printExpr(pr *exprPrinter) {
	switch {
	case len(x.Fields) == 0:
		pr.writeStringAndType("[.]", x.T)
	case len(x.Fields) == 1 && likelySmall(x.Fields[0]) ||
		len(x.Fields) == 2 && likelySmall(x.Fields[0]) && likelySmall(x.Fields[1]):
		pr.writeString("[")
		for i := range x.Fields {
			if i > 0 {
				pr.writeString(", ")
			}
			pr.writeString(x.Struct.Fields[i].Name + " ")
			x.Fields[i].printExpr(pr)
		}
		pr.writeStringAndType("]", x.T)
	default:
		pr.writeString("[")
		pr.nest()
		for i := range x.Fields {
			pr.writeString(x.Struct.Fields[i].Name + " ")
			x.Fields[i].printExpr(pr)
			pr.writeString(",\n")
		}
		pr.unnest()
		pr.writeStringAndType("]", x.T)
	}
}

func (x *UnionLit) printExpr(pr *exprPrinter) {
	if x.Val == nil {
		pr.writeStringAndType("["+x.Case.Name+"]", x.T)
		return
	}
	pr.writeString("[" + x.Case.Name + " ")
	x.Val.printExpr(pr)
	pr.writeStringAndType("]", x.T)
}

func (x *BlockLit) printExpr(pr *exprPrinter) {
	var s strings.Builder
	s.WriteString("(")
	for i, parm := range x.Parms {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(parm.Name)
		if parm.T != nil {
			s.WriteString(" " + pr.typeString(parm.T))
		}
	}
	s.WriteString("){")
	switch {
	case len(x.Exprs) == 0:
		pr.writeStringAndType(s.String()+"}", x.T)
	case len(x.Exprs) < 2 && allLikelySmall(x.Exprs):
		pr.writeString(s.String())
		for i, xx := range x.Exprs {
			if i > 0 {
				pr.writeString(", ")
			}
			xx.printExpr(pr)
		}
		pr.writeStringAndType("}", x.T)
	default:
		pr.writeString(s.String())
		pr.nest()
		for _, xx := range x.Exprs {
			xx.printExpr(pr)
			pr.writeString(",\n")
		}
		pr.unnest()
		pr.writeStringAndType("}", x.T)
	}
}

func (x *StrLit) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *IntLit) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (x *FloatLit) printExpr(pr *exprPrinter) {
	pr.writeExprAndType(x)
}

func (pr *exprPrinter) writeExprAndType(x Expr) {
	pr.writeStringAndType(x.String(), x.Type())
}

func (pr *exprPrinter) writeStringAndType(s string, t Type) {
	pr.writeString(s + " ‹" + pr.typeString(t) + "›")
}

func (pr *exprPrinter) typeString(t Type) string {
	var w stringBuilder
	w.useSubScripts = true
	makeTypePattern(NewTypeParmSet(pr.tparms...), t).buildString(&w)
	return w.builder.String()
}

func (pr *exprPrinter) nest() {
	pr.indent += "\t"
	pr.writeString("\n")
}

func (pr *exprPrinter) unnest() {
	pr.indent = strings.TrimSuffix(pr.indent, "\t")
}

func (pr *exprPrinter) writeString(s string) {
	if pr.nl {
		s = pr.indent + s
	}
	if _, err := io.WriteString(pr.w, s); err != nil {
		panic(printExprError{err})
	}
	pr.nl = strings.HasSuffix(s, "\n")
}

func allLikelySmall(xs []Expr) bool {
	for _, x := range xs {
		if !likelySmall(x) {
			return false
		}
	}
	return true
}

func likelySmall(x Expr) bool {
	switch x := x.(type) {
	case *Var, *Local, *Parm, *Cap, *unresolvedID, *StrLit, *IntLit, *FloatLit:
		return true
	case *Convert:
		return !x.Explicit && likelySmall(x.Expr)
	default:
		return false
	}
}
