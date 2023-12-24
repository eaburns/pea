package printer

import (
	"fmt"
	"io"
	"strings"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type ioError struct{ err error }

func Print(w io.Writer, file *parser.File, locs loc.Files) (err error) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		if ioErr, ok := r.(ioError); ok {
			err = ioErr.err
			return
		}
		panic(r)
	}()
	p := newPrinter(w, file, locs)
	if len(file.Imports) > 0 {
		for _, imp := range file.Imports {
			printImport(p, imp)
			printLineBreak(p)
		}
		printLineBreak(p)
	}
	p = p.withBlankLinesAllowed()
	for _, def := range file.Defs {
		switch def := def.(type) {
		case *parser.VarDef:
			printVarDef(p, def)
		case *parser.TypeDef:
			printTypeDef(p, def)
		case *parser.IfaceDef:
			printIfaceDef(p, def)
		case *parser.FuncDef:
			printFuncDef(p, def)
		case *parser.TestDef:
			printTestDef(p, def)
		default:
			panic(fmt.Sprintf("unknown def type: %T", def))
		}
	}
	printTrailingComments(p)
	p.dst.writeNewLine()
	return nil
}

func printStartDef(p *printer, name string, exp bool, l loc.Loc) loc.Loc {
	if exp {
		// All names are ASCII, so byte-indexing is OK.
		name = strings.ToUpper(name[:1]) + name[1:]
	}
	l[1] = l[0] + len(name)
	printToken(p, name, l)
	return l
}

func printImport(p *printer, imp *parser.Import) {
	printStartDef(p, "import", imp.Exp, imp.L)
	pathPrevLoc := imp.L
	pathPrevLoc[1] = pathPrevLoc[0] + len("import")
	if imp.Name != nil {
		printSpace(p)
		printIdent(p, *imp.Name)
		pathPrevLoc = imp.Name.L
	}
	printSpace(p)
	printLostTokenAfter(p, `"`+imp.Path+`"`, pathPrevLoc)
}

func printVarDef(p *printer, def *parser.VarDef) {
	if def.Const {
		printStartDef(p, "const", def.Exp, def.L)
	} else {
		printStartDef(p, "var", def.Exp, def.L)
	}
	printSpace(p)
	printIdent(p, def.Name)
	printSpace(p)
	printLostTokenAfter(p, ":=", def.Name.L)
	printSpace(p)
	printType(p, def.Type)
	printSpace(p)
	printLostTokenAfter(p, "::", def.Type.Loc())
	printSpace(p)
	printExpr(p, def.Expr)
	printLineBreak(p)
}

func printTypeDef(p *printer, def *parser.TypeDef) {
	prevLoc := printStartDef(p, "type", def.Exp, def.L)
	if len(def.TypeParms) > 0 {
		printSpace(p)
		printTypeParmList(p, def.TypeParms, prevLoc)
	}
	printSpace(p)
	printIdent(p, def.Name)
	prevLoc = def.Name.Loc()
	if def.Alias {
		printSpace(p)
		prevLoc = printLostTokenAfter(p, ":=", prevLoc)
	}
	printSpace(p)
	if def.Opaque {
		printLostTokenAfter(p, "(", prevLoc)
	}
	printType(p, def.Type)
	if def.Opaque {
		printLostTokenBefore(p, ")", end(def.L))
	}
	printLineBreak(p)
}

func printTypeParmList(p *printer, parms []parser.TypeVar, prevLoc loc.Loc) {
	if len(parms) == 1 {
		printType(p, parms[0])
		return
	}
	prevLoc = printLostTokenAfter(p, "(", prevLoc)
	p = p.withIndent()
	p = p.withLineBreaksDisallowed()
	for i, parm := range parms {
		if i > 0 {
			printLostTokenAfter(p, ",", prevLoc)
			printSpace(p)
		}
		printType(p, parm)
		prevLoc = parm.Loc()
	}
	printLostTokenAfter(p, ")", prevLoc)
}

func printIfaceDef(p *printer, def *parser.IfaceDef) {
	prevLoc := printStartDef(p, "iface", def.Exp, def.L)
	if len(def.TypeParms) > 0 {
		printSpace(p)
		printTypeParmList(p, def.TypeParms, prevLoc)
	}
	printSpace(p)
	printIdent(p, def.Name)
	prevLoc = def.Name.Loc()
	if def.Opaque {
		printSpace(p)
		printLostTokenAfter(p, "(", prevLoc)
	}
	printSpace(p)
	if def.Alias != nil {
		printLostTokenAfter(p, ":=", prevLoc)
	} else {
		printConstraints(p, def.Iface, prevLoc)
	}
	if def.Opaque {
		printLostTokenBefore(p, ")", end(def.L))
	}
	printLineBreak(p)
}

func printFuncDef(p *printer, def *parser.FuncDef) {
	printStartDef(p, "func", def.Exp, def.L)
	printSpace(p)
	printIdent(p, def.Name)
	prevLoc := printFuncParms(p, def.Parms, def.Name.L)
	if def.Ret != nil {
		printSpace(p)
		printType(p, def.Ret)
		prevLoc = def.Ret.Loc()
	}
	if def.Constraints != nil {
		printSpace(p)
		prevLoc = printLostTokenAfter(p, ":", prevLoc)
		printSpace(p)
		prevLoc = printConstraints(p, def.Constraints, prevLoc)
	}
	if def.Exprs != nil {
		printSpace(p)
		printFuncBody(p, def.Exprs, prevLoc, end(def.L))
	}
	printLineBreak(p)
}

func printFuncParms(p *printer, parms []parser.FuncParm, prevLoc loc.Loc) loc.Loc {
	prevLoc = printLostTokenAfter(p, "(", prevLoc)

	var multiLine bool
	for _, fp := range parms {
		if p.src.startLine(fp.L) != p.src.endLine(fp.L) {
			multiLine = true
			break
		}
	}
	if multiLine {
		printLineBreak(p)
	}
	{
		p := p.withIndent()
		for i, parm := range parms {
			if !multiLine && i > 0 {
				prevLoc = printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			// TODO(eaburns): don't distribute parm types in the parser.
			if i < len(parms)-1 && parm.Type != nil &&
				parm.Type.Loc()[0] > parms[i+1].Name.L[0] {
				parm.Type = nil
			}
			prevLoc = printFuncParm(p, &parm)
			if multiLine {
				printLostTokenAfter(p, ",", prevLoc)
				printLineBreak(p)
			}
		}
	}
	return printLostTokenAfter(p, ")", prevLoc)
}

func printFuncParm(p *printer, parm *parser.FuncParm) loc.Loc {
	printIdent(p, parm.Name)
	prevLoc := parm.Name.L
	if parm.Type == nil {
		return prevLoc
	}
	printSpace(p)
	printType(p, parm.Type)
	prevLoc = parm.Type.Loc()
	if len(parm.Constraints) == 0 {
		return prevLoc
	}
	printSpace(p)
	prevLoc = printLostTokenAfter(p, ":", prevLoc)
	printSpace(p)
	return printConstraints(p, parm.Constraints, prevLoc)
}

func isNamedType(x interface{}) bool {
	_, ok := x.(*parser.NamedType)
	return ok
}

func printConstraints(p *printer, cs []interface{}, prevLoc loc.Loc) loc.Loc {
	if len(cs) == 1 && isNamedType(cs[0]) {
		return printConstraint(p, cs[0])
	}
	prevLoc = printLostTokenAfter(p, "{", prevLoc)
	printSpace(p)
	multiLine := len(cs) > 0 &&
		p.src.startLine(prevLoc) < p.src.endLine(constraintLoc(cs[len(cs)-1]))
	{
		p := p.withIndent()
		p = p.withBlankLinesAllowed()
		if multiLine {
			printLineBreak(p)
		}
		for i, c := range cs {
			if !multiLine && i > 0 {
				printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			prevLoc = printConstraint(p, c)
			if multiLine {
				printLostTokenAfter(p, ",", prevLoc)
				printLineBreak(p)
			}
		}
	}
	printSpace(p)
	return printLostTokenAfter(p, "}", prevLoc)
}

func constraintLoc(c interface{}) loc.Loc {
	switch c := c.(type) {
	case *parser.FuncDecl:
		return c.L
	case *parser.NamedType:
		return c.Loc()
	default:
		panic(fmt.Sprintf("unknown constraint type: %T", c))
	}
}

func printConstraint(p *printer, c interface{}) loc.Loc {
	switch c := c.(type) {
	case *parser.FuncDecl:
		printIdent(p, c.Name)
		printTypeList(p, c.Parms, c.Name.Loc())
		if c.Ret != nil {
			printType(p, c.Ret)
		}
		return c.L
	case *parser.NamedType:
		printType(p, c)
		return c.Loc()
	default:
		panic(fmt.Sprintf("unknown constraint type: %T", c))
	}
}

func printFuncBody(p *printer, exprs []parser.Expr, prevLoc, endLoc loc.Loc) {
	prevLoc = printLostTokenAfter(p, "{", prevLoc)
	if len(exprs) == 0 {
		// This is silly, but withLineBreaksAllowed adds a continuation tab.
		// Then withLineBreaksDisallowed sets maxNewLines to 0.
		p = p.withLineBreaksAllowed()
		p = p.withLineBreaksDisallowed()
		printLostTokenBefore(p.withForcedUnnest(), "}", endLoc)
		p.resetContinuationLine()
		return
	}
	multiLine := len(exprs) > 0 &&
		p.src.startLine(prevLoc) < p.src.endLine(exprs[len(exprs)-1].Loc())
	p = p.withLineBreaksAllowed()
	p = p.withBlankLinesAllowed()
	if multiLine {
		printLineBreak(p)
	} else {
		printSpace(p)
	}
	for i, expr := range exprs {
		if !multiLine && i > 0 {
			printLostTokenAfter(p.withLineBreaksDisallowed(), ",", prevLoc)
			printSpace(p)
		}
		printExpr(p, expr)
		if multiLine {
			if i < len(exprs)-1 {
				// The , must be on the same line as expr.
				p := p.withLineBreaksDisallowed()
				printLostTokenAfter(p, ",", expr.Loc())
			}
			printLineBreak(p)
			printSpace(p)
		}
		prevLoc = expr.Loc()
	}
	if !multiLine {
		printSpace(p)
	}
	printLostTokenBefore(p.withForcedUnnest(), "}", endLoc)
	p.resetContinuationLine()
}

func printTestDef(p *printer, def *parser.TestDef) {
	printStartDef(p, "test", false, def.L)
	printSpace(p)
	printIdent(p, def.Name)
	printSpace(p)
	printFuncBody(p, def.Exprs, def.Name.Loc(), end(def.L))
	printLineBreak(p)
}

func printType(p *printer, typ parser.Type) {
	switch typ := typ.(type) {
	case *parser.RefType:
		printLostTokenAfter(p, "&", start(typ))
		printType(p.withLineBreaksDisallowed(), typ.Type)
	case *parser.NamedType:
		printNamedType(p, typ)
	case *parser.ArrayType:
		printArrayType(p, typ)
	case *parser.StructType:
		printStructType(p, typ)
	case *parser.UnionType:
		printUnionType(p, typ)
	case *parser.FuncType:
		printFuncType(p, typ)
	case parser.TypeVar:
		printToken(p, typ.Name, typ.L)
	default:
		panic(fmt.Sprintf("unknown type type: %T", typ))
	}
}

func printNamedType(p *printer, typ *parser.NamedType) {
	if len(typ.Args) > 0 {
		if len(typ.Args) == 1 && !isRef(typ.Args[0]) {
			printType(p, typ.Args[0])
		} else {
			printTypeList(p, typ.Args, start(typ))
		}
		p = p.withLineBreaksDisallowed()
		printSpace(p)
	}
	if typ.Mod != nil {
		printIdent(p, *typ.Mod)
		printLostTokenAfter(p, "#", typ.Mod.Loc())
		p = p.withLineBreaksDisallowed()
	}
	printIdent(p, typ.Name)
}

func isRef(typ parser.Type) bool {
	_, ok := typ.(*parser.RefType)
	return ok
}

func printTypeList(p *printer, args []parser.Type, prevLoc loc.Loc) {
	var multiLine bool
	for _, typ := range args {
		if p.src.startLine(typ) != p.src.endLine(typ) {
			multiLine = true
			break
		}
	}
	prevLoc = printLostTokenAfter(p, "(", prevLoc)
	if multiLine {
		printLineBreak(p)
	}
	{
		p := p.withIndent()
		for i, arg := range args {
			if !multiLine && i > 0 {
				printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			printType(p, arg)
			prevLoc = arg.Loc()
			if multiLine {
				printLostTokenAfter(p, ",", prevLoc)
				printLineBreak(p)
			}
		}
	}
	printLostTokenAfter(p, ")", prevLoc)
}

func printArrayType(p *printer, typ *parser.ArrayType) {
	printLostTokenAfter(p, "[", start(typ))
	p = p.withLineBreaksDisallowed()
	printType(p, typ.ElemType)
	printLostTokenBefore(p, "]", end(typ))
}

func printStructType(p *printer, typ *parser.StructType) {
	prevLoc := printLostTokenAfter(p, "[", start(typ))
	if len(typ.Fields) == 0 {
		// For an empty struct type, [.] are all on the same line.
		p = p.withLineBreaksDisallowed()
		prevLoc = printLostTokenAfter(p, ".", prevLoc)
		printLostTokenBefore(p, "]", end(typ))
		return
	}
	p = p.withBlankLinesAllowed()
	multiLine := p.src.startLine(typ) < p.src.endLine(typ)
	if multiLine {
		printLineBreak(p)
	}
	{
		p := p.withIndent()
		for i, field := range typ.Fields {
			if !multiLine && i > 0 {
				prevLoc = printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			printIdent(p, field.Name)
			printSpace(p)
			printType(p, field.Type)
			prevLoc = field.Type.Loc()
			if multiLine {
				prevLoc = printLostTokenAfter(p, ",", prevLoc)
				printLineBreak(p)
				printSpace(p)
			}
		}
	}
	p = p.withLineBreaksDisallowed()
	printLostTokenBefore(p, "]", end(typ))
}

func printUnionType(p *printer, typ *parser.UnionType) {
	prevLoc := printLostTokenAfter(p, "[", start(typ))
	p = p.withBlankLinesAllowed()
	multiLine := p.src.startLine(typ) < p.src.endLine(typ)
	if multiLine {
		printLineBreak(p)
	}
	{
		p := p.withIndent()
		for i, cas := range typ.Cases {
			if !multiLine && i > 0 {
				prevLoc = printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			printIdent(p, cas.Name)
			prevLoc = cas.Name.Loc()
			if cas.Type != nil {
				printSpace(p)
				printType(p, cas.Type)
				prevLoc = cas.Type.Loc()
			}
			if multiLine {
				prevLoc = printLostTokenAfter(p, ",", prevLoc)
				printLineBreak(p)
				printSpace(p)
			}
		}
	}
	p = p.withLineBreaksDisallowed()
	printLostTokenBefore(p, "]", end(typ))
}

func printFuncType(p *printer, typ *parser.FuncType) {
	prevLoc := start(typ)
	prevLoc = printLostTokenAfter(p, "(", prevLoc)
	var multiLine bool
	for _, parm := range typ.Parms {
		if p.src.startLine(parm) != p.src.endLine(parm) {
			multiLine = true
			break
		}
	}
	if multiLine {
		printLineBreak(p)
	}
	{
		p := p.withIndent()
		for i, parm := range typ.Parms {
			if !multiLine && i > 0 {
				printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			printType(p, parm)
			prevLoc = parm.Loc()
			if multiLine {
				printLostTokenAfter(p, ",", prevLoc)
				printLineBreak(p)
			}
		}
	}
	prevLoc = printLostTokenAfter(p, ")", prevLoc)
	prevLoc = printLostTokenAfter(p, "{", prevLoc)
	if typ.Ret != nil {
		printType(p, typ.Ret)
		prevLoc = typ.Ret.Loc()
	}
	printLostTokenAfter(p, "}", prevLoc)
}

func printExpr(p *printer, expr parser.Expr) {
	switch expr := expr.(type) {
	case *parser.Call:
		printCall(p, expr)
	case *parser.Convert:
		printConvert(p, expr)
	case *parser.SubExpr:
		printSubExpr(p, expr)
	case *parser.ModSel:
		printModSel(p, expr)
	case *parser.ArrayLit:
		printArrayLit(p, expr)
	case *parser.StructLit:
		printStructLit(p, expr)
	case *parser.UnionLit:
		printUnionLit(p, expr)
	case *parser.BlockLit:
		printBlockLit(p, expr)
	case *parser.StrLit:
		printToken(p, expr.Source, expr.L)
	case *parser.CharLit:
		printToken(p, expr.Source, expr.L)
	case *parser.IntLit:
		printToken(p, expr.Text, expr.L)
	case *parser.FloatLit:
		printToken(p, expr.Text, expr.L)
	case parser.Ident:
		printIdent(p, expr)
	default:
		panic(fmt.Sprintf("unknown expr type: %T", expr))
	}
}

func hasOpRune(r rune) bool {
	return strings.ContainsRune(`*/%+\-^=!<>&|~@$`, r)
}

func printCall(p *printer, e *parser.Call) {
	var id, mod *parser.Ident
	switch f := e.Fun.(type) {
	case parser.Ident:
		id = &f
	case *parser.ModSel:
		mod = &f.Mod
		id = &f.Name
	}
	switch {
	case id == nil:
		break

	case id.Name == "[]":
		printIndexCall(p, mod, *id, e)
		return

	case strings.HasPrefix(id.Name, "."):
		printSelectorCall(p, mod, *id, e)
		return

	case strings.ContainsRune(id.Name, '?'):
		printKQCall(p, mod, *id, e, "?")
		return

	case strings.ContainsRune(id.Name, ':') && id.Name != ":=":
		printKQCall(p, mod, *id, e, ":")
		return

	case strings.IndexFunc(id.Name, hasOpRune) >= 0:
		printOpCall(p, mod, *id, e)
		return
	}
	printExpr(p, e.Fun)
	prevLoc := printLostTokenAfter(p, "(", e.Fun.Loc())
	{
		p := p.withLineBreaksAllowed()
		for i, arg := range e.Args {
			if i > 0 {
				prevLoc = printLostTokenAfter(p, ",", prevLoc)
				printSpace(p)
			}
			printExpr(p, arg)
			prevLoc = arg.Loc()
		}
	}
	printLostTokenBefore(p, ")", end(e))
}

func isUnOp(e parser.Expr) bool {
	if call, ok := e.(*parser.Call); ok {
		ident, ok := call.Fun.(parser.Ident)
		return ok &&
			strings.IndexFunc(ident.Name, hasOpRune) >= 0 &&
			len(call.Args) == 1
	}
	if lit, ok := e.(*parser.IntLit); ok {
		return strings.HasPrefix(lit.Text, "-") || strings.HasPrefix(lit.Text, "+")
	}
	if lit, ok := e.(*parser.FloatLit); ok {
		return strings.HasPrefix(lit.Text, "-") || strings.HasPrefix(lit.Text, "+")
	}
	return false
}

func printOpCall(p *printer, mod *parser.Ident, id parser.Ident, e *parser.Call) {
	switch len(e.Args) {
	case 1:
		p = p.withLineBreaksDisallowed()
		printModAndIdent(p, mod, id)
		if isUnOp(e.Args[0]) {
			// Two unary operators in a row need to be separated by whitespace.
			printSpace(p)
		}
		printExpr(p, e.Args[0])
	case 2:
		printExpr(p, e.Args[0])
		if !strings.ContainsAny(id.Name, "*/%") || strings.HasSuffix(id.Name, "=") {
			printSpace(p)
		}
		p = p.withLineBreaksAllowed()
		printModAndIdent(p, mod, id)
		if isUnOp(e.Args[1]) || !strings.ContainsAny(id.Name, "*/%") || strings.HasSuffix(id.Name, "=") {
			printSpace(p)
		}
		printExpr(p, e.Args[1])
		p.resetContinuationLine()
	default:
		panic(fmt.Sprintf("invalid op %s args: %d", id, len(e.Args)))
	}
}

func printIndexCall(p *printer, mod *parser.Ident, id parser.Ident, e *parser.Call) {
	if len(e.Args) < 2 {
		panic("impossible")
	}
	printExpr(p, e.Args[0])
	prevLoc := e.Args[0].Loc()
	if mod != nil {
		p = p.withLineBreaksDisallowed()
		printIdent(p, *mod)
		printLostTokenAfter(p, "#", mod.Loc())
	}
	prevLoc = printLostTokenAfter(p.withLineBreaksDisallowed(), "[", prevLoc)
	p = p.withLineBreaksAllowed()
	for i, arg := range e.Args[1:] {
		if i > 0 {
			printLostTokenAfter(p, ",", prevLoc)
			printSpace(p)
		}
		printExpr(p, arg)
		prevLoc = arg.Loc()
	}
	printLostTokenBefore(p.withForcedUnnest(), "]", end(e))
	p.resetContinuationLine()
}

func printSelectorCall(p *printer, mod *parser.Ident, id parser.Ident, e *parser.Call) {
	if len(e.Args) != 1 {
		panic("impossible")
	}
	printExpr(p, e.Args[0])
	printModAndIdent(p.withLineBreaksAllowed(), mod, id)
}

func printKQCall(p *printer, mod *parser.Ident, id parser.Ident, e *parser.Call, sep string) {
	if len(id.Parts) < 1 {
		panic("impossible")
	}

	args := e.Args
	// There is a leading argument; print it.
	if len(id.Parts) < len(args) {
		printExpr(p, args[0])
		printSpace(p)
		args = args[1:]
		p = p.withLineBreaksAllowed()
	}
	if len(args) != len(id.Parts) {
		panic("impossible")
	}
	for i := range args {
		if i == 0 {
			printModAndIdent(p, mod, id.Parts[i])
		} else {
			printSpace(p)
			printIdent(p, id.Parts[i])
		}
		printSpace(p)
		printExpr(p.withLineBreaksAllowed(), args[i])
	}
}

func printConvert(p *printer, expr *parser.Convert) {
	printType(p, expr.Type)
	printSpace(p)
	printLostTokenAfter(p, "::", expr.Type.Loc())
	printSpace(p)
	printExpr(p, expr.Expr)
}

func printSubExpr(p *printer, expr *parser.SubExpr) {
	printLostTokenAfter(p, "(", start(expr.Loc()))
	// This is silly, but withLineBreaksAllowed adds a continuation tab.
	// Then withLineBreaksDisallowed sets maxNewLines to 0.
	p = p.withLineBreaksAllowed()
	p = p.withLineBreaksDisallowed()
	printExpr(p, expr.Expr)
	printLostTokenBefore(p, ")", end(expr.Loc()))
}

func printModSel(p *printer, expr *parser.ModSel) {
	printModAndIdent(p, &expr.Mod, expr.Name)
}

func printModAndIdent(p *printer, mod *parser.Ident, id parser.Ident) {
	if mod != nil {
		printIdent(p, *mod)
		// The mod can start on a new line,
		// but the folling # and id should be
		// on the same lines as the mod.
		p = p.withLineBreaksDisallowed()
		printLostTokenAfter(p, "#", mod.Loc())
	}
	printIdent(p, id)
}

func printArrayLit(p *printer, array *parser.ArrayLit) {
	if len(array.Exprs) == 0 {
		// For an empty array lit, print [] on a single line.
		p = p.withLineBreaksDisallowed()
		printLostTokenAfter(p, "[", start(array))
		printLostTokenBefore(p, "]", end(array))
		return
	}
	prevLoc := printLostTokenAfter(p, "[", start(array))
	// We don't want to force a level of nesting.
	// If the outer scope introduced nesting but didn't use it,
	// we will re-use it instead of introducing lots of extra indentation:
	// 	[[[[[[
	// 		1,
	// 		2,
	// 	]]]]]]
	//
	// Note, though that because this, the indentation we are using
	// is inherited from the outer printer, not this WithLineBreaksAllowed() call,
	// which means that we need to force a level of unnesting
	// before printing the closing ] if this WithLineBreaksAllowed() didn't add a level.
	p = p.withLineBreaksAllowed()
	for i, expr := range array.Exprs {
		if i > 0 {
			printLostTokenAfter(p, ",", prevLoc)
			printSpace(p)
		}
		printExpr(p, expr)
		prevLoc = expr.Loc()
	}
	// If p had any added continuation indentation,
	// whether from the above call to WithLineBreaksAllowed
	// or from an outer scope call to it,
	// remove that before printing the closing ],
	// so it's not on the same indentation as the elements.
	//
	// When doing this, we need to reset the continuationLine
	// of the outer scope's printer, so any following item
	// starting on the same line as the closing ]
	// will itself be able to re-use the parent's continuation:
	// 	[[
	// 		1,
	// 		2,
	// 		3,
	// 	], [
	// 		4,
	// 		5,
	// 		6,
	// 	]],
	if p.src.endLine(prevLoc) < p.src.startLine(end(array)) {
		printLostTokenAfter(p, ",", prevLoc)
		printLineBreak(p)
	}
	printLostTokenBefore(p.withForcedUnnest(), "]", end(array))
	p.resetContinuationLine()
}

func printStructLit(p *printer, strct *parser.StructLit) {
	prevLoc := printLostTokenAfter(p, "[", start(strct))
	if len(strct.FieldVals) == 0 {
		// For an empty struct lit, [.] are all on the same line.
		p = p.withLineBreaksDisallowed()
		prevLoc = printLostTokenAfter(p, ".", prevLoc)
		printLostTokenBefore(p, "]", end(strct))
		return
	}
	p = p.withLineBreaksAllowed()
	for i, fieldVal := range strct.FieldVals {
		if i > 0 {
			printLostTokenAfter(p, ",", prevLoc)
			printSpace(p)
		}
		printIdent(p, fieldVal.Name)
		printSpace(p)
		printExpr(p.withLineBreaksAllowed(), fieldVal.Val)
		prevLoc = fieldVal.Val.Loc()
	}
	if p.src.endLine(prevLoc) < p.src.startLine(end(strct)) {
		printLostTokenAfter(p, ",", prevLoc)
		printLineBreak(p)
	}
	printLostTokenBefore(p.withForcedUnnest(), "]", end(strct))
	p.resetContinuationLine()
}

func printUnionLit(p *printer, union *parser.UnionLit) {
	printLostTokenAfter(p, "[", start(union))
	p = p.withLineBreaksAllowed()
	printIdent(p, union.CaseVal.Name)
	if union.CaseVal.Val != nil {
		printSpace(p)
		printExpr(p.withLineBreaksAllowed(), union.CaseVal.Val)
	}
	printLostTokenBefore(p.withForcedUnnest(), "]", end(union))
	p.resetContinuationLine()
}

func printBlockLit(p *printer, block *parser.BlockLit) {
	prevLoc := start(block)
	if len(block.Parms) > 0 {
		prevLoc = printFuncParms(p, block.Parms, prevLoc)
	}
	printFuncBody(p, block.Exprs, prevLoc, end(block))
}

func printIdent(p *printer, id parser.Ident) {
	printToken(p, id.Name, id.L)
}

// Prints a token that does not have a Loc of its own by guessing a location
// using the location of the previous token, and returns the guessed Loc.
//
// TODO(eaburns): get rid of all uses of lostTokenAfter.
func printLostTokenAfter(p *printer, s string, prevLoc loc.Loc) loc.Loc {
	l := loc.Loc{prevLoc[1], prevLoc[1] + len(s)}
	printToken(p, s, l)
	return l
}

// Prints a token that does not have a Loc of its own by guessing a location
// using the location of the following token.
//
// TODO(eaburns): get rid of all uses of lostTokenBefore.
func printLostTokenBefore(p *printer, s string, nextLoc loc.Loc) loc.Loc {
	l := loc.Loc{nextLoc[0] - len(s), nextLoc[0]}
	printToken(p, s, l)
	return l
}

// token prints comments and newlines before the next token,
// s at source location l.
//
// This handles inserting necessary newlines,
// and optionally inserting allowed blank lines.
// It does this cognizant of comments that are either
// attached to (before the token with no intervening blank line)
// or detatched from (separated by a blank line) the token.
func printToken(p *printer, s string, l loc.Loc) {
	if s == "}" {
		// Comments on } should be indented,
		// and not on the same level of nesting as the } itself.
		p.nesting += "\t"
	}

	detatched, attached := tokenComments(p, l)

	// Add any detatched comments.
	for _, c := range detatched {
		sourceLine := p.src.endLine(p.src.loc)
		commentLine := p.src.startLine(c)
		if sourceLine < commentLine {
			p.dst.writeNewLine()
			// Allow blank lines in detatched comments.
			if sourceLine < commentLine-1 {
				p.dst.writeNewLine()
			}
		}
		p.print(c.Text, c.L)
	}

	tokenLine := p.src.startLine(l)
	if len(attached) > 0 {
		tokenLine = p.src.startLine(attached[0].L)
	}

	var newLines int
	if n := len(detatched); n > 0 {
		// Print 0, 1, or 2 newlines between the detatched comments
		// and the token+attached comments.
		// To guard against the case that some token locations are imprecise,
		// we always print a \n after a // comment.
		sourceLine := p.src.endLine(p.src.loc)
		if strings.HasPrefix(detatched[n-1].Text, "//") || sourceLine < tokenLine {
			p.dst.writeNewLine()
			newLines++
			if sourceLine < tokenLine-1 {
				p.dst.writeNewLine()
				newLines++
			}
		}
	}

	// Print up to the required minNewLines.
	for newLines < p.dst.minNewLines {
		p.dst.writeNewLine()
		newLines++
	}
	p.dst.minNewLines = 0

	// Add any optional newlines that appear in the source up to maxNewLines.
	prevLine := p.src.endLine(p.src.loc) + newLines
	maxNewLines := p.maxNewLines
	if len(attached) > 0 && maxNewLines == 0 {
		// If there is an attached comment and new lines are not allowed,
		// allow one new line, so the comment can be on its own line
		// and not lifted to the line of the preceeding token.
		// For example, allow:
		// 	(
		// 	// comment
		// 	x)
		// instead of forcing:
		// 	(//comment
		// 	x)
		// (Note ()-sub expressions disallow newlines.)
		maxNewLines++
	}
	for prevLine < tokenLine && newLines < maxNewLines {
		p.dst.writeNewLine()
		newLines++
		prevLine++
	}

	for i, c := range attached {
		if i > 0 && p.src.endLine(p.src.loc) < p.src.startLine(c) {
			p.dst.writeNewLine()
		}
		p.print(c.Text, c.L)
	}
	// Add a newline if there was one between the last comment and the token.
	// We are careful here to always add one if the last comment was //-style,
	// to handle the case that l is not a precise location
	// (which can happen in cases where we make up an approximate location
	// for tokens that do not have exact location information).
	if n := len(attached); n > 0 &&
		(strings.HasPrefix(attached[n-1].Text, "//") ||
			p.src.endLine(p.src.loc) < p.src.startLine(l)) {
		p.dst.writeNewLine()
	}
	if s == "}" {
		// Remove \t we inserted above for comments.
		p.nesting = strings.TrimSuffix(p.nesting, "\t")
	}
	p.print(s, l)
}

// tokenComments returns comments before the token at l;
// the returned comments are split into two sets: detatched and attached.
// Detatched comments are separated from l by at least one blank line.
// Attached comments are before l, but are not separated from it by a blank line.
func tokenComments(p *printer, l loc.Loc) (detatched, attached []parser.Comment) {
	var i int
	for ; i < len(p.src.comments) && p.src.comments[i].L[1] <= l[0]; i++ {
	}
	before := p.src.comments[:i]
	p.src.comments = p.src.comments[i:]

	i = len(before) - 1
	line := p.src.startLine(l)
	for i >= 0 && p.src.endLine(before[i].L) >= line-1 {
		line = p.src.startLine(before[i].L)
		i--
	}
	prevLine := p.src.endLine(p.src.loc)
	if i < len(before)-1 && prevLine > 1 && prevLine == p.src.startLine(before[i+1]) {
		// The top-most comment of the block of comments
		// that is on the line just before our token,
		// is at the end of a line that has preceeding tokens:
		//	foo // DETATCHED
		// 	// ATTACHED
		// 	bar
		// In this case, the DETATCHED comment
		// is not attached to bar, but ATTACHED is.
		i++
	}
	return before[:i+1], before[i+1:]
}

func printTrailingComments(p *printer) {
	for len(p.src.comments) > 0 {
		c := p.src.comments[0]
		if strings.HasPrefix(c.Text, "//") || p.src.endLine(p.src.loc) < p.src.startLine(c) {
			p.dst.writeNewLine()
		}
		if p.src.endLine(p.src.loc) < p.src.startLine(c)-1 {
			p.dst.writeNewLine()
		}
		p.print(c.Text, c.L)
		p.src.comments = p.src.comments[1:]
	}
}

func printLineBreak(p *printer) {
	p.dst.minNewLines++
}

func printSpace(p *printer) {
	p.dst.minSpaces++
}

type printer struct {
	prev *printer
	src  *source
	dst  *destination
	// nesting is indentation applied after newlines
	// to denote a nested scope.
	nesting string
	// continuation is indentation applied after newlines
	// to denote the continuation of an item that allows line breaks.
	continuation     string
	continuationLine int
	maxNewLines      int
}

func newPrinter(w io.Writer, file *parser.File, locs loc.Files) *printer {
	return &printer{
		src: &source{
			locs:     locs,
			loc:      loc.Loc{1, 1},
			comments: file.Comments,
		},
		dst: &destination{
			w:           w,
			minNewLines: 0,
			line:        1,
			col:         0,
		},
		nesting:      "",
		continuation: "",
		maxNewLines:  0,
	}
}

func (p *printer) print(s string, l loc.Loc) {
	if p.dst.col == 0 {
		p.dst.writeString(p.continuation)
		p.dst.writeString(p.nesting)
	}
	for p.dst.minSpaces > 0 {
		p.dst.writeString(" ")
		p.dst.minSpaces--
	}
	p.dst.writeString(s)
	p.src.loc = l
}

func (p *printer) withIndent() *printer {
	q := *p
	q.prev = p
	q.nesting += "\t"
	q.continuation = ""
	q.continuationLine = 0
	return &q
}

func (p *printer) withLineBreaksDisallowed() *printer {
	q := *p
	q.prev = p
	q.maxNewLines = 0
	return &q
}

func (p *printer) withLineBreaksAllowed() *printer {
	q := *p
	q.prev = p
	q.maxNewLines = 1
	// Don't add a double continuation indent
	// if the previous one never encountered a line break.
	if p.dst.line != p.continuationLine {
		q.continuation += "\t"
		q.continuationLine = p.dst.line
	}
	return &q
}

func (p *printer) withForcedUnnest() *printer {
	q := *p
	q.prev = p
	q.continuation = strings.TrimSuffix(q.continuation, "\t")
	return &q
}

func (p *printer) resetContinuationLine() {
	l := p.continuationLine
	for x := p; x != nil && x.continuationLine == l; x = x.prev {
		x.continuationLine = x.dst.line
	}
}

func (p *printer) withBlankLinesAllowed() *printer {
	q := *p
	q.prev = p
	q.maxNewLines = 2
	return &q
}

type source struct {
	locs     loc.Files
	loc      loc.Loc
	comments []parser.Comment
}

func (src *source) startLine(l loc.Locer) int {
	return src.locs.Location(l.Loc()).Line[0]
}

func (src *source) endLine(l loc.Locer) int {
	return src.locs.Location(l.Loc()).Line[1]
}

type destination struct {
	w io.Writer
	// The number of spaces needed before the next token.
	minSpaces int
	// The number of newlines needed before the next token.
	minNewLines int
	line        int
	col         int
}

func (dst *destination) writeNewLine() {
	dst.writeString("\n")
	dst.minSpaces = 0
}

func (dst *destination) writeString(s string) {
	if _, err := io.WriteString(dst.w, s); err != nil {
		panic(ioError{err})
	}
	for _, r := range s {
		dst.col++
		if r == '\n' {
			dst.line++
			dst.col = 0
		}
	}
}

func start(l loc.Locer) loc.Loc { return loc.Loc{l.Loc()[0], l.Loc()[0]} }

func end(l loc.Locer) loc.Loc { return loc.Loc{l.Loc()[1], l.Loc()[1]} }
