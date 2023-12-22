package checker

import (
	"fmt"
	"strconv"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

func checkParserExprs(x scope, next *int, pexprs []parser.Expr) ([]*TypeParm, []Expr, []Error) {
	var typeParms []*TypeParm
	var exprs []Expr
	var errors []Error
	for _, pexpr := range pexprs {
		xx, tparms, expr, errs := checkParserExprOrNewLocal(x, next, pexpr)
		if len(errs) > 0 {
			errors = append(errors, errs...)
		} else {
			x = xx
			typeParms = append(typeParms, tparms...)
			exprs = append(exprs, expr)
		}
	}
	return typeParms, exprs, errors
}

func checkParserExprOrNewLocal(x scope, next *int, pexpr parser.Expr) (scope, []*TypeParm, Expr, []Error) {
	if isNewLocal(x, pexpr) {
		return checkParserNewLocalAssign(x, next, pexpr.(*parser.Call))
	}
	tparms, expr, errs := checkParserExpr(x, next, pexpr)
	return x, tparms, expr, errs
}

func isNewLocal(x scope, pexpr parser.Expr) bool {
	if _, ok := isAssign(pexpr); !ok {
		return false
	}
	_, ok := isNewID(x, pexpr.(*parser.Call).Args[0])
	return ok
}

func checkParserExpr(x scope, next *int, pexpr parser.Expr) (tparms []*TypeParm, expr Expr, errs []Error) {
	tr := trItem(x, "%s (%v)", pexpr, pexpr.Loc())
	defer func() {
		if expr != nil {
			tr.trace("%s", exprAndTypeString(tparms, expr))
		} else {
			tr.trace("error: %s", errs)
		}
		tr.done()
	}()

	switch pexpr := pexpr.(type) {
	case *parser.Call:
		return checkParserCall(x, next, pexpr)
	case *parser.Convert:
		return checkParserConvert(x, next, pexpr)
	case *parser.SubExpr:
		return checkParserSubExpr(x, next, pexpr)
	case *parser.ArrayLit:
		return checkParserArrayLit(x, next, pexpr)
	case *parser.StructLit:
		return checkParserStructLit(x, next, pexpr)
	case *parser.UnionLit:
		return checkParserUnionLit(x, next, pexpr)
	case *parser.BlockLit:
		return checkParserBlockLit(x, next, pexpr)
	case *parser.StrLit:
		return errorToSlice(checkParserStrLit(x, next, pexpr))
	case *parser.CharLit:
		return errorToSlice(checkParserCharLit(x, next, pexpr))
	case *parser.IntLit:
		return errorToSlice(checkParserIntLit(x, next, pexpr))
	case *parser.FloatLit:
		return errorToSlice(checkParserFloatLit(x, next, pexpr))
	case *parser.ModSel:
		return errorToSlice(checkParserModSel(x, next, pexpr))
	case parser.Ident:
		return errorToSlice(checkParserIdent(x, next, false, pexpr))
	default:
		panic(fmt.Sprintf("impossible expr type: %T", pexpr))
	}
}

func errorToSlice(tparms []*TypeParm, e Expr, err Error) ([]*TypeParm, Expr, []Error) {
	if err != nil {
		return tparms, e, []Error{err}
	}
	return tparms, e, nil
}

func checkParserCall(x scope, next *int, pexpr *parser.Call) ([]*TypeParm, Expr, []Error) {
	switch pexpr.Fun.(type) {
	case parser.Ident:
		return checkParserIdentCall(x, next, pexpr)
	case *parser.ModSel:
		return checkParserModSelCall(x, next, pexpr)
	default:
		return checkParserExprCall(x, next, pexpr)
	}
}

func checkParserIdentCall(x scope, next *int, pexpr *parser.Call) ([]*TypeParm, Expr, []Error) {
	pident := pexpr.Fun.(parser.Ident)
	if pident.Name == ":=" && len(pexpr.Args) == 2 {
		// Assignment when the left-hand-side is a new local variable,
		// is checked above as a special case in checkParserExprs,
		// because we need it to insert a new scope for the following exprs.
		//
		// This calls checks all other assignments.
		return checkParserNonNewLocalAssign(x, next, pexpr)
	}
	tparms, args, errs := checkParserExprs(x, next, pexpr.Args)
	if len(errs) > 0 {
		return nil, nil, errs
	}
	p, t := newTypeVar(next, pexpr.Loc())
	tparms = append(tparms, p)
	fun := &unresolvedID{
		x:    x,
		Name: pident.Name,
		T: &FuncType{
			Parms: types(args),
			Ret:   t,
			L:     pexpr.Fun.Loc(),
		},
		L: pexpr.Fun.Loc(),
	}
	expr := &Call{
		Func: fun,
		Args: args,
		T:    t,
		L:    pexpr.L,
	}
	return tparms, expr, nil
}

func checkParserModSelCall(x scope, next *int, pexpr *parser.Call) ([]*TypeParm, Expr, []Error) {
	pmodsel := pexpr.Fun.(*parser.ModSel)
	imp := findImport(x, pmodsel.Mod.Name)
	if imp == nil {
		return nil, nil, []Error{
			&NotFoundError{Item: pmodsel.Mod, scope: x},
		}
	}
	tparms, args, errs := checkParserExprs(x, next, pexpr.Args)
	if len(errs) > 0 {
		return nil, nil, errs
	}
	p, t := newTypeVar(next, pexpr.Loc())
	tparms = append(tparms, p)
	fun := &unresolvedID{
		x:    imp,
		Name: pmodsel.Name.Name,
		T: &FuncType{
			Parms: types(args),
			Ret:   t,
			L:     pexpr.Fun.Loc(),
		},
		L: pexpr.Fun.Loc(),
	}
	expr := &Call{
		Func: fun,
		Args: args,
		T:    t,
		L:    pexpr.L,
	}
	return tparms, expr, nil
}

func checkParserExprCall(x scope, next *int, pexpr *parser.Call) ([]*TypeParm, Expr, []Error) {
	typeParms, fun, errors := checkParserExpr(x, next, pexpr.Fun)
	tparms, args, errs := checkParserExprs(x, next, pexpr.Args)
	typeParms = append(typeParms, tparms...)
	errors = append(errors, errs...)
	if len(errors) > 0 {
		return nil, nil, errors
	}
	funcType, ok := fun.Type().(*FuncType)
	if !ok {
		p, t := newTypeVar(next, pexpr.Fun.Loc())
		typeParms = append(typeParms, p)
		funcType = &FuncType{
			Parms: types(args),
			Ret:   t,
			L:     pexpr.Fun.Loc(),
		}
	}
	expr := &Call{
		Func: &ExprFunc{
			Expr:     fun,
			FuncType: funcType,
		},
		Args: args,
		T:    funcType.Ret,
		L:    pexpr.L,
	}
	return typeParms, expr, nil
}

func types(exprs []Expr) []Type {
	var types []Type
	for _, expr := range exprs {
		types = append(types, expr.Type())
	}
	return types
}

func checkParserNonNewLocalAssign(x scope, next *int, pexpr *parser.Call) ([]*TypeParm, Expr, []Error) {
	var typeParms []*TypeParm
	var errors []Error
	var lhs Expr
	if ident, ok := pexpr.Args[0].(parser.Ident); ok {
		var err Error
		typeParms, lhs, err = checkParserIdent(x, next, true, ident)
		errors = appendIfError(errors, err)
	} else {
		typeParms, lhs, errors = checkParserExpr(x, next, pexpr.Args[0])
	}
	tparms, rhs, errs := checkParserExpr(x, next, pexpr.Args[1])
	errors = append(errors, errs...)
	if len(errors) > 0 {
		return typeParms, nil, errs
	}
	typeParms = append(typeParms, tparms...)
	expr := &Call{
		Func: &Builtin{
			N:     ":=",
			Op:    Assign,
			Parms: []Type{lhs.Type(), rhs.Type()},
			Ret:   _empty,
		},
		Args: []Expr{lhs, rhs},
		T:    _empty,
		L:    pexpr.L,
	}
	return typeParms, expr, nil
}

func appendIfError(errs []Error, err Error) []Error {
	if err != nil {
		errs = append(errs, err)
	}
	return errs
}

func checkParserNewLocalAssign(x scope, next *int, pexpr *parser.Call) (_ scope, tparms []*TypeParm, expr Expr, errs []Error) {
	tr := trItem(x, "%s (%v)", pexpr, pexpr.Loc())
	defer func() {
		if expr != nil {
			tr.trace("%s", exprAndTypeString(tparms, expr))
		} else {
			tr.trace("error: %s", errs)
		}
		tr.done()
	}()

	var rhs Expr
	tparms, rhs, errs = checkParserExpr(x, next, pexpr.Args[1])
	if len(errs) > 0 {
		return x, nil, nil, errs
	}
	ident := pexpr.Args[0].(parser.Ident)
	local := newLocal(x, ident.Name, rhs.Type(), ident.L)
	if local == nil {
		errs = append(errs, newError(pexpr.Loc(), "local defined outside of a block"))
		return x, nil, nil, errs
	}
	lhsExpr := &Local{Def: local, T: refLiteral(rhs.Type()), L: ident.Loc()}
	expr = &Call{
		Func: &Builtin{
			N:     ":=",
			Op:    Assign,
			Parms: []Type{lhsExpr.Type(), rhs.Type()},
			Ret:   _empty,
		},
		Args: []Expr{lhsExpr, rhs},
		T:    _empty,
		L:    pexpr.L,
	}
	return &localScope{parent: x, LocalDef: local}, tparms, expr, nil
}

func checkParserSubExpr(x scope, next *int, pexpr *parser.SubExpr) ([]*TypeParm, Expr, []Error) {
	return checkParserExpr(x, next, pexpr.Expr)
}

func checkParserConvert(x scope, next *int, pexpr *parser.Convert) ([]*TypeParm, Expr, []Error) {
	typ, errors := makeType(x, pexpr.Type)
	tparms, expr, errs := checkParserExpr(x, next, pexpr.Expr)
	errors = append(errors, errs...)
	if len(errors) > 0 {
		return nil, nil, errors
	}
	expr = &Convert{
		Kind:     unresolvedConvert,
		Explicit: true,
		Expr:     expr,
		T:        typ,
		L:        pexpr.Loc(),
	}
	return tparms, expr, nil
}

func checkParserArrayLit(x scope, next *int, pexpr *parser.ArrayLit) ([]*TypeParm, Expr, []Error) {
	typ := &ArrayType{L: pexpr.L}
	al := &ArrayLit{Array: typ, T: typ, L: pexpr.L}
	var errors []Error
	var typeParms []*TypeParm
	var elemType Type
	for i, pelem := range pexpr.Exprs {
		tparms, elem, errs := checkParserExpr(x, next, pelem)
		if len(errs) > 0 {
			errors = append(errors, errs...)
			continue
		}
		typeParms = append(typeParms, tparms...)
		al.Elems = append(al.Elems, elem)
		if i == 0 {
			elemType = elem.Type()
			continue
		}
		if elemType != nil {
			tparms, typ := isection(typeParms, elemType, elem.Type())
			if typ == nil {
				elemType = nil
				continue
			}
			elemType = typ
			typeParms = tparms
		}
	}
	if len(errors) > 0 {
		return nil, nil, errors
	}
	if elemType != nil {
		typ.ElemType = elemType
	} else {
		p, t := newTypeVar(next, pexpr.L)
		typeParms = append(typeParms, p)
		typ.ElemType = t
	}
	return typeParms, al, nil
}

func checkParserStructLit(x scope, next *int, pexpr *parser.StructLit) ([]*TypeParm, Expr, []Error) {
	typ := &StructType{L: pexpr.L}
	sl := &StructLit{Struct: typ, T: typ, L: pexpr.L}
	var errors []Error
	var typeParms []*TypeParm
	for _, pfield := range pexpr.FieldVals {
		tparms, field, errs := checkParserExpr(x, next, pfield.Val)
		if len(errs) > 0 {
			errors = append(errors, errs...)
			continue
		}
		typeParms = append(typeParms, tparms...)
		typ.Fields = append(typ.Fields, FieldDef{
			Name: pfield.Name.Name,
			Type: field.Type(),
			L:    pfield.L,
		})
		sl.Fields = append(sl.Fields, field)
	}
	if len(errors) > 0 {
		return nil, nil, errors
	}
	return typeParms, sl, nil
}

func checkParserUnionLit(x scope, next *int, pexpr *parser.UnionLit) ([]*TypeParm, Expr, []Error) {
	typ := &UnionType{
		Cases: []CaseDef{{
			Name: pexpr.CaseVal.Name.Name,
			L:    pexpr.CaseVal.L,
		}},
		Open: true,
		L:    pexpr.L,
	}
	ul := &UnionLit{
		Union: typ,
		Case:  &typ.Cases[0],
		T:     typ,
		L:     pexpr.L,
	}
	if pexpr.CaseVal.Val == nil {
		return nil, ul, nil
	}
	tparms, expr, errs := checkParserExpr(x, next, pexpr.CaseVal.Val)
	if len(errs) > 0 {
		return nil, nil, errs
	}
	ul.Val = expr
	typ.Cases[0].Type = expr.Type()
	return tparms, ul, nil
}

func checkParserBlockLit(x scope, next *int, pexpr *parser.BlockLit) ([]*TypeParm, Expr, []Error) {
	var typeParms []*TypeParm
	var errors []Error
	bl := &BlockLit{
		L:      pexpr.L,
		Ret:    _empty,
		Func:   &FuncType{L: pexpr.L, Ret: _empty},
		pexprs: pexpr.Exprs,
	}
	bl.T = bl.Func
	bl.Parms, errors = makeFuncParms(x, pexpr.Parms)

	for i := range bl.Parms {
		if bl.Parms[i].T == nil {
			p, t := newTypeVar(next, bl.Parms[i].L)
			bl.Parms[i].T = t
			typeParms = append(typeParms, p)
		}
		bl.Func.Parms = append(bl.Func.Parms, bl.Parms[i].T)
	}

	xx := &blockLitScope{parent: x, BlockLit: bl}
	tparms, bodyExprs, errs := checkParserExprs(xx, next, pexpr.Exprs)
	errors = append(errors, errs...)
	typeParms = append(typeParms, tparms...)
	bl.Exprs = bodyExprs
	if n := len(bodyExprs); n > 0 {
		t := bodyExprs[n-1].Type()
		bl.Ret = t
		bl.Func.Ret = t
	}
	return typeParms, bl, errors
}

func checkParserStrLit(x scope, next *int, pexpr *parser.StrLit) ([]*TypeParm, Expr, Error) {
	p, t := newTypeVar(next, pexpr.L)
	return []*TypeParm{p}, &StrLit{Text: pexpr.Data, T: t}, nil
}

func checkParserCharLit(x scope, next *int, pexpr *parser.CharLit) ([]*TypeParm, Expr, Error) {
	parserIntLit := &parser.IntLit{
		Text: strconv.FormatInt(int64(pexpr.Rune), 10),
		L:    pexpr.L,
	}
	return checkParserIntLit(x, next, parserIntLit)
}

func checkParserIntLit(x scope, next *int, pexpr *parser.IntLit) ([]*TypeParm, Expr, Error) {
	p, t := newTypeVar(next, pexpr.L)
	// Val set later
	return []*TypeParm{p}, &IntLit{Text: pexpr.Text, T: t}, nil
}

func checkParserFloatLit(x scope, next *int, pexpr *parser.FloatLit) ([]*TypeParm, Expr, Error) {
	p, t := newTypeVar(next, pexpr.L)
	// Val set later
	return []*TypeParm{p}, &FloatLit{Text: pexpr.Text, T: t}, nil
}

func checkParserModSel(x scope, next *int, pexpr *parser.ModSel) ([]*TypeParm, Expr, Error) {
	imp := findImport(x, pexpr.Mod.Name)
	if imp == nil {
		return nil, nil, &NotFoundError{Item: pexpr.Mod, scope: x}
	}
	return checkParserIdent(imp, next, false, pexpr.Name)
}

func checkParserIdent(x scope, next *int, assignLHS bool, pexpr parser.Ident) ([]*TypeParm, Expr, Error) {
	ids := findIDs(x, pexpr.Name)
	if len(ids) == 1 {
		_, isParm := ids[0].(*ParmDef)
		_, isLocal := ids[0].(*LocalDef)
		if isParm || isLocal {
			return nil, useID(x, pexpr.L, assignLHS, ids[0]), nil
		}
	}
	p, t := newTypeVar(next, pexpr.L)
	expr := &unresolvedID{
		x:    x,
		Name: pexpr.Name,
		T:    t,
		L:    pexpr.L,
	}
	return []*TypeParm{p}, expr, nil
}

func newTypeVar(next *int, l loc.Loc) (*TypeParm, *TypeVar) {
	p := &TypeParm{Name: fmt.Sprintf("T%d", *next), L: l}
	t := &TypeVar{Def: p, L: l}
	*next++
	return p, t
}
