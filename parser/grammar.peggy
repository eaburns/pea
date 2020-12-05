{
package parser

import (
	. "github.com/eaburns/pea/tree"
	"github.com/eaburns/peggy/peg"
)
}

File <- imports:Import* defs:Def* Eof {
	return File{Imports: imports, Defs: defs}
}

Import <- _ reserved:("import" / "Import") id:Id? path:StrLit {
	return &Import{
		Exp: reserved == "Import",
		Name: id,
		Path: path.(*StrLit).Data,
		L: l(parser, start, end),
	}
}

Def <- ConstDef / VarDef / TypeDef / FuncDef / TestDef

ConstDef <- _ reserved:("const" / "Const") name:Id t:Type? (_ "=" expr:Expr)? {
	var typ Type; if t != nil { typ = *t }
	return Def(&VarDef{
		Exp: reserved == "Const",
		Const: true,
		Name: name,
		Type: typ,
		Expr: expr,
		L: l(parser, start, end),
	})
}
VarDef <- _ reserved:("var" / "Var") name:Id t:Type? (_ "=" expr:Expr)? {
	var typ Type; if t != nil { typ = *t }
	return Def(&VarDef{
		Exp: reserved == "Var",
		Const: false,
		Name: name,
		Type: typ,
		Expr: expr,
		L: l(parser, start, end),
	})
}

TypeDef <- _ reserved:("type" / "Type") ps:TypeVars? name:Id t:Type? {
	var parms []TypeVar; if ps != nil { parms = *ps }
	var typ Type; if t != nil { typ = *t }
	return Def(&TypeDef{
		Exp: reserved == "Type",
		TypeParms: parms,
		Name: name,
		Type: typ,
		L: l(parser, start, end),
	})
}

TypeVars <-
	tv:TypeVar { return []TypeVar{tv} } /
	_ "(" t0:TypeVar ts:(_ "," t1:TypeVar { return TypeVar(t1) })* (_ ",")? _ ")" {
		return []TypeVar(append([]TypeVar{t0}, ts...))
	}

Type <-
	RefType /
	NamedType /
	tvar:TypeVar { return Type(tvar) } /
	_"(" typ:Type _ ")" { return Type(typ) } /
	StructType /
	UnionType /
	FuncType

Types <- t0:Type ts:(_ "," t1:Type { return Type(t1) })* (_ ",")? {
	return []Type(append([]Type{t0}, ts...))
}

RefType <- _ "&" typ:Type {
	return Type(&RefType{Type: typ, L: l(parser, start, end)})
}

NamedType <- (args:TypeArgs)? names:QName+ {
	return Type(namedType(l(parser, start, end), args, names))
}

TypeArgs <-
	tv:TypeVar { return []Type{tv} } /
	_ "(" arg:Type _ ")" { return []Type{arg} } /
	_ "(" args: Types _ ")" { return []Type(args) }

QName <- id0:Id id1:(_ "." id:Id { return Id(id) })? {
	return qname(makeQname(id0, id1))
}

StructType <- "[" fs:Fields? _ "]" {
	var fields []Field; if fs != nil { fields = *fs }
	return Type(&StructType{Fields: fields, L: l(parser, start, end)})
}

Field <- name:Id _ ":" typ:Type {
	return Field{
		Name: name,
		Type: typ,
		L: l(parser, start, end),
	}
}

Fields <- f0:Field fs:(_ "," f1:Field { return Field(f1) })* (_ ",")? {
	return []Field(append([]Field{f0}, fs...))
}

UnionType <- _ "[" cases:Cases _ "]" {
	return Type(&UnionType{Cases: cases, L: l(parser, start, end)})
}

Case <- name:Id (_ ":" typ:Type)? {
	return Case{
		Name: name,
		Type: typ,
		L: l(parser, start, end),
	}
}

Cases <- (_ "|")? c0:Case cs:(_ "|" c1:Case { return Case(c1) })* {
	return []Case(append([]Case{c0}, cs...))
}

FuncType <-
	_ "{" _ "}" { return Type(&FuncType{L: l(parser, start, end)}) } /
	_ "{" ps:Types? _ "|" r:Types? _"}" {
		var parms []Type; if ps != nil { parms = *ps }
		var ret Type; if r != nil { ret = *r }
		return Type(&FuncType{
			Parms: parms,
			Ret: ret,
			L: l(parser, start, end),
		})
	}

FuncDef <- _ reserved:("func" / "Func") name:FuncName _ "(" ps:FuncParms? _ ")" r:Type? (_ ":" iface:FuncDecls)? (_ "{" es:Exprs? _ "}")? {
	var parms []FuncParm; if ps != nil { parms = *ps }
	var ret Type; if r != nil { ret = *r }
	var exprs []Expr; if es != nil { exprs = *es }
	return Def(&FuncDef{
		Exp: reserved == "Func",
		Name: name,
		Parms: parms,
		Ret: ret,
		Iface: iface,
		Exprs: exprs,
		L: l(parser, start, end),
	})
}

FuncName <- IdxOp / Op / Id / Kwds

FuncParms <- p0:FuncParm ps:(_ "," p1:FuncParm { return FuncParm(p1) })* (_ ",") ? {
	return []FuncParm(append([]FuncParm{p0}, ps...))
}

FuncParm <- name:Id typ:Type (_ "=" init:Expr)? {
	return FuncParm{
		Name: name,
		Type: typ,
		Init: init,
		L: l(parser, start, end),
	}
}

FuncDecl <- name:FuncName _ "(" parms:Types _ ")" r:Type? {
	var ret Type; if r != nil { ret = *r }
	return FuncDecl{
		Name: name,
		Parms: parms,
		Ret: ret,
		L: l(parser, start, end),
	}
}

FuncDecls <- fd0:FuncDecl fds:(_ "," fd1:FuncDecl { return FuncDecl(fd1) })* (_ ",")? {
	return []FuncDecl(append([]FuncDecl{fd0}, fds...))
}

TestDef <- _ "test" name:Id _ "{" es:Exprs? _ "}" {
	var exprs []Expr; if es != nil { exprs = *es }
	return Def(&TestDef{
		Name: name,
		Exprs: exprs,
		L: l(parser, start, end),
	})
}

Exprs <- e0:Expr es:(_ "," e1:Expr { return Expr(e1) })* (_ ",")? {
	return []Expr(append([]Expr{e0}, es...))
}

Expr <- KwCall / KwArg

KwCall <- kws:Kw+ {
	return Expr(kwCall(l(parser, start, end), kws))
}

Kw <- name:Kwd arg:KwArg {
	return kw{name: name, arg: arg}
}

KwArg <- Bin2 / Bin2Arg

Bin2 <- arg0:Bin2Arg calls:Bin2Tail+ {
	return Expr(bins(arg0, calls))
}

Bin2Tail <- name:Op2 arg1:Bin2Arg {
	return &Call{
		Fun: name,
		Args: []Expr{nil, arg1},
		L: l(parser, start, end),
	}
}

Bin2Arg <- Bin1 / Bin1Arg

Bin1 <- arg0:Bin1Arg calls:Bin1Tail+ {
	return Expr(bins(arg0, calls))
}

Bin1Tail <- name:Op1 arg1:Bin1Arg {
	return &Call{
		Fun: name,
		Args: []Expr{nil, arg1},
		L: l(parser, start, end),
	}
}

Bin1Arg <- Cvt / UnArg

Cvt <- expr:UnArg _ ":" typ:Type {
	return Expr(&Convert{
		Expr: expr,
		Type: typ,
		L: l(parser, start, end),
	})
}

Un <- name:Op arg:UnArg {
	return Expr(&Call{
		Fun: name,
		Args: []Expr{arg},
		L: l(parser, start, end),
	})
}

UnArg <- Un / Idx / Sel / Call / Pri

Idx <- expr:IdxArg indexes:Idxs+ {
	return Expr(idxs(expr, indexes))
}

Idxs <- _ "[" index:Expr _ "]" {
	return Expr(&Index{Index: index, L: l(parser, start, end)})
}

IdxArg <- Call / Sel / Pri

Call <- fun:CallArg argss:ArgList+ {
	return Expr(calls(fun, argss))
}

ArgList <- _ "(" as:Exprs? _ ")" {
	return &Call{Args: args(as), L: l(parser, start, end)}
}

CallArg <- Sel / Pri

Sel <- expr:Pri ids:(_ "." id:Id { return Id(id) })+ {
	return Expr(sel(expr, ids))
}

Pri <-
	_ "(" expr:Expr _ ")" { return Expr(expr) } /
	QualOp /
	QualKwds /
	StructLit /
	ArrayLit /
	BlkLit /
	CharLit /
	StrLit /
	NumLit /
	id:Id { return Expr(id) }

QualOp <- _ "(" (qual:Id _ ".")? name:(IdxOp / Op) _ ")" {
	return Expr(&Selector{Expr: qual, Id: name, L: l(parser, start, end)})
}

QualKwds <- (qual:Id _ ".")? name:Kwds {
	return Expr(&Selector{Expr: qual, Id: name, L: l(parser, start, end)})
}

StructLit <- "[" fs:FieldVals? _ "]" {
	var fields []FieldVal; if fs != nil { fields = *fs }
	return Expr(&StructLit{Fields: fields, L: l(parser, start, end)})
}

ArrayLit <- _ "[" exprs:Exprs _ "]" {
	return Expr(&ArrayLit{Exprs: exprs, L: l(parser, start, end)})
}

FieldVal <- id:Id _ ":" expr:Expr {
	return FieldVal{Id: id, Expr: expr, L: l(parser, start, end)}
}

FieldVals <- v0:FieldVal vs:(_ "," v:FieldVal { return FieldVal(v) })* (_ ",")? {
	return []FieldVal(append([]FieldVal{v0}, vs...))
}

BlkLit <- _ "{" (parms:FuncParms _ "|")? es:Exprs? _ "}" {
	var exprs []Expr; if es != nil { exprs = *es }
	return Expr(&BlkLit{
		Parms: parms,
		Exprs: exprs,
		L: l(parser, start, end),
	})
}

CharLit "character" <- _ data:(['] !"\n" Esc / "\\'" / [^'] [']) {
	return Expr(&CharLit{
		Rune: interpRune(data),
		L: l(parser, start, end),
	})
}

StrLit "string" <- InterpStr / RawStr

InterpStr <- _ ["] data:(!"\n" (Esc / "\\\"" { return "\"" }/ [^"]))* ["] {
	return Expr(&StrLit{
		Raw: false,
		Data: data,
		L: l(parser, start, end),
	})
}

Esc <-
	"\\n" { return "\n" } /
	"\\t" { return "\t" } /
	"\\b" { return "\b"} /
	"\\\\" { return "\\" } /
	"\\" x0:( X X ) { return string(interpHex(x0)) } /
	"\\x" x1:( X X X X ) { return string(interpHex(x0)) } /
	"\\X" x2:( X X X X X X X X ) { return string(interpHex(x0)) }

RawStr <- _ [`] data:("\\`" / [^`])* [`] {
	return Expr(&StrLit{
		Raw: true,
		Data: data,
		L: l(parser, start, end),
	})
}

NumLit "number" <- DecLit / HexLit / FloatLit

DecLit <- _ text:(D+) {
	return Expr(&IntLit{Text: text, L: l(parser, start, end)})
}

HexLit <- _ text:(("0x" / "0X") X+) {
	return Expr(&IntLit{Text: text, L: l(parser, start, end)})
}

FloatLit <- _ text:(D+ "." D+ ([eE] [+\-]? D+)?) {
	return Expr(&FloatLit{Text: text, L: l(parser, start, end)})
}

Op "operator" <- _ name:[\-+*/%~!?^<>=&|@$]+ {
	return Id{Name: name, L: l(parser, start, end)}
}

Op2 "operator2" <- _ !Op1 name:[\-+*/%~!?^<>=&|@$]+ {
	return Id{Name: name, L: l(parser, start, end)}
}

Op1 "operator1" <- _ name:([*/%] [\-+*/%~!?^<>=&|@$]*) {
	return Id{Name: name, L: l(parser, start, end)}
}

IdxOp "index operator" <- _ "[" _ "]" {
	return Id{Name: "[]", L: l(parser, start, end)}
}

Kwd "keyword" <- _ id:Id ":" { return Id(id) }

Kwds "keywords" <- _ keywords:Kwd+ {
	var name string
	for _, k := range keywords {
		name += k.Name
	}
	return Id{Name: name, L: l(parser, start, end)}
}

Id "identifier" <- _ !Reserved !TypeVar name:(("_" / L) ("_" / L / D)*) {
	return Id{Name: name, L: l(parser, start, end)}
}

TypeVar "type variable" <- _ name:[A-Z] !L !D {
	return TypeVar{Name: name, L: l(parser, start, end)}
}

Reserved <- "import" / "Import" / "const" / "Const" / "var" / "Var" / "type" / "Type" / "func" / "Func" / "test"
D <- [0-9]
X <- [a-fA-F0-9]
L <- r:. &{isLetter(r)}
_ "" <- (Space / Cmnt)*
Space <- r:. &{isSpace(r)}
Cmnt <- "//" (!"\n" .)* / "/*" (!"*/" .)* "*/"
Eof <- _ !.
