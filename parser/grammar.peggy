{
package parser

import "github.com/eaburns/peggy/peg"
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

ConstDef <- _ reserved:("const" / "Const") name:Id t:Type? (_ ":=" expr:Expr)? {
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

VarDef <- _ reserved:("var" / "Var") name:Id t:Type? (_ ":=" expr:Expr)? {
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

TypeDef <- _ reserved:("type" / "Type") ps:TypeVars? name:Id alias:(_ ":=")? t:Type? {
	var parms []TypeVar; if ps != nil { parms = *ps }
	var typ Type; if t != nil { typ = *t }
	return Def(&TypeDef{
		Exp: reserved == "Type",
		Alias: alias != "",
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
	NamedType /
	tvar:TypeVar { return Type(tvar) } /
	TypeLit

Types <- t0:Type ts:(_ "," t1:Type { return Type(t1) })* (_ ",")? {
	return []Type(append([]Type{t0}, ts...))
}

RefType <- _ "&" typ:Type {
	return Type(&RefType{Type: typ, L: l(parser, start, end)})
}

NamedType <- (args:TypeArgs)? names:ModName+ {
	return Type(namedType(args, names, l(parser, start, end)))
}

ModName <- mod:(m:Id _ "#" { return Id(m) })? name:Id {
	return &NamedType{
		Mod: mod,
		Name: name,
		L: l(parser, start, end),
	}
}

TypeArgs <-
	t:(tv:TypeVar { return Type(tv) } / TypeLit) { return []Type{t} } /
	_ "(" arg:Type _ ")" { return []Type{arg} } /
	_ "(" args: Types _ ")" { return []Type(args) }

TypeLit <- 
	RefType /
	FuncType /
	ArrayType /
	StructType /
	UnionType

ArrayType <- _ "[" elem:Type _ "]" {
	return Type(&ArrayType{ElemType: elem, L: l(parser, start, end)})
}

StructType <- _ "[" fs:Fields? _ "]" {
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

FuncType <- _ "(" ps:Types? _ ")" _ "{" r:Type? _ "}" {
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

FuncParms <- p0:FuncParm ps:(_ "," p1:FuncParm { return FuncParm(p1) })* (_ ",")? {
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

FuncDecl <- name:FuncName _ "(" ps:Types? _ ")" r:Type? {
	var parms []Type; if ps != nil { parms = *ps }
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

Expr <- Asgn / AsgnArg

Asgn <- Bin<AsgnOp, AsgnArg>
AsgnArg <- KwCall / KwArg
AsgnOp "assignment operator" <- _ name:":=" {
	return Id{Name: name, L: l(parser, start, end)}
}

KwCall <- kws:Kw+ {
	return Expr(kwCall(l(parser, start, end), kws))
}

Kw <- name:Kwd arg:KwArg {
	return kw{name: name, arg: arg}
}

Kwd "keyword" <- _ id:Id ":" {
	return Id{Name: id.Name + ":", L: l(parser, start, end)}
}

Kwds "keywords" <- _ keywords:Kwd+ {
	var name string
	for _, k := range keywords {
		name += k.Name
	}
	return Id{Name: name, L: l(parser, start, end)}
}

KwArg <- Bin5 / Bin5Arg

Bin5 <- Bin<Bin5Op, Bin5Arg>
Bin5Arg <- Bin4 / Bin4Arg
Bin5Op "operator" <- _ name:("|" O*) {
	return Id{Name: name, L: l(parser, start, end)}
}

Bin4 <- Bin<Bin4Op, Bin4Arg>
Bin4Arg <- Bin3 / Bin3Arg
Bin4Op "operator" <- _ name:("&" O*) {
	return Id{Name: name, L: l(parser, start, end)}
}

Bin3 <- Bin<Bin3Op, Bin3Arg>
Bin3Arg <- Bin2 / Bin2Arg
Bin3Op "operator 3" <- _ name:([=!<>] O*) {
	return Id{Name: name, L: l(parser, start, end)}
}

Bin2 <- Bin<Bin2Op, Bin2Arg>
Bin2Arg <- Bin1 / Bin1Arg
Bin2Op "operator" <- _ name:([+\-^] O*) {
	return Id{Name: name, L: l(parser, start, end)}
}

# TODO: what precedence are [~?@$]?
Bin1 <- Bin<Bin1Op, Bin1Arg>
Bin1Arg <- Cvt / UnArg
Bin1Op "operator" <- _ name:([*/%] O*) {
	return Id{Name: name, L: l(parser, start, end)}
}

Bin<O, A> <- arg0:A calls:BinTail<O, A>+ {
	return Expr(bins(arg0, calls))
}

BinTail<O, A> <- name:O arg1:A {
	return &Call{
		Fun: name,
		Args: []Expr{nil, arg1},
		L: l(parser, start, end),
	}
}

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

Op "operator" <- _ name:[\-+*/%~!?^<>=&|@$]+ {
	return Id{Name: name, L: l(parser, start, end)}
}

Idx <- expr:IdxArg indexes:Idxs+ {
	return Expr(bins(expr, indexes))
}

Idxs <- _ "[" indices:Exprs _ "]" {
	return &Call{
		Fun: Id{Name: "[]", L: l(parser, start, end)},
		Args: append([]Expr{nil}, indices...),
		L: l(parser, start, end),
	}
}

IdxArg <- Call / Sel / Pri

Call <- fun:CallArg argss:ArgList+ {
	return Expr(calls(fun, argss))
}

ArgList <- _ "(" as:Exprs? _ ")" {
	return &Call{Args: args(as), L: l(parser, start, end)}
}

CallArg <- Sel / Pri

Sel <- expr:Pri ids:(id:DotId { return Id(id) })+ {
	return Expr(sel(expr, ids))
}

Pri <-
	BlkLit /
	SubExpr /
	ModSel /
	CompLit /
	CharLit /
	StrLit /
	NumLit /
	id:Id { return Expr(id) }

SubExpr <-
	_ "(" expr:Expr _ ")" {
		return Expr(&SubExpr{Expr: expr, L: l(parser, start, end)})
	} /
	_ "(" id:(DotId / IdxOp / Op / Kwds) _ ")" {
		return Expr(&SubExpr{Expr: id, L: l(parser, start, end)})
	}

ModSel <-
	mod0:Id _ "#" name0:FuncName {
		return Expr(&ModSel{
			Mod: mod0,
			Name: name0,
			L: l(parser, start, end),
		})
	} /
	mod1:Id _ "#" _ "(" name1:FuncName _")" {
		return Expr(&ModSel{
			Mod: mod1,
			Name: name1,
			L: l(parser, start, end),
		})
	}

FuncName <- DotId / IdxOp / Op / Kwds / Id

IdxOp "index operator" <- _ "[" _ "]" {
	return Id{Name: "[]", L: l(parser, start, end)}
}

DotId <- _ "." id:Id {
	return Id{Name: "." + id.Name, L: l(parser, start, end)}
}

CompLit <- _ "[" exprs:Exprs _ "]" {
	return Expr(&CompLit{Exprs: exprs, L: l(parser, start, end)})
}

BlkLit <- _ "(" ps:BlkParms? _ ")" _ "{" es:Exprs? _ "}" {
	var parms []FuncParm; if ps != nil { parms = *ps }
	var exprs []Expr; if es != nil { exprs = *es }
	return Expr(&BlkLit{
		Parms: parms,
		Exprs: exprs,
		L: l(parser, start, end),
	})
}

BlkParm <- id:Id typ:Type {
	return FuncParm{Name: id, Type: typ, L: l(parser, start, end)}
}

BlkParms <- p0:BlkParm ps:(_ "," p1:BlkParm { return FuncParm(p1) })* (_ ",")? {
	return []FuncParm(append([]FuncParm{p0}, ps...))
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

NumLit "number" <- FloatLit / HexLit / DecLit

DecLit <- _ text:(D+) {
	return Expr(&IntLit{Text: text, L: l(parser, start, end)})
}

HexLit <- _ text:(("0x" / "0X") X+) {
	return Expr(&IntLit{Text: text, L: l(parser, start, end)})
}

FloatLit <- _ text:(D+ "." D+ ([eE] [+\-]? D+)?) {
	return Expr(&FloatLit{Text: text, L: l(parser, start, end)})
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
O <- [*/%+\-^=!<>&|~?@$]
_ "" <- (Space / Cmnt)*
Space <- r:. &{isSpace(r)}
Cmnt <- "//" (!"\n" .)* / "/*" (!"*/" .)* "*/"
Eof <- _ !.
