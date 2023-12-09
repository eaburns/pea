{
package parser

import "github.com/eaburns/peggy/peg"
}

ImportsOnly <- imports:Import* !Import .* _ EOF {
	return []*Import(imports)
}

File <- imports:Import* defs:Def* _ EOF {
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

Def <- VarDef / TypeDef / IfaceDef / FuncDef / TestDef

VarDef <- _ reserved:("var" / "Var" / "const" / "Const") name:Id v:VarBody {
	v.Exp = reserved == "Var" || reserved == "Const"
	v.Const = reserved == "const" || reserved == "Const"
	v.Name = name
	v.L = l(parser, start, end)
	return Def(&v)
}

VarBody <-
	typ0:Type {
		return VarDef{Type: typ0}
	} /
	_ ":=" typ1:Type _ "::" expr:Expr {
		return VarDef{Type: typ1, Expr: expr}
	}

TypeDef <- _ reserved:("type" / "Type") ps:TypeVars? name:Id def:DefType {
	def.Exp = reserved == "Type"
	if ps != nil { def.TypeParms = *ps }
	def.Name = name
	def.L = l(parser, start, end)
	return Def(def)
}

DefType <-
	t:Type _ {
		return &TypeDef{Type: t}
	} /
	_ ":=" t:Type _ {
		return &TypeDef{Alias: true, Type: t}
	} /
	_ "(" t:Type _ ")" {
		return &TypeDef{Opaque: true, Type: t}
	}

TypeVars <-
	tv:TypeVar { return []TypeVar{tv} } /
	_ "(" t0:TypeVar ts:(_ "," t1:TypeVar { return TypeVar(t1) })* (_ ",")? _ ")" {
		return []TypeVar(append([]TypeVar{t0}, ts...))
	}

Type <- RefType / NamedType / LiteralType

Types <- t0:Type ts:(_ "," t1:Type { return Type(t1) })* (_ ",")? {
	return []Type(append([]Type{t0}, ts...))
}

RefType <- _ "&" typ:(NamedType / LiteralType) {
	return Type(&RefType{Type: typ, L: l(parser, start, end)})
}

NamedType <-
	(args:TypeArgs)? names:ModTypeName+ {
		return Type(namedType(args, names, l(parser, start, end)))
	} /
	tvar:TypeVar {
		return Type(tvar)
	}

ModTypeName <- mod:ModTag? name:TypeNameId  {
	return &NamedType{Mod: mod, Name: name, L: l(parser, start, end)}
}

TypeNameId <-
	Id /
	_ "!" { return Ident{Name: "!", L: l(parser, start, end)} }

TypeArgs <-
	t:(tv:TypeVar { return Type(tv) } / LiteralType) { return []Type{t} } /
	_ "(" arg:Type _ ")" { return []Type{arg} } /
	_ "(" args: Types _ ")" { return []Type(args) }

LiteralType <-
	FuncType /
	ArrayType /
	StructType /
	UnionType

ArrayType <- _ "[" elem:Type _ "]" {
	return Type(&ArrayType{ElemType: elem, L: l(parser, start, end)})
}

StructType <-
	_ "[" _ "." _ "]" {
		return Type(&StructType{L: l(parser, start, end)})
	} /
	_ "[" fields:FieldDefs _ "]" {
		return Type(&StructType{Fields: fields, L: l(parser, start, end)})
	}

FieldDef <- name:FieldId typ:Type {
	return FieldDef{Name: name, Type: typ, L: l(parser, start, end)}
}

FieldDefs <- f0:FieldDef fs:(_ "," f1:FieldDef { return FieldDef(f1) })* (_ ",")? {
	return []FieldDef(append([]FieldDef{f0}, fs...))
}

UnionType <- _ "[" cases:CaseDefs _ "]" {
	return Type(&UnionType{Cases: cases, L: l(parser, start, end)})
}

CaseDef <- name:CaseId t:Type? {
	var typ Type; if t != nil { typ = *t }
	return CaseDef{Name: name, Type: typ, L: l(parser, start, end)}
}

CaseDefs <- c0:CaseDef cs:(_ "," c1:CaseDef { return CaseDef(c1) })* (_ ",")? {
	return []CaseDef(append([]CaseDef{c0}, cs...))
}

FuncType <- _ "(" ps:Types? _ ")" _ "{" r:Type? _ "}" {
	var parms []Type; if ps != nil { parms = *ps }
	var ret Type; if r != nil { ret = *r }
	return Type(&FuncType{Parms: parms, Ret: ret, L: l(parser, start, end)})
}

IfaceDef <- _ reserved:("iface" / "Iface") ps:TypeVars? name:Id _ def:DefIface {
	def.Exp = reserved == "Iface"
	if ps != nil { def.TypeParms = *ps }
	def.Name = name
	def.L = l(parser, start, end)
	return Def(&def)
}

DefIface <-
	_ "{" _"}" {
		return IfaceDef{}
	} /
	_ "{" iface:Iface _"}" {
		return IfaceDef{Iface: iface}
	} /
	_ ":=" iface:IfaceName _ {
		return IfaceDef{Alias: iface}
	} /
	_ "(" iface:DefIface _ ")" {
		iface.Opaque = true
		return IfaceDef(iface)
	}

Iface <- e0:IfaceElem es:(_ "," e1:IfaceElem { return interface{}(e1) })* (_ ",")? {
	return []interface{}(append([]interface{}{e0}, es...))
}

IfaceElem <-
	decl:FuncDecl { return interface{}(&decl) } /
	name:IfaceName { return interface{}(name) }

IfaceName <- (args:TypeArgs)? names:ModTypeName+ {
	return namedType(args, names, l(parser, start, end)).(*NamedType)
}

FuncDef <- _ reserved:("func" / "Func") name:FuncName _ "(" ps:FuncParms? _ ")" r:Type? (_ ":" c:Constraint)? es:FuncBody? {
	var parms []FuncParm; if ps != nil { parms = *ps }
	var ret Type; if r != nil { ret = *r }
	var exprs []Expr; if es != nil { exprs = *es }
	return Def(&FuncDef{
		Exp: reserved == "Func",
		Name: name,
		Parms: parms,
		Ret: ret,
		Constraints: c,
		Exprs: exprs,
		L: l(parser, start, end),
	})
}

FuncBody <- _ "{" exprs:Exprs? _ "}" {
	if exprs == nil { return []Expr{} }
	return []Expr(*exprs)
}

FuncParms <-
	p:(FuncParmNameType / FuncParmName) _ "," ps:FuncParms {
		if p.Type == nil {
			p.Type = ps[0].Type
		}
		return []FuncParm(append([]FuncParm{p}, ps...))
	} /
	p:FuncParmNameType (_ ",")? {
		return []FuncParm{p}
	}

FuncParmName <- name:Id {
	return FuncParm{Name: name, L: l(parser, start, end)}
}

FuncParmNameType <- name:Id typ:Type (_ ":" c:Constraint)? {
	return FuncParm{
		Name: name,
		Type: typ,
		Constraints: c,
		L: l(parser, start, end),
	}
}

Constraint <-
	iface:IfaceElem {
		return []interface{}{iface}
	} /
	_ "{" iface:Iface _ "}" {
		return []interface{}(iface)
	}

FuncDecl <- name:FuncName _ "(" ps:Types? _ ")" r:Type? {
	var parms []Type; if ps != nil { parms = *ps }
	var ret Type; if r != nil { ret = *r }
	return FuncDecl{Name: name, Parms: parms, Ret: ret, L: l(parser, start, end)}
}

FuncDecls <- fd0:FuncDecl fds:(_ "," fd1:FuncDecl { return FuncDecl(fd1) })* (_ ",")? {
	return []FuncDecl(append([]FuncDecl{fd0}, fds...))
}

TestDef <- _ "test" name:Id _ "{" es:Exprs? _ "}" {
	var exprs []Expr; if es != nil { exprs = *es }
	return Def(&TestDef{Name: name, Exprs: exprs, L: l(parser, start, end)})
}

Expr <- Asgn / AsgnArg

Exprs <- e0:Expr es:(_ "," e1:Expr { return Expr(e1) })* (_ ",")? {
	return []Expr(append([]Expr{e0}, es...))
}

Asgn <- Bin<AsgnOp, AsgnArg>
AsgnArg <- Cvt / KwCall / KwArg
AsgnOp "assignment operator" <- _ name:":=" {
	return Ident{Name: name, L: l(parser, start, end)}
}

KwCall <- arg0:KwArg mod:ModTag? args:NameArg<Kwd, KwArg>+ {
	call := nary(l(parser, start, end), mod, args)
	call.Args = append([]Expr{arg0}, call.Args...)
	return Expr(call)
}
KwArg <- KwCall1 / KwArg1

KwCall1 <- mod:ModTag? args:NameArg<Kwd, KwArg1>+ {
	return Expr(nary(l(parser, start, end), mod, args))
}
KwArg1 <- Cvt / CvtArg

Cvt <- typ:Type _ "::" expr:CvtArg {
	return Expr(&Convert{Expr: expr, Type: typ, L: l(parser, start, end)})
}
CvtArg <- Switch / SwitchArg

Switch <- arg0:SwitchArg mod:ModTag? args:NameArg<CaseId, SwitchArg>+ {
	call := nary(l(parser, start, end), mod, args)
	call.Args = append([]Expr{arg0}, call.Args...)
	return Expr(call)
}
SwitchArg <- Bin5 / Bin5Arg

NameArg<Name, Arg> <- name:Name arg:Arg {
	return nameArg{name: name, arg: arg}
}

Bin5 <- Bin<Bin5Op, Bin5Arg>
Bin5Arg <- Bin4 / Bin4Arg
Bin5Op "operator" <- _ name:("|" O*) {
	return Ident{Name: name, L: l(parser, start, end)}
}

Bin4 <- Bin<Bin4Op, Bin4Arg>
Bin4Arg <- Bin3 / Bin3Arg
Bin4Op "operator" <- _ name:("&" O*) {
	return Ident{Name: name, L: l(parser, start, end)}
}

Bin3 <- Bin<Bin3Op, Bin3Arg>
Bin3Arg <- Bin2 / Bin2Arg
Bin3Op "operator 3" <- _ name:([=!<>] O*) {
	return Ident{Name: name, L: l(parser, start, end)}
}

Bin2 <- Bin<Bin2Op, Bin2Arg>
Bin2Arg <- Bin1 / Bin1Arg
Bin2Op "operator" <- _ name:([+\-^] O*) {
	return Ident{Name: name, L: l(parser, start, end)}
}

# TODO: what precedence are [~?@$]?
Bin1 <- Bin<Bin1Op, Bin1Arg>
Bin1Arg <- UnArg
Bin1Op "operator" <- _ name:([*/%] O*) {
	return Ident{Name: name, L: l(parser, start, end)}
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

# Signed numeric literals are a primary, not a Un.
Un <- !NumLit name:Op arg:UnArg {
	return Expr(&Call{Fun: name, Args: []Expr{arg}, L: l(parser, start, end)})
}

Op "operator" <- _ name:O+ {
	return Ident{Name: name, L: l(parser, start, end)}
}

UnArg <- Un / Pri / PriArg

Pri <- head:PriArg tail:(Sel / Call / Idx)+ {
	return Expr(primaries(head, tail))
}

Sel <- mod:ModTag? name:DotName {
	return primary(sel{mod:mod, name: name, l: l(parser, start, end)})
}

DotName <- _ "." name:Id {
	name.Name = "." + name.Name
	name.L = l(parser, start, end)
	return Ident(name)
}

Call <- _ "(" es:Exprs? _ ")" {
	var exprs []Expr; if es != nil { exprs = *es }
	return primary(call{args: exprs, l: l(parser, start, end)})
}

Idx <- _ "[" exprs:Exprs _ "]" {
	return primary(idx{args: exprs, l: l(parser, start, end)})
}

PriArg <-
	BlkLit /
	ArrayLit /
	StructLit /
	UnionLit /
	CharLit /
	StrLit /
	NumLit /
	SubExpr /
	ModSel /
	id:Id { return Expr(id) }

SubExpr <-
	_ "(" id:FuncName _ ")" {
		return Expr(&SubExpr{Expr: id, L: l(parser, start, end)})
	} /
	_ "(" expr:Expr _ ")" {
		return Expr(&SubExpr{Expr: expr, L: l(parser, start, end)})
	}

ModSel <-
	mod0:ModTag name0:FuncName {
		return Expr(&ModSel{
			Mod: mod0,
			Name: name0,
			L: l(parser, start, end),
		})
	} /
	mod1:ModTag _ "(" name1:FuncName _")" {
		return Expr(&ModSel{
			Mod: mod1,
			Name: name1,
			L: l(parser, start, end),
		})
	}

ModTag <- ids:ModId+ {
	return Ident(modTag(ids))
}

FuncName <- FieldId / ConvertOp / IdxOp / Op / Kwds / Cases / Id

ConvertOp "conversion operator" <- _ "::" {
	return Ident{Name: "::", L: l(parser, start, end)}
}

IdxOp "index operator" <- _ "[" _ "]" {
	return Ident{Name: "[]", L: l(parser, start, end)}
}

Cases "cases" <- cases:CaseId+ {
	return Ident{Name: catNames(cases), L: l(parser, start, end)}
}

Kwds "keywords" <- keywords:Kwd+ {
	return Ident{Name: catNames(keywords), L: l(parser, start, end)}
}

ArrayLit <-
	_ "[" _ "]" {
		return Expr(&ArrayLit{L: l(parser, start, end)})
	} /
	_ "[" exprs:Exprs _ "]" {
		return Expr(&ArrayLit{Exprs: exprs, L: l(parser, start, end)})
	}

StructLit <-
	_ "[" _ "." _ "]" {
		return Expr(&StructLit{L: l(parser, start, end)})
	} /
	_ "[" fields:FieldVals _ "]" {
		return Expr(&StructLit{FieldVals: fields, L: l(parser, start, end)})
	}

FieldVals <- f0:FieldVal fs:(_ "," f1:FieldVal { return FieldVal(f1) })* (_ ",")? {
	return []FieldVal(append([]FieldVal{f0}, fs...))
}

FieldVal <- name:FieldId val:Expr {
	return FieldVal{Name: name, Val: val, L: l(parser, start, end)}
}

FieldId "field id" <- _ "." id:Id {
	return Ident{Name: "." + id.Name, L: l(parser, start, end)}
}

UnionLit <- _ "[" cas:CaseVal _ "]" {
		return Expr(&UnionLit{CaseVal: cas, L: l(parser, start, end)})
	}

CaseVal <- name:CaseId v:Expr? {
	var val Expr; if v != nil { val = *v }
	return CaseVal{Name: name, Val: val, L: l(parser, start, end)}
}

BlkLit <- (_ "(" ps:(FuncParms / FuncParmNames)? _ ")")? _ "{" es:Exprs? _ "}" {
	var parms []FuncParm; if ps != nil { parms = *ps }
	var exprs []Expr; if es != nil { exprs = *es }
	return Expr(&BlockLit{Parms: parms, Exprs: exprs, L: l(parser, start, end)})
}

FuncParmNames <-
	p:FuncParmName _ "," ps:FuncParmNames {
		return []FuncParm(append([]FuncParm{p}, ps...))
	} /
	p:FuncParmName {
		return []FuncParm{p}
	}

CharLit "character" <- _ data:(['] !"\n" (Esc / "\\'" / [^']) [']) {
	return Expr(&CharLit{
		Source: source(parser, start, end),
		Rune: interpRune(data),
		L: l(parser, start, end),
	})
}

StrLit "string" <- InterpStr / RawStr

InterpStr <- _ ["] data:(!"\n" (Esc / "\\\"" { return "\"" }/ [^"]))* ["] {
	return Expr(&StrLit{
		Source: source(parser, start, end),
		Raw: false,
		Data: data,
		L: l(parser, start, end),
	})
}

Esc <-
	"\\n" { return "\n" } /
	"\\t" { return "\t" } /
	"\\b" { return "\b"} /
	"\\\\" { return "\\\\" } /
	"\\x" x0:(X X) { return string(interpHex(x0)) } /
	"\\u" x1:(X X X X) { return string(interpHex(x1)) } /
	"\\U" x2:(X X X X X X X X) { return string(interpHex(x2)) }

RawStr <- _ [`] data:("\\`" / [^`])* [`] {
	return Expr(&StrLit{
		Source: source(parser, start, end),
		Raw: true,
		Data: data,
		L: l(parser, start, end),
	})
}

NumLit "number" <- FloatLit / HexLit / DecLit

DecLit <- _ text:(("+" / "-")? D+) {
	return Expr(&IntLit{Text: text, L: l(parser, start, end)})
}

HexLit <- _ text:(("0x" / "0X") X+) {
	return Expr(&IntLit{Text: text, L: l(parser, start, end)})
}

FloatLit <- _ text:(("+" / "-")? D+ "." D+ ([eE] [+\-]? D+)?) {
	return Expr(&FloatLit{Text: text, L: l(parser, start, end)})
}

TypeVar "type variable" <- _ name:([A-Z] D*) !L {
	return TypeVar{Name: name, L: l(parser, start, end)}
}

Kwd "keyword" <- _ name:NAME ":" !":" {
	return Ident{Name: name+":", L: l(parser, start, end)}
}

CaseId "case id" <- _ name:NAME "?" {
	return Ident{Name: name+"?", L: l(parser, start, end)}
}

ModId "mod id" <- _ name:NAME "#" {
	return Ident{Name: name, L: l(parser, start, end)}
}

Id "identifier" <- _ !Kwd !CaseId !ModId name:NAME {
	return Ident{Name: name, L: l(parser, start, end)}
}

NAME <- !RESERVED !TypeVar ("_" / L) ("_" / L / D)*
RESERVED <- ("import" / "Import" / "const" / "Const" / "var" / "Var" / "type" / "Type" / "iface" / "Iface" / "func" / "Func" / "test") !("_" / L / D)
D <- [0-9]
X <- [a-fA-F0-9]
L <- r:. &{isLetter(r)}
O <- [*/%+\-^=!<>&|~@$]
_ "" <- (SPACE / LINE_COMMENT / BLOCK_COMMENT)*
SPACE <- r:. &{isSpace(r)}

LINE_COMMENT <- text:("//" (!"\n" .)*)  {
	return string(comment(parser, text, start, end))
}

BLOCK_COMMENT <- text:("/*" (NEST_BLOCK_COMMENT / (!"*/" .))* "*/") {
	return string(comment(parser, text, start, end))
}

NEST_BLOCK_COMMENT <- "/*" (NEST_BLOCK_COMMENT / (!"*/" .))* "*/"

EOF <- !.
