{
package parser

import "github.com/eaburns/peggy/peg"
}

File <- Import* (ConstDef / VarDef / TypeDef / FuncDef / TestDef)* Eof
Import <- _ ("import" / "Import") Id? StrLit
ConstDef <- _ ("const" / "Const") Id Type? _ "=" Expr
VarDef <- _ ("var" / "Var") Id Type? _ "=" Expr
TypeDef <- _ ("type" / "Type") TypeVars? Id Type
TypeVars <- TypeVar / _ "(" TypeVar (_ "," TypeVar)* (_ ",")?
Type <-
	_"(" Type _ ")" /
	RefType /
	NamedType /
	TypeVar /
	StructType /
	UnionType /
	FuncType
Types <- Type (_ "," Type)* (_ ",")?
RefType <- _ "&" Type
NamedType <- (_ "(" Types _ ")")? ((Id _ ".")? Id)+
StructType <- "[" Fields? _ "]"
Field <- Id _ ":" Type
Fields <- Field (_ "," Field)* (_ ",")?
UnionType <- _ "[" Cases _ "]"
Case <- Id / Id _ ":" Type
Cases <- (_ "|")? Case (_ "|" Case)*
FuncType <- _ "{" _ "}" / _ "{" Types? _ "|" Types? _"}"
FuncDef <- _ ("func" / "Func") FuncName _ "(" FuncParms? _ ")" Type? (_ ":" FuncDecls) _ "{" Exprs? _ "}"
FuncName <- Op / Id / Kwds
FuncParms <- FuncParm (_ "," FuncParm)* (_ ",") ?
FuncParm <- Id Type (_ "=" Expr)?
FuncDecl <- FuncName _ "(" Types _ ")" Type?
FuncDecls <- FuncDecl (_ "," FuncDecl)* (_ ",")?
TestDef <- _ "test" Id _ "{" Exprs? _ "}"
Exprs <- Expr (_ "," Expr)* (_ ",")?
Expr <- Kw / KwArg
Kw <- (Kwd KwArg)+
KwArg <- Bin2 / Bin2Arg
Bin2 <- Bin2Arg (Op2 Bin2Arg)+
Bin2Arg <- Bin1 / Bin1Arg
Bin1 <- Bin1Arg (Op1 Bin1Arg)+
Bin1Arg <- Lab / UnArg
Lab <- UnArg _ ":" Type
Un <- Op UnArg
UnArg <- Un / Sel / Call / Pri
Call <- Pri (_ "(" Exprs _ ")")+
Sel <- Pri (_ "." Id)+
Pri <-
	_ "(" Expr _ ")" /
	(Id _ ".")? Op /
	(Id _ ".")? Kwds /
	CompLit /
	BlkLit /
	CharLit /
	StrLit /
	NumLit /
	Id
CompLit <- _ "[" FieldVals? _ "]" / _ "[" Exprs _ "]"
FieldVal <- Id _ ":" Expr
FieldVals <- FieldVal (_ "," FieldVal)* (_ ",")?
BlkLit <- _ "{" (FuncParms _ "|")? Exprs? _ "}"
CharLit "character" <- _ ['] !"\n" Esc / "\\'" / [^'] [']
StrLit "string" <- InterpStr / RawStr
InterpStr <- _ ["] (!"\n" (Esc / "\\\"" / [^"]))* ["]
RawStr <- _ [`] ("\\`" / [^`])* [`]
Esc <-
	"\\n" /
	"\\t" /
	"\\b" /
	"\\\\" /
	"\\" x0:( X X ) /
	"\\x" x1:( X X X X ) /
	"\\X" x2:( X X X X X X X X )
NumLit "number" <- DecLit / HexLit / FloatLit
DecLit <- _ D+
HexLit <- _ ("0x" / "0X") X+
FloatLit <- _ D+ "." D+ ([eE] [+\-]? D+)?
X <- [a-fA-F0-9]
Op "operator" <- _ [\-+*/%~!?^<>=&|@$]+
Op2 "operator2" <- _ [\-+~!?^<>=&|@$] [\-+*/%~!?^<>=&|@$]*
Op1 "operator1" <- _ [*/%] [\-+*/%~!?^<>=&|@$]*
Kwds "keywords" <- _ Kwd+
Kwd "keyword" <- _ Id ":"
TypeVar "type variable" <- _ !Id [A-Z]
Id "identifier" <- _ !Reserved (![A-Z] L ("_" / L / D)*) / ([A-Z] ("_" / L / D)+)
L <- r:. &{isLetter(r)}
D <- [0-9]
Reserved <- "import" / "Import" / "const" / "Const" / "var" / "Var" / "type" / "Type" / "func" / "Func" / "test"
_ "" <- (Space / Cmnt)*
Space <- r:. &{isSpace(r)}
Cmnt <- "//" (!"\n" .)* / "/*" (!"*/" .)* "*/"
Eof <- !.