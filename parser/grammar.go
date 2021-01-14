package parser

import "github.com/eaburns/peggy/peg"

const (
	_File                       int = 0
	_Import                     int = 1
	_Def                        int = 2
	_ConstDef                   int = 3
	_VarDef                     int = 4
	_TypeDef                    int = 5
	_TypeVars                   int = 6
	_Type                       int = 7
	_Types                      int = 8
	_RefType                    int = 9
	_NamedType                  int = 10
	_ModName                    int = 11
	_TypeArgs                   int = 12
	_TypeLit                    int = 13
	_ArrayType                  int = 14
	_StructType                 int = 15
	_FieldDef                   int = 16
	_FieldDefs                  int = 17
	_UnionType                  int = 18
	_CaseDef                    int = 19
	_CaseDefs                   int = 20
	_FuncType                   int = 21
	_FuncDef                    int = 22
	_FuncParms                  int = 23
	_FuncParm                   int = 24
	_FuncDecl                   int = 25
	_FuncDecls                  int = 26
	_TestDef                    int = 27
	_Expr                       int = 28
	_Exprs                      int = 29
	_Asgn                       int = 30
	_AsgnArg                    int = 31
	_AsgnOp                     int = 32
	_KwCall                     int = 33
	_KwArg                      int = 34
	_Switch                     int = 35
	_SwitchArg                  int = 36
	_Bin5                       int = 37
	_Bin5Arg                    int = 38
	_Bin5Op                     int = 39
	_Bin4                       int = 40
	_Bin4Arg                    int = 41
	_Bin4Op                     int = 42
	_Bin3                       int = 43
	_Bin3Arg                    int = 44
	_Bin3Op                     int = 45
	_Bin2                       int = 46
	_Bin2Arg                    int = 47
	_Bin2Op                     int = 48
	_Bin1                       int = 49
	_Bin1Arg                    int = 50
	_Bin1Op                     int = 51
	_Cvt                        int = 52
	_Un                         int = 53
	_Op                         int = 54
	_UnArg                      int = 55
	_Pri                        int = 56
	_Sel                        int = 57
	_Call                       int = 58
	_Idx                        int = 59
	_PriArg                     int = 60
	_SubExpr                    int = 61
	_ModSel                     int = 62
	_FuncName                   int = 63
	_IdxOp                      int = 64
	_Cases                      int = 65
	_Kwds                       int = 66
	_Kwd                        int = 67
	_ArrayLit                   int = 68
	_StructLit                  int = 69
	_FieldVals                  int = 70
	_FieldVal                   int = 71
	_FieldId                    int = 72
	_UnionLit                   int = 73
	_CaseVal                    int = 74
	_CaseId                     int = 75
	_BlkLit                     int = 76
	_BlkParm                    int = 77
	_BlkParms                   int = 78
	_CharLit                    int = 79
	_StrLit                     int = 80
	_InterpStr                  int = 81
	_Esc                        int = 82
	_RawStr                     int = 83
	_NumLit                     int = 84
	_DecLit                     int = 85
	_HexLit                     int = 86
	_FloatLit                   int = 87
	_Id                         int = 88
	_TypeVar                    int = 89
	_Reserved                   int = 90
	_D                          int = 91
	_X                          int = 92
	_L                          int = 93
	_O                          int = 94
	__                          int = 95
	_Space                      int = 96
	_Cmnt                       int = 97
	_Eof                        int = 98
	_Bin__AsgnOp__AsgnArg       int = 99
	_NameArg__Kwd__KwArg        int = 100
	_NameArg__CaseId__SwitchArg int = 101
	_Bin__Bin5Op__Bin5Arg       int = 102
	_Bin__Bin4Op__Bin4Arg       int = 103
	_Bin__Bin3Op__Bin3Arg       int = 104
	_Bin__Bin2Op__Bin2Arg       int = 105
	_Bin__Bin1Op__Bin1Arg       int = 106
	_BinTail__AsgnOp__AsgnArg   int = 107
	_BinTail__Bin5Op__Bin5Arg   int = 108
	_BinTail__Bin4Op__Bin4Arg   int = 109
	_BinTail__Bin3Op__Bin3Arg   int = 110
	_BinTail__Bin2Op__Bin2Arg   int = 111
	_BinTail__Bin1Op__Bin1Arg   int = 112

	_N int = 113
)

type _Parser struct {
	text     string
	deltaPos [][_N]int32
	deltaErr [][_N]int32
	node     map[_key]*peg.Node
	fail     map[_key]*peg.Fail
	act      map[_key]interface{}
	lastFail int
	data     interface{}
}

type _key struct {
	start int
	rule  int
}

func _NewParser(text string) *_Parser {
	return &_Parser{
		text:     text,
		deltaPos: make([][_N]int32, len(text)+1),
		deltaErr: make([][_N]int32, len(text)+1),
		node:     make(map[_key]*peg.Node),
		fail:     make(map[_key]*peg.Fail),
		act:      make(map[_key]interface{}),
	}
}

func _max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func _memoize(parser *_Parser, rule, start, pos, perr int) (int, int) {
	parser.lastFail = perr
	derr := perr - start
	parser.deltaErr[start][rule] = int32(derr + 1)
	if pos >= 0 {
		dpos := pos - start
		parser.deltaPos[start][rule] = int32(dpos + 1)
		return dpos, derr
	}
	parser.deltaPos[start][rule] = -1
	return -1, derr
}

func _memo(parser *_Parser, rule, start int) (int, int, bool) {
	dp := parser.deltaPos[start][rule]
	if dp == 0 {
		return 0, 0, false
	}
	if dp > 0 {
		dp--
	}
	de := parser.deltaErr[start][rule] - 1
	return int(dp), int(de), true
}

func _failMemo(parser *_Parser, rule, start, errPos int) (int, *peg.Fail) {
	if start > parser.lastFail {
		return -1, &peg.Fail{}
	}
	dp := parser.deltaPos[start][rule]
	de := parser.deltaErr[start][rule]
	if start+int(de-1) < errPos {
		if dp > 0 {
			return start + int(dp-1), &peg.Fail{}
		}
		return -1, &peg.Fail{}
	}
	f := parser.fail[_key{start: start, rule: rule}]
	if dp < 0 && f != nil {
		return -1, f
	}
	if dp > 0 && f != nil {
		return start + int(dp-1), f
	}
	return start, nil
}

func _accept(parser *_Parser, f func(*_Parser, int) (int, int), pos, perr *int) bool {
	dp, de := f(parser, *pos)
	*perr = _max(*perr, *pos+de)
	if dp < 0 {
		return false
	}
	*pos += dp
	return true
}

func _node(parser *_Parser, f func(*_Parser, int) (int, *peg.Node), node *peg.Node, pos *int) bool {
	p, kid := f(parser, *pos)
	if kid == nil {
		return false
	}
	node.Kids = append(node.Kids, kid)
	*pos = p
	return true
}

func _fail(parser *_Parser, f func(*_Parser, int, int) (int, *peg.Fail), errPos int, node *peg.Fail, pos *int) bool {
	p, kid := f(parser, *pos, errPos)
	if kid.Want != "" || len(kid.Kids) > 0 {
		node.Kids = append(node.Kids, kid)
	}
	if p < 0 {
		return false
	}
	*pos = p
	return true
}

func _next(parser *_Parser, pos int) (rune, int) {
	r, w := peg.DecodeRuneInString(parser.text[pos:])
	return r, w
}

func _sub(parser *_Parser, start, end int, kids []*peg.Node) *peg.Node {
	node := &peg.Node{
		Text: parser.text[start:end],
		Kids: make([]*peg.Node, len(kids)),
	}
	copy(node.Kids, kids)
	return node
}

func _leaf(parser *_Parser, start, end int) *peg.Node {
	return &peg.Node{Text: parser.text[start:end]}
}

// A no-op function to mark a variable as used.
func use(interface{}) {}

func _FileAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _File, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// imports:Import* defs:Def* Eof
	// imports:Import*
	{
		pos1 := pos
		// Import*
		for {
			pos3 := pos
			// Import
			if !_accept(parser, _ImportAccepts, &pos, &perr) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	// defs:Def*
	{
		pos6 := pos
		// Def*
		for {
			pos8 := pos
			// Def
			if !_accept(parser, _DefAccepts, &pos, &perr) {
				goto fail10
			}
			continue
		fail10:
			pos = pos8
			break
		}
		labels[1] = parser.text[pos6:pos]
	}
	// Eof
	if !_accept(parser, _EofAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _File, start, pos, perr)
fail:
	return _memoize(parser, _File, start, -1, perr)
}

func _FileFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _File, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "File",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _File}
	// action
	// imports:Import* defs:Def* Eof
	// imports:Import*
	{
		pos1 := pos
		// Import*
		for {
			pos3 := pos
			// Import
			if !_fail(parser, _ImportFail, errPos, failure, &pos) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	// defs:Def*
	{
		pos6 := pos
		// Def*
		for {
			pos8 := pos
			// Def
			if !_fail(parser, _DefFail, errPos, failure, &pos) {
				goto fail10
			}
			continue
		fail10:
			pos = pos8
			break
		}
		labels[1] = parser.text[pos6:pos]
	}
	// Eof
	if !_fail(parser, _EofFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FileAction(parser *_Parser, start int) (int, *File) {
	var labels [2]string
	use(labels)
	var label0 []*Import
	var label1 []Def
	dp := parser.deltaPos[start][_File]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _File}
	n := parser.act[key]
	if n != nil {
		n := n.(File)
		return start + int(dp-1), &n
	}
	var node File
	pos := start
	// action
	{
		start0 := pos
		// imports:Import* defs:Def* Eof
		// imports:Import*
		{
			pos2 := pos
			// Import*
			for {
				pos4 := pos
				var node5 *Import
				// Import
				if p, n := _ImportAction(parser, pos); n == nil {
					goto fail6
				} else {
					node5 = *n
					pos = p
				}
				label0 = append(label0, node5)
				continue
			fail6:
				pos = pos4
				break
			}
			labels[0] = parser.text[pos2:pos]
		}
		// defs:Def*
		{
			pos7 := pos
			// Def*
			for {
				pos9 := pos
				var node10 Def
				// Def
				if p, n := _DefAction(parser, pos); n == nil {
					goto fail11
				} else {
					node10 = *n
					pos = p
				}
				label1 = append(label1, node10)
				continue
			fail11:
				pos = pos9
				break
			}
			labels[1] = parser.text[pos7:pos]
		}
		// Eof
		if p, n := _EofAction(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		node = func(
			start, end int, defs []Def, imports []*Import) File {
			return File{Imports: imports, Defs: defs}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ImportAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Import, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ reserved:("import"/"Import") id:Id? path:StrLit
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// reserved:("import"/"Import")
	{
		pos1 := pos
		// ("import"/"Import")
		// "import"/"Import"
		{
			pos5 := pos
			// "import"
			if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
				perr = _max(perr, pos)
				goto fail6
			}
			pos += 6
			goto ok2
		fail6:
			pos = pos5
			// "Import"
			if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
				perr = _max(perr, pos)
				goto fail7
			}
			pos += 6
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// id:Id?
	{
		pos8 := pos
		// Id?
		{
			pos10 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail11
			}
			goto ok12
		fail11:
			pos = pos10
		ok12:
		}
		labels[1] = parser.text[pos8:pos]
	}
	// path:StrLit
	{
		pos13 := pos
		// StrLit
		if !_accept(parser, _StrLitAccepts, &pos, &perr) {
			goto fail
		}
		labels[2] = parser.text[pos13:pos]
	}
	return _memoize(parser, _Import, start, pos, perr)
fail:
	return _memoize(parser, _Import, start, -1, perr)
}

func _ImportFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _Import, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Import",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Import}
	// action
	// _ reserved:("import"/"Import") id:Id? path:StrLit
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// reserved:("import"/"Import")
	{
		pos1 := pos
		// ("import"/"Import")
		// "import"/"Import"
		{
			pos5 := pos
			// "import"
			if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"import\"",
					})
				}
				goto fail6
			}
			pos += 6
			goto ok2
		fail6:
			pos = pos5
			// "Import"
			if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"Import\"",
					})
				}
				goto fail7
			}
			pos += 6
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// id:Id?
	{
		pos8 := pos
		// Id?
		{
			pos10 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail11
			}
			goto ok12
		fail11:
			pos = pos10
		ok12:
		}
		labels[1] = parser.text[pos8:pos]
	}
	// path:StrLit
	{
		pos13 := pos
		// StrLit
		if !_fail(parser, _StrLitFail, errPos, failure, &pos) {
			goto fail
		}
		labels[2] = parser.text[pos13:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ImportAction(parser *_Parser, start int) (int, **Import) {
	var labels [3]string
	use(labels)
	var label0 string
	var label1 *Ident
	var label2 Expr
	dp := parser.deltaPos[start][_Import]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Import}
	n := parser.act[key]
	if n != nil {
		n := n.(*Import)
		return start + int(dp-1), &n
	}
	var node *Import
	pos := start
	// action
	{
		start0 := pos
		// _ reserved:("import"/"Import") id:Id? path:StrLit
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// reserved:("import"/"Import")
		{
			pos2 := pos
			// ("import"/"Import")
			// "import"/"Import"
			{
				pos6 := pos
				var node5 string
				// "import"
				if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
					goto fail7
				}
				label0 = parser.text[pos : pos+6]
				pos += 6
				goto ok3
			fail7:
				label0 = node5
				pos = pos6
				// "Import"
				if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
					goto fail8
				}
				label0 = parser.text[pos : pos+6]
				pos += 6
				goto ok3
			fail8:
				label0 = node5
				pos = pos6
				goto fail
			ok3:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// id:Id?
		{
			pos9 := pos
			// Id?
			{
				pos11 := pos
				label1 = new(Ident)
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail12
				} else {
					*label1 = *n
					pos = p
				}
				goto ok13
			fail12:
				label1 = nil
				pos = pos11
			ok13:
			}
			labels[1] = parser.text[pos9:pos]
		}
		// path:StrLit
		{
			pos14 := pos
			// StrLit
			if p, n := _StrLitAction(parser, pos); n == nil {
				goto fail
			} else {
				label2 = *n
				pos = p
			}
			labels[2] = parser.text[pos14:pos]
		}
		node = func(
			start, end int, id *Ident, path Expr, reserved string) *Import {
			return &Import{
				Exp:  reserved == "Import",
				Name: id,
				Path: path.(*StrLit).Data,
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label2, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _DefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Def, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// ConstDef/VarDef/TypeDef/FuncDef/TestDef
	{
		pos3 := pos
		// ConstDef
		if !_accept(parser, _ConstDefAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// VarDef
		if !_accept(parser, _VarDefAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// TypeDef
		if !_accept(parser, _TypeDefAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// FuncDef
		if !_accept(parser, _FuncDefAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// TestDef
		if !_accept(parser, _TestDefAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Def, start, pos, perr)
fail:
	return _memoize(parser, _Def, start, -1, perr)
}

func _DefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Def, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Def",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Def}
	// ConstDef/VarDef/TypeDef/FuncDef/TestDef
	{
		pos3 := pos
		// ConstDef
		if !_fail(parser, _ConstDefFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// VarDef
		if !_fail(parser, _VarDefFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// TypeDef
		if !_fail(parser, _TypeDefFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// FuncDef
		if !_fail(parser, _FuncDefFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// TestDef
		if !_fail(parser, _TestDefFail, errPos, failure, &pos) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _DefAction(parser *_Parser, start int) (int, *Def) {
	dp := parser.deltaPos[start][_Def]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Def}
	n := parser.act[key]
	if n != nil {
		n := n.(Def)
		return start + int(dp-1), &n
	}
	var node Def
	pos := start
	// ConstDef/VarDef/TypeDef/FuncDef/TestDef
	{
		pos3 := pos
		var node2 Def
		// ConstDef
		if p, n := _ConstDefAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// VarDef
		if p, n := _VarDefAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// TypeDef
		if p, n := _TypeDefAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// FuncDef
		if p, n := _FuncDefAction(parser, pos); n == nil {
			goto fail7
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// TestDef
		if p, n := _TestDefAction(parser, pos); n == nil {
			goto fail8
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ConstDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [4]string
	use(labels)
	if dp, de, ok := _memo(parser, _ConstDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ reserved:("const"/"Const") name:Id typ:Type (_ ":=" expr:Expr)?
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// reserved:("const"/"Const")
	{
		pos1 := pos
		// ("const"/"Const")
		// "const"/"Const"
		{
			pos5 := pos
			// "const"
			if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
				perr = _max(perr, pos)
				goto fail6
			}
			pos += 5
			goto ok2
		fail6:
			pos = pos5
			// "Const"
			if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
				perr = _max(perr, pos)
				goto fail7
			}
			pos += 5
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// name:Id
	{
		pos8 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos8:pos]
	}
	// typ:Type
	{
		pos9 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ ":=" expr:Expr)?
	{
		pos11 := pos
		// (_ ":=" expr:Expr)
		// _ ":=" expr:Expr
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail12
		}
		// ":="
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
			perr = _max(perr, pos)
			goto fail12
		}
		pos += 2
		// expr:Expr
		{
			pos14 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail12
			}
			labels[3] = parser.text[pos14:pos]
		}
		goto ok15
	fail12:
		pos = pos11
	ok15:
	}
	return _memoize(parser, _ConstDef, start, pos, perr)
fail:
	return _memoize(parser, _ConstDef, start, -1, perr)
}

func _ConstDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [4]string
	use(labels)
	pos, failure := _failMemo(parser, _ConstDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "ConstDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _ConstDef}
	// action
	// _ reserved:("const"/"Const") name:Id typ:Type (_ ":=" expr:Expr)?
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// reserved:("const"/"Const")
	{
		pos1 := pos
		// ("const"/"Const")
		// "const"/"Const"
		{
			pos5 := pos
			// "const"
			if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"const\"",
					})
				}
				goto fail6
			}
			pos += 5
			goto ok2
		fail6:
			pos = pos5
			// "Const"
			if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"Const\"",
					})
				}
				goto fail7
			}
			pos += 5
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// name:Id
	{
		pos8 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos8:pos]
	}
	// typ:Type
	{
		pos9 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ ":=" expr:Expr)?
	{
		pos11 := pos
		// (_ ":=" expr:Expr)
		// _ ":=" expr:Expr
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail12
		}
		// ":="
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\":=\"",
				})
			}
			goto fail12
		}
		pos += 2
		// expr:Expr
		{
			pos14 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail12
			}
			labels[3] = parser.text[pos14:pos]
		}
		goto ok15
	fail12:
		pos = pos11
	ok15:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ConstDefAction(parser *_Parser, start int) (int, *Def) {
	var labels [4]string
	use(labels)
	var label0 string
	var label1 Ident
	var label2 Type
	var label3 Expr
	dp := parser.deltaPos[start][_ConstDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _ConstDef}
	n := parser.act[key]
	if n != nil {
		n := n.(Def)
		return start + int(dp-1), &n
	}
	var node Def
	pos := start
	// action
	{
		start0 := pos
		// _ reserved:("const"/"Const") name:Id typ:Type (_ ":=" expr:Expr)?
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// reserved:("const"/"Const")
		{
			pos2 := pos
			// ("const"/"Const")
			// "const"/"Const"
			{
				pos6 := pos
				var node5 string
				// "const"
				if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
					goto fail7
				}
				label0 = parser.text[pos : pos+5]
				pos += 5
				goto ok3
			fail7:
				label0 = node5
				pos = pos6
				// "Const"
				if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
					goto fail8
				}
				label0 = parser.text[pos : pos+5]
				pos += 5
				goto ok3
			fail8:
				label0 = node5
				pos = pos6
				goto fail
			ok3:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// name:Id
		{
			pos9 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos9:pos]
		}
		// typ:Type
		{
			pos10 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label2 = *n
				pos = p
			}
			labels[2] = parser.text[pos10:pos]
		}
		// (_ ":=" expr:Expr)?
		{
			pos12 := pos
			// (_ ":=" expr:Expr)
			// _ ":=" expr:Expr
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ":="
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
				goto fail13
			}
			pos += 2
			// expr:Expr
			{
				pos15 := pos
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail13
				} else {
					label3 = *n
					pos = p
				}
				labels[3] = parser.text[pos15:pos]
			}
			goto ok16
		fail13:
			pos = pos12
		ok16:
		}
		node = func(
			start, end int, expr Expr, name Ident, reserved string, typ Type) Def {
			return Def(&VarDef{
				Exp:   reserved == "Const",
				Const: true,
				Name:  name,
				Type:  typ,
				Expr:  expr,
				L:     l(parser, start, end),
			})
		}(
			start0, pos, label3, label1, label0, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _VarDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [4]string
	use(labels)
	if dp, de, ok := _memo(parser, _VarDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ reserved:("var"/"Var") name:Id typ:Type (_ ":=" expr:Expr)?
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// reserved:("var"/"Var")
	{
		pos1 := pos
		// ("var"/"Var")
		// "var"/"Var"
		{
			pos5 := pos
			// "var"
			if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
				perr = _max(perr, pos)
				goto fail6
			}
			pos += 3
			goto ok2
		fail6:
			pos = pos5
			// "Var"
			if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
				perr = _max(perr, pos)
				goto fail7
			}
			pos += 3
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// name:Id
	{
		pos8 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos8:pos]
	}
	// typ:Type
	{
		pos9 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ ":=" expr:Expr)?
	{
		pos11 := pos
		// (_ ":=" expr:Expr)
		// _ ":=" expr:Expr
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail12
		}
		// ":="
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
			perr = _max(perr, pos)
			goto fail12
		}
		pos += 2
		// expr:Expr
		{
			pos14 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail12
			}
			labels[3] = parser.text[pos14:pos]
		}
		goto ok15
	fail12:
		pos = pos11
	ok15:
	}
	return _memoize(parser, _VarDef, start, pos, perr)
fail:
	return _memoize(parser, _VarDef, start, -1, perr)
}

func _VarDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [4]string
	use(labels)
	pos, failure := _failMemo(parser, _VarDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "VarDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _VarDef}
	// action
	// _ reserved:("var"/"Var") name:Id typ:Type (_ ":=" expr:Expr)?
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// reserved:("var"/"Var")
	{
		pos1 := pos
		// ("var"/"Var")
		// "var"/"Var"
		{
			pos5 := pos
			// "var"
			if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"var\"",
					})
				}
				goto fail6
			}
			pos += 3
			goto ok2
		fail6:
			pos = pos5
			// "Var"
			if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"Var\"",
					})
				}
				goto fail7
			}
			pos += 3
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// name:Id
	{
		pos8 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos8:pos]
	}
	// typ:Type
	{
		pos9 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ ":=" expr:Expr)?
	{
		pos11 := pos
		// (_ ":=" expr:Expr)
		// _ ":=" expr:Expr
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail12
		}
		// ":="
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\":=\"",
				})
			}
			goto fail12
		}
		pos += 2
		// expr:Expr
		{
			pos14 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail12
			}
			labels[3] = parser.text[pos14:pos]
		}
		goto ok15
	fail12:
		pos = pos11
	ok15:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _VarDefAction(parser *_Parser, start int) (int, *Def) {
	var labels [4]string
	use(labels)
	var label0 string
	var label1 Ident
	var label2 Type
	var label3 Expr
	dp := parser.deltaPos[start][_VarDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _VarDef}
	n := parser.act[key]
	if n != nil {
		n := n.(Def)
		return start + int(dp-1), &n
	}
	var node Def
	pos := start
	// action
	{
		start0 := pos
		// _ reserved:("var"/"Var") name:Id typ:Type (_ ":=" expr:Expr)?
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// reserved:("var"/"Var")
		{
			pos2 := pos
			// ("var"/"Var")
			// "var"/"Var"
			{
				pos6 := pos
				var node5 string
				// "var"
				if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
					goto fail7
				}
				label0 = parser.text[pos : pos+3]
				pos += 3
				goto ok3
			fail7:
				label0 = node5
				pos = pos6
				// "Var"
				if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
					goto fail8
				}
				label0 = parser.text[pos : pos+3]
				pos += 3
				goto ok3
			fail8:
				label0 = node5
				pos = pos6
				goto fail
			ok3:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// name:Id
		{
			pos9 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos9:pos]
		}
		// typ:Type
		{
			pos10 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label2 = *n
				pos = p
			}
			labels[2] = parser.text[pos10:pos]
		}
		// (_ ":=" expr:Expr)?
		{
			pos12 := pos
			// (_ ":=" expr:Expr)
			// _ ":=" expr:Expr
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ":="
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
				goto fail13
			}
			pos += 2
			// expr:Expr
			{
				pos15 := pos
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail13
				} else {
					label3 = *n
					pos = p
				}
				labels[3] = parser.text[pos15:pos]
			}
			goto ok16
		fail13:
			pos = pos12
		ok16:
		}
		node = func(
			start, end int, expr Expr, name Ident, reserved string, typ Type) Def {
			return Def(&VarDef{
				Exp:   reserved == "Var",
				Const: false,
				Name:  name,
				Type:  typ,
				Expr:  expr,
				L:     l(parser, start, end),
			})
		}(
			start0, pos, label3, label1, label0, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [5]string
	use(labels)
	if dp, de, ok := _memo(parser, _TypeDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ reserved:("type"/"Type") ps:TypeVars? name:Id alias:(_ ":=")? t:Type?
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// reserved:("type"/"Type")
	{
		pos1 := pos
		// ("type"/"Type")
		// "type"/"Type"
		{
			pos5 := pos
			// "type"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
				perr = _max(perr, pos)
				goto fail6
			}
			pos += 4
			goto ok2
		fail6:
			pos = pos5
			// "Type"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
				perr = _max(perr, pos)
				goto fail7
			}
			pos += 4
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ps:TypeVars?
	{
		pos8 := pos
		// TypeVars?
		{
			pos10 := pos
			// TypeVars
			if !_accept(parser, _TypeVarsAccepts, &pos, &perr) {
				goto fail11
			}
			goto ok12
		fail11:
			pos = pos10
		ok12:
		}
		labels[1] = parser.text[pos8:pos]
	}
	// name:Id
	{
		pos13 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[2] = parser.text[pos13:pos]
	}
	// alias:(_ ":=")?
	{
		pos14 := pos
		// (_ ":=")?
		{
			pos16 := pos
			// (_ ":=")
			// _ ":="
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail17
			}
			// ":="
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
				perr = _max(perr, pos)
				goto fail17
			}
			pos += 2
			goto ok19
		fail17:
			pos = pos16
		ok19:
		}
		labels[3] = parser.text[pos14:pos]
	}
	// t:Type?
	{
		pos20 := pos
		// Type?
		{
			pos22 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail23
			}
			goto ok24
		fail23:
			pos = pos22
		ok24:
		}
		labels[4] = parser.text[pos20:pos]
	}
	return _memoize(parser, _TypeDef, start, pos, perr)
fail:
	return _memoize(parser, _TypeDef, start, -1, perr)
}

func _TypeDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [5]string
	use(labels)
	pos, failure := _failMemo(parser, _TypeDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "TypeDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _TypeDef}
	// action
	// _ reserved:("type"/"Type") ps:TypeVars? name:Id alias:(_ ":=")? t:Type?
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// reserved:("type"/"Type")
	{
		pos1 := pos
		// ("type"/"Type")
		// "type"/"Type"
		{
			pos5 := pos
			// "type"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"type\"",
					})
				}
				goto fail6
			}
			pos += 4
			goto ok2
		fail6:
			pos = pos5
			// "Type"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"Type\"",
					})
				}
				goto fail7
			}
			pos += 4
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ps:TypeVars?
	{
		pos8 := pos
		// TypeVars?
		{
			pos10 := pos
			// TypeVars
			if !_fail(parser, _TypeVarsFail, errPos, failure, &pos) {
				goto fail11
			}
			goto ok12
		fail11:
			pos = pos10
		ok12:
		}
		labels[1] = parser.text[pos8:pos]
	}
	// name:Id
	{
		pos13 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[2] = parser.text[pos13:pos]
	}
	// alias:(_ ":=")?
	{
		pos14 := pos
		// (_ ":=")?
		{
			pos16 := pos
			// (_ ":=")
			// _ ":="
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail17
			}
			// ":="
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\":=\"",
					})
				}
				goto fail17
			}
			pos += 2
			goto ok19
		fail17:
			pos = pos16
		ok19:
		}
		labels[3] = parser.text[pos14:pos]
	}
	// t:Type?
	{
		pos20 := pos
		// Type?
		{
			pos22 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail23
			}
			goto ok24
		fail23:
			pos = pos22
		ok24:
		}
		labels[4] = parser.text[pos20:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypeDefAction(parser *_Parser, start int) (int, *Def) {
	var labels [5]string
	use(labels)
	var label0 string
	var label1 *[]TypeVar
	var label2 Ident
	var label3 string
	var label4 *Type
	dp := parser.deltaPos[start][_TypeDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _TypeDef}
	n := parser.act[key]
	if n != nil {
		n := n.(Def)
		return start + int(dp-1), &n
	}
	var node Def
	pos := start
	// action
	{
		start0 := pos
		// _ reserved:("type"/"Type") ps:TypeVars? name:Id alias:(_ ":=")? t:Type?
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// reserved:("type"/"Type")
		{
			pos2 := pos
			// ("type"/"Type")
			// "type"/"Type"
			{
				pos6 := pos
				var node5 string
				// "type"
				if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
					goto fail7
				}
				label0 = parser.text[pos : pos+4]
				pos += 4
				goto ok3
			fail7:
				label0 = node5
				pos = pos6
				// "Type"
				if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
					goto fail8
				}
				label0 = parser.text[pos : pos+4]
				pos += 4
				goto ok3
			fail8:
				label0 = node5
				pos = pos6
				goto fail
			ok3:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ps:TypeVars?
		{
			pos9 := pos
			// TypeVars?
			{
				pos11 := pos
				label1 = new([]TypeVar)
				// TypeVars
				if p, n := _TypeVarsAction(parser, pos); n == nil {
					goto fail12
				} else {
					*label1 = *n
					pos = p
				}
				goto ok13
			fail12:
				label1 = nil
				pos = pos11
			ok13:
			}
			labels[1] = parser.text[pos9:pos]
		}
		// name:Id
		{
			pos14 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label2 = *n
				pos = p
			}
			labels[2] = parser.text[pos14:pos]
		}
		// alias:(_ ":=")?
		{
			pos15 := pos
			// (_ ":=")?
			{
				pos17 := pos
				// (_ ":=")
				// _ ":="
				{
					var node19 string
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail18
					} else {
						node19 = *n
						pos = p
					}
					label3, node19 = label3+node19, ""
					// ":="
					if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
						goto fail18
					}
					node19 = parser.text[pos : pos+2]
					pos += 2
					label3, node19 = label3+node19, ""
				}
				goto ok20
			fail18:
				label3 = ""
				pos = pos17
			ok20:
			}
			labels[3] = parser.text[pos15:pos]
		}
		// t:Type?
		{
			pos21 := pos
			// Type?
			{
				pos23 := pos
				label4 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail24
				} else {
					*label4 = *n
					pos = p
				}
				goto ok25
			fail24:
				label4 = nil
				pos = pos23
			ok25:
			}
			labels[4] = parser.text[pos21:pos]
		}
		node = func(
			start, end int, alias string, name Ident, ps *[]TypeVar, reserved string, t *Type) Def {
			var parms []TypeVar
			if ps != nil {
				parms = *ps
			}
			var typ Type
			if t != nil {
				typ = *t
			}
			return Def(&TypeDef{
				Exp:       reserved == "Type",
				Alias:     alias != "",
				TypeParms: parms,
				Name:      name,
				Type:      typ,
				L:         l(parser, start, end),
			})
		}(
			start0, pos, label3, label2, label1, label0, label4)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeVarsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [4]string
	use(labels)
	if dp, de, ok := _memo(parser, _TypeVars, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// tv:TypeVar {…}/_ "(" t0:TypeVar ts:(_ "," t1:TypeVar {…})* (_ ",")? _ ")" {…}
	{
		pos3 := pos
		// action
		// tv:TypeVar
		{
			pos5 := pos
			// TypeVar
			if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
				goto fail4
			}
			labels[0] = parser.text[pos5:pos]
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "(" t0:TypeVar ts:(_ "," t1:TypeVar {…})* (_ ",")? _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		// t0:TypeVar
		{
			pos8 := pos
			// TypeVar
			if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
				goto fail6
			}
			labels[1] = parser.text[pos8:pos]
		}
		// ts:(_ "," t1:TypeVar {…})*
		{
			pos9 := pos
			// (_ "," t1:TypeVar {…})*
			for {
				pos11 := pos
				// (_ "," t1:TypeVar {…})
				// action
				// _ "," t1:TypeVar
				// _
				if !_accept(parser, __Accepts, &pos, &perr) {
					goto fail13
				}
				// ","
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
					perr = _max(perr, pos)
					goto fail13
				}
				pos++
				// t1:TypeVar
				{
					pos15 := pos
					// TypeVar
					if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
						goto fail13
					}
					labels[2] = parser.text[pos15:pos]
				}
				continue
			fail13:
				pos = pos11
				break
			}
			labels[3] = parser.text[pos9:pos]
		}
		// (_ ",")?
		{
			pos17 := pos
			// (_ ",")
			// _ ","
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail18
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail18
			}
			pos++
			goto ok20
		fail18:
			pos = pos17
		ok20:
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _TypeVars, start, pos, perr)
fail:
	return _memoize(parser, _TypeVars, start, -1, perr)
}

func _TypeVarsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [4]string
	use(labels)
	pos, failure := _failMemo(parser, _TypeVars, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "TypeVars",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _TypeVars}
	// tv:TypeVar {…}/_ "(" t0:TypeVar ts:(_ "," t1:TypeVar {…})* (_ ",")? _ ")" {…}
	{
		pos3 := pos
		// action
		// tv:TypeVar
		{
			pos5 := pos
			// TypeVar
			if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
				goto fail4
			}
			labels[0] = parser.text[pos5:pos]
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "(" t0:TypeVar ts:(_ "," t1:TypeVar {…})* (_ ",")? _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail6
		}
		pos++
		// t0:TypeVar
		{
			pos8 := pos
			// TypeVar
			if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
				goto fail6
			}
			labels[1] = parser.text[pos8:pos]
		}
		// ts:(_ "," t1:TypeVar {…})*
		{
			pos9 := pos
			// (_ "," t1:TypeVar {…})*
			for {
				pos11 := pos
				// (_ "," t1:TypeVar {…})
				// action
				// _ "," t1:TypeVar
				// _
				if !_fail(parser, __Fail, errPos, failure, &pos) {
					goto fail13
				}
				// ","
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\",\"",
						})
					}
					goto fail13
				}
				pos++
				// t1:TypeVar
				{
					pos15 := pos
					// TypeVar
					if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
						goto fail13
					}
					labels[2] = parser.text[pos15:pos]
				}
				continue
			fail13:
				pos = pos11
				break
			}
			labels[3] = parser.text[pos9:pos]
		}
		// (_ ",")?
		{
			pos17 := pos
			// (_ ",")
			// _ ","
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail18
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail18
			}
			pos++
			goto ok20
		fail18:
			pos = pos17
		ok20:
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypeVarsAction(parser *_Parser, start int) (int, *[]TypeVar) {
	var labels [4]string
	use(labels)
	var label0 TypeVar
	var label1 TypeVar
	var label2 TypeVar
	var label3 []TypeVar
	dp := parser.deltaPos[start][_TypeVars]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _TypeVars}
	n := parser.act[key]
	if n != nil {
		n := n.([]TypeVar)
		return start + int(dp-1), &n
	}
	var node []TypeVar
	pos := start
	// tv:TypeVar {…}/_ "(" t0:TypeVar ts:(_ "," t1:TypeVar {…})* (_ ",")? _ ")" {…}
	{
		pos3 := pos
		var node2 []TypeVar
		// action
		{
			start5 := pos
			// tv:TypeVar
			{
				pos6 := pos
				// TypeVar
				if p, n := _TypeVarAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos6:pos]
			}
			node = func(
				start, end int, tv TypeVar) []TypeVar {
				return []TypeVar{tv}
			}(
				start5, pos, label0)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start8 := pos
			// _ "(" t0:TypeVar ts:(_ "," t1:TypeVar {…})* (_ ",")? _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail7
			}
			pos++
			// t0:TypeVar
			{
				pos10 := pos
				// TypeVar
				if p, n := _TypeVarAction(parser, pos); n == nil {
					goto fail7
				} else {
					label1 = *n
					pos = p
				}
				labels[1] = parser.text[pos10:pos]
			}
			// ts:(_ "," t1:TypeVar {…})*
			{
				pos11 := pos
				// (_ "," t1:TypeVar {…})*
				for {
					pos13 := pos
					var node14 TypeVar
					// (_ "," t1:TypeVar {…})
					// action
					{
						start16 := pos
						// _ "," t1:TypeVar
						// _
						if p, n := __Action(parser, pos); n == nil {
							goto fail15
						} else {
							pos = p
						}
						// ","
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
							goto fail15
						}
						pos++
						// t1:TypeVar
						{
							pos18 := pos
							// TypeVar
							if p, n := _TypeVarAction(parser, pos); n == nil {
								goto fail15
							} else {
								label2 = *n
								pos = p
							}
							labels[2] = parser.text[pos18:pos]
						}
						node14 = func(
							start, end int, t0 TypeVar, t1 TypeVar, tv TypeVar) TypeVar {
							return TypeVar(t1)
						}(
							start16, pos, label1, label2, label0)
					}
					label3 = append(label3, node14)
					continue
				fail15:
					pos = pos13
					break
				}
				labels[3] = parser.text[pos11:pos]
			}
			// (_ ",")?
			{
				pos20 := pos
				// (_ ",")
				// _ ","
				// _
				if p, n := __Action(parser, pos); n == nil {
					goto fail21
				} else {
					pos = p
				}
				// ","
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
					goto fail21
				}
				pos++
				goto ok23
			fail21:
				pos = pos20
			ok23:
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail7
			}
			pos++
			node = func(
				start, end int, t0 TypeVar, t1 TypeVar, ts []TypeVar, tv TypeVar) []TypeVar {
				return []TypeVar(append([]TypeVar{t0}, ts...))
			}(
				start8, pos, label1, label2, label3, label0)
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Type, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// NamedType/tvar:TypeVar {…}/TypeLit
	{
		pos3 := pos
		// NamedType
		if !_accept(parser, _NamedTypeAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// tvar:TypeVar
		{
			pos6 := pos
			// TypeVar
			if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
				goto fail5
			}
			labels[0] = parser.text[pos6:pos]
		}
		goto ok0
	fail5:
		pos = pos3
		// TypeLit
		if !_accept(parser, _TypeLitAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Type, start, pos, perr)
fail:
	return _memoize(parser, _Type, start, -1, perr)
}

func _TypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Type, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Type",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Type}
	// NamedType/tvar:TypeVar {…}/TypeLit
	{
		pos3 := pos
		// NamedType
		if !_fail(parser, _NamedTypeFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// tvar:TypeVar
		{
			pos6 := pos
			// TypeVar
			if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
				goto fail5
			}
			labels[0] = parser.text[pos6:pos]
		}
		goto ok0
	fail5:
		pos = pos3
		// TypeLit
		if !_fail(parser, _TypeLitFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [1]string
	use(labels)
	var label0 TypeVar
	dp := parser.deltaPos[start][_Type]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Type}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// NamedType/tvar:TypeVar {…}/TypeLit
	{
		pos3 := pos
		var node2 Type
		// NamedType
		if p, n := _NamedTypeAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start6 := pos
			// tvar:TypeVar
			{
				pos7 := pos
				// TypeVar
				if p, n := _TypeVarAction(parser, pos); n == nil {
					goto fail5
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos7:pos]
			}
			node = func(
				start, end int, tvar TypeVar) Type {
				return Type(tvar)
			}(
				start6, pos, label0)
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// TypeLit
		if p, n := _TypeLitAction(parser, pos); n == nil {
			goto fail8
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypesAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Types, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// t0:Type ts:(_ "," t1:Type {…})* (_ ",")?
	// t0:Type
	{
		pos1 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ts:(_ "," t1:Type {…})*
	{
		pos2 := pos
		// (_ "," t1:Type {…})*
		for {
			pos4 := pos
			// (_ "," t1:Type {…})
			// action
			// _ "," t1:Type
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// t1:Type
			{
				pos8 := pos
				// Type
				if !_accept(parser, _TypeAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _Types, start, pos, perr)
fail:
	return _memoize(parser, _Types, start, -1, perr)
}

func _TypesFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _Types, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Types",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Types}
	// action
	// t0:Type ts:(_ "," t1:Type {…})* (_ ",")?
	// t0:Type
	{
		pos1 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ts:(_ "," t1:Type {…})*
	{
		pos2 := pos
		// (_ "," t1:Type {…})*
		for {
			pos4 := pos
			// (_ "," t1:Type {…})
			// action
			// _ "," t1:Type
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// t1:Type
			{
				pos8 := pos
				// Type
				if !_fail(parser, _TypeFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypesAction(parser *_Parser, start int) (int, *[]Type) {
	var labels [3]string
	use(labels)
	var label0 Type
	var label1 Type
	var label2 []Type
	dp := parser.deltaPos[start][_Types]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Types}
	n := parser.act[key]
	if n != nil {
		n := n.([]Type)
		return start + int(dp-1), &n
	}
	var node []Type
	pos := start
	// action
	{
		start0 := pos
		// t0:Type ts:(_ "," t1:Type {…})* (_ ",")?
		// t0:Type
		{
			pos2 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ts:(_ "," t1:Type {…})*
		{
			pos3 := pos
			// (_ "," t1:Type {…})*
			for {
				pos5 := pos
				var node6 Type
				// (_ "," t1:Type {…})
				// action
				{
					start8 := pos
					// _ "," t1:Type
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// t1:Type
					{
						pos10 := pos
						// Type
						if p, n := _TypeAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, t0 Type, t1 Type) Type {
						return Type(t1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, t0 Type, t1 Type, ts []Type) []Type {
			return []Type(append([]Type{t0}, ts...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _RefTypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _RefType, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "&" typ:Type
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "&"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "&" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// typ:Type
	{
		pos1 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	return _memoize(parser, _RefType, start, pos, perr)
fail:
	return _memoize(parser, _RefType, start, -1, perr)
}

func _RefTypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _RefType, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "RefType",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _RefType}
	// action
	// _ "&" typ:Type
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "&"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "&" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"&\"",
			})
		}
		goto fail
	}
	pos++
	// typ:Type
	{
		pos1 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _RefTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [1]string
	use(labels)
	var label0 Type
	dp := parser.deltaPos[start][_RefType]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _RefType}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// action
	{
		start0 := pos
		// _ "&" typ:Type
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "&"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "&" {
			goto fail
		}
		pos++
		// typ:Type
		{
			pos2 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, typ Type) Type {
			return Type(&RefType{Type: typ, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _NamedTypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _NamedType, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// (args:TypeArgs)? names:ModName+
	// (args:TypeArgs)?
	{
		pos2 := pos
		// (args:TypeArgs)
		// args:TypeArgs
		{
			pos4 := pos
			// TypeArgs
			if !_accept(parser, _TypeArgsAccepts, &pos, &perr) {
				goto fail3
			}
			labels[0] = parser.text[pos4:pos]
		}
		goto ok5
	fail3:
		pos = pos2
	ok5:
	}
	// names:ModName+
	{
		pos6 := pos
		// ModName+
		// ModName
		if !_accept(parser, _ModNameAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos8 := pos
			// ModName
			if !_accept(parser, _ModNameAccepts, &pos, &perr) {
				goto fail10
			}
			continue
		fail10:
			pos = pos8
			break
		}
		labels[1] = parser.text[pos6:pos]
	}
	return _memoize(parser, _NamedType, start, pos, perr)
fail:
	return _memoize(parser, _NamedType, start, -1, perr)
}

func _NamedTypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _NamedType, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "NamedType",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _NamedType}
	// action
	// (args:TypeArgs)? names:ModName+
	// (args:TypeArgs)?
	{
		pos2 := pos
		// (args:TypeArgs)
		// args:TypeArgs
		{
			pos4 := pos
			// TypeArgs
			if !_fail(parser, _TypeArgsFail, errPos, failure, &pos) {
				goto fail3
			}
			labels[0] = parser.text[pos4:pos]
		}
		goto ok5
	fail3:
		pos = pos2
	ok5:
	}
	// names:ModName+
	{
		pos6 := pos
		// ModName+
		// ModName
		if !_fail(parser, _ModNameFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos8 := pos
			// ModName
			if !_fail(parser, _ModNameFail, errPos, failure, &pos) {
				goto fail10
			}
			continue
		fail10:
			pos = pos8
			break
		}
		labels[1] = parser.text[pos6:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _NamedTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [2]string
	use(labels)
	var label0 []Type
	var label1 []*NamedType
	dp := parser.deltaPos[start][_NamedType]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _NamedType}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// action
	{
		start0 := pos
		// (args:TypeArgs)? names:ModName+
		// (args:TypeArgs)?
		{
			pos3 := pos
			// (args:TypeArgs)
			// args:TypeArgs
			{
				pos5 := pos
				// TypeArgs
				if p, n := _TypeArgsAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos5:pos]
			}
			goto ok6
		fail4:
			pos = pos3
		ok6:
		}
		// names:ModName+
		{
			pos7 := pos
			// ModName+
			{
				var node10 *NamedType
				// ModName
				if p, n := _ModNameAction(parser, pos); n == nil {
					goto fail
				} else {
					node10 = *n
					pos = p
				}
				label1 = append(label1, node10)
			}
			for {
				pos9 := pos
				var node10 *NamedType
				// ModName
				if p, n := _ModNameAction(parser, pos); n == nil {
					goto fail11
				} else {
					node10 = *n
					pos = p
				}
				label1 = append(label1, node10)
				continue
			fail11:
				pos = pos9
				break
			}
			labels[1] = parser.text[pos7:pos]
		}
		node = func(
			start, end int, args []Type, names []*NamedType) Type {
			return Type(namedType(args, names, l(parser, start, end)))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ModNameAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _ModName, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// mod:(m:Id _ "#" {…})? name:Id
	// mod:(m:Id _ "#" {…})?
	{
		pos1 := pos
		// (m:Id _ "#" {…})?
		{
			pos3 := pos
			// (m:Id _ "#" {…})
			// action
			// m:Id _ "#"
			// m:Id
			{
				pos6 := pos
				// Id
				if !_accept(parser, _IdAccepts, &pos, &perr) {
					goto fail4
				}
				labels[0] = parser.text[pos6:pos]
			}
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail4
			}
			// "#"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
				perr = _max(perr, pos)
				goto fail4
			}
			pos++
			goto ok7
		fail4:
			pos = pos3
		ok7:
		}
		labels[1] = parser.text[pos1:pos]
	}
	// name:Id
	{
		pos8 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[2] = parser.text[pos8:pos]
	}
	return _memoize(parser, _ModName, start, pos, perr)
fail:
	return _memoize(parser, _ModName, start, -1, perr)
}

func _ModNameFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _ModName, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "ModName",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _ModName}
	// action
	// mod:(m:Id _ "#" {…})? name:Id
	// mod:(m:Id _ "#" {…})?
	{
		pos1 := pos
		// (m:Id _ "#" {…})?
		{
			pos3 := pos
			// (m:Id _ "#" {…})
			// action
			// m:Id _ "#"
			// m:Id
			{
				pos6 := pos
				// Id
				if !_fail(parser, _IdFail, errPos, failure, &pos) {
					goto fail4
				}
				labels[0] = parser.text[pos6:pos]
			}
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail4
			}
			// "#"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"#\"",
					})
				}
				goto fail4
			}
			pos++
			goto ok7
		fail4:
			pos = pos3
		ok7:
		}
		labels[1] = parser.text[pos1:pos]
	}
	// name:Id
	{
		pos8 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[2] = parser.text[pos8:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ModNameAction(parser *_Parser, start int) (int, **NamedType) {
	var labels [3]string
	use(labels)
	var label0 Ident
	var label1 *Ident
	var label2 Ident
	dp := parser.deltaPos[start][_ModName]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _ModName}
	n := parser.act[key]
	if n != nil {
		n := n.(*NamedType)
		return start + int(dp-1), &n
	}
	var node *NamedType
	pos := start
	// action
	{
		start0 := pos
		// mod:(m:Id _ "#" {…})? name:Id
		// mod:(m:Id _ "#" {…})?
		{
			pos2 := pos
			// (m:Id _ "#" {…})?
			{
				pos4 := pos
				label1 = new(Ident)
				// (m:Id _ "#" {…})
				// action
				{
					start6 := pos
					// m:Id _ "#"
					// m:Id
					{
						pos8 := pos
						// Id
						if p, n := _IdAction(parser, pos); n == nil {
							goto fail5
						} else {
							label0 = *n
							pos = p
						}
						labels[0] = parser.text[pos8:pos]
					}
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail5
					} else {
						pos = p
					}
					// "#"
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
						goto fail5
					}
					pos++
					*label1 = func(
						start, end int, m Ident) Ident {
						return Ident(m)
					}(
						start6, pos, label0)
				}
				goto ok9
			fail5:
				label1 = nil
				pos = pos4
			ok9:
			}
			labels[1] = parser.text[pos2:pos]
		}
		// name:Id
		{
			pos10 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label2 = *n
				pos = p
			}
			labels[2] = parser.text[pos10:pos]
		}
		node = func(
			start, end int, m Ident, mod *Ident, name Ident) *NamedType {
			return &NamedType{Mod: mod, Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeArgsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [4]string
	use(labels)
	if dp, de, ok := _memo(parser, _TypeArgs, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// t:(tv:TypeVar {…}/TypeLit) {…}/_ "(" arg:Type _ ")" {…}/_ "(" args:Types _ ")" {…}
	{
		pos3 := pos
		// action
		// t:(tv:TypeVar {…}/TypeLit)
		{
			pos5 := pos
			// (tv:TypeVar {…}/TypeLit)
			// tv:TypeVar {…}/TypeLit
			{
				pos9 := pos
				// action
				// tv:TypeVar
				{
					pos11 := pos
					// TypeVar
					if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
						goto fail10
					}
					labels[0] = parser.text[pos11:pos]
				}
				goto ok6
			fail10:
				pos = pos9
				// TypeLit
				if !_accept(parser, _TypeLitAccepts, &pos, &perr) {
					goto fail12
				}
				goto ok6
			fail12:
				pos = pos9
				goto fail4
			ok6:
			}
			labels[1] = parser.text[pos5:pos]
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "(" arg:Type _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail13
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail13
		}
		pos++
		// arg:Type
		{
			pos15 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail13
			}
			labels[2] = parser.text[pos15:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail13
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail13
		}
		pos++
		goto ok0
	fail13:
		pos = pos3
		// action
		// _ "(" args:Types _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail16
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail16
		}
		pos++
		// args:Types
		{
			pos18 := pos
			// Types
			if !_accept(parser, _TypesAccepts, &pos, &perr) {
				goto fail16
			}
			labels[3] = parser.text[pos18:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail16
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail16
		}
		pos++
		goto ok0
	fail16:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _TypeArgs, start, pos, perr)
fail:
	return _memoize(parser, _TypeArgs, start, -1, perr)
}

func _TypeArgsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [4]string
	use(labels)
	pos, failure := _failMemo(parser, _TypeArgs, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "TypeArgs",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _TypeArgs}
	// t:(tv:TypeVar {…}/TypeLit) {…}/_ "(" arg:Type _ ")" {…}/_ "(" args:Types _ ")" {…}
	{
		pos3 := pos
		// action
		// t:(tv:TypeVar {…}/TypeLit)
		{
			pos5 := pos
			// (tv:TypeVar {…}/TypeLit)
			// tv:TypeVar {…}/TypeLit
			{
				pos9 := pos
				// action
				// tv:TypeVar
				{
					pos11 := pos
					// TypeVar
					if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
						goto fail10
					}
					labels[0] = parser.text[pos11:pos]
				}
				goto ok6
			fail10:
				pos = pos9
				// TypeLit
				if !_fail(parser, _TypeLitFail, errPos, failure, &pos) {
					goto fail12
				}
				goto ok6
			fail12:
				pos = pos9
				goto fail4
			ok6:
			}
			labels[1] = parser.text[pos5:pos]
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "(" arg:Type _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail13
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail13
		}
		pos++
		// arg:Type
		{
			pos15 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail13
			}
			labels[2] = parser.text[pos15:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail13
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail13
		}
		pos++
		goto ok0
	fail13:
		pos = pos3
		// action
		// _ "(" args:Types _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail16
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail16
		}
		pos++
		// args:Types
		{
			pos18 := pos
			// Types
			if !_fail(parser, _TypesFail, errPos, failure, &pos) {
				goto fail16
			}
			labels[3] = parser.text[pos18:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail16
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail16
		}
		pos++
		goto ok0
	fail16:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypeArgsAction(parser *_Parser, start int) (int, *[]Type) {
	var labels [4]string
	use(labels)
	var label0 TypeVar
	var label1 Type
	var label2 Type
	var label3 []Type
	dp := parser.deltaPos[start][_TypeArgs]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _TypeArgs}
	n := parser.act[key]
	if n != nil {
		n := n.([]Type)
		return start + int(dp-1), &n
	}
	var node []Type
	pos := start
	// t:(tv:TypeVar {…}/TypeLit) {…}/_ "(" arg:Type _ ")" {…}/_ "(" args:Types _ ")" {…}
	{
		pos3 := pos
		var node2 []Type
		// action
		{
			start5 := pos
			// t:(tv:TypeVar {…}/TypeLit)
			{
				pos6 := pos
				// (tv:TypeVar {…}/TypeLit)
				// tv:TypeVar {…}/TypeLit
				{
					pos10 := pos
					var node9 Type
					// action
					{
						start12 := pos
						// tv:TypeVar
						{
							pos13 := pos
							// TypeVar
							if p, n := _TypeVarAction(parser, pos); n == nil {
								goto fail11
							} else {
								label0 = *n
								pos = p
							}
							labels[0] = parser.text[pos13:pos]
						}
						label1 = func(
							start, end int, tv TypeVar) Type {
							return Type(tv)
						}(
							start12, pos, label0)
					}
					goto ok7
				fail11:
					label1 = node9
					pos = pos10
					// TypeLit
					if p, n := _TypeLitAction(parser, pos); n == nil {
						goto fail14
					} else {
						label1 = *n
						pos = p
					}
					goto ok7
				fail14:
					label1 = node9
					pos = pos10
					goto fail4
				ok7:
				}
				labels[1] = parser.text[pos6:pos]
			}
			node = func(
				start, end int, t Type, tv TypeVar) []Type {
				return []Type{t}
			}(
				start5, pos, label1, label0)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start16 := pos
			// _ "(" arg:Type _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail15
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail15
			}
			pos++
			// arg:Type
			{
				pos18 := pos
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail15
				} else {
					label2 = *n
					pos = p
				}
				labels[2] = parser.text[pos18:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail15
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail15
			}
			pos++
			node = func(
				start, end int, arg Type, t Type, tv TypeVar) []Type {
				return []Type{arg}
			}(
				start16, pos, label2, label1, label0)
		}
		goto ok0
	fail15:
		node = node2
		pos = pos3
		// action
		{
			start20 := pos
			// _ "(" args:Types _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail19
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail19
			}
			pos++
			// args:Types
			{
				pos22 := pos
				// Types
				if p, n := _TypesAction(parser, pos); n == nil {
					goto fail19
				} else {
					label3 = *n
					pos = p
				}
				labels[3] = parser.text[pos22:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail19
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail19
			}
			pos++
			node = func(
				start, end int, arg Type, args []Type, t Type, tv TypeVar) []Type {
				return []Type(args)
			}(
				start20, pos, label2, label3, label1, label0)
		}
		goto ok0
	fail19:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _TypeLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// RefType/FuncType/ArrayType/StructType/UnionType
	{
		pos3 := pos
		// RefType
		if !_accept(parser, _RefTypeAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// FuncType
		if !_accept(parser, _FuncTypeAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// ArrayType
		if !_accept(parser, _ArrayTypeAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// StructType
		if !_accept(parser, _StructTypeAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// UnionType
		if !_accept(parser, _UnionTypeAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _TypeLit, start, pos, perr)
fail:
	return _memoize(parser, _TypeLit, start, -1, perr)
}

func _TypeLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _TypeLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "TypeLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _TypeLit}
	// RefType/FuncType/ArrayType/StructType/UnionType
	{
		pos3 := pos
		// RefType
		if !_fail(parser, _RefTypeFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// FuncType
		if !_fail(parser, _FuncTypeFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// ArrayType
		if !_fail(parser, _ArrayTypeFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// StructType
		if !_fail(parser, _StructTypeFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// UnionType
		if !_fail(parser, _UnionTypeFail, errPos, failure, &pos) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypeLitAction(parser *_Parser, start int) (int, *Type) {
	dp := parser.deltaPos[start][_TypeLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _TypeLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// RefType/FuncType/ArrayType/StructType/UnionType
	{
		pos3 := pos
		var node2 Type
		// RefType
		if p, n := _RefTypeAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// FuncType
		if p, n := _FuncTypeAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// ArrayType
		if p, n := _ArrayTypeAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// StructType
		if p, n := _StructTypeAction(parser, pos); n == nil {
			goto fail7
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// UnionType
		if p, n := _UnionTypeAction(parser, pos); n == nil {
			goto fail8
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ArrayTypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _ArrayType, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "[" elem:Type _ "]"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// elem:Type
	{
		pos1 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _ArrayType, start, pos, perr)
fail:
	return _memoize(parser, _ArrayType, start, -1, perr)
}

func _ArrayTypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _ArrayType, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "ArrayType",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _ArrayType}
	// action
	// _ "[" elem:Type _ "]"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"[\"",
			})
		}
		goto fail
	}
	pos++
	// elem:Type
	{
		pos1 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"]\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ArrayTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [1]string
	use(labels)
	var label0 Type
	dp := parser.deltaPos[start][_ArrayType]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _ArrayType}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// action
	{
		start0 := pos
		// _ "[" elem:Type _ "]"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			goto fail
		}
		pos++
		// elem:Type
		{
			pos2 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			goto fail
		}
		pos++
		node = func(
			start, end int, elem Type) Type {
			return Type(&ArrayType{ElemType: elem, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _StructTypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _StructType, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// _ "[" _ "." _ "]" {…}/_ "[" fields:FieldDefs _ "]" {…}
	{
		pos3 := pos
		// action
		// _ "[" _ "." _ "]"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "[" fields:FieldDefs _ "]"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		// fields:FieldDefs
		{
			pos8 := pos
			// FieldDefs
			if !_accept(parser, _FieldDefsAccepts, &pos, &perr) {
				goto fail6
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _StructType, start, pos, perr)
fail:
	return _memoize(parser, _StructType, start, -1, perr)
}

func _StructTypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _StructType, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "StructType",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _StructType}
	// _ "[" _ "." _ "]" {…}/_ "[" fields:FieldDefs _ "]" {…}
	{
		pos3 := pos
		// action
		// _ "[" _ "." _ "]"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"[\"",
				})
			}
			goto fail4
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\".\"",
				})
			}
			goto fail4
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"]\"",
				})
			}
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "[" fields:FieldDefs _ "]"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"[\"",
				})
			}
			goto fail6
		}
		pos++
		// fields:FieldDefs
		{
			pos8 := pos
			// FieldDefs
			if !_fail(parser, _FieldDefsFail, errPos, failure, &pos) {
				goto fail6
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"]\"",
				})
			}
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _StructTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [1]string
	use(labels)
	var label0 []FieldDef
	dp := parser.deltaPos[start][_StructType]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _StructType}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// _ "[" _ "." _ "]" {…}/_ "[" fields:FieldDefs _ "]" {…}
	{
		pos3 := pos
		var node2 Type
		// action
		{
			start5 := pos
			// _ "[" _ "." _ "]"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "["
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
				goto fail4
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "."
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
				goto fail4
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "]"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
				goto fail4
			}
			pos++
			node = func(
				start, end int) Type {
				return Type(&StructType{L: l(parser, start, end)})
			}(
				start5, pos)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start8 := pos
			// _ "[" fields:FieldDefs _ "]"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "["
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
				goto fail7
			}
			pos++
			// fields:FieldDefs
			{
				pos10 := pos
				// FieldDefs
				if p, n := _FieldDefsAction(parser, pos); n == nil {
					goto fail7
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos10:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "]"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
				goto fail7
			}
			pos++
			node = func(
				start, end int, fields []FieldDef) Type {
				return Type(&StructType{Fields: fields, L: l(parser, start, end)})
			}(
				start8, pos, label0)
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _FieldDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:FieldId typ:Type
	// name:FieldId
	{
		pos1 := pos
		// FieldId
		if !_accept(parser, _FieldIdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// typ:Type
	{
		pos2 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _FieldDef, start, pos, perr)
fail:
	return _memoize(parser, _FieldDef, start, -1, perr)
}

func _FieldDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _FieldDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FieldDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FieldDef}
	// action
	// name:FieldId typ:Type
	// name:FieldId
	{
		pos1 := pos
		// FieldId
		if !_fail(parser, _FieldIdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// typ:Type
	{
		pos2 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FieldDefAction(parser *_Parser, start int) (int, *FieldDef) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Type
	dp := parser.deltaPos[start][_FieldDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FieldDef}
	n := parser.act[key]
	if n != nil {
		n := n.(FieldDef)
		return start + int(dp-1), &n
	}
	var node FieldDef
	pos := start
	// action
	{
		start0 := pos
		// name:FieldId typ:Type
		// name:FieldId
		{
			pos2 := pos
			// FieldId
			if p, n := _FieldIdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// typ:Type
		{
			pos3 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, name Ident, typ Type) FieldDef {
			return FieldDef{Name: name, Type: typ, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldDefsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _FieldDefs, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// f0:FieldDef fs:(_ "," f1:FieldDef {…})* (_ ",")?
	// f0:FieldDef
	{
		pos1 := pos
		// FieldDef
		if !_accept(parser, _FieldDefAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fs:(_ "," f1:FieldDef {…})*
	{
		pos2 := pos
		// (_ "," f1:FieldDef {…})*
		for {
			pos4 := pos
			// (_ "," f1:FieldDef {…})
			// action
			// _ "," f1:FieldDef
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// f1:FieldDef
			{
				pos8 := pos
				// FieldDef
				if !_accept(parser, _FieldDefAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _FieldDefs, start, pos, perr)
fail:
	return _memoize(parser, _FieldDefs, start, -1, perr)
}

func _FieldDefsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _FieldDefs, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FieldDefs",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FieldDefs}
	// action
	// f0:FieldDef fs:(_ "," f1:FieldDef {…})* (_ ",")?
	// f0:FieldDef
	{
		pos1 := pos
		// FieldDef
		if !_fail(parser, _FieldDefFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fs:(_ "," f1:FieldDef {…})*
	{
		pos2 := pos
		// (_ "," f1:FieldDef {…})*
		for {
			pos4 := pos
			// (_ "," f1:FieldDef {…})
			// action
			// _ "," f1:FieldDef
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// f1:FieldDef
			{
				pos8 := pos
				// FieldDef
				if !_fail(parser, _FieldDefFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FieldDefsAction(parser *_Parser, start int) (int, *[]FieldDef) {
	var labels [3]string
	use(labels)
	var label0 FieldDef
	var label1 FieldDef
	var label2 []FieldDef
	dp := parser.deltaPos[start][_FieldDefs]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FieldDefs}
	n := parser.act[key]
	if n != nil {
		n := n.([]FieldDef)
		return start + int(dp-1), &n
	}
	var node []FieldDef
	pos := start
	// action
	{
		start0 := pos
		// f0:FieldDef fs:(_ "," f1:FieldDef {…})* (_ ",")?
		// f0:FieldDef
		{
			pos2 := pos
			// FieldDef
			if p, n := _FieldDefAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// fs:(_ "," f1:FieldDef {…})*
		{
			pos3 := pos
			// (_ "," f1:FieldDef {…})*
			for {
				pos5 := pos
				var node6 FieldDef
				// (_ "," f1:FieldDef {…})
				// action
				{
					start8 := pos
					// _ "," f1:FieldDef
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// f1:FieldDef
					{
						pos10 := pos
						// FieldDef
						if p, n := _FieldDefAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, f0 FieldDef, f1 FieldDef) FieldDef {
						return FieldDef(f1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, f0 FieldDef, f1 FieldDef, fs []FieldDef) []FieldDef {
			return []FieldDef(append([]FieldDef{f0}, fs...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _UnionTypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _UnionType, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "[" cases:CaseDefs _ "]"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// cases:CaseDefs
	{
		pos1 := pos
		// CaseDefs
		if !_accept(parser, _CaseDefsAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _UnionType, start, pos, perr)
fail:
	return _memoize(parser, _UnionType, start, -1, perr)
}

func _UnionTypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _UnionType, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "UnionType",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _UnionType}
	// action
	// _ "[" cases:CaseDefs _ "]"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"[\"",
			})
		}
		goto fail
	}
	pos++
	// cases:CaseDefs
	{
		pos1 := pos
		// CaseDefs
		if !_fail(parser, _CaseDefsFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"]\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _UnionTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [1]string
	use(labels)
	var label0 []CaseDef
	dp := parser.deltaPos[start][_UnionType]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _UnionType}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// action
	{
		start0 := pos
		// _ "[" cases:CaseDefs _ "]"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			goto fail
		}
		pos++
		// cases:CaseDefs
		{
			pos2 := pos
			// CaseDefs
			if p, n := _CaseDefsAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			goto fail
		}
		pos++
		node = func(
			start, end int, cases []CaseDef) Type {
			return Type(&UnionType{Cases: cases, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CaseDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _CaseDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:CaseId t:Type?
	// name:CaseId
	{
		pos1 := pos
		// CaseId
		if !_accept(parser, _CaseIdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// t:Type?
	{
		pos2 := pos
		// Type?
		{
			pos4 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _CaseDef, start, pos, perr)
fail:
	return _memoize(parser, _CaseDef, start, -1, perr)
}

func _CaseDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _CaseDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CaseDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CaseDef}
	// action
	// name:CaseId t:Type?
	// name:CaseId
	{
		pos1 := pos
		// CaseId
		if !_fail(parser, _CaseIdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// t:Type?
	{
		pos2 := pos
		// Type?
		{
			pos4 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CaseDefAction(parser *_Parser, start int) (int, *CaseDef) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 *Type
	dp := parser.deltaPos[start][_CaseDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CaseDef}
	n := parser.act[key]
	if n != nil {
		n := n.(CaseDef)
		return start + int(dp-1), &n
	}
	var node CaseDef
	pos := start
	// action
	{
		start0 := pos
		// name:CaseId t:Type?
		// name:CaseId
		{
			pos2 := pos
			// CaseId
			if p, n := _CaseIdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// t:Type?
		{
			pos3 := pos
			// Type?
			{
				pos5 := pos
				label1 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail6
				} else {
					*label1 = *n
					pos = p
				}
				goto ok7
			fail6:
				label1 = nil
				pos = pos5
			ok7:
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, name Ident, t *Type) CaseDef {
			var typ Type
			if t != nil {
				typ = *t
			}
			return CaseDef{Name: name, Type: typ, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CaseDefsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _CaseDefs, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// c0:CaseDef cs:(_ "," c1:CaseDef {…})* (_ ",")?
	// c0:CaseDef
	{
		pos1 := pos
		// CaseDef
		if !_accept(parser, _CaseDefAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// cs:(_ "," c1:CaseDef {…})*
	{
		pos2 := pos
		// (_ "," c1:CaseDef {…})*
		for {
			pos4 := pos
			// (_ "," c1:CaseDef {…})
			// action
			// _ "," c1:CaseDef
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// c1:CaseDef
			{
				pos8 := pos
				// CaseDef
				if !_accept(parser, _CaseDefAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _CaseDefs, start, pos, perr)
fail:
	return _memoize(parser, _CaseDefs, start, -1, perr)
}

func _CaseDefsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _CaseDefs, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CaseDefs",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CaseDefs}
	// action
	// c0:CaseDef cs:(_ "," c1:CaseDef {…})* (_ ",")?
	// c0:CaseDef
	{
		pos1 := pos
		// CaseDef
		if !_fail(parser, _CaseDefFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// cs:(_ "," c1:CaseDef {…})*
	{
		pos2 := pos
		// (_ "," c1:CaseDef {…})*
		for {
			pos4 := pos
			// (_ "," c1:CaseDef {…})
			// action
			// _ "," c1:CaseDef
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// c1:CaseDef
			{
				pos8 := pos
				// CaseDef
				if !_fail(parser, _CaseDefFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CaseDefsAction(parser *_Parser, start int) (int, *[]CaseDef) {
	var labels [3]string
	use(labels)
	var label0 CaseDef
	var label1 CaseDef
	var label2 []CaseDef
	dp := parser.deltaPos[start][_CaseDefs]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CaseDefs}
	n := parser.act[key]
	if n != nil {
		n := n.([]CaseDef)
		return start + int(dp-1), &n
	}
	var node []CaseDef
	pos := start
	// action
	{
		start0 := pos
		// c0:CaseDef cs:(_ "," c1:CaseDef {…})* (_ ",")?
		// c0:CaseDef
		{
			pos2 := pos
			// CaseDef
			if p, n := _CaseDefAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// cs:(_ "," c1:CaseDef {…})*
		{
			pos3 := pos
			// (_ "," c1:CaseDef {…})*
			for {
				pos5 := pos
				var node6 CaseDef
				// (_ "," c1:CaseDef {…})
				// action
				{
					start8 := pos
					// _ "," c1:CaseDef
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// c1:CaseDef
					{
						pos10 := pos
						// CaseDef
						if p, n := _CaseDefAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, c0 CaseDef, c1 CaseDef) CaseDef {
						return CaseDef(c1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, c0 CaseDef, c1 CaseDef, cs []CaseDef) []CaseDef {
			return []CaseDef(append([]CaseDef{c0}, cs...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncTypeAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncType, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "(" ps:Types? _ ")" _ "{" r:Type? _ "}"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// ps:Types?
	{
		pos1 := pos
		// Types?
		{
			pos3 := pos
			// Types
			if !_accept(parser, _TypesAccepts, &pos, &perr) {
				goto fail4
			}
			goto ok5
		fail4:
			pos = pos3
		ok5:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "{"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// r:Type?
	{
		pos6 := pos
		// Type?
		{
			pos8 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail9
			}
			goto ok10
		fail9:
			pos = pos8
		ok10:
		}
		labels[1] = parser.text[pos6:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "}"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _FuncType, start, pos, perr)
fail:
	return _memoize(parser, _FuncType, start, -1, perr)
}

func _FuncTypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _FuncType, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncType",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncType}
	// action
	// _ "(" ps:Types? _ ")" _ "{" r:Type? _ "}"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"(\"",
			})
		}
		goto fail
	}
	pos++
	// ps:Types?
	{
		pos1 := pos
		// Types?
		{
			pos3 := pos
			// Types
			if !_fail(parser, _TypesFail, errPos, failure, &pos) {
				goto fail4
			}
			goto ok5
		fail4:
			pos = pos3
		ok5:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\")\"",
			})
		}
		goto fail
	}
	pos++
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "{"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"{\"",
			})
		}
		goto fail
	}
	pos++
	// r:Type?
	{
		pos6 := pos
		// Type?
		{
			pos8 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail9
			}
			goto ok10
		fail9:
			pos = pos8
		ok10:
		}
		labels[1] = parser.text[pos6:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "}"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"}\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [2]string
	use(labels)
	var label0 *[]Type
	var label1 *Type
	dp := parser.deltaPos[start][_FuncType]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncType}
	n := parser.act[key]
	if n != nil {
		n := n.(Type)
		return start + int(dp-1), &n
	}
	var node Type
	pos := start
	// action
	{
		start0 := pos
		// _ "(" ps:Types? _ ")" _ "{" r:Type? _ "}"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			goto fail
		}
		pos++
		// ps:Types?
		{
			pos2 := pos
			// Types?
			{
				pos4 := pos
				label0 = new([]Type)
				// Types
				if p, n := _TypesAction(parser, pos); n == nil {
					goto fail5
				} else {
					*label0 = *n
					pos = p
				}
				goto ok6
			fail5:
				label0 = nil
				pos = pos4
			ok6:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			goto fail
		}
		pos++
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			goto fail
		}
		pos++
		// r:Type?
		{
			pos7 := pos
			// Type?
			{
				pos9 := pos
				label1 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail10
				} else {
					*label1 = *n
					pos = p
				}
				goto ok11
			fail10:
				label1 = nil
				pos = pos9
			ok11:
			}
			labels[1] = parser.text[pos7:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			goto fail
		}
		pos++
		node = func(
			start, end int, ps *[]Type, r *Type) Type {
			var parms []Type
			if ps != nil {
				parms = *ps
			}
			var ret Type
			if r != nil {
				ret = *r
			}
			return Type(&FuncType{Parms: parms, Ret: ret, L: l(parser, start, end)})
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [6]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ reserved:("func"/"Func") name:FuncName _ "(" ps:FuncParms? _ ")" r:Type? (_ ":" iface:FuncDecls)? (_ "{" es:Exprs? _ "}")?
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// reserved:("func"/"Func")
	{
		pos1 := pos
		// ("func"/"Func")
		// "func"/"Func"
		{
			pos5 := pos
			// "func"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
				perr = _max(perr, pos)
				goto fail6
			}
			pos += 4
			goto ok2
		fail6:
			pos = pos5
			// "Func"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
				perr = _max(perr, pos)
				goto fail7
			}
			pos += 4
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// name:FuncName
	{
		pos8 := pos
		// FuncName
		if !_accept(parser, _FuncNameAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos8:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// ps:FuncParms?
	{
		pos9 := pos
		// FuncParms?
		{
			pos11 := pos
			// FuncParms
			if !_accept(parser, _FuncParmsAccepts, &pos, &perr) {
				goto fail12
			}
			goto ok13
		fail12:
			pos = pos11
		ok13:
		}
		labels[2] = parser.text[pos9:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// r:Type?
	{
		pos14 := pos
		// Type?
		{
			pos16 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail17
			}
			goto ok18
		fail17:
			pos = pos16
		ok18:
		}
		labels[3] = parser.text[pos14:pos]
	}
	// (_ ":" iface:FuncDecls)?
	{
		pos20 := pos
		// (_ ":" iface:FuncDecls)
		// _ ":" iface:FuncDecls
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail21
		}
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			perr = _max(perr, pos)
			goto fail21
		}
		pos++
		// iface:FuncDecls
		{
			pos23 := pos
			// FuncDecls
			if !_accept(parser, _FuncDeclsAccepts, &pos, &perr) {
				goto fail21
			}
			labels[4] = parser.text[pos23:pos]
		}
		goto ok24
	fail21:
		pos = pos20
	ok24:
	}
	// (_ "{" es:Exprs? _ "}")?
	{
		pos26 := pos
		// (_ "{" es:Exprs? _ "}")
		// _ "{" es:Exprs? _ "}"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail27
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			perr = _max(perr, pos)
			goto fail27
		}
		pos++
		// es:Exprs?
		{
			pos29 := pos
			// Exprs?
			{
				pos31 := pos
				// Exprs
				if !_accept(parser, _ExprsAccepts, &pos, &perr) {
					goto fail32
				}
				goto ok33
			fail32:
				pos = pos31
			ok33:
			}
			labels[5] = parser.text[pos29:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail27
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			perr = _max(perr, pos)
			goto fail27
		}
		pos++
		goto ok34
	fail27:
		pos = pos26
	ok34:
	}
	return _memoize(parser, _FuncDef, start, pos, perr)
fail:
	return _memoize(parser, _FuncDef, start, -1, perr)
}

func _FuncDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [6]string
	use(labels)
	pos, failure := _failMemo(parser, _FuncDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncDef}
	// action
	// _ reserved:("func"/"Func") name:FuncName _ "(" ps:FuncParms? _ ")" r:Type? (_ ":" iface:FuncDecls)? (_ "{" es:Exprs? _ "}")?
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// reserved:("func"/"Func")
	{
		pos1 := pos
		// ("func"/"Func")
		// "func"/"Func"
		{
			pos5 := pos
			// "func"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"func\"",
					})
				}
				goto fail6
			}
			pos += 4
			goto ok2
		fail6:
			pos = pos5
			// "Func"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"Func\"",
					})
				}
				goto fail7
			}
			pos += 4
			goto ok2
		fail7:
			pos = pos5
			goto fail
		ok2:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// name:FuncName
	{
		pos8 := pos
		// FuncName
		if !_fail(parser, _FuncNameFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos8:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"(\"",
			})
		}
		goto fail
	}
	pos++
	// ps:FuncParms?
	{
		pos9 := pos
		// FuncParms?
		{
			pos11 := pos
			// FuncParms
			if !_fail(parser, _FuncParmsFail, errPos, failure, &pos) {
				goto fail12
			}
			goto ok13
		fail12:
			pos = pos11
		ok13:
		}
		labels[2] = parser.text[pos9:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\")\"",
			})
		}
		goto fail
	}
	pos++
	// r:Type?
	{
		pos14 := pos
		// Type?
		{
			pos16 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail17
			}
			goto ok18
		fail17:
			pos = pos16
		ok18:
		}
		labels[3] = parser.text[pos14:pos]
	}
	// (_ ":" iface:FuncDecls)?
	{
		pos20 := pos
		// (_ ":" iface:FuncDecls)
		// _ ":" iface:FuncDecls
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail21
		}
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\":\"",
				})
			}
			goto fail21
		}
		pos++
		// iface:FuncDecls
		{
			pos23 := pos
			// FuncDecls
			if !_fail(parser, _FuncDeclsFail, errPos, failure, &pos) {
				goto fail21
			}
			labels[4] = parser.text[pos23:pos]
		}
		goto ok24
	fail21:
		pos = pos20
	ok24:
	}
	// (_ "{" es:Exprs? _ "}")?
	{
		pos26 := pos
		// (_ "{" es:Exprs? _ "}")
		// _ "{" es:Exprs? _ "}"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail27
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"{\"",
				})
			}
			goto fail27
		}
		pos++
		// es:Exprs?
		{
			pos29 := pos
			// Exprs?
			{
				pos31 := pos
				// Exprs
				if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
					goto fail32
				}
				goto ok33
			fail32:
				pos = pos31
			ok33:
			}
			labels[5] = parser.text[pos29:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail27
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"}\"",
				})
			}
			goto fail27
		}
		pos++
		goto ok34
	fail27:
		pos = pos26
	ok34:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncDefAction(parser *_Parser, start int) (int, *Def) {
	var labels [6]string
	use(labels)
	var label0 string
	var label1 Ident
	var label2 *[]FuncParm
	var label3 *Type
	var label4 []FuncDecl
	var label5 *[]Expr
	dp := parser.deltaPos[start][_FuncDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncDef}
	n := parser.act[key]
	if n != nil {
		n := n.(Def)
		return start + int(dp-1), &n
	}
	var node Def
	pos := start
	// action
	{
		start0 := pos
		// _ reserved:("func"/"Func") name:FuncName _ "(" ps:FuncParms? _ ")" r:Type? (_ ":" iface:FuncDecls)? (_ "{" es:Exprs? _ "}")?
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// reserved:("func"/"Func")
		{
			pos2 := pos
			// ("func"/"Func")
			// "func"/"Func"
			{
				pos6 := pos
				var node5 string
				// "func"
				if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
					goto fail7
				}
				label0 = parser.text[pos : pos+4]
				pos += 4
				goto ok3
			fail7:
				label0 = node5
				pos = pos6
				// "Func"
				if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
					goto fail8
				}
				label0 = parser.text[pos : pos+4]
				pos += 4
				goto ok3
			fail8:
				label0 = node5
				pos = pos6
				goto fail
			ok3:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// name:FuncName
		{
			pos9 := pos
			// FuncName
			if p, n := _FuncNameAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos9:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			goto fail
		}
		pos++
		// ps:FuncParms?
		{
			pos10 := pos
			// FuncParms?
			{
				pos12 := pos
				label2 = new([]FuncParm)
				// FuncParms
				if p, n := _FuncParmsAction(parser, pos); n == nil {
					goto fail13
				} else {
					*label2 = *n
					pos = p
				}
				goto ok14
			fail13:
				label2 = nil
				pos = pos12
			ok14:
			}
			labels[2] = parser.text[pos10:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			goto fail
		}
		pos++
		// r:Type?
		{
			pos15 := pos
			// Type?
			{
				pos17 := pos
				label3 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail18
				} else {
					*label3 = *n
					pos = p
				}
				goto ok19
			fail18:
				label3 = nil
				pos = pos17
			ok19:
			}
			labels[3] = parser.text[pos15:pos]
		}
		// (_ ":" iface:FuncDecls)?
		{
			pos21 := pos
			// (_ ":" iface:FuncDecls)
			// _ ":" iface:FuncDecls
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail22
			} else {
				pos = p
			}
			// ":"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
				goto fail22
			}
			pos++
			// iface:FuncDecls
			{
				pos24 := pos
				// FuncDecls
				if p, n := _FuncDeclsAction(parser, pos); n == nil {
					goto fail22
				} else {
					label4 = *n
					pos = p
				}
				labels[4] = parser.text[pos24:pos]
			}
			goto ok25
		fail22:
			pos = pos21
		ok25:
		}
		// (_ "{" es:Exprs? _ "}")?
		{
			pos27 := pos
			// (_ "{" es:Exprs? _ "}")
			// _ "{" es:Exprs? _ "}"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail28
			} else {
				pos = p
			}
			// "{"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
				goto fail28
			}
			pos++
			// es:Exprs?
			{
				pos30 := pos
				// Exprs?
				{
					pos32 := pos
					label5 = new([]Expr)
					// Exprs
					if p, n := _ExprsAction(parser, pos); n == nil {
						goto fail33
					} else {
						*label5 = *n
						pos = p
					}
					goto ok34
				fail33:
					label5 = nil
					pos = pos32
				ok34:
				}
				labels[5] = parser.text[pos30:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail28
			} else {
				pos = p
			}
			// "}"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
				goto fail28
			}
			pos++
			goto ok35
		fail28:
			pos = pos27
		ok35:
		}
		node = func(
			start, end int, es *[]Expr, iface []FuncDecl, name Ident, ps *[]FuncParm, r *Type, reserved string) Def {
			var parms []FuncParm
			if ps != nil {
				parms = *ps
			}
			var ret Type
			if r != nil {
				ret = *r
			}
			var exprs []Expr
			if es != nil {
				exprs = *es
			}
			return Def(&FuncDef{
				Exp:   reserved == "Func",
				Name:  name,
				Parms: parms,
				Ret:   ret,
				Iface: iface,
				Exprs: exprs,
				L:     l(parser, start, end),
			})
		}(
			start0, pos, label5, label4, label1, label2, label3, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncParmsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncParms, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// p0:FuncParm ps:(_ "," p1:FuncParm {…})* (_ ",")?
	// p0:FuncParm
	{
		pos1 := pos
		// FuncParm
		if !_accept(parser, _FuncParmAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ps:(_ "," p1:FuncParm {…})*
	{
		pos2 := pos
		// (_ "," p1:FuncParm {…})*
		for {
			pos4 := pos
			// (_ "," p1:FuncParm {…})
			// action
			// _ "," p1:FuncParm
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// p1:FuncParm
			{
				pos8 := pos
				// FuncParm
				if !_accept(parser, _FuncParmAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _FuncParms, start, pos, perr)
fail:
	return _memoize(parser, _FuncParms, start, -1, perr)
}

func _FuncParmsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _FuncParms, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncParms",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncParms}
	// action
	// p0:FuncParm ps:(_ "," p1:FuncParm {…})* (_ ",")?
	// p0:FuncParm
	{
		pos1 := pos
		// FuncParm
		if !_fail(parser, _FuncParmFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ps:(_ "," p1:FuncParm {…})*
	{
		pos2 := pos
		// (_ "," p1:FuncParm {…})*
		for {
			pos4 := pos
			// (_ "," p1:FuncParm {…})
			// action
			// _ "," p1:FuncParm
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// p1:FuncParm
			{
				pos8 := pos
				// FuncParm
				if !_fail(parser, _FuncParmFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncParmsAction(parser *_Parser, start int) (int, *[]FuncParm) {
	var labels [3]string
	use(labels)
	var label0 FuncParm
	var label1 FuncParm
	var label2 []FuncParm
	dp := parser.deltaPos[start][_FuncParms]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncParms}
	n := parser.act[key]
	if n != nil {
		n := n.([]FuncParm)
		return start + int(dp-1), &n
	}
	var node []FuncParm
	pos := start
	// action
	{
		start0 := pos
		// p0:FuncParm ps:(_ "," p1:FuncParm {…})* (_ ",")?
		// p0:FuncParm
		{
			pos2 := pos
			// FuncParm
			if p, n := _FuncParmAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ps:(_ "," p1:FuncParm {…})*
		{
			pos3 := pos
			// (_ "," p1:FuncParm {…})*
			for {
				pos5 := pos
				var node6 FuncParm
				// (_ "," p1:FuncParm {…})
				// action
				{
					start8 := pos
					// _ "," p1:FuncParm
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// p1:FuncParm
					{
						pos10 := pos
						// FuncParm
						if p, n := _FuncParmAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, p0 FuncParm, p1 FuncParm) FuncParm {
						return FuncParm(p1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, p0 FuncParm, p1 FuncParm, ps []FuncParm) []FuncParm {
			return []FuncParm(append([]FuncParm{p0}, ps...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncParmAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncParm, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Id typ:Type
	// name:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// typ:Type
	{
		pos2 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _FuncParm, start, pos, perr)
fail:
	return _memoize(parser, _FuncParm, start, -1, perr)
}

func _FuncParmFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _FuncParm, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncParm",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncParm}
	// action
	// name:Id typ:Type
	// name:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// typ:Type
	{
		pos2 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncParmAction(parser *_Parser, start int) (int, *FuncParm) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Type
	dp := parser.deltaPos[start][_FuncParm]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncParm}
	n := parser.act[key]
	if n != nil {
		n := n.(FuncParm)
		return start + int(dp-1), &n
	}
	var node FuncParm
	pos := start
	// action
	{
		start0 := pos
		// name:Id typ:Type
		// name:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// typ:Type
		{
			pos3 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, name Ident, typ Type) FuncParm {
			return FuncParm{Name: name, Type: typ, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncDeclAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncDecl, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:FuncName _ "(" ps:Types? _ ")" r:Type?
	// name:FuncName
	{
		pos1 := pos
		// FuncName
		if !_accept(parser, _FuncNameAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// ps:Types?
	{
		pos2 := pos
		// Types?
		{
			pos4 := pos
			// Types
			if !_accept(parser, _TypesAccepts, &pos, &perr) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// r:Type?
	{
		pos7 := pos
		// Type?
		{
			pos9 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail10
			}
			goto ok11
		fail10:
			pos = pos9
		ok11:
		}
		labels[2] = parser.text[pos7:pos]
	}
	return _memoize(parser, _FuncDecl, start, pos, perr)
fail:
	return _memoize(parser, _FuncDecl, start, -1, perr)
}

func _FuncDeclFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _FuncDecl, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncDecl",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncDecl}
	// action
	// name:FuncName _ "(" ps:Types? _ ")" r:Type?
	// name:FuncName
	{
		pos1 := pos
		// FuncName
		if !_fail(parser, _FuncNameFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"(\"",
			})
		}
		goto fail
	}
	pos++
	// ps:Types?
	{
		pos2 := pos
		// Types?
		{
			pos4 := pos
			// Types
			if !_fail(parser, _TypesFail, errPos, failure, &pos) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\")\"",
			})
		}
		goto fail
	}
	pos++
	// r:Type?
	{
		pos7 := pos
		// Type?
		{
			pos9 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail10
			}
			goto ok11
		fail10:
			pos = pos9
		ok11:
		}
		labels[2] = parser.text[pos7:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncDeclAction(parser *_Parser, start int) (int, *FuncDecl) {
	var labels [3]string
	use(labels)
	var label0 Ident
	var label1 *[]Type
	var label2 *Type
	dp := parser.deltaPos[start][_FuncDecl]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncDecl}
	n := parser.act[key]
	if n != nil {
		n := n.(FuncDecl)
		return start + int(dp-1), &n
	}
	var node FuncDecl
	pos := start
	// action
	{
		start0 := pos
		// name:FuncName _ "(" ps:Types? _ ")" r:Type?
		// name:FuncName
		{
			pos2 := pos
			// FuncName
			if p, n := _FuncNameAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			goto fail
		}
		pos++
		// ps:Types?
		{
			pos3 := pos
			// Types?
			{
				pos5 := pos
				label1 = new([]Type)
				// Types
				if p, n := _TypesAction(parser, pos); n == nil {
					goto fail6
				} else {
					*label1 = *n
					pos = p
				}
				goto ok7
			fail6:
				label1 = nil
				pos = pos5
			ok7:
			}
			labels[1] = parser.text[pos3:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			goto fail
		}
		pos++
		// r:Type?
		{
			pos8 := pos
			// Type?
			{
				pos10 := pos
				label2 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail11
				} else {
					*label2 = *n
					pos = p
				}
				goto ok12
			fail11:
				label2 = nil
				pos = pos10
			ok12:
			}
			labels[2] = parser.text[pos8:pos]
		}
		node = func(
			start, end int, name Ident, ps *[]Type, r *Type) FuncDecl {
			var parms []Type
			if ps != nil {
				parms = *ps
			}
			var ret Type
			if r != nil {
				ret = *r
			}
			return FuncDecl{Name: name, Parms: parms, Ret: ret, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncDeclsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncDecls, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// fd0:FuncDecl fds:(_ "," fd1:FuncDecl {…})* (_ ",")?
	// fd0:FuncDecl
	{
		pos1 := pos
		// FuncDecl
		if !_accept(parser, _FuncDeclAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fds:(_ "," fd1:FuncDecl {…})*
	{
		pos2 := pos
		// (_ "," fd1:FuncDecl {…})*
		for {
			pos4 := pos
			// (_ "," fd1:FuncDecl {…})
			// action
			// _ "," fd1:FuncDecl
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// fd1:FuncDecl
			{
				pos8 := pos
				// FuncDecl
				if !_accept(parser, _FuncDeclAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _FuncDecls, start, pos, perr)
fail:
	return _memoize(parser, _FuncDecls, start, -1, perr)
}

func _FuncDeclsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _FuncDecls, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncDecls",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncDecls}
	// action
	// fd0:FuncDecl fds:(_ "," fd1:FuncDecl {…})* (_ ",")?
	// fd0:FuncDecl
	{
		pos1 := pos
		// FuncDecl
		if !_fail(parser, _FuncDeclFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fds:(_ "," fd1:FuncDecl {…})*
	{
		pos2 := pos
		// (_ "," fd1:FuncDecl {…})*
		for {
			pos4 := pos
			// (_ "," fd1:FuncDecl {…})
			// action
			// _ "," fd1:FuncDecl
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// fd1:FuncDecl
			{
				pos8 := pos
				// FuncDecl
				if !_fail(parser, _FuncDeclFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncDeclsAction(parser *_Parser, start int) (int, *[]FuncDecl) {
	var labels [3]string
	use(labels)
	var label0 FuncDecl
	var label1 FuncDecl
	var label2 []FuncDecl
	dp := parser.deltaPos[start][_FuncDecls]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncDecls}
	n := parser.act[key]
	if n != nil {
		n := n.([]FuncDecl)
		return start + int(dp-1), &n
	}
	var node []FuncDecl
	pos := start
	// action
	{
		start0 := pos
		// fd0:FuncDecl fds:(_ "," fd1:FuncDecl {…})* (_ ",")?
		// fd0:FuncDecl
		{
			pos2 := pos
			// FuncDecl
			if p, n := _FuncDeclAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// fds:(_ "," fd1:FuncDecl {…})*
		{
			pos3 := pos
			// (_ "," fd1:FuncDecl {…})*
			for {
				pos5 := pos
				var node6 FuncDecl
				// (_ "," fd1:FuncDecl {…})
				// action
				{
					start8 := pos
					// _ "," fd1:FuncDecl
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// fd1:FuncDecl
					{
						pos10 := pos
						// FuncDecl
						if p, n := _FuncDeclAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, fd0 FuncDecl, fd1 FuncDecl) FuncDecl {
						return FuncDecl(fd1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, fd0 FuncDecl, fd1 FuncDecl, fds []FuncDecl) []FuncDecl {
			return []FuncDecl(append([]FuncDecl{fd0}, fds...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TestDefAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _TestDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "test" name:Id _ "{" es:Exprs? _ "}"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "test"
	if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
		perr = _max(perr, pos)
		goto fail
	}
	pos += 4
	// name:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "{"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// es:Exprs?
	{
		pos2 := pos
		// Exprs?
		{
			pos4 := pos
			// Exprs
			if !_accept(parser, _ExprsAccepts, &pos, &perr) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "}"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _TestDef, start, pos, perr)
fail:
	return _memoize(parser, _TestDef, start, -1, perr)
}

func _TestDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _TestDef, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "TestDef",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _TestDef}
	// action
	// _ "test" name:Id _ "{" es:Exprs? _ "}"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "test"
	if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"test\"",
			})
		}
		goto fail
	}
	pos += 4
	// name:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "{"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"{\"",
			})
		}
		goto fail
	}
	pos++
	// es:Exprs?
	{
		pos2 := pos
		// Exprs?
		{
			pos4 := pos
			// Exprs
			if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "}"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"}\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TestDefAction(parser *_Parser, start int) (int, *Def) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 *[]Expr
	dp := parser.deltaPos[start][_TestDef]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _TestDef}
	n := parser.act[key]
	if n != nil {
		n := n.(Def)
		return start + int(dp-1), &n
	}
	var node Def
	pos := start
	// action
	{
		start0 := pos
		// _ "test" name:Id _ "{" es:Exprs? _ "}"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "test"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
			goto fail
		}
		pos += 4
		// name:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			goto fail
		}
		pos++
		// es:Exprs?
		{
			pos3 := pos
			// Exprs?
			{
				pos5 := pos
				label1 = new([]Expr)
				// Exprs
				if p, n := _ExprsAction(parser, pos); n == nil {
					goto fail6
				} else {
					*label1 = *n
					pos = p
				}
				goto ok7
			fail6:
				label1 = nil
				pos = pos5
			ok7:
			}
			labels[1] = parser.text[pos3:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			goto fail
		}
		pos++
		node = func(
			start, end int, es *[]Expr, name Ident) Def {
			var exprs []Expr
			if es != nil {
				exprs = *es
			}
			return Def(&TestDef{Name: name, Exprs: exprs, L: l(parser, start, end)})
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ExprAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Expr, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Asgn/AsgnArg
	{
		pos3 := pos
		// Asgn
		if !_accept(parser, _AsgnAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// AsgnArg
		if !_accept(parser, _AsgnArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Expr, start, pos, perr)
fail:
	return _memoize(parser, _Expr, start, -1, perr)
}

func _ExprFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Expr, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Expr",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Expr}
	// Asgn/AsgnArg
	{
		pos3 := pos
		// Asgn
		if !_fail(parser, _AsgnFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// AsgnArg
		if !_fail(parser, _AsgnArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ExprAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Expr]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Expr}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Asgn/AsgnArg
	{
		pos3 := pos
		var node2 Expr
		// Asgn
		if p, n := _AsgnAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// AsgnArg
		if p, n := _AsgnArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ExprsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Exprs, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// e0:Expr es:(_ "," e1:Expr {…})* (_ ",")?
	// e0:Expr
	{
		pos1 := pos
		// Expr
		if !_accept(parser, _ExprAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// es:(_ "," e1:Expr {…})*
	{
		pos2 := pos
		// (_ "," e1:Expr {…})*
		for {
			pos4 := pos
			// (_ "," e1:Expr {…})
			// action
			// _ "," e1:Expr
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// e1:Expr
			{
				pos8 := pos
				// Expr
				if !_accept(parser, _ExprAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _Exprs, start, pos, perr)
fail:
	return _memoize(parser, _Exprs, start, -1, perr)
}

func _ExprsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _Exprs, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Exprs",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Exprs}
	// action
	// e0:Expr es:(_ "," e1:Expr {…})* (_ ",")?
	// e0:Expr
	{
		pos1 := pos
		// Expr
		if !_fail(parser, _ExprFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// es:(_ "," e1:Expr {…})*
	{
		pos2 := pos
		// (_ "," e1:Expr {…})*
		for {
			pos4 := pos
			// (_ "," e1:Expr {…})
			// action
			// _ "," e1:Expr
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// e1:Expr
			{
				pos8 := pos
				// Expr
				if !_fail(parser, _ExprFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ExprsAction(parser *_Parser, start int) (int, *[]Expr) {
	var labels [3]string
	use(labels)
	var label0 Expr
	var label1 Expr
	var label2 []Expr
	dp := parser.deltaPos[start][_Exprs]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Exprs}
	n := parser.act[key]
	if n != nil {
		n := n.([]Expr)
		return start + int(dp-1), &n
	}
	var node []Expr
	pos := start
	// action
	{
		start0 := pos
		// e0:Expr es:(_ "," e1:Expr {…})* (_ ",")?
		// e0:Expr
		{
			pos2 := pos
			// Expr
			if p, n := _ExprAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// es:(_ "," e1:Expr {…})*
		{
			pos3 := pos
			// (_ "," e1:Expr {…})*
			for {
				pos5 := pos
				var node6 Expr
				// (_ "," e1:Expr {…})
				// action
				{
					start8 := pos
					// _ "," e1:Expr
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// e1:Expr
					{
						pos10 := pos
						// Expr
						if p, n := _ExprAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, e0 Expr, e1 Expr) Expr {
						return Expr(e1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, e0 Expr, e1 Expr, es []Expr) []Expr {
			return []Expr(append([]Expr{e0}, es...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _AsgnAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Asgn, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin<AsgnOp, AsgnArg>
	if !_accept(parser, _Bin__AsgnOp__AsgnArgAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _Asgn, start, pos, perr)
fail:
	return _memoize(parser, _Asgn, start, -1, perr)
}

func _AsgnFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Asgn, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Asgn",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Asgn}
	// Bin<AsgnOp, AsgnArg>
	if !_fail(parser, _Bin__AsgnOp__AsgnArgFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _AsgnAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Asgn]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Asgn}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin<AsgnOp, AsgnArg>
	if p, n := _Bin__AsgnOp__AsgnArgAction(parser, pos); n == nil {
		goto fail
	} else {
		node = *n
		pos = p
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _AsgnArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _AsgnArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// KwCall/KwArg
	{
		pos3 := pos
		// KwCall
		if !_accept(parser, _KwCallAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// KwArg
		if !_accept(parser, _KwArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _AsgnArg, start, pos, perr)
fail:
	return _memoize(parser, _AsgnArg, start, -1, perr)
}

func _AsgnArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _AsgnArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "AsgnArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _AsgnArg}
	// KwCall/KwArg
	{
		pos3 := pos
		// KwCall
		if !_fail(parser, _KwCallFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// KwArg
		if !_fail(parser, _KwArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _AsgnArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_AsgnArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _AsgnArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// KwCall/KwArg
	{
		pos3 := pos
		var node2 Expr
		// KwCall
		if p, n := _KwCallAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// KwArg
		if p, n := _KwArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _AsgnOpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _AsgnOp, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:":="
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:":="
	{
		pos1 := pos
		// ":="
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
			perr = _max(perr, pos)
			goto fail
		}
		pos += 2
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _AsgnOp, start, pos, perr)
fail:
	return _memoize(parser, _AsgnOp, start, -1, perr)
}

func _AsgnOpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _AsgnOp, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "AsgnOp",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _AsgnOp}
	// action
	// _ name:":="
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:":="
	{
		pos1 := pos
		// ":="
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\":=\"",
				})
			}
			goto fail
		}
		pos += 2
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "assignment operator"
	parser.fail[key] = failure
	return -1, failure
}

func _AsgnOpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_AsgnOp]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _AsgnOp}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:":="
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:":="
		{
			pos2 := pos
			// ":="
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != ":=" {
				goto fail
			}
			label0 = parser.text[pos : pos+2]
			pos += 2
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _KwCallAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _KwCall, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// nameArgs:NameArg<Kwd, KwArg>+
	{
		pos0 := pos
		// NameArg<Kwd, KwArg>+
		// NameArg<Kwd, KwArg>
		if !_accept(parser, _NameArg__Kwd__KwArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos2 := pos
			// NameArg<Kwd, KwArg>
			if !_accept(parser, _NameArg__Kwd__KwArgAccepts, &pos, &perr) {
				goto fail4
			}
			continue
		fail4:
			pos = pos2
			break
		}
		labels[0] = parser.text[pos0:pos]
	}
	return _memoize(parser, _KwCall, start, pos, perr)
fail:
	return _memoize(parser, _KwCall, start, -1, perr)
}

func _KwCallFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _KwCall, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "KwCall",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _KwCall}
	// action
	// nameArgs:NameArg<Kwd, KwArg>+
	{
		pos0 := pos
		// NameArg<Kwd, KwArg>+
		// NameArg<Kwd, KwArg>
		if !_fail(parser, _NameArg__Kwd__KwArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos2 := pos
			// NameArg<Kwd, KwArg>
			if !_fail(parser, _NameArg__Kwd__KwArgFail, errPos, failure, &pos) {
				goto fail4
			}
			continue
		fail4:
			pos = pos2
			break
		}
		labels[0] = parser.text[pos0:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _KwCallAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 []nameArg
	dp := parser.deltaPos[start][_KwCall]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _KwCall}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// nameArgs:NameArg<Kwd, KwArg>+
		{
			pos1 := pos
			// NameArg<Kwd, KwArg>+
			{
				var node4 nameArg
				// NameArg<Kwd, KwArg>
				if p, n := _NameArg__Kwd__KwArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node4 = *n
					pos = p
				}
				label0 = append(label0, node4)
			}
			for {
				pos3 := pos
				var node4 nameArg
				// NameArg<Kwd, KwArg>
				if p, n := _NameArg__Kwd__KwArgAction(parser, pos); n == nil {
					goto fail5
				} else {
					node4 = *n
					pos = p
				}
				label0 = append(label0, node4)
				continue
			fail5:
				pos = pos3
				break
			}
			labels[0] = parser.text[pos1:pos]
		}
		node = func(
			start, end int, nameArgs []nameArg) Expr {
			return Expr(nary(l(parser, start, end), nameArgs))
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _KwArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _KwArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Switch/SwitchArg
	{
		pos3 := pos
		// Switch
		if !_accept(parser, _SwitchAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// SwitchArg
		if !_accept(parser, _SwitchArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _KwArg, start, pos, perr)
fail:
	return _memoize(parser, _KwArg, start, -1, perr)
}

func _KwArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _KwArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "KwArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _KwArg}
	// Switch/SwitchArg
	{
		pos3 := pos
		// Switch
		if !_fail(parser, _SwitchFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// SwitchArg
		if !_fail(parser, _SwitchArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _KwArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_KwArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _KwArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Switch/SwitchArg
	{
		pos3 := pos
		var node2 Expr
		// Switch
		if p, n := _SwitchAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// SwitchArg
		if p, n := _SwitchArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _SwitchAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Switch, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:SwitchArg nameArgs:NameArg<CaseId, SwitchArg>+
	// arg0:SwitchArg
	{
		pos1 := pos
		// SwitchArg
		if !_accept(parser, _SwitchArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// nameArgs:NameArg<CaseId, SwitchArg>+
	{
		pos2 := pos
		// NameArg<CaseId, SwitchArg>+
		// NameArg<CaseId, SwitchArg>
		if !_accept(parser, _NameArg__CaseId__SwitchArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// NameArg<CaseId, SwitchArg>
			if !_accept(parser, _NameArg__CaseId__SwitchArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Switch, start, pos, perr)
fail:
	return _memoize(parser, _Switch, start, -1, perr)
}

func _SwitchFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Switch, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Switch",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Switch}
	// action
	// arg0:SwitchArg nameArgs:NameArg<CaseId, SwitchArg>+
	// arg0:SwitchArg
	{
		pos1 := pos
		// SwitchArg
		if !_fail(parser, _SwitchArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// nameArgs:NameArg<CaseId, SwitchArg>+
	{
		pos2 := pos
		// NameArg<CaseId, SwitchArg>+
		// NameArg<CaseId, SwitchArg>
		if !_fail(parser, _NameArg__CaseId__SwitchArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// NameArg<CaseId, SwitchArg>
			if !_fail(parser, _NameArg__CaseId__SwitchArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _SwitchAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []nameArg
	dp := parser.deltaPos[start][_Switch]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Switch}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:SwitchArg nameArgs:NameArg<CaseId, SwitchArg>+
		// arg0:SwitchArg
		{
			pos2 := pos
			// SwitchArg
			if p, n := _SwitchArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// nameArgs:NameArg<CaseId, SwitchArg>+
		{
			pos3 := pos
			// NameArg<CaseId, SwitchArg>+
			{
				var node6 nameArg
				// NameArg<CaseId, SwitchArg>
				if p, n := _NameArg__CaseId__SwitchArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 nameArg
				// NameArg<CaseId, SwitchArg>
				if p, n := _NameArg__CaseId__SwitchArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, nameArgs []nameArg) Expr {
			call := nary(l(parser, start, end), nameArgs)
			call.Args = append([]Expr{arg0}, call.Args...)
			return Expr(call)
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _SwitchArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _SwitchArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin5/Bin5Arg
	{
		pos3 := pos
		// Bin5
		if !_accept(parser, _Bin5Accepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin5Arg
		if !_accept(parser, _Bin5ArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _SwitchArg, start, pos, perr)
fail:
	return _memoize(parser, _SwitchArg, start, -1, perr)
}

func _SwitchArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _SwitchArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "SwitchArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _SwitchArg}
	// Bin5/Bin5Arg
	{
		pos3 := pos
		// Bin5
		if !_fail(parser, _Bin5Fail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin5Arg
		if !_fail(parser, _Bin5ArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _SwitchArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_SwitchArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _SwitchArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin5/Bin5Arg
	{
		pos3 := pos
		var node2 Expr
		// Bin5
		if p, n := _Bin5Action(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Bin5Arg
		if p, n := _Bin5ArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin5Accepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin5, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin<Bin5Op, Bin5Arg>
	if !_accept(parser, _Bin__Bin5Op__Bin5ArgAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _Bin5, start, pos, perr)
fail:
	return _memoize(parser, _Bin5, start, -1, perr)
}

func _Bin5Fail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin5, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin5",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin5}
	// Bin<Bin5Op, Bin5Arg>
	if !_fail(parser, _Bin__Bin5Op__Bin5ArgFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin5Action(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin5]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin5}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin<Bin5Op, Bin5Arg>
	if p, n := _Bin__Bin5Op__Bin5ArgAction(parser, pos); n == nil {
		goto fail
	} else {
		node = *n
		pos = p
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin5ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin5Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin4/Bin4Arg
	{
		pos3 := pos
		// Bin4
		if !_accept(parser, _Bin4Accepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin4Arg
		if !_accept(parser, _Bin4ArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Bin5Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin5Arg, start, -1, perr)
}

func _Bin5ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin5Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin5Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin5Arg}
	// Bin4/Bin4Arg
	{
		pos3 := pos
		// Bin4
		if !_fail(parser, _Bin4Fail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin4Arg
		if !_fail(parser, _Bin4ArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin5ArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin5Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin5Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin4/Bin4Arg
	{
		pos3 := pos
		var node2 Expr
		// Bin4
		if p, n := _Bin4Action(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Bin4Arg
		if p, n := _Bin4ArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin5OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin5Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:("|" O*)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:("|" O*)
	{
		pos1 := pos
		// ("|" O*)
		// "|" O*
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			perr = _max(perr, pos)
			goto fail
		}
		pos++
		// O*
		for {
			pos4 := pos
			// O
			if !_accept(parser, _OAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Bin5Op, start, pos, perr)
fail:
	return _memoize(parser, _Bin5Op, start, -1, perr)
}

func _Bin5OpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin5Op, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin5Op",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin5Op}
	// action
	// _ name:("|" O*)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:("|" O*)
	{
		pos1 := pos
		// ("|" O*)
		// "|" O*
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"|\"",
				})
			}
			goto fail
		}
		pos++
		// O*
		for {
			pos4 := pos
			// O
			if !_fail(parser, _OFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "operator"
	parser.fail[key] = failure
	return -1, failure
}

func _Bin5OpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Bin5Op]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin5Op}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:("|" O*)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:("|" O*)
		{
			pos2 := pos
			// ("|" O*)
			// "|" O*
			{
				var node3 string
				// "|"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
					goto fail
				}
				node3 = parser.text[pos : pos+1]
				pos++
				label0, node3 = label0+node3, ""
				// O*
				for {
					pos5 := pos
					var node6 string
					// O
					if p, n := _OAction(parser, pos); n == nil {
						goto fail7
					} else {
						node6 = *n
						pos = p
					}
					node3 += node6
					continue
				fail7:
					pos = pos5
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin4Accepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin4, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin<Bin4Op, Bin4Arg>
	if !_accept(parser, _Bin__Bin4Op__Bin4ArgAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _Bin4, start, pos, perr)
fail:
	return _memoize(parser, _Bin4, start, -1, perr)
}

func _Bin4Fail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin4, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin4",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin4}
	// Bin<Bin4Op, Bin4Arg>
	if !_fail(parser, _Bin__Bin4Op__Bin4ArgFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin4Action(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin4]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin4}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin<Bin4Op, Bin4Arg>
	if p, n := _Bin__Bin4Op__Bin4ArgAction(parser, pos); n == nil {
		goto fail
	} else {
		node = *n
		pos = p
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin4ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin4Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin3/Bin3Arg
	{
		pos3 := pos
		// Bin3
		if !_accept(parser, _Bin3Accepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin3Arg
		if !_accept(parser, _Bin3ArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Bin4Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin4Arg, start, -1, perr)
}

func _Bin4ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin4Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin4Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin4Arg}
	// Bin3/Bin3Arg
	{
		pos3 := pos
		// Bin3
		if !_fail(parser, _Bin3Fail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin3Arg
		if !_fail(parser, _Bin3ArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin4ArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin4Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin4Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin3/Bin3Arg
	{
		pos3 := pos
		var node2 Expr
		// Bin3
		if p, n := _Bin3Action(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Bin3Arg
		if p, n := _Bin3ArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin4OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin4Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:("&" O*)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:("&" O*)
	{
		pos1 := pos
		// ("&" O*)
		// "&" O*
		// "&"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "&" {
			perr = _max(perr, pos)
			goto fail
		}
		pos++
		// O*
		for {
			pos4 := pos
			// O
			if !_accept(parser, _OAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Bin4Op, start, pos, perr)
fail:
	return _memoize(parser, _Bin4Op, start, -1, perr)
}

func _Bin4OpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin4Op, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin4Op",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin4Op}
	// action
	// _ name:("&" O*)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:("&" O*)
	{
		pos1 := pos
		// ("&" O*)
		// "&" O*
		// "&"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "&" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"&\"",
				})
			}
			goto fail
		}
		pos++
		// O*
		for {
			pos4 := pos
			// O
			if !_fail(parser, _OFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "operator"
	parser.fail[key] = failure
	return -1, failure
}

func _Bin4OpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Bin4Op]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin4Op}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:("&" O*)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:("&" O*)
		{
			pos2 := pos
			// ("&" O*)
			// "&" O*
			{
				var node3 string
				// "&"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "&" {
					goto fail
				}
				node3 = parser.text[pos : pos+1]
				pos++
				label0, node3 = label0+node3, ""
				// O*
				for {
					pos5 := pos
					var node6 string
					// O
					if p, n := _OAction(parser, pos); n == nil {
						goto fail7
					} else {
						node6 = *n
						pos = p
					}
					node3 += node6
					continue
				fail7:
					pos = pos5
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin3Accepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin3, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin<Bin3Op, Bin3Arg>
	if !_accept(parser, _Bin__Bin3Op__Bin3ArgAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _Bin3, start, pos, perr)
fail:
	return _memoize(parser, _Bin3, start, -1, perr)
}

func _Bin3Fail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin3, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin3",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin3}
	// Bin<Bin3Op, Bin3Arg>
	if !_fail(parser, _Bin__Bin3Op__Bin3ArgFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin3Action(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin3]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin3}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin<Bin3Op, Bin3Arg>
	if p, n := _Bin__Bin3Op__Bin3ArgAction(parser, pos); n == nil {
		goto fail
	} else {
		node = *n
		pos = p
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin3ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin3Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin2/Bin2Arg
	{
		pos3 := pos
		// Bin2
		if !_accept(parser, _Bin2Accepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin2Arg
		if !_accept(parser, _Bin2ArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Bin3Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin3Arg, start, -1, perr)
}

func _Bin3ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin3Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin3Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin3Arg}
	// Bin2/Bin2Arg
	{
		pos3 := pos
		// Bin2
		if !_fail(parser, _Bin2Fail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin2Arg
		if !_fail(parser, _Bin2ArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin3ArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin3Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin3Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin2/Bin2Arg
	{
		pos3 := pos
		var node2 Expr
		// Bin2
		if p, n := _Bin2Action(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Bin2Arg
		if p, n := _Bin2ArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin3OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin3Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:([=!<>] O*)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:([=!<>] O*)
	{
		pos1 := pos
		// ([=!<>] O*)
		// [=!<>] O*
		// [=!<>]
		if r, w := _next(parser, pos); r != '=' && r != '!' && r != '<' && r != '>' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		// O*
		for {
			pos4 := pos
			// O
			if !_accept(parser, _OAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Bin3Op, start, pos, perr)
fail:
	return _memoize(parser, _Bin3Op, start, -1, perr)
}

func _Bin3OpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin3Op, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin3Op",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin3Op}
	// action
	// _ name:([=!<>] O*)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:([=!<>] O*)
	{
		pos1 := pos
		// ([=!<>] O*)
		// [=!<>] O*
		// [=!<>]
		if r, w := _next(parser, pos); r != '=' && r != '!' && r != '<' && r != '>' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[=!<>]",
				})
			}
			goto fail
		} else {
			pos += w
		}
		// O*
		for {
			pos4 := pos
			// O
			if !_fail(parser, _OFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "operator 3"
	parser.fail[key] = failure
	return -1, failure
}

func _Bin3OpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Bin3Op]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin3Op}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:([=!<>] O*)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:([=!<>] O*)
		{
			pos2 := pos
			// ([=!<>] O*)
			// [=!<>] O*
			{
				var node3 string
				// [=!<>]
				if r, w := _next(parser, pos); r != '=' && r != '!' && r != '<' && r != '>' {
					goto fail
				} else {
					node3 = parser.text[pos : pos+w]
					pos += w
				}
				label0, node3 = label0+node3, ""
				// O*
				for {
					pos5 := pos
					var node6 string
					// O
					if p, n := _OAction(parser, pos); n == nil {
						goto fail7
					} else {
						node6 = *n
						pos = p
					}
					node3 += node6
					continue
				fail7:
					pos = pos5
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin2Accepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin2, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin<Bin2Op, Bin2Arg>
	if !_accept(parser, _Bin__Bin2Op__Bin2ArgAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _Bin2, start, pos, perr)
fail:
	return _memoize(parser, _Bin2, start, -1, perr)
}

func _Bin2Fail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin2, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin2",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin2}
	// Bin<Bin2Op, Bin2Arg>
	if !_fail(parser, _Bin__Bin2Op__Bin2ArgFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin2Action(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin2]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin2}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin<Bin2Op, Bin2Arg>
	if p, n := _Bin__Bin2Op__Bin2ArgAction(parser, pos); n == nil {
		goto fail
	} else {
		node = *n
		pos = p
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin2ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin2Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin1/Bin1Arg
	{
		pos3 := pos
		// Bin1
		if !_accept(parser, _Bin1Accepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin1Arg
		if !_accept(parser, _Bin1ArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Bin2Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin2Arg, start, -1, perr)
}

func _Bin2ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin2Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin2Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin2Arg}
	// Bin1/Bin1Arg
	{
		pos3 := pos
		// Bin1
		if !_fail(parser, _Bin1Fail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Bin1Arg
		if !_fail(parser, _Bin1ArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin2ArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin2Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin2Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin1/Bin1Arg
	{
		pos3 := pos
		var node2 Expr
		// Bin1
		if p, n := _Bin1Action(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Bin1Arg
		if p, n := _Bin1ArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin2OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin2Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:([+\-\^] O*)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:([+\-\^] O*)
	{
		pos1 := pos
		// ([+\-\^] O*)
		// [+\-\^] O*
		// [+\-\^]
		if r, w := _next(parser, pos); r != '+' && r != '-' && r != '^' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		// O*
		for {
			pos4 := pos
			// O
			if !_accept(parser, _OAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Bin2Op, start, pos, perr)
fail:
	return _memoize(parser, _Bin2Op, start, -1, perr)
}

func _Bin2OpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin2Op, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin2Op",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin2Op}
	// action
	// _ name:([+\-\^] O*)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:([+\-\^] O*)
	{
		pos1 := pos
		// ([+\-\^] O*)
		// [+\-\^] O*
		// [+\-\^]
		if r, w := _next(parser, pos); r != '+' && r != '-' && r != '^' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[+\\-\\^]",
				})
			}
			goto fail
		} else {
			pos += w
		}
		// O*
		for {
			pos4 := pos
			// O
			if !_fail(parser, _OFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "operator"
	parser.fail[key] = failure
	return -1, failure
}

func _Bin2OpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Bin2Op]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin2Op}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:([+\-\^] O*)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:([+\-\^] O*)
		{
			pos2 := pos
			// ([+\-\^] O*)
			// [+\-\^] O*
			{
				var node3 string
				// [+\-\^]
				if r, w := _next(parser, pos); r != '+' && r != '-' && r != '^' {
					goto fail
				} else {
					node3 = parser.text[pos : pos+w]
					pos += w
				}
				label0, node3 = label0+node3, ""
				// O*
				for {
					pos5 := pos
					var node6 string
					// O
					if p, n := _OAction(parser, pos); n == nil {
						goto fail7
					} else {
						node6 = *n
						pos = p
					}
					node3 += node6
					continue
				fail7:
					pos = pos5
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin1Accepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin1, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Bin<Bin1Op, Bin1Arg>
	if !_accept(parser, _Bin__Bin1Op__Bin1ArgAccepts, &pos, &perr) {
		goto fail
	}
	return _memoize(parser, _Bin1, start, pos, perr)
fail:
	return _memoize(parser, _Bin1, start, -1, perr)
}

func _Bin1Fail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin1, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin1",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin1}
	// Bin<Bin1Op, Bin1Arg>
	if !_fail(parser, _Bin__Bin1Op__Bin1ArgFail, errPos, failure, &pos) {
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin1Action(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin1]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin1}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Bin<Bin1Op, Bin1Arg>
	if p, n := _Bin__Bin1Op__Bin1ArgAction(parser, pos); n == nil {
		goto fail
	} else {
		node = *n
		pos = p
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin1ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Bin1Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Cvt/UnArg
	{
		pos3 := pos
		// Cvt
		if !_accept(parser, _CvtAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// UnArg
		if !_accept(parser, _UnArgAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Bin1Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin1Arg, start, -1, perr)
}

func _Bin1ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Bin1Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin1Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin1Arg}
	// Cvt/UnArg
	{
		pos3 := pos
		// Cvt
		if !_fail(parser, _CvtFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// UnArg
		if !_fail(parser, _UnArgFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin1ArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_Bin1Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin1Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Cvt/UnArg
	{
		pos3 := pos
		var node2 Expr
		// Cvt
		if p, n := _CvtAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// UnArg
		if p, n := _UnArgAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin1OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin1Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:([*/%] O*)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:([*/%] O*)
	{
		pos1 := pos
		// ([*/%] O*)
		// [*/%] O*
		// [*/%]
		if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		// O*
		for {
			pos4 := pos
			// O
			if !_accept(parser, _OAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Bin1Op, start, pos, perr)
fail:
	return _memoize(parser, _Bin1Op, start, -1, perr)
}

func _Bin1OpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin1Op, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin1Op",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin1Op}
	// action
	// _ name:([*/%] O*)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:([*/%] O*)
	{
		pos1 := pos
		// ([*/%] O*)
		// [*/%] O*
		// [*/%]
		if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[*/%]",
				})
			}
			goto fail
		} else {
			pos += w
		}
		// O*
		for {
			pos4 := pos
			// O
			if !_fail(parser, _OFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "operator"
	parser.fail[key] = failure
	return -1, failure
}

func _Bin1OpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Bin1Op]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin1Op}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:([*/%] O*)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:([*/%] O*)
		{
			pos2 := pos
			// ([*/%] O*)
			// [*/%] O*
			{
				var node3 string
				// [*/%]
				if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' {
					goto fail
				} else {
					node3 = parser.text[pos : pos+w]
					pos += w
				}
				label0, node3 = label0+node3, ""
				// O*
				for {
					pos5 := pos
					var node6 string
					// O
					if p, n := _OAction(parser, pos); n == nil {
						goto fail7
					} else {
						node6 = *n
						pos = p
					}
					node3 += node6
					continue
				fail7:
					pos = pos5
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CvtAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Cvt, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// typ:Type (Space/Cmnt) ":" (Space/Cmnt) expr:UnArg
	// typ:Type
	{
		pos1 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// (Space/Cmnt)
	// Space/Cmnt
	{
		pos5 := pos
		// Space
		if !_accept(parser, _SpaceAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok2
	fail6:
		pos = pos5
		// Cmnt
		if !_accept(parser, _CmntAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok2
	fail7:
		pos = pos5
		goto fail
	ok2:
	}
	// ":"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// (Space/Cmnt)
	// Space/Cmnt
	{
		pos11 := pos
		// Space
		if !_accept(parser, _SpaceAccepts, &pos, &perr) {
			goto fail12
		}
		goto ok8
	fail12:
		pos = pos11
		// Cmnt
		if !_accept(parser, _CmntAccepts, &pos, &perr) {
			goto fail13
		}
		goto ok8
	fail13:
		pos = pos11
		goto fail
	ok8:
	}
	// expr:UnArg
	{
		pos14 := pos
		// UnArg
		if !_accept(parser, _UnArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos14:pos]
	}
	return _memoize(parser, _Cvt, start, pos, perr)
fail:
	return _memoize(parser, _Cvt, start, -1, perr)
}

func _CvtFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Cvt, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Cvt",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Cvt}
	// action
	// typ:Type (Space/Cmnt) ":" (Space/Cmnt) expr:UnArg
	// typ:Type
	{
		pos1 := pos
		// Type
		if !_fail(parser, _TypeFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// (Space/Cmnt)
	// Space/Cmnt
	{
		pos5 := pos
		// Space
		if !_fail(parser, _SpaceFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok2
	fail6:
		pos = pos5
		// Cmnt
		if !_fail(parser, _CmntFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok2
	fail7:
		pos = pos5
		goto fail
	ok2:
	}
	// ":"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\":\"",
			})
		}
		goto fail
	}
	pos++
	// (Space/Cmnt)
	// Space/Cmnt
	{
		pos11 := pos
		// Space
		if !_fail(parser, _SpaceFail, errPos, failure, &pos) {
			goto fail12
		}
		goto ok8
	fail12:
		pos = pos11
		// Cmnt
		if !_fail(parser, _CmntFail, errPos, failure, &pos) {
			goto fail13
		}
		goto ok8
	fail13:
		pos = pos11
		goto fail
	ok8:
	}
	// expr:UnArg
	{
		pos14 := pos
		// UnArg
		if !_fail(parser, _UnArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos14:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CvtAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Type
	var label1 Expr
	dp := parser.deltaPos[start][_Cvt]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Cvt}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// typ:Type (Space/Cmnt) ":" (Space/Cmnt) expr:UnArg
		// typ:Type
		{
			pos2 := pos
			// Type
			if p, n := _TypeAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// (Space/Cmnt)
		// Space/Cmnt
		{
			pos6 := pos
			// Space
			if p, n := _SpaceAction(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			goto ok3
		fail7:
			pos = pos6
			// Cmnt
			if p, n := _CmntAction(parser, pos); n == nil {
				goto fail8
			} else {
				pos = p
			}
			goto ok3
		fail8:
			pos = pos6
			goto fail
		ok3:
		}
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			goto fail
		}
		pos++
		// (Space/Cmnt)
		// Space/Cmnt
		{
			pos12 := pos
			// Space
			if p, n := _SpaceAction(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			goto ok9
		fail13:
			pos = pos12
			// Cmnt
			if p, n := _CmntAction(parser, pos); n == nil {
				goto fail14
			} else {
				pos = p
			}
			goto ok9
		fail14:
			pos = pos12
			goto fail
		ok9:
		}
		// expr:UnArg
		{
			pos15 := pos
			// UnArg
			if p, n := _UnArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos15:pos]
		}
		node = func(
			start, end int, expr Expr, typ Type) Expr {
			return Expr(&Convert{Expr: expr, Type: typ, L: l(parser, start, end)})
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _UnAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Un, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// !NumLit name:Op arg:UnArg
	// !NumLit
	{
		pos2 := pos
		perr4 := perr
		// NumLit
		if !_accept(parser, _NumLitAccepts, &pos, &perr) {
			goto ok1
		}
		pos = pos2
		perr = _max(perr4, pos)
		goto fail
	ok1:
		pos = pos2
		perr = perr4
	}
	// name:Op
	{
		pos5 := pos
		// Op
		if !_accept(parser, _OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos5:pos]
	}
	// arg:UnArg
	{
		pos6 := pos
		// UnArg
		if !_accept(parser, _UnArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos6:pos]
	}
	return _memoize(parser, _Un, start, pos, perr)
fail:
	return _memoize(parser, _Un, start, -1, perr)
}

func _UnFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Un, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Un",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Un}
	// action
	// !NumLit name:Op arg:UnArg
	// !NumLit
	{
		pos2 := pos
		nkids3 := len(failure.Kids)
		// NumLit
		if !_fail(parser, _NumLitFail, errPos, failure, &pos) {
			goto ok1
		}
		pos = pos2
		failure.Kids = failure.Kids[:nkids3]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!NumLit",
			})
		}
		goto fail
	ok1:
		pos = pos2
		failure.Kids = failure.Kids[:nkids3]
	}
	// name:Op
	{
		pos5 := pos
		// Op
		if !_fail(parser, _OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos5:pos]
	}
	// arg:UnArg
	{
		pos6 := pos
		// UnArg
		if !_fail(parser, _UnArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos6:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _UnAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_Un]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Un}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// !NumLit name:Op arg:UnArg
		// !NumLit
		{
			pos3 := pos
			// NumLit
			if p, n := _NumLitAction(parser, pos); n == nil {
				goto ok2
			} else {
				pos = p
			}
			pos = pos3
			goto fail
		ok2:
			pos = pos3
		}
		// name:Op
		{
			pos6 := pos
			// Op
			if p, n := _OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos6:pos]
		}
		// arg:UnArg
		{
			pos7 := pos
			// UnArg
			if p, n := _UnArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos7:pos]
		}
		node = func(
			start, end int, arg Expr, name Ident) Expr {
			return Expr(&Call{Fun: name, Args: []Expr{arg}, L: l(parser, start, end)})
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:O+
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:O+
	{
		pos1 := pos
		// O+
		// O
		if !_accept(parser, _OAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos3 := pos
			// O
			if !_accept(parser, _OAccepts, &pos, &perr) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Op, start, pos, perr)
fail:
	return _memoize(parser, _Op, start, -1, perr)
}

func _OpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Op, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Op",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Op}
	// action
	// _ name:O+
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:O+
	{
		pos1 := pos
		// O+
		// O
		if !_fail(parser, _OFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos3 := pos
			// O
			if !_fail(parser, _OFail, errPos, failure, &pos) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "operator"
	parser.fail[key] = failure
	return -1, failure
}

func _OpAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Op]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Op}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ name:O+
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:O+
		{
			pos2 := pos
			// O+
			{
				var node5 string
				// O
				if p, n := _OAction(parser, pos); n == nil {
					goto fail
				} else {
					node5 = *n
					pos = p
				}
				label0 += node5
			}
			for {
				pos4 := pos
				var node5 string
				// O
				if p, n := _OAction(parser, pos); n == nil {
					goto fail6
				} else {
					node5 = *n
					pos = p
				}
				label0 += node5
				continue
			fail6:
				pos = pos4
				break
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _UnArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _UnArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Un/Pri/PriArg
	{
		pos3 := pos
		// Un
		if !_accept(parser, _UnAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Pri
		if !_accept(parser, _PriAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// PriArg
		if !_accept(parser, _PriArgAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _UnArg, start, pos, perr)
fail:
	return _memoize(parser, _UnArg, start, -1, perr)
}

func _UnArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _UnArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "UnArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _UnArg}
	// Un/Pri/PriArg
	{
		pos3 := pos
		// Un
		if !_fail(parser, _UnFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Pri
		if !_fail(parser, _PriFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// PriArg
		if !_fail(parser, _PriArgFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _UnArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_UnArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _UnArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Un/Pri/PriArg
	{
		pos3 := pos
		var node2 Expr
		// Un
		if p, n := _UnAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Pri
		if p, n := _PriAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// PriArg
		if p, n := _PriArgAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _PriAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Pri, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// head:PriArg tail:(Sel/Call/Idx)+
	// head:PriArg
	{
		pos1 := pos
		// PriArg
		if !_accept(parser, _PriArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// tail:(Sel/Call/Idx)+
	{
		pos2 := pos
		// (Sel/Call/Idx)+
		// (Sel/Call/Idx)
		// Sel/Call/Idx
		{
			pos10 := pos
			// Sel
			if !_accept(parser, _SelAccepts, &pos, &perr) {
				goto fail11
			}
			goto ok7
		fail11:
			pos = pos10
			// Call
			if !_accept(parser, _CallAccepts, &pos, &perr) {
				goto fail12
			}
			goto ok7
		fail12:
			pos = pos10
			// Idx
			if !_accept(parser, _IdxAccepts, &pos, &perr) {
				goto fail13
			}
			goto ok7
		fail13:
			pos = pos10
			goto fail
		ok7:
		}
		for {
			pos4 := pos
			// (Sel/Call/Idx)
			// Sel/Call/Idx
			{
				pos17 := pos
				// Sel
				if !_accept(parser, _SelAccepts, &pos, &perr) {
					goto fail18
				}
				goto ok14
			fail18:
				pos = pos17
				// Call
				if !_accept(parser, _CallAccepts, &pos, &perr) {
					goto fail19
				}
				goto ok14
			fail19:
				pos = pos17
				// Idx
				if !_accept(parser, _IdxAccepts, &pos, &perr) {
					goto fail20
				}
				goto ok14
			fail20:
				pos = pos17
				goto fail6
			ok14:
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Pri, start, pos, perr)
fail:
	return _memoize(parser, _Pri, start, -1, perr)
}

func _PriFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Pri, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Pri",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Pri}
	// action
	// head:PriArg tail:(Sel/Call/Idx)+
	// head:PriArg
	{
		pos1 := pos
		// PriArg
		if !_fail(parser, _PriArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// tail:(Sel/Call/Idx)+
	{
		pos2 := pos
		// (Sel/Call/Idx)+
		// (Sel/Call/Idx)
		// Sel/Call/Idx
		{
			pos10 := pos
			// Sel
			if !_fail(parser, _SelFail, errPos, failure, &pos) {
				goto fail11
			}
			goto ok7
		fail11:
			pos = pos10
			// Call
			if !_fail(parser, _CallFail, errPos, failure, &pos) {
				goto fail12
			}
			goto ok7
		fail12:
			pos = pos10
			// Idx
			if !_fail(parser, _IdxFail, errPos, failure, &pos) {
				goto fail13
			}
			goto ok7
		fail13:
			pos = pos10
			goto fail
		ok7:
		}
		for {
			pos4 := pos
			// (Sel/Call/Idx)
			// Sel/Call/Idx
			{
				pos17 := pos
				// Sel
				if !_fail(parser, _SelFail, errPos, failure, &pos) {
					goto fail18
				}
				goto ok14
			fail18:
				pos = pos17
				// Call
				if !_fail(parser, _CallFail, errPos, failure, &pos) {
					goto fail19
				}
				goto ok14
			fail19:
				pos = pos17
				// Idx
				if !_fail(parser, _IdxFail, errPos, failure, &pos) {
					goto fail20
				}
				goto ok14
			fail20:
				pos = pos17
				goto fail6
			ok14:
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _PriAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []primary
	dp := parser.deltaPos[start][_Pri]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Pri}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// head:PriArg tail:(Sel/Call/Idx)+
		// head:PriArg
		{
			pos2 := pos
			// PriArg
			if p, n := _PriArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// tail:(Sel/Call/Idx)+
		{
			pos3 := pos
			// (Sel/Call/Idx)+
			{
				var node6 primary
				// (Sel/Call/Idx)
				// Sel/Call/Idx
				{
					pos11 := pos
					var node10 primary
					// Sel
					if p, n := _SelAction(parser, pos); n == nil {
						goto fail12
					} else {
						node6 = *n
						pos = p
					}
					goto ok8
				fail12:
					node6 = node10
					pos = pos11
					// Call
					if p, n := _CallAction(parser, pos); n == nil {
						goto fail13
					} else {
						node6 = *n
						pos = p
					}
					goto ok8
				fail13:
					node6 = node10
					pos = pos11
					// Idx
					if p, n := _IdxAction(parser, pos); n == nil {
						goto fail14
					} else {
						node6 = *n
						pos = p
					}
					goto ok8
				fail14:
					node6 = node10
					pos = pos11
					goto fail
				ok8:
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 primary
				// (Sel/Call/Idx)
				// Sel/Call/Idx
				{
					pos18 := pos
					var node17 primary
					// Sel
					if p, n := _SelAction(parser, pos); n == nil {
						goto fail19
					} else {
						node6 = *n
						pos = p
					}
					goto ok15
				fail19:
					node6 = node17
					pos = pos18
					// Call
					if p, n := _CallAction(parser, pos); n == nil {
						goto fail20
					} else {
						node6 = *n
						pos = p
					}
					goto ok15
				fail20:
					node6 = node17
					pos = pos18
					// Idx
					if p, n := _IdxAction(parser, pos); n == nil {
						goto fail21
					} else {
						node6 = *n
						pos = p
					}
					goto ok15
				fail21:
					node6 = node17
					pos = pos18
					goto fail7
				ok15:
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, head Expr, tail []primary) Expr {
			return Expr(primaries(head, tail))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _SelAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Sel, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "." _ name:Id
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "."
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	return _memoize(parser, _Sel, start, pos, perr)
fail:
	return _memoize(parser, _Sel, start, -1, perr)
}

func _SelFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Sel, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Sel",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Sel}
	// action
	// _ "." _ name:Id
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "."
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\".\"",
			})
		}
		goto fail
	}
	pos++
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _SelAction(parser *_Parser, start int) (int, *primary) {
	var labels [1]string
	use(labels)
	var label0 Ident
	dp := parser.deltaPos[start][_Sel]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Sel}
	n := parser.act[key]
	if n != nil {
		n := n.(primary)
		return start + int(dp-1), &n
	}
	var node primary
	pos := start
	// action
	{
		start0 := pos
		// _ "." _ name:Id
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			goto fail
		}
		pos++
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, name Ident) primary {
			name.Name = "." + name.Name
			name.L = l(parser, start, end)
			return primary(sel{name: name, l: name.L})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CallAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Call, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "(" es:Exprs? _ ")"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// es:Exprs?
	{
		pos1 := pos
		// Exprs?
		{
			pos3 := pos
			// Exprs
			if !_accept(parser, _ExprsAccepts, &pos, &perr) {
				goto fail4
			}
			goto ok5
		fail4:
			pos = pos3
		ok5:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _Call, start, pos, perr)
fail:
	return _memoize(parser, _Call, start, -1, perr)
}

func _CallFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Call, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Call",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Call}
	// action
	// _ "(" es:Exprs? _ ")"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "("
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"(\"",
			})
		}
		goto fail
	}
	pos++
	// es:Exprs?
	{
		pos1 := pos
		// Exprs?
		{
			pos3 := pos
			// Exprs
			if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
				goto fail4
			}
			goto ok5
		fail4:
			pos = pos3
		ok5:
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// ")"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\")\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CallAction(parser *_Parser, start int) (int, *primary) {
	var labels [1]string
	use(labels)
	var label0 *[]Expr
	dp := parser.deltaPos[start][_Call]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Call}
	n := parser.act[key]
	if n != nil {
		n := n.(primary)
		return start + int(dp-1), &n
	}
	var node primary
	pos := start
	// action
	{
		start0 := pos
		// _ "(" es:Exprs? _ ")"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			goto fail
		}
		pos++
		// es:Exprs?
		{
			pos2 := pos
			// Exprs?
			{
				pos4 := pos
				label0 = new([]Expr)
				// Exprs
				if p, n := _ExprsAction(parser, pos); n == nil {
					goto fail5
				} else {
					*label0 = *n
					pos = p
				}
				goto ok6
			fail5:
				label0 = nil
				pos = pos4
			ok6:
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			goto fail
		}
		pos++
		node = func(
			start, end int, es *[]Expr) primary {
			var exprs []Expr
			if es != nil {
				exprs = *es
			}
			return primary(call{args: exprs, l: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _IdxAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Idx, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "[" exprs:Exprs _ "]"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// exprs:Exprs
	{
		pos1 := pos
		// Exprs
		if !_accept(parser, _ExprsAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _Idx, start, pos, perr)
fail:
	return _memoize(parser, _Idx, start, -1, perr)
}

func _IdxFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Idx, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Idx",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Idx}
	// action
	// _ "[" exprs:Exprs _ "]"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"[\"",
			})
		}
		goto fail
	}
	pos++
	// exprs:Exprs
	{
		pos1 := pos
		// Exprs
		if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"]\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _IdxAction(parser *_Parser, start int) (int, *primary) {
	var labels [1]string
	use(labels)
	var label0 []Expr
	dp := parser.deltaPos[start][_Idx]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Idx}
	n := parser.act[key]
	if n != nil {
		n := n.(primary)
		return start + int(dp-1), &n
	}
	var node primary
	pos := start
	// action
	{
		start0 := pos
		// _ "[" exprs:Exprs _ "]"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			goto fail
		}
		pos++
		// exprs:Exprs
		{
			pos2 := pos
			// Exprs
			if p, n := _ExprsAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			goto fail
		}
		pos++
		node = func(
			start, end int, exprs []Expr) primary {
			return primary(idx{args: exprs, l: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _PriArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _PriArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// BlkLit/ArrayLit/StructLit/UnionLit/CharLit/StrLit/NumLit/SubExpr/ModSel/id:Id {…}
	{
		pos3 := pos
		// BlkLit
		if !_accept(parser, _BlkLitAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// ArrayLit
		if !_accept(parser, _ArrayLitAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// StructLit
		if !_accept(parser, _StructLitAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// UnionLit
		if !_accept(parser, _UnionLitAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// CharLit
		if !_accept(parser, _CharLitAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		// StrLit
		if !_accept(parser, _StrLitAccepts, &pos, &perr) {
			goto fail9
		}
		goto ok0
	fail9:
		pos = pos3
		// NumLit
		if !_accept(parser, _NumLitAccepts, &pos, &perr) {
			goto fail10
		}
		goto ok0
	fail10:
		pos = pos3
		// SubExpr
		if !_accept(parser, _SubExprAccepts, &pos, &perr) {
			goto fail11
		}
		goto ok0
	fail11:
		pos = pos3
		// ModSel
		if !_accept(parser, _ModSelAccepts, &pos, &perr) {
			goto fail12
		}
		goto ok0
	fail12:
		pos = pos3
		// action
		// id:Id
		{
			pos14 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail13
			}
			labels[0] = parser.text[pos14:pos]
		}
		goto ok0
	fail13:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _PriArg, start, pos, perr)
fail:
	return _memoize(parser, _PriArg, start, -1, perr)
}

func _PriArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _PriArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "PriArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _PriArg}
	// BlkLit/ArrayLit/StructLit/UnionLit/CharLit/StrLit/NumLit/SubExpr/ModSel/id:Id {…}
	{
		pos3 := pos
		// BlkLit
		if !_fail(parser, _BlkLitFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// ArrayLit
		if !_fail(parser, _ArrayLitFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// StructLit
		if !_fail(parser, _StructLitFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// UnionLit
		if !_fail(parser, _UnionLitFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// CharLit
		if !_fail(parser, _CharLitFail, errPos, failure, &pos) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		// StrLit
		if !_fail(parser, _StrLitFail, errPos, failure, &pos) {
			goto fail9
		}
		goto ok0
	fail9:
		pos = pos3
		// NumLit
		if !_fail(parser, _NumLitFail, errPos, failure, &pos) {
			goto fail10
		}
		goto ok0
	fail10:
		pos = pos3
		// SubExpr
		if !_fail(parser, _SubExprFail, errPos, failure, &pos) {
			goto fail11
		}
		goto ok0
	fail11:
		pos = pos3
		// ModSel
		if !_fail(parser, _ModSelFail, errPos, failure, &pos) {
			goto fail12
		}
		goto ok0
	fail12:
		pos = pos3
		// action
		// id:Id
		{
			pos14 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail13
			}
			labels[0] = parser.text[pos14:pos]
		}
		goto ok0
	fail13:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _PriArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 Ident
	dp := parser.deltaPos[start][_PriArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _PriArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// BlkLit/ArrayLit/StructLit/UnionLit/CharLit/StrLit/NumLit/SubExpr/ModSel/id:Id {…}
	{
		pos3 := pos
		var node2 Expr
		// BlkLit
		if p, n := _BlkLitAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// ArrayLit
		if p, n := _ArrayLitAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// StructLit
		if p, n := _StructLitAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// UnionLit
		if p, n := _UnionLitAction(parser, pos); n == nil {
			goto fail7
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// CharLit
		if p, n := _CharLitAction(parser, pos); n == nil {
			goto fail8
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		// StrLit
		if p, n := _StrLitAction(parser, pos); n == nil {
			goto fail9
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail9:
		node = node2
		pos = pos3
		// NumLit
		if p, n := _NumLitAction(parser, pos); n == nil {
			goto fail10
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail10:
		node = node2
		pos = pos3
		// SubExpr
		if p, n := _SubExprAction(parser, pos); n == nil {
			goto fail11
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail11:
		node = node2
		pos = pos3
		// ModSel
		if p, n := _ModSelAction(parser, pos); n == nil {
			goto fail12
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail12:
		node = node2
		pos = pos3
		// action
		{
			start14 := pos
			// id:Id
			{
				pos15 := pos
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail13
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos15:pos]
			}
			node = func(
				start, end int, id Ident) Expr {
				return Expr(id)
			}(
				start14, pos, label0)
		}
		goto ok0
	fail13:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _SubExprAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _SubExpr, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// _ "(" expr:Expr _ ")" {…}/_ "(" id:(FieldId/IdxOp/Op/Kwds) _ ")" {…}
	{
		pos3 := pos
		// action
		// _ "(" expr:Expr _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// expr:Expr
		{
			pos6 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail4
			}
			labels[0] = parser.text[pos6:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "(" id:(FieldId/IdxOp/Op/Kwds) _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail7
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail7
		}
		pos++
		// id:(FieldId/IdxOp/Op/Kwds)
		{
			pos9 := pos
			// (FieldId/IdxOp/Op/Kwds)
			// FieldId/IdxOp/Op/Kwds
			{
				pos13 := pos
				// FieldId
				if !_accept(parser, _FieldIdAccepts, &pos, &perr) {
					goto fail14
				}
				goto ok10
			fail14:
				pos = pos13
				// IdxOp
				if !_accept(parser, _IdxOpAccepts, &pos, &perr) {
					goto fail15
				}
				goto ok10
			fail15:
				pos = pos13
				// Op
				if !_accept(parser, _OpAccepts, &pos, &perr) {
					goto fail16
				}
				goto ok10
			fail16:
				pos = pos13
				// Kwds
				if !_accept(parser, _KwdsAccepts, &pos, &perr) {
					goto fail17
				}
				goto ok10
			fail17:
				pos = pos13
				goto fail7
			ok10:
			}
			labels[1] = parser.text[pos9:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail7
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail7
		}
		pos++
		goto ok0
	fail7:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _SubExpr, start, pos, perr)
fail:
	return _memoize(parser, _SubExpr, start, -1, perr)
}

func _SubExprFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _SubExpr, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "SubExpr",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _SubExpr}
	// _ "(" expr:Expr _ ")" {…}/_ "(" id:(FieldId/IdxOp/Op/Kwds) _ ")" {…}
	{
		pos3 := pos
		// action
		// _ "(" expr:Expr _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail4
		}
		pos++
		// expr:Expr
		{
			pos6 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail4
			}
			labels[0] = parser.text[pos6:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "(" id:(FieldId/IdxOp/Op/Kwds) _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail7
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail7
		}
		pos++
		// id:(FieldId/IdxOp/Op/Kwds)
		{
			pos9 := pos
			// (FieldId/IdxOp/Op/Kwds)
			// FieldId/IdxOp/Op/Kwds
			{
				pos13 := pos
				// FieldId
				if !_fail(parser, _FieldIdFail, errPos, failure, &pos) {
					goto fail14
				}
				goto ok10
			fail14:
				pos = pos13
				// IdxOp
				if !_fail(parser, _IdxOpFail, errPos, failure, &pos) {
					goto fail15
				}
				goto ok10
			fail15:
				pos = pos13
				// Op
				if !_fail(parser, _OpFail, errPos, failure, &pos) {
					goto fail16
				}
				goto ok10
			fail16:
				pos = pos13
				// Kwds
				if !_fail(parser, _KwdsFail, errPos, failure, &pos) {
					goto fail17
				}
				goto ok10
			fail17:
				pos = pos13
				goto fail7
			ok10:
			}
			labels[1] = parser.text[pos9:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail7
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail7
		}
		pos++
		goto ok0
	fail7:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _SubExprAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 Ident
	dp := parser.deltaPos[start][_SubExpr]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _SubExpr}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// _ "(" expr:Expr _ ")" {…}/_ "(" id:(FieldId/IdxOp/Op/Kwds) _ ")" {…}
	{
		pos3 := pos
		var node2 Expr
		// action
		{
			start5 := pos
			// _ "(" expr:Expr _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail4
			}
			pos++
			// expr:Expr
			{
				pos7 := pos
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos7:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail4
			}
			pos++
			node = func(
				start, end int, expr Expr) Expr {
				return Expr(&SubExpr{Expr: expr, L: l(parser, start, end)})
			}(
				start5, pos, label0)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start9 := pos
			// _ "(" id:(FieldId/IdxOp/Op/Kwds) _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail8
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail8
			}
			pos++
			// id:(FieldId/IdxOp/Op/Kwds)
			{
				pos11 := pos
				// (FieldId/IdxOp/Op/Kwds)
				// FieldId/IdxOp/Op/Kwds
				{
					pos15 := pos
					var node14 Ident
					// FieldId
					if p, n := _FieldIdAction(parser, pos); n == nil {
						goto fail16
					} else {
						label1 = *n
						pos = p
					}
					goto ok12
				fail16:
					label1 = node14
					pos = pos15
					// IdxOp
					if p, n := _IdxOpAction(parser, pos); n == nil {
						goto fail17
					} else {
						label1 = *n
						pos = p
					}
					goto ok12
				fail17:
					label1 = node14
					pos = pos15
					// Op
					if p, n := _OpAction(parser, pos); n == nil {
						goto fail18
					} else {
						label1 = *n
						pos = p
					}
					goto ok12
				fail18:
					label1 = node14
					pos = pos15
					// Kwds
					if p, n := _KwdsAction(parser, pos); n == nil {
						goto fail19
					} else {
						label1 = *n
						pos = p
					}
					goto ok12
				fail19:
					label1 = node14
					pos = pos15
					goto fail8
				ok12:
				}
				labels[1] = parser.text[pos11:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail8
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail8
			}
			pos++
			node = func(
				start, end int, expr Expr, id Ident) Expr {
				return Expr(&SubExpr{Expr: id, L: l(parser, start, end)})
			}(
				start9, pos, label0, label1)
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ModSelAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [4]string
	use(labels)
	if dp, de, ok := _memo(parser, _ModSel, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// mod0:Id _ "#" name0:FuncName {…}/mod1:Id _ "#" _ "(" name1:FuncName _ ")" {…}
	{
		pos3 := pos
		// action
		// mod0:Id _ "#" name0:FuncName
		// mod0:Id
		{
			pos6 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail4
			}
			labels[0] = parser.text[pos6:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "#"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// name0:FuncName
		{
			pos7 := pos
			// FuncName
			if !_accept(parser, _FuncNameAccepts, &pos, &perr) {
				goto fail4
			}
			labels[1] = parser.text[pos7:pos]
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// mod1:Id _ "#" _ "(" name1:FuncName _ ")"
		// mod1:Id
		{
			pos10 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail8
			}
			labels[2] = parser.text[pos10:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail8
		}
		// "#"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail8
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos++
		// name1:FuncName
		{
			pos11 := pos
			// FuncName
			if !_accept(parser, _FuncNameAccepts, &pos, &perr) {
				goto fail8
			}
			labels[3] = parser.text[pos11:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail8
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos++
		goto ok0
	fail8:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _ModSel, start, pos, perr)
fail:
	return _memoize(parser, _ModSel, start, -1, perr)
}

func _ModSelFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [4]string
	use(labels)
	pos, failure := _failMemo(parser, _ModSel, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "ModSel",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _ModSel}
	// mod0:Id _ "#" name0:FuncName {…}/mod1:Id _ "#" _ "(" name1:FuncName _ ")" {…}
	{
		pos3 := pos
		// action
		// mod0:Id _ "#" name0:FuncName
		// mod0:Id
		{
			pos6 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail4
			}
			labels[0] = parser.text[pos6:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "#"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"#\"",
				})
			}
			goto fail4
		}
		pos++
		// name0:FuncName
		{
			pos7 := pos
			// FuncName
			if !_fail(parser, _FuncNameFail, errPos, failure, &pos) {
				goto fail4
			}
			labels[1] = parser.text[pos7:pos]
		}
		goto ok0
	fail4:
		pos = pos3
		// action
		// mod1:Id _ "#" _ "(" name1:FuncName _ ")"
		// mod1:Id
		{
			pos10 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail8
			}
			labels[2] = parser.text[pos10:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail8
		}
		// "#"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"#\"",
				})
			}
			goto fail8
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail8
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail8
		}
		pos++
		// name1:FuncName
		{
			pos11 := pos
			// FuncName
			if !_fail(parser, _FuncNameFail, errPos, failure, &pos) {
				goto fail8
			}
			labels[3] = parser.text[pos11:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail8
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail8
		}
		pos++
		goto ok0
	fail8:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ModSelAction(parser *_Parser, start int) (int, *Expr) {
	var labels [4]string
	use(labels)
	var label0 Ident
	var label1 Ident
	var label2 Ident
	var label3 Ident
	dp := parser.deltaPos[start][_ModSel]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _ModSel}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// mod0:Id _ "#" name0:FuncName {…}/mod1:Id _ "#" _ "(" name1:FuncName _ ")" {…}
	{
		pos3 := pos
		var node2 Expr
		// action
		{
			start5 := pos
			// mod0:Id _ "#" name0:FuncName
			// mod0:Id
			{
				pos7 := pos
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos7:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "#"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
				goto fail4
			}
			pos++
			// name0:FuncName
			{
				pos8 := pos
				// FuncName
				if p, n := _FuncNameAction(parser, pos); n == nil {
					goto fail4
				} else {
					label1 = *n
					pos = p
				}
				labels[1] = parser.text[pos8:pos]
			}
			node = func(
				start, end int, mod0 Ident, name0 Ident) Expr {
				return Expr(&ModSel{
					Mod:  mod0,
					Name: name0,
					L:    l(parser, start, end),
				})
			}(
				start5, pos, label0, label1)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start10 := pos
			// mod1:Id _ "#" _ "(" name1:FuncName _ ")"
			// mod1:Id
			{
				pos12 := pos
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail9
				} else {
					label2 = *n
					pos = p
				}
				labels[2] = parser.text[pos12:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail9
			} else {
				pos = p
			}
			// "#"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "#" {
				goto fail9
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail9
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail9
			}
			pos++
			// name1:FuncName
			{
				pos13 := pos
				// FuncName
				if p, n := _FuncNameAction(parser, pos); n == nil {
					goto fail9
				} else {
					label3 = *n
					pos = p
				}
				labels[3] = parser.text[pos13:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail9
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail9
			}
			pos++
			node = func(
				start, end int, mod0 Ident, mod1 Ident, name0 Ident, name1 Ident) Expr {
				return Expr(&ModSel{
					Mod:  mod1,
					Name: name1,
					L:    l(parser, start, end),
				})
			}(
				start10, pos, label0, label2, label1, label3)
		}
		goto ok0
	fail9:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FuncNameAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _FuncName, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// FieldId/IdxOp/Op/Kwds/Cases/Id
	{
		pos3 := pos
		// FieldId
		if !_accept(parser, _FieldIdAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// IdxOp
		if !_accept(parser, _IdxOpAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// Op
		if !_accept(parser, _OpAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// Kwds
		if !_accept(parser, _KwdsAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// Cases
		if !_accept(parser, _CasesAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail9
		}
		goto ok0
	fail9:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _FuncName, start, pos, perr)
fail:
	return _memoize(parser, _FuncName, start, -1, perr)
}

func _FuncNameFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _FuncName, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FuncName",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FuncName}
	// FieldId/IdxOp/Op/Kwds/Cases/Id
	{
		pos3 := pos
		// FieldId
		if !_fail(parser, _FieldIdFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// IdxOp
		if !_fail(parser, _IdxOpFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// Op
		if !_fail(parser, _OpFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// Kwds
		if !_fail(parser, _KwdsFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// Cases
		if !_fail(parser, _CasesFail, errPos, failure, &pos) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail9
		}
		goto ok0
	fail9:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncNameAction(parser *_Parser, start int) (int, *Ident) {
	dp := parser.deltaPos[start][_FuncName]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncName}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// FieldId/IdxOp/Op/Kwds/Cases/Id
	{
		pos3 := pos
		var node2 Ident
		// FieldId
		if p, n := _FieldIdAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// IdxOp
		if p, n := _IdxOpAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// Op
		if p, n := _OpAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// Kwds
		if p, n := _KwdsAction(parser, pos); n == nil {
			goto fail7
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// Cases
		if p, n := _CasesAction(parser, pos); n == nil {
			goto fail8
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		// Id
		if p, n := _IdAction(parser, pos); n == nil {
			goto fail9
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail9:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _IdxOpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _IdxOp, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "[" _ "]"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	perr = start
	return _memoize(parser, _IdxOp, start, pos, perr)
fail:
	return _memoize(parser, _IdxOp, start, -1, perr)
}

func _IdxOpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _IdxOp, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "IdxOp",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _IdxOp}
	// action
	// _ "[" _ "]"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"[\"",
			})
		}
		goto fail
	}
	pos++
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"]\"",
			})
		}
		goto fail
	}
	pos++
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "index operator"
	parser.fail[key] = failure
	return -1, failure
}

func _IdxOpAction(parser *_Parser, start int) (int, *Ident) {
	dp := parser.deltaPos[start][_IdxOp]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _IdxOp}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ "[" _ "]"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			goto fail
		}
		pos++
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			goto fail
		}
		pos++
		node = func(
			start, end int) Ident {
			return Ident{Name: "[]", L: l(parser, start, end)}
		}(
			start0, pos)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CasesAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Cases, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ cases:CaseId+
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// cases:CaseId+
	{
		pos1 := pos
		// CaseId+
		// CaseId
		if !_accept(parser, _CaseIdAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos3 := pos
			// CaseId
			if !_accept(parser, _CaseIdAccepts, &pos, &perr) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Cases, start, pos, perr)
fail:
	return _memoize(parser, _Cases, start, -1, perr)
}

func _CasesFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Cases, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Cases",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Cases}
	// action
	// _ cases:CaseId+
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// cases:CaseId+
	{
		pos1 := pos
		// CaseId+
		// CaseId
		if !_fail(parser, _CaseIdFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos3 := pos
			// CaseId
			if !_fail(parser, _CaseIdFail, errPos, failure, &pos) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "cases"
	parser.fail[key] = failure
	return -1, failure
}

func _CasesAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 []Ident
	dp := parser.deltaPos[start][_Cases]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Cases}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ cases:CaseId+
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// cases:CaseId+
		{
			pos2 := pos
			// CaseId+
			{
				var node5 Ident
				// CaseId
				if p, n := _CaseIdAction(parser, pos); n == nil {
					goto fail
				} else {
					node5 = *n
					pos = p
				}
				label0 = append(label0, node5)
			}
			for {
				pos4 := pos
				var node5 Ident
				// CaseId
				if p, n := _CaseIdAction(parser, pos); n == nil {
					goto fail6
				} else {
					node5 = *n
					pos = p
				}
				label0 = append(label0, node5)
				continue
			fail6:
				pos = pos4
				break
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, cases []Ident) Ident {
			return Ident{Name: catNames(cases), L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _KwdsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Kwds, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ keywords:Kwd+
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// keywords:Kwd+
	{
		pos1 := pos
		// Kwd+
		// Kwd
		if !_accept(parser, _KwdAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos3 := pos
			// Kwd
			if !_accept(parser, _KwdAccepts, &pos, &perr) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _Kwds, start, pos, perr)
fail:
	return _memoize(parser, _Kwds, start, -1, perr)
}

func _KwdsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Kwds, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Kwds",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Kwds}
	// action
	// _ keywords:Kwd+
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// keywords:Kwd+
	{
		pos1 := pos
		// Kwd+
		// Kwd
		if !_fail(parser, _KwdFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos3 := pos
			// Kwd
			if !_fail(parser, _KwdFail, errPos, failure, &pos) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "keywords"
	parser.fail[key] = failure
	return -1, failure
}

func _KwdsAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 []Ident
	dp := parser.deltaPos[start][_Kwds]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Kwds}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ keywords:Kwd+
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// keywords:Kwd+
		{
			pos2 := pos
			// Kwd+
			{
				var node5 Ident
				// Kwd
				if p, n := _KwdAction(parser, pos); n == nil {
					goto fail
				} else {
					node5 = *n
					pos = p
				}
				label0 = append(label0, node5)
			}
			for {
				pos4 := pos
				var node5 Ident
				// Kwd
				if p, n := _KwdAction(parser, pos); n == nil {
					goto fail6
				} else {
					node5 = *n
					pos = p
				}
				label0 = append(label0, node5)
				continue
			fail6:
				pos = pos4
				break
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, keywords []Ident) Ident {
			return Ident{Name: catNames(keywords), L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _KwdAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Kwd, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ id:Id ":"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// id:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ":"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	perr = start
	return _memoize(parser, _Kwd, start, pos, perr)
fail:
	return _memoize(parser, _Kwd, start, -1, perr)
}

func _KwdFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Kwd, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Kwd",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Kwd}
	// action
	// _ id:Id ":"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// id:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ":"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\":\"",
			})
		}
		goto fail
	}
	pos++
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "keyword"
	parser.fail[key] = failure
	return -1, failure
}

func _KwdAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 Ident
	dp := parser.deltaPos[start][_Kwd]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Kwd}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ id:Id ":"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// id:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			goto fail
		}
		pos++
		node = func(
			start, end int, id Ident) Ident {
			return Ident{Name: id.Name + ":", L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ArrayLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _ArrayLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// _ "[" _ "]" {…}/_ "[" exprs:Exprs _ "]" {…}
	{
		pos3 := pos
		// action
		// _ "[" _ "]"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "[" exprs:Exprs _ "]"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		// exprs:Exprs
		{
			pos8 := pos
			// Exprs
			if !_accept(parser, _ExprsAccepts, &pos, &perr) {
				goto fail6
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _ArrayLit, start, pos, perr)
fail:
	return _memoize(parser, _ArrayLit, start, -1, perr)
}

func _ArrayLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _ArrayLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "ArrayLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _ArrayLit}
	// _ "[" _ "]" {…}/_ "[" exprs:Exprs _ "]" {…}
	{
		pos3 := pos
		// action
		// _ "[" _ "]"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"[\"",
				})
			}
			goto fail4
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"]\"",
				})
			}
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "[" exprs:Exprs _ "]"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"[\"",
				})
			}
			goto fail6
		}
		pos++
		// exprs:Exprs
		{
			pos8 := pos
			// Exprs
			if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
				goto fail6
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"]\"",
				})
			}
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ArrayLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 []Expr
	dp := parser.deltaPos[start][_ArrayLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _ArrayLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// _ "[" _ "]" {…}/_ "[" exprs:Exprs _ "]" {…}
	{
		pos3 := pos
		var node2 Expr
		// action
		{
			start5 := pos
			// _ "[" _ "]"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "["
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
				goto fail4
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "]"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
				goto fail4
			}
			pos++
			node = func(
				start, end int) Expr {
				return Expr(&ArrayLit{L: l(parser, start, end)})
			}(
				start5, pos)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start8 := pos
			// _ "[" exprs:Exprs _ "]"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "["
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
				goto fail7
			}
			pos++
			// exprs:Exprs
			{
				pos10 := pos
				// Exprs
				if p, n := _ExprsAction(parser, pos); n == nil {
					goto fail7
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos10:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "]"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
				goto fail7
			}
			pos++
			node = func(
				start, end int, exprs []Expr) Expr {
				return Expr(&ArrayLit{Exprs: exprs, L: l(parser, start, end)})
			}(
				start8, pos, label0)
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _StructLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _StructLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// _ "[" _ "." _ "]" {…}/_ "[" fields:FieldVals _ "]" {…}
	{
		pos3 := pos
		// action
		// _ "[" _ "." _ "]"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "[" fields:FieldVals _ "]"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		// fields:FieldVals
		{
			pos8 := pos
			// FieldVals
			if !_accept(parser, _FieldValsAccepts, &pos, &perr) {
				goto fail6
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _StructLit, start, pos, perr)
fail:
	return _memoize(parser, _StructLit, start, -1, perr)
}

func _StructLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _StructLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "StructLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _StructLit}
	// _ "[" _ "." _ "]" {…}/_ "[" fields:FieldVals _ "]" {…}
	{
		pos3 := pos
		// action
		// _ "[" _ "." _ "]"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"[\"",
				})
			}
			goto fail4
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\".\"",
				})
			}
			goto fail4
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"]\"",
				})
			}
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "[" fields:FieldVals _ "]"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"[\"",
				})
			}
			goto fail6
		}
		pos++
		// fields:FieldVals
		{
			pos8 := pos
			// FieldVals
			if !_fail(parser, _FieldValsFail, errPos, failure, &pos) {
				goto fail6
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"]\"",
				})
			}
			goto fail6
		}
		pos++
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _StructLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 []FieldVal
	dp := parser.deltaPos[start][_StructLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _StructLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// _ "[" _ "." _ "]" {…}/_ "[" fields:FieldVals _ "]" {…}
	{
		pos3 := pos
		var node2 Expr
		// action
		{
			start5 := pos
			// _ "[" _ "." _ "]"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "["
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
				goto fail4
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "."
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
				goto fail4
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "]"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
				goto fail4
			}
			pos++
			node = func(
				start, end int) Expr {
				return Expr(&StructLit{L: l(parser, start, end)})
			}(
				start5, pos)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start8 := pos
			// _ "[" fields:FieldVals _ "]"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "["
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
				goto fail7
			}
			pos++
			// fields:FieldVals
			{
				pos10 := pos
				// FieldVals
				if p, n := _FieldValsAction(parser, pos); n == nil {
					goto fail7
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos10:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "]"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
				goto fail7
			}
			pos++
			node = func(
				start, end int, fields []FieldVal) Expr {
				return Expr(&StructLit{FieldVals: fields, L: l(parser, start, end)})
			}(
				start8, pos, label0)
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldValsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _FieldVals, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// f0:FieldVal fs:(_ "," f1:FieldVal {…})* (_ ",")?
	// f0:FieldVal
	{
		pos1 := pos
		// FieldVal
		if !_accept(parser, _FieldValAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fs:(_ "," f1:FieldVal {…})*
	{
		pos2 := pos
		// (_ "," f1:FieldVal {…})*
		for {
			pos4 := pos
			// (_ "," f1:FieldVal {…})
			// action
			// _ "," f1:FieldVal
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// f1:FieldVal
			{
				pos8 := pos
				// FieldVal
				if !_accept(parser, _FieldValAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _FieldVals, start, pos, perr)
fail:
	return _memoize(parser, _FieldVals, start, -1, perr)
}

func _FieldValsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _FieldVals, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FieldVals",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FieldVals}
	// action
	// f0:FieldVal fs:(_ "," f1:FieldVal {…})* (_ ",")?
	// f0:FieldVal
	{
		pos1 := pos
		// FieldVal
		if !_fail(parser, _FieldValFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fs:(_ "," f1:FieldVal {…})*
	{
		pos2 := pos
		// (_ "," f1:FieldVal {…})*
		for {
			pos4 := pos
			// (_ "," f1:FieldVal {…})
			// action
			// _ "," f1:FieldVal
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// f1:FieldVal
			{
				pos8 := pos
				// FieldVal
				if !_fail(parser, _FieldValFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FieldValsAction(parser *_Parser, start int) (int, *[]FieldVal) {
	var labels [3]string
	use(labels)
	var label0 FieldVal
	var label1 FieldVal
	var label2 []FieldVal
	dp := parser.deltaPos[start][_FieldVals]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FieldVals}
	n := parser.act[key]
	if n != nil {
		n := n.([]FieldVal)
		return start + int(dp-1), &n
	}
	var node []FieldVal
	pos := start
	// action
	{
		start0 := pos
		// f0:FieldVal fs:(_ "," f1:FieldVal {…})* (_ ",")?
		// f0:FieldVal
		{
			pos2 := pos
			// FieldVal
			if p, n := _FieldValAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// fs:(_ "," f1:FieldVal {…})*
		{
			pos3 := pos
			// (_ "," f1:FieldVal {…})*
			for {
				pos5 := pos
				var node6 FieldVal
				// (_ "," f1:FieldVal {…})
				// action
				{
					start8 := pos
					// _ "," f1:FieldVal
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// f1:FieldVal
					{
						pos10 := pos
						// FieldVal
						if p, n := _FieldValAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, f0 FieldVal, f1 FieldVal) FieldVal {
						return FieldVal(f1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, f0 FieldVal, f1 FieldVal, fs []FieldVal) []FieldVal {
			return []FieldVal(append([]FieldVal{f0}, fs...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldValAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _FieldVal, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:FieldId val:Expr
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:FieldId
	{
		pos1 := pos
		// FieldId
		if !_accept(parser, _FieldIdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// val:Expr
	{
		pos2 := pos
		// Expr
		if !_accept(parser, _ExprAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _FieldVal, start, pos, perr)
fail:
	return _memoize(parser, _FieldVal, start, -1, perr)
}

func _FieldValFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _FieldVal, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FieldVal",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FieldVal}
	// action
	// _ name:FieldId val:Expr
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:FieldId
	{
		pos1 := pos
		// FieldId
		if !_fail(parser, _FieldIdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// val:Expr
	{
		pos2 := pos
		// Expr
		if !_fail(parser, _ExprFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FieldValAction(parser *_Parser, start int) (int, *FieldVal) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_FieldVal]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FieldVal}
	n := parser.act[key]
	if n != nil {
		n := n.(FieldVal)
		return start + int(dp-1), &n
	}
	var node FieldVal
	pos := start
	// action
	{
		start0 := pos
		// _ name:FieldId val:Expr
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:FieldId
		{
			pos2 := pos
			// FieldId
			if p, n := _FieldIdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// val:Expr
		{
			pos3 := pos
			// Expr
			if p, n := _ExprAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, name Ident, val Expr) FieldVal {
			return FieldVal{Name: name, Val: val, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldIdAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _FieldId, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "." id:Id
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "."
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// id:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _FieldId, start, pos, perr)
fail:
	return _memoize(parser, _FieldId, start, -1, perr)
}

func _FieldIdFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _FieldId, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FieldId",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FieldId}
	// action
	// _ "." id:Id
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "."
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\".\"",
			})
		}
		goto fail
	}
	pos++
	// id:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "field id"
	parser.fail[key] = failure
	return -1, failure
}

func _FieldIdAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 Ident
	dp := parser.deltaPos[start][_FieldId]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FieldId}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ "." id:Id
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			goto fail
		}
		pos++
		// id:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, id Ident) Ident {
			return Ident{Name: "." + id.Name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _UnionLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _UnionLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "[" cas:CaseVal _ "]"
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// cas:CaseVal
	{
		pos1 := pos
		// CaseVal
		if !_accept(parser, _CaseValAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _UnionLit, start, pos, perr)
fail:
	return _memoize(parser, _UnionLit, start, -1, perr)
}

func _UnionLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _UnionLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "UnionLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _UnionLit}
	// action
	// _ "[" cas:CaseVal _ "]"
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "["
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"[\"",
			})
		}
		goto fail
	}
	pos++
	// cas:CaseVal
	{
		pos1 := pos
		// CaseVal
		if !_fail(parser, _CaseValFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"]\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _UnionLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 CaseVal
	dp := parser.deltaPos[start][_UnionLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _UnionLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ "[" cas:CaseVal _ "]"
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "["
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "[" {
			goto fail
		}
		pos++
		// cas:CaseVal
		{
			pos2 := pos
			// CaseVal
			if p, n := _CaseValAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			goto fail
		}
		pos++
		node = func(
			start, end int, cas CaseVal) Expr {
			return Expr(&UnionLit{CaseVal: cas, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CaseValAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _CaseVal, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:CaseId v:Expr?
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:CaseId
	{
		pos1 := pos
		// CaseId
		if !_accept(parser, _CaseIdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// v:Expr?
	{
		pos2 := pos
		// Expr?
		{
			pos4 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _CaseVal, start, pos, perr)
fail:
	return _memoize(parser, _CaseVal, start, -1, perr)
}

func _CaseValFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _CaseVal, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CaseVal",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CaseVal}
	// action
	// _ name:CaseId v:Expr?
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:CaseId
	{
		pos1 := pos
		// CaseId
		if !_fail(parser, _CaseIdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// v:Expr?
	{
		pos2 := pos
		// Expr?
		{
			pos4 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CaseValAction(parser *_Parser, start int) (int, *CaseVal) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 *Expr
	dp := parser.deltaPos[start][_CaseVal]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CaseVal}
	n := parser.act[key]
	if n != nil {
		n := n.(CaseVal)
		return start + int(dp-1), &n
	}
	var node CaseVal
	pos := start
	// action
	{
		start0 := pos
		// _ name:CaseId v:Expr?
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:CaseId
		{
			pos2 := pos
			// CaseId
			if p, n := _CaseIdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// v:Expr?
		{
			pos3 := pos
			// Expr?
			{
				pos5 := pos
				label1 = new(Expr)
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail6
				} else {
					*label1 = *n
					pos = p
				}
				goto ok7
			fail6:
				label1 = nil
				pos = pos5
			ok7:
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, name Ident, v *Expr) CaseVal {
			var val Expr
			if v != nil {
				val = *v
			}
			return CaseVal{Name: name, Val: val, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CaseIdAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _CaseId, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "?" id:Id
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "?"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "?" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// id:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _CaseId, start, pos, perr)
fail:
	return _memoize(parser, _CaseId, start, -1, perr)
}

func _CaseIdFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _CaseId, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CaseId",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CaseId}
	// action
	// _ "?" id:Id
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "?"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "?" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"?\"",
			})
		}
		goto fail
	}
	pos++
	// id:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "case id"
	parser.fail[key] = failure
	return -1, failure
}

func _CaseIdAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 Ident
	dp := parser.deltaPos[start][_CaseId]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CaseId}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ "?" id:Id
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "?"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "?" {
			goto fail
		}
		pos++
		// id:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, id Ident) Ident {
			return Ident{Name: "?" + id.Name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BlkLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BlkLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// (_ "(" ps:BlkParms? _ ")")? _ "{" es:Exprs? _ "}"
	// (_ "(" ps:BlkParms? _ ")")?
	{
		pos2 := pos
		// (_ "(" ps:BlkParms? _ ")")
		// _ "(" ps:BlkParms? _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail3
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail3
		}
		pos++
		// ps:BlkParms?
		{
			pos5 := pos
			// BlkParms?
			{
				pos7 := pos
				// BlkParms
				if !_accept(parser, _BlkParmsAccepts, &pos, &perr) {
					goto fail8
				}
				goto ok9
			fail8:
				pos = pos7
			ok9:
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail3
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail3
		}
		pos++
		goto ok10
	fail3:
		pos = pos2
	ok10:
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "{"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// es:Exprs?
	{
		pos11 := pos
		// Exprs?
		{
			pos13 := pos
			// Exprs
			if !_accept(parser, _ExprsAccepts, &pos, &perr) {
				goto fail14
			}
			goto ok15
		fail14:
			pos = pos13
		ok15:
		}
		labels[1] = parser.text[pos11:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// "}"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	return _memoize(parser, _BlkLit, start, pos, perr)
fail:
	return _memoize(parser, _BlkLit, start, -1, perr)
}

func _BlkLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BlkLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BlkLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BlkLit}
	// action
	// (_ "(" ps:BlkParms? _ ")")? _ "{" es:Exprs? _ "}"
	// (_ "(" ps:BlkParms? _ ")")?
	{
		pos2 := pos
		// (_ "(" ps:BlkParms? _ ")")
		// _ "(" ps:BlkParms? _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail3
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail3
		}
		pos++
		// ps:BlkParms?
		{
			pos5 := pos
			// BlkParms?
			{
				pos7 := pos
				// BlkParms
				if !_fail(parser, _BlkParmsFail, errPos, failure, &pos) {
					goto fail8
				}
				goto ok9
			fail8:
				pos = pos7
			ok9:
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail3
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail3
		}
		pos++
		goto ok10
	fail3:
		pos = pos2
	ok10:
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "{"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"{\"",
			})
		}
		goto fail
	}
	pos++
	// es:Exprs?
	{
		pos11 := pos
		// Exprs?
		{
			pos13 := pos
			// Exprs
			if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
				goto fail14
			}
			goto ok15
		fail14:
			pos = pos13
		ok15:
		}
		labels[1] = parser.text[pos11:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// "}"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "\"}\"",
			})
		}
		goto fail
	}
	pos++
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BlkLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 *[]FuncParm
	var label1 *[]Expr
	dp := parser.deltaPos[start][_BlkLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BlkLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// (_ "(" ps:BlkParms? _ ")")? _ "{" es:Exprs? _ "}"
		// (_ "(" ps:BlkParms? _ ")")?
		{
			pos3 := pos
			// (_ "(" ps:BlkParms? _ ")")
			// _ "(" ps:BlkParms? _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail4
			}
			pos++
			// ps:BlkParms?
			{
				pos6 := pos
				// BlkParms?
				{
					pos8 := pos
					label0 = new([]FuncParm)
					// BlkParms
					if p, n := _BlkParmsAction(parser, pos); n == nil {
						goto fail9
					} else {
						*label0 = *n
						pos = p
					}
					goto ok10
				fail9:
					label0 = nil
					pos = pos8
				ok10:
				}
				labels[0] = parser.text[pos6:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail4
			}
			pos++
			goto ok11
		fail4:
			pos = pos3
		ok11:
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			goto fail
		}
		pos++
		// es:Exprs?
		{
			pos12 := pos
			// Exprs?
			{
				pos14 := pos
				label1 = new([]Expr)
				// Exprs
				if p, n := _ExprsAction(parser, pos); n == nil {
					goto fail15
				} else {
					*label1 = *n
					pos = p
				}
				goto ok16
			fail15:
				label1 = nil
				pos = pos14
			ok16:
			}
			labels[1] = parser.text[pos12:pos]
		}
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			goto fail
		}
		pos++
		node = func(
			start, end int, es *[]Expr, ps *[]FuncParm) Expr {
			var parms []FuncParm
			if ps != nil {
				parms = *ps
			}
			var exprs []Expr
			if es != nil {
				exprs = *es
			}
			return Expr(&BlockLit{Parms: parms, Exprs: exprs, L: l(parser, start, end)})
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BlkParmAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BlkParm, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// id:Id t:Type?
	// id:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// t:Type?
	{
		pos2 := pos
		// Type?
		{
			pos4 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BlkParm, start, pos, perr)
fail:
	return _memoize(parser, _BlkParm, start, -1, perr)
}

func _BlkParmFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BlkParm, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BlkParm",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BlkParm}
	// action
	// id:Id t:Type?
	// id:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// t:Type?
	{
		pos2 := pos
		// Type?
		{
			pos4 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail5
			}
			goto ok6
		fail5:
			pos = pos4
		ok6:
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BlkParmAction(parser *_Parser, start int) (int, *FuncParm) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 *Type
	dp := parser.deltaPos[start][_BlkParm]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BlkParm}
	n := parser.act[key]
	if n != nil {
		n := n.(FuncParm)
		return start + int(dp-1), &n
	}
	var node FuncParm
	pos := start
	// action
	{
		start0 := pos
		// id:Id t:Type?
		// id:Id
		{
			pos2 := pos
			// Id
			if p, n := _IdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// t:Type?
		{
			pos3 := pos
			// Type?
			{
				pos5 := pos
				label1 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail6
				} else {
					*label1 = *n
					pos = p
				}
				goto ok7
			fail6:
				label1 = nil
				pos = pos5
			ok7:
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, id Ident, t *Type) FuncParm {
			var typ Type
			if t != nil {
				typ = *t
			}
			return FuncParm{Name: id, Type: typ, L: l(parser, start, end)}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BlkParmsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _BlkParms, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// p0:BlkParm ps:(_ "," p1:BlkParm {…})* (_ ",")?
	// p0:BlkParm
	{
		pos1 := pos
		// BlkParm
		if !_accept(parser, _BlkParmAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ps:(_ "," p1:BlkParm {…})*
	{
		pos2 := pos
		// (_ "," p1:BlkParm {…})*
		for {
			pos4 := pos
			// (_ "," p1:BlkParm {…})
			// action
			// _ "," p1:BlkParm
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				perr = _max(perr, pos)
				goto fail6
			}
			pos++
			// p1:BlkParm
			{
				pos8 := pos
				// BlkParm
				if !_accept(parser, _BlkParmAccepts, &pos, &perr) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			perr = _max(perr, pos)
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	return _memoize(parser, _BlkParms, start, pos, perr)
fail:
	return _memoize(parser, _BlkParms, start, -1, perr)
}

func _BlkParmsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _BlkParms, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BlkParms",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BlkParms}
	// action
	// p0:BlkParm ps:(_ "," p1:BlkParm {…})* (_ ",")?
	// p0:BlkParm
	{
		pos1 := pos
		// BlkParm
		if !_fail(parser, _BlkParmFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ps:(_ "," p1:BlkParm {…})*
	{
		pos2 := pos
		// (_ "," p1:BlkParm {…})*
		for {
			pos4 := pos
			// (_ "," p1:BlkParm {…})
			// action
			// _ "," p1:BlkParm
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail6
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\",\"",
					})
				}
				goto fail6
			}
			pos++
			// p1:BlkParm
			{
				pos8 := pos
				// BlkParm
				if !_fail(parser, _BlkParmFail, errPos, failure, &pos) {
					goto fail6
				}
				labels[1] = parser.text[pos8:pos]
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[2] = parser.text[pos2:pos]
	}
	// (_ ",")?
	{
		pos10 := pos
		// (_ ",")
		// _ ","
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail11
		}
		// ","
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\",\"",
				})
			}
			goto fail11
		}
		pos++
		goto ok13
	fail11:
		pos = pos10
	ok13:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BlkParmsAction(parser *_Parser, start int) (int, *[]FuncParm) {
	var labels [3]string
	use(labels)
	var label0 FuncParm
	var label1 FuncParm
	var label2 []FuncParm
	dp := parser.deltaPos[start][_BlkParms]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BlkParms}
	n := parser.act[key]
	if n != nil {
		n := n.([]FuncParm)
		return start + int(dp-1), &n
	}
	var node []FuncParm
	pos := start
	// action
	{
		start0 := pos
		// p0:BlkParm ps:(_ "," p1:BlkParm {…})* (_ ",")?
		// p0:BlkParm
		{
			pos2 := pos
			// BlkParm
			if p, n := _BlkParmAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ps:(_ "," p1:BlkParm {…})*
		{
			pos3 := pos
			// (_ "," p1:BlkParm {…})*
			for {
				pos5 := pos
				var node6 FuncParm
				// (_ "," p1:BlkParm {…})
				// action
				{
					start8 := pos
					// _ "," p1:BlkParm
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail7
					} else {
						pos = p
					}
					// ","
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
						goto fail7
					}
					pos++
					// p1:BlkParm
					{
						pos10 := pos
						// BlkParm
						if p, n := _BlkParmAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, p0 FuncParm, p1 FuncParm) FuncParm {
						return FuncParm(p1)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		// (_ ",")?
		{
			pos12 := pos
			// (_ ",")
			// _ ","
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail13
			} else {
				pos = p
			}
			// ","
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "," {
				goto fail13
			}
			pos++
			goto ok15
		fail13:
			pos = pos12
		ok15:
		}
		node = func(
			start, end int, p0 FuncParm, p1 FuncParm, ps []FuncParm) []FuncParm {
			return []FuncParm(append([]FuncParm{p0}, ps...))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CharLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _CharLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ data:([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// data:([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
	{
		pos1 := pos
		// ([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
		// [\'] !"\n" (Esc/"\\'"/[^\']) [\']
		// [\']
		if r, w := _next(parser, pos); r != '\'' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		// !"\n"
		{
			pos4 := pos
			perr6 := perr
			// "\n"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
				perr = _max(perr, pos)
				goto ok3
			}
			pos++
			pos = pos4
			perr = _max(perr6, pos)
			goto fail
		ok3:
			pos = pos4
			perr = perr6
		}
		// (Esc/"\\'"/[^\'])
		// Esc/"\\'"/[^\']
		{
			pos10 := pos
			// Esc
			if !_accept(parser, _EscAccepts, &pos, &perr) {
				goto fail11
			}
			goto ok7
		fail11:
			pos = pos10
			// "\\'"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\'" {
				perr = _max(perr, pos)
				goto fail12
			}
			pos += 2
			goto ok7
		fail12:
			pos = pos10
			// [^\']
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '\'' {
				perr = _max(perr, pos)
				goto fail13
			} else {
				pos += w
			}
			goto ok7
		fail13:
			pos = pos10
			goto fail
		ok7:
		}
		// [\']
		if r, w := _next(parser, pos); r != '\'' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	perr = start
	return _memoize(parser, _CharLit, start, pos, perr)
fail:
	return _memoize(parser, _CharLit, start, -1, perr)
}

func _CharLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _CharLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CharLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CharLit}
	// action
	// _ data:([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// data:([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
	{
		pos1 := pos
		// ([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
		// [\'] !"\n" (Esc/"\\'"/[^\']) [\']
		// [\']
		if r, w := _next(parser, pos); r != '\'' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[\\']",
				})
			}
			goto fail
		} else {
			pos += w
		}
		// !"\n"
		{
			pos4 := pos
			nkids5 := len(failure.Kids)
			// "\n"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"\\n\"",
					})
				}
				goto ok3
			}
			pos++
			pos = pos4
			failure.Kids = failure.Kids[:nkids5]
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "!\"\\n\"",
				})
			}
			goto fail
		ok3:
			pos = pos4
			failure.Kids = failure.Kids[:nkids5]
		}
		// (Esc/"\\'"/[^\'])
		// Esc/"\\'"/[^\']
		{
			pos10 := pos
			// Esc
			if !_fail(parser, _EscFail, errPos, failure, &pos) {
				goto fail11
			}
			goto ok7
		fail11:
			pos = pos10
			// "\\'"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\'" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"\\\\'\"",
					})
				}
				goto fail12
			}
			pos += 2
			goto ok7
		fail12:
			pos = pos10
			// [^\']
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '\'' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "[^\\']",
					})
				}
				goto fail13
			} else {
				pos += w
			}
			goto ok7
		fail13:
			pos = pos10
			goto fail
		ok7:
		}
		// [\']
		if r, w := _next(parser, pos); r != '\'' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[\\']",
				})
			}
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "character"
	parser.fail[key] = failure
	return -1, failure
}

func _CharLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_CharLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CharLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ data:([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// data:([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
		{
			pos2 := pos
			// ([\'] !"\n" (Esc/"\\'"/[^\']) [\'])
			// [\'] !"\n" (Esc/"\\'"/[^\']) [\']
			{
				var node3 string
				// [\']
				if r, w := _next(parser, pos); r != '\'' {
					goto fail
				} else {
					node3 = parser.text[pos : pos+w]
					pos += w
				}
				label0, node3 = label0+node3, ""
				// !"\n"
				{
					pos5 := pos
					// "\n"
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
						goto ok4
					}
					pos++
					pos = pos5
					goto fail
				ok4:
					pos = pos5
					node3 = ""
				}
				label0, node3 = label0+node3, ""
				// (Esc/"\\'"/[^\'])
				// Esc/"\\'"/[^\']
				{
					pos11 := pos
					var node10 string
					// Esc
					if p, n := _EscAction(parser, pos); n == nil {
						goto fail12
					} else {
						node3 = *n
						pos = p
					}
					goto ok8
				fail12:
					node3 = node10
					pos = pos11
					// "\\'"
					if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\'" {
						goto fail13
					}
					node3 = parser.text[pos : pos+2]
					pos += 2
					goto ok8
				fail13:
					node3 = node10
					pos = pos11
					// [^\']
					if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '\'' {
						goto fail14
					} else {
						node3 = parser.text[pos : pos+w]
						pos += w
					}
					goto ok8
				fail14:
					node3 = node10
					pos = pos11
					goto fail
				ok8:
				}
				label0, node3 = label0+node3, ""
				// [\']
				if r, w := _next(parser, pos); r != '\'' {
					goto fail
				} else {
					node3 = parser.text[pos : pos+w]
					pos += w
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, data string) Expr {
			return Expr(&CharLit{Rune: interpRune(data), L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _StrLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _StrLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// InterpStr/RawStr
	{
		pos3 := pos
		// InterpStr
		if !_accept(parser, _InterpStrAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// RawStr
		if !_accept(parser, _RawStrAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	perr = start
	return _memoize(parser, _StrLit, start, pos, perr)
fail:
	return _memoize(parser, _StrLit, start, -1, perr)
}

func _StrLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _StrLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "StrLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _StrLit}
	// InterpStr/RawStr
	{
		pos3 := pos
		// InterpStr
		if !_fail(parser, _InterpStrFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// RawStr
		if !_fail(parser, _RawStrFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		goto fail
	ok0:
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "string"
	parser.fail[key] = failure
	return -1, failure
}

func _StrLitAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_StrLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _StrLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// InterpStr/RawStr
	{
		pos3 := pos
		var node2 Expr
		// InterpStr
		if p, n := _InterpStrAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// RawStr
		if p, n := _RawStrAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _InterpStrAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _InterpStr, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ ["] data:(!"\n" (Esc/"\\\"" {…}/[^"]))* ["]
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// ["]
	if r, w := _next(parser, pos); r != '"' {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	// data:(!"\n" (Esc/"\\\"" {…}/[^"]))*
	{
		pos1 := pos
		// (!"\n" (Esc/"\\\"" {…}/[^"]))*
		for {
			pos3 := pos
			// (!"\n" (Esc/"\\\"" {…}/[^"]))
			// !"\n" (Esc/"\\\"" {…}/[^"])
			// !"\n"
			{
				pos8 := pos
				perr10 := perr
				// "\n"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
					perr = _max(perr, pos)
					goto ok7
				}
				pos++
				pos = pos8
				perr = _max(perr10, pos)
				goto fail5
			ok7:
				pos = pos8
				perr = perr10
			}
			// (Esc/"\\\"" {…}/[^"])
			// Esc/"\\\"" {…}/[^"]
			{
				pos14 := pos
				// Esc
				if !_accept(parser, _EscAccepts, &pos, &perr) {
					goto fail15
				}
				goto ok11
			fail15:
				pos = pos14
				// action
				// "\\\""
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\\"" {
					perr = _max(perr, pos)
					goto fail16
				}
				pos += 2
				goto ok11
			fail16:
				pos = pos14
				// [^"]
				if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '"' {
					perr = _max(perr, pos)
					goto fail17
				} else {
					pos += w
				}
				goto ok11
			fail17:
				pos = pos14
				goto fail5
			ok11:
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ["]
	if r, w := _next(parser, pos); r != '"' {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	return _memoize(parser, _InterpStr, start, pos, perr)
fail:
	return _memoize(parser, _InterpStr, start, -1, perr)
}

func _InterpStrFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _InterpStr, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "InterpStr",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _InterpStr}
	// action
	// _ ["] data:(!"\n" (Esc/"\\\"" {…}/[^"]))* ["]
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// ["]
	if r, w := _next(parser, pos); r != '"' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[\"]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	// data:(!"\n" (Esc/"\\\"" {…}/[^"]))*
	{
		pos1 := pos
		// (!"\n" (Esc/"\\\"" {…}/[^"]))*
		for {
			pos3 := pos
			// (!"\n" (Esc/"\\\"" {…}/[^"]))
			// !"\n" (Esc/"\\\"" {…}/[^"])
			// !"\n"
			{
				pos8 := pos
				nkids9 := len(failure.Kids)
				// "\n"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"\\n\"",
						})
					}
					goto ok7
				}
				pos++
				pos = pos8
				failure.Kids = failure.Kids[:nkids9]
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "!\"\\n\"",
					})
				}
				goto fail5
			ok7:
				pos = pos8
				failure.Kids = failure.Kids[:nkids9]
			}
			// (Esc/"\\\"" {…}/[^"])
			// Esc/"\\\"" {…}/[^"]
			{
				pos14 := pos
				// Esc
				if !_fail(parser, _EscFail, errPos, failure, &pos) {
					goto fail15
				}
				goto ok11
			fail15:
				pos = pos14
				// action
				// "\\\""
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\\"" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"\\\\\\\"\"",
						})
					}
					goto fail16
				}
				pos += 2
				goto ok11
			fail16:
				pos = pos14
				// [^"]
				if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '"' {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "[^\"]",
						})
					}
					goto fail17
				} else {
					pos += w
				}
				goto ok11
			fail17:
				pos = pos14
				goto fail5
			ok11:
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ["]
	if r, w := _next(parser, pos); r != '"' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[\"]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _InterpStrAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_InterpStr]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _InterpStr}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ ["] data:(!"\n" (Esc/"\\\"" {…}/[^"]))* ["]
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// ["]
		if r, w := _next(parser, pos); r != '"' {
			goto fail
		} else {
			pos += w
		}
		// data:(!"\n" (Esc/"\\\"" {…}/[^"]))*
		{
			pos2 := pos
			// (!"\n" (Esc/"\\\"" {…}/[^"]))*
			for {
				pos4 := pos
				var node5 string
				// (!"\n" (Esc/"\\\"" {…}/[^"]))
				// !"\n" (Esc/"\\\"" {…}/[^"])
				{
					var node7 string
					// !"\n"
					{
						pos9 := pos
						// "\n"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
							goto ok8
						}
						pos++
						pos = pos9
						goto fail6
					ok8:
						pos = pos9
						node7 = ""
					}
					node5, node7 = node5+node7, ""
					// (Esc/"\\\"" {…}/[^"])
					// Esc/"\\\"" {…}/[^"]
					{
						pos15 := pos
						var node14 string
						// Esc
						if p, n := _EscAction(parser, pos); n == nil {
							goto fail16
						} else {
							node7 = *n
							pos = p
						}
						goto ok12
					fail16:
						node7 = node14
						pos = pos15
						// action
						{
							start18 := pos
							// "\\\""
							if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\\"" {
								goto fail17
							}
							pos += 2
							node7 = func(
								start, end int) string {
								return "\""
							}(
								start18, pos)
						}
						goto ok12
					fail17:
						node7 = node14
						pos = pos15
						// [^"]
						if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '"' {
							goto fail19
						} else {
							node7 = parser.text[pos : pos+w]
							pos += w
						}
						goto ok12
					fail19:
						node7 = node14
						pos = pos15
						goto fail6
					ok12:
					}
					node5, node7 = node5+node7, ""
				}
				label0 += node5
				continue
			fail6:
				pos = pos4
				break
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ["]
		if r, w := _next(parser, pos); r != '"' {
			goto fail
		} else {
			pos += w
		}
		node = func(
			start, end int, data string) Expr {
			return Expr(&StrLit{Raw: false, Data: data, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _EscAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Esc, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// "\\n" {…}/"\\t" {…}/"\\b" {…}/"\\\\" {…}/"\\x" x0:(X X) {…}/"\\u" x1:(X X X X) {…}/"\\U" x2:(X X X X X X X X) {…}
	{
		pos3 := pos
		// action
		// "\\n"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\n" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos += 2
		goto ok0
	fail4:
		pos = pos3
		// action
		// "\\t"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\t" {
			perr = _max(perr, pos)
			goto fail5
		}
		pos += 2
		goto ok0
	fail5:
		pos = pos3
		// action
		// "\\b"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\b" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos += 2
		goto ok0
	fail6:
		pos = pos3
		// action
		// "\\\\"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\\\" {
			perr = _max(perr, pos)
			goto fail7
		}
		pos += 2
		goto ok0
	fail7:
		pos = pos3
		// action
		// "\\x" x0:(X X)
		// "\\x"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\x" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos += 2
		// x0:(X X)
		{
			pos10 := pos
			// (X X)
			// X X
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail8
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail8
			}
			labels[0] = parser.text[pos10:pos]
		}
		goto ok0
	fail8:
		pos = pos3
		// action
		// "\\u" x1:(X X X X)
		// "\\u"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\u" {
			perr = _max(perr, pos)
			goto fail12
		}
		pos += 2
		// x1:(X X X X)
		{
			pos14 := pos
			// (X X X X)
			// X X X X
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail12
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail12
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail12
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail12
			}
			labels[1] = parser.text[pos14:pos]
		}
		goto ok0
	fail12:
		pos = pos3
		// action
		// "\\U" x2:(X X X X X X X X)
		// "\\U"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\U" {
			perr = _max(perr, pos)
			goto fail16
		}
		pos += 2
		// x2:(X X X X X X X X)
		{
			pos18 := pos
			// (X X X X X X X X)
			// X X X X X X X X
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail16
			}
			labels[2] = parser.text[pos18:pos]
		}
		goto ok0
	fail16:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Esc, start, pos, perr)
fail:
	return _memoize(parser, _Esc, start, -1, perr)
}

func _EscFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _Esc, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Esc",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Esc}
	// "\\n" {…}/"\\t" {…}/"\\b" {…}/"\\\\" {…}/"\\x" x0:(X X) {…}/"\\u" x1:(X X X X) {…}/"\\U" x2:(X X X X X X X X) {…}
	{
		pos3 := pos
		// action
		// "\\n"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\n" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\n\"",
				})
			}
			goto fail4
		}
		pos += 2
		goto ok0
	fail4:
		pos = pos3
		// action
		// "\\t"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\t" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\t\"",
				})
			}
			goto fail5
		}
		pos += 2
		goto ok0
	fail5:
		pos = pos3
		// action
		// "\\b"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\b" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\b\"",
				})
			}
			goto fail6
		}
		pos += 2
		goto ok0
	fail6:
		pos = pos3
		// action
		// "\\\\"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\\\" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\\\\\\"",
				})
			}
			goto fail7
		}
		pos += 2
		goto ok0
	fail7:
		pos = pos3
		// action
		// "\\x" x0:(X X)
		// "\\x"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\x" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\x\"",
				})
			}
			goto fail8
		}
		pos += 2
		// x0:(X X)
		{
			pos10 := pos
			// (X X)
			// X X
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail8
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail8
			}
			labels[0] = parser.text[pos10:pos]
		}
		goto ok0
	fail8:
		pos = pos3
		// action
		// "\\u" x1:(X X X X)
		// "\\u"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\u" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\u\"",
				})
			}
			goto fail12
		}
		pos += 2
		// x1:(X X X X)
		{
			pos14 := pos
			// (X X X X)
			// X X X X
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail12
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail12
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail12
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail12
			}
			labels[1] = parser.text[pos14:pos]
		}
		goto ok0
	fail12:
		pos = pos3
		// action
		// "\\U" x2:(X X X X X X X X)
		// "\\U"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\U" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\U\"",
				})
			}
			goto fail16
		}
		pos += 2
		// x2:(X X X X X X X X)
		{
			pos18 := pos
			// (X X X X X X X X)
			// X X X X X X X X
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail16
			}
			labels[2] = parser.text[pos18:pos]
		}
		goto ok0
	fail16:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _EscAction(parser *_Parser, start int) (int, *string) {
	var labels [3]string
	use(labels)
	var label0 string
	var label1 string
	var label2 string
	dp := parser.deltaPos[start][_Esc]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Esc}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// "\\n" {…}/"\\t" {…}/"\\b" {…}/"\\\\" {…}/"\\x" x0:(X X) {…}/"\\u" x1:(X X X X) {…}/"\\U" x2:(X X X X X X X X) {…}
	{
		pos3 := pos
		var node2 string
		// action
		{
			start5 := pos
			// "\\n"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\n" {
				goto fail4
			}
			pos += 2
			node = func(
				start, end int) string {
				return "\n"
			}(
				start5, pos)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// action
		{
			start7 := pos
			// "\\t"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\t" {
				goto fail6
			}
			pos += 2
			node = func(
				start, end int) string {
				return "\t"
			}(
				start7, pos)
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// action
		{
			start9 := pos
			// "\\b"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\b" {
				goto fail8
			}
			pos += 2
			node = func(
				start, end int) string {
				return "\b"
			}(
				start9, pos)
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		// action
		{
			start11 := pos
			// "\\\\"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\\\" {
				goto fail10
			}
			pos += 2
			node = func(
				start, end int) string {
				return "\\"
			}(
				start11, pos)
		}
		goto ok0
	fail10:
		node = node2
		pos = pos3
		// action
		{
			start13 := pos
			// "\\x" x0:(X X)
			// "\\x"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\x" {
				goto fail12
			}
			pos += 2
			// x0:(X X)
			{
				pos15 := pos
				// (X X)
				// X X
				{
					var node16 string
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail12
					} else {
						node16 = *n
						pos = p
					}
					label0, node16 = label0+node16, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail12
					} else {
						node16 = *n
						pos = p
					}
					label0, node16 = label0+node16, ""
				}
				labels[0] = parser.text[pos15:pos]
			}
			node = func(
				start, end int, x0 string) string {
				return string(interpHex(x0))
			}(
				start13, pos, label0)
		}
		goto ok0
	fail12:
		node = node2
		pos = pos3
		// action
		{
			start18 := pos
			// "\\u" x1:(X X X X)
			// "\\u"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\u" {
				goto fail17
			}
			pos += 2
			// x1:(X X X X)
			{
				pos20 := pos
				// (X X X X)
				// X X X X
				{
					var node21 string
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail17
					} else {
						node21 = *n
						pos = p
					}
					label1, node21 = label1+node21, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail17
					} else {
						node21 = *n
						pos = p
					}
					label1, node21 = label1+node21, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail17
					} else {
						node21 = *n
						pos = p
					}
					label1, node21 = label1+node21, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail17
					} else {
						node21 = *n
						pos = p
					}
					label1, node21 = label1+node21, ""
				}
				labels[1] = parser.text[pos20:pos]
			}
			node = func(
				start, end int, x0 string, x1 string) string {
				return string(interpHex(x1))
			}(
				start18, pos, label0, label1)
		}
		goto ok0
	fail17:
		node = node2
		pos = pos3
		// action
		{
			start23 := pos
			// "\\U" x2:(X X X X X X X X)
			// "\\U"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\U" {
				goto fail22
			}
			pos += 2
			// x2:(X X X X X X X X)
			{
				pos25 := pos
				// (X X X X X X X X)
				// X X X X X X X X
				{
					var node26 string
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail22
					} else {
						node26 = *n
						pos = p
					}
					label2, node26 = label2+node26, ""
				}
				labels[2] = parser.text[pos25:pos]
			}
			node = func(
				start, end int, x0 string, x1 string, x2 string) string {
				return string(interpHex(x2))
			}(
				start23, pos, label0, label1, label2)
		}
		goto ok0
	fail22:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _RawStrAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _RawStr, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ [`] data:("\\`"/[^`])* [`]
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// [`]
	if r, w := _next(parser, pos); r != '`' {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	// data:("\\`"/[^`])*
	{
		pos1 := pos
		// ("\\`"/[^`])*
		for {
			pos3 := pos
			// ("\\`"/[^`])
			// "\\`"/[^`]
			{
				pos9 := pos
				// "\\`"
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\`" {
					perr = _max(perr, pos)
					goto fail10
				}
				pos += 2
				goto ok6
			fail10:
				pos = pos9
				// [^`]
				if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '`' {
					perr = _max(perr, pos)
					goto fail11
				} else {
					pos += w
				}
				goto ok6
			fail11:
				pos = pos9
				goto fail5
			ok6:
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	// [`]
	if r, w := _next(parser, pos); r != '`' {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	return _memoize(parser, _RawStr, start, pos, perr)
fail:
	return _memoize(parser, _RawStr, start, -1, perr)
}

func _RawStrFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _RawStr, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "RawStr",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _RawStr}
	// action
	// _ [`] data:("\\`"/[^`])* [`]
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// [`]
	if r, w := _next(parser, pos); r != '`' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[`]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	// data:("\\`"/[^`])*
	{
		pos1 := pos
		// ("\\`"/[^`])*
		for {
			pos3 := pos
			// ("\\`"/[^`])
			// "\\`"/[^`]
			{
				pos9 := pos
				// "\\`"
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\`" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"\\\\`\"",
						})
					}
					goto fail10
				}
				pos += 2
				goto ok6
			fail10:
				pos = pos9
				// [^`]
				if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '`' {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "[^`]",
						})
					}
					goto fail11
				} else {
					pos += w
				}
				goto ok6
			fail11:
				pos = pos9
				goto fail5
			ok6:
			}
			continue
		fail5:
			pos = pos3
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	// [`]
	if r, w := _next(parser, pos); r != '`' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[`]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _RawStrAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_RawStr]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _RawStr}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ [`] data:("\\`"/[^`])* [`]
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// [`]
		if r, w := _next(parser, pos); r != '`' {
			goto fail
		} else {
			pos += w
		}
		// data:("\\`"/[^`])*
		{
			pos2 := pos
			// ("\\`"/[^`])*
			for {
				pos4 := pos
				var node5 string
				// ("\\`"/[^`])
				// "\\`"/[^`]
				{
					pos10 := pos
					var node9 string
					// "\\`"
					if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\`" {
						goto fail11
					}
					node5 = parser.text[pos : pos+2]
					pos += 2
					goto ok7
				fail11:
					node5 = node9
					pos = pos10
					// [^`]
					if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '`' {
						goto fail12
					} else {
						node5 = parser.text[pos : pos+w]
						pos += w
					}
					goto ok7
				fail12:
					node5 = node9
					pos = pos10
					goto fail6
				ok7:
				}
				label0 += node5
				continue
			fail6:
				pos = pos4
				break
			}
			labels[0] = parser.text[pos2:pos]
		}
		// [`]
		if r, w := _next(parser, pos); r != '`' {
			goto fail
		} else {
			pos += w
		}
		node = func(
			start, end int, data string) Expr {
			return Expr(&StrLit{Raw: true, Data: data, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _NumLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _NumLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// FloatLit/HexLit/DecLit
	{
		pos3 := pos
		// FloatLit
		if !_accept(parser, _FloatLitAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// HexLit
		if !_accept(parser, _HexLitAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// DecLit
		if !_accept(parser, _DecLitAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	perr = start
	return _memoize(parser, _NumLit, start, pos, perr)
fail:
	return _memoize(parser, _NumLit, start, -1, perr)
}

func _NumLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _NumLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "NumLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _NumLit}
	// FloatLit/HexLit/DecLit
	{
		pos3 := pos
		// FloatLit
		if !_fail(parser, _FloatLitFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// HexLit
		if !_fail(parser, _HexLitFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// DecLit
		if !_fail(parser, _DecLitFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "number"
	parser.fail[key] = failure
	return -1, failure
}

func _NumLitAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_NumLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _NumLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// FloatLit/HexLit/DecLit
	{
		pos3 := pos
		var node2 Expr
		// FloatLit
		if p, n := _FloatLitAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// HexLit
		if p, n := _HexLitAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// DecLit
		if p, n := _DecLitAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _DecLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _DecLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ text:(("+"/"-")? D+)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// text:(("+"/"-")? D+)
	{
		pos1 := pos
		// (("+"/"-")? D+)
		// ("+"/"-")? D+
		// ("+"/"-")?
		{
			pos4 := pos
			// ("+"/"-")
			// "+"/"-"
			{
				pos9 := pos
				// "+"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "+" {
					perr = _max(perr, pos)
					goto fail10
				}
				pos++
				goto ok6
			fail10:
				pos = pos9
				// "-"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "-" {
					perr = _max(perr, pos)
					goto fail11
				}
				pos++
				goto ok6
			fail11:
				pos = pos9
				goto fail5
			ok6:
			}
			goto ok12
		fail5:
			pos = pos4
		ok12:
		}
		// D+
		// D
		if !_accept(parser, _DAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos14 := pos
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail16
			}
			continue
		fail16:
			pos = pos14
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	return _memoize(parser, _DecLit, start, pos, perr)
fail:
	return _memoize(parser, _DecLit, start, -1, perr)
}

func _DecLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _DecLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "DecLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _DecLit}
	// action
	// _ text:(("+"/"-")? D+)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// text:(("+"/"-")? D+)
	{
		pos1 := pos
		// (("+"/"-")? D+)
		// ("+"/"-")? D+
		// ("+"/"-")?
		{
			pos4 := pos
			// ("+"/"-")
			// "+"/"-"
			{
				pos9 := pos
				// "+"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "+" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"+\"",
						})
					}
					goto fail10
				}
				pos++
				goto ok6
			fail10:
				pos = pos9
				// "-"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "-" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"-\"",
						})
					}
					goto fail11
				}
				pos++
				goto ok6
			fail11:
				pos = pos9
				goto fail5
			ok6:
			}
			goto ok12
		fail5:
			pos = pos4
		ok12:
		}
		// D+
		// D
		if !_fail(parser, _DFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos14 := pos
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail16
			}
			continue
		fail16:
			pos = pos14
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _DecLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_DecLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _DecLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ text:(("+"/"-")? D+)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// text:(("+"/"-")? D+)
		{
			pos2 := pos
			// (("+"/"-")? D+)
			// ("+"/"-")? D+
			{
				var node3 string
				// ("+"/"-")?
				{
					pos5 := pos
					// ("+"/"-")
					// "+"/"-"
					{
						pos10 := pos
						var node9 string
						// "+"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "+" {
							goto fail11
						}
						node3 = parser.text[pos : pos+1]
						pos++
						goto ok7
					fail11:
						node3 = node9
						pos = pos10
						// "-"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "-" {
							goto fail12
						}
						node3 = parser.text[pos : pos+1]
						pos++
						goto ok7
					fail12:
						node3 = node9
						pos = pos10
						goto fail6
					ok7:
					}
					goto ok13
				fail6:
					node3 = ""
					pos = pos5
				ok13:
				}
				label0, node3 = label0+node3, ""
				// D+
				{
					var node16 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail
					} else {
						node16 = *n
						pos = p
					}
					node3 += node16
				}
				for {
					pos15 := pos
					var node16 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail17
					} else {
						node16 = *n
						pos = p
					}
					node3 += node16
					continue
				fail17:
					pos = pos15
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, text string) Expr {
			return Expr(&IntLit{Text: text, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _HexLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _HexLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ text:(("0x"/"0X") X+)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// text:(("0x"/"0X") X+)
	{
		pos1 := pos
		// (("0x"/"0X") X+)
		// ("0x"/"0X") X+
		// ("0x"/"0X")
		// "0x"/"0X"
		{
			pos6 := pos
			// "0x"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "0x" {
				perr = _max(perr, pos)
				goto fail7
			}
			pos += 2
			goto ok3
		fail7:
			pos = pos6
			// "0X"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "0X" {
				perr = _max(perr, pos)
				goto fail8
			}
			pos += 2
			goto ok3
		fail8:
			pos = pos6
			goto fail
		ok3:
		}
		// X+
		// X
		if !_accept(parser, _XAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos10 := pos
			// X
			if !_accept(parser, _XAccepts, &pos, &perr) {
				goto fail12
			}
			continue
		fail12:
			pos = pos10
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	return _memoize(parser, _HexLit, start, pos, perr)
fail:
	return _memoize(parser, _HexLit, start, -1, perr)
}

func _HexLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _HexLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "HexLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _HexLit}
	// action
	// _ text:(("0x"/"0X") X+)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// text:(("0x"/"0X") X+)
	{
		pos1 := pos
		// (("0x"/"0X") X+)
		// ("0x"/"0X") X+
		// ("0x"/"0X")
		// "0x"/"0X"
		{
			pos6 := pos
			// "0x"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "0x" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"0x\"",
					})
				}
				goto fail7
			}
			pos += 2
			goto ok3
		fail7:
			pos = pos6
			// "0X"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "0X" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"0X\"",
					})
				}
				goto fail8
			}
			pos += 2
			goto ok3
		fail8:
			pos = pos6
			goto fail
		ok3:
		}
		// X+
		// X
		if !_fail(parser, _XFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos10 := pos
			// X
			if !_fail(parser, _XFail, errPos, failure, &pos) {
				goto fail12
			}
			continue
		fail12:
			pos = pos10
			break
		}
		labels[0] = parser.text[pos1:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _HexLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_HexLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _HexLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ text:(("0x"/"0X") X+)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// text:(("0x"/"0X") X+)
		{
			pos2 := pos
			// (("0x"/"0X") X+)
			// ("0x"/"0X") X+
			{
				var node3 string
				// ("0x"/"0X")
				// "0x"/"0X"
				{
					pos7 := pos
					var node6 string
					// "0x"
					if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "0x" {
						goto fail8
					}
					node3 = parser.text[pos : pos+2]
					pos += 2
					goto ok4
				fail8:
					node3 = node6
					pos = pos7
					// "0X"
					if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "0X" {
						goto fail9
					}
					node3 = parser.text[pos : pos+2]
					pos += 2
					goto ok4
				fail9:
					node3 = node6
					pos = pos7
					goto fail
				ok4:
				}
				label0, node3 = label0+node3, ""
				// X+
				{
					var node12 string
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail
					} else {
						node12 = *n
						pos = p
					}
					node3 += node12
				}
				for {
					pos11 := pos
					var node12 string
					// X
					if p, n := _XAction(parser, pos); n == nil {
						goto fail13
					} else {
						node12 = *n
						pos = p
					}
					node3 += node12
					continue
				fail13:
					pos = pos11
					break
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, text string) Expr {
			return Expr(&IntLit{Text: text, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FloatLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _FloatLit, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ text:(("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// text:(("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
	{
		pos1 := pos
		// (("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
		// ("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?
		// ("+"/"-")?
		{
			pos4 := pos
			// ("+"/"-")
			// "+"/"-"
			{
				pos9 := pos
				// "+"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "+" {
					perr = _max(perr, pos)
					goto fail10
				}
				pos++
				goto ok6
			fail10:
				pos = pos9
				// "-"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "-" {
					perr = _max(perr, pos)
					goto fail11
				}
				pos++
				goto ok6
			fail11:
				pos = pos9
				goto fail5
			ok6:
			}
			goto ok12
		fail5:
			pos = pos4
		ok12:
		}
		// D+
		// D
		if !_accept(parser, _DAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos14 := pos
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail16
			}
			continue
		fail16:
			pos = pos14
			break
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			perr = _max(perr, pos)
			goto fail
		}
		pos++
		// D+
		// D
		if !_accept(parser, _DAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos18 := pos
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail20
			}
			continue
		fail20:
			pos = pos18
			break
		}
		// ([eE] [+\-]? D+)?
		{
			pos22 := pos
			// ([eE] [+\-]? D+)
			// [eE] [+\-]? D+
			// [eE]
			if r, w := _next(parser, pos); r != 'e' && r != 'E' {
				perr = _max(perr, pos)
				goto fail23
			} else {
				pos += w
			}
			// [+\-]?
			{
				pos26 := pos
				// [+\-]
				if r, w := _next(parser, pos); r != '+' && r != '-' {
					perr = _max(perr, pos)
					goto fail27
				} else {
					pos += w
				}
				goto ok28
			fail27:
				pos = pos26
			ok28:
			}
			// D+
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail23
			}
			for {
				pos30 := pos
				// D
				if !_accept(parser, _DAccepts, &pos, &perr) {
					goto fail32
				}
				continue
			fail32:
				pos = pos30
				break
			}
			goto ok33
		fail23:
			pos = pos22
		ok33:
		}
		labels[0] = parser.text[pos1:pos]
	}
	return _memoize(parser, _FloatLit, start, pos, perr)
fail:
	return _memoize(parser, _FloatLit, start, -1, perr)
}

func _FloatLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _FloatLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "FloatLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _FloatLit}
	// action
	// _ text:(("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// text:(("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
	{
		pos1 := pos
		// (("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
		// ("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?
		// ("+"/"-")?
		{
			pos4 := pos
			// ("+"/"-")
			// "+"/"-"
			{
				pos9 := pos
				// "+"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "+" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"+\"",
						})
					}
					goto fail10
				}
				pos++
				goto ok6
			fail10:
				pos = pos9
				// "-"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "-" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"-\"",
						})
					}
					goto fail11
				}
				pos++
				goto ok6
			fail11:
				pos = pos9
				goto fail5
			ok6:
			}
			goto ok12
		fail5:
			pos = pos4
		ok12:
		}
		// D+
		// D
		if !_fail(parser, _DFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos14 := pos
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail16
			}
			continue
		fail16:
			pos = pos14
			break
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\".\"",
				})
			}
			goto fail
		}
		pos++
		// D+
		// D
		if !_fail(parser, _DFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos18 := pos
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail20
			}
			continue
		fail20:
			pos = pos18
			break
		}
		// ([eE] [+\-]? D+)?
		{
			pos22 := pos
			// ([eE] [+\-]? D+)
			// [eE] [+\-]? D+
			// [eE]
			if r, w := _next(parser, pos); r != 'e' && r != 'E' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "[eE]",
					})
				}
				goto fail23
			} else {
				pos += w
			}
			// [+\-]?
			{
				pos26 := pos
				// [+\-]
				if r, w := _next(parser, pos); r != '+' && r != '-' {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "[+\\-]",
						})
					}
					goto fail27
				} else {
					pos += w
				}
				goto ok28
			fail27:
				pos = pos26
			ok28:
			}
			// D+
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail23
			}
			for {
				pos30 := pos
				// D
				if !_fail(parser, _DFail, errPos, failure, &pos) {
					goto fail32
				}
				continue
			fail32:
				pos = pos30
				break
			}
			goto ok33
		fail23:
			pos = pos22
		ok33:
		}
		labels[0] = parser.text[pos1:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FloatLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_FloatLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FloatLit}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// _ text:(("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// text:(("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
		{
			pos2 := pos
			// (("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?)
			// ("+"/"-")? D+ "." D+ ([eE] [+\-]? D+)?
			{
				var node3 string
				// ("+"/"-")?
				{
					pos5 := pos
					// ("+"/"-")
					// "+"/"-"
					{
						pos10 := pos
						var node9 string
						// "+"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "+" {
							goto fail11
						}
						node3 = parser.text[pos : pos+1]
						pos++
						goto ok7
					fail11:
						node3 = node9
						pos = pos10
						// "-"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "-" {
							goto fail12
						}
						node3 = parser.text[pos : pos+1]
						pos++
						goto ok7
					fail12:
						node3 = node9
						pos = pos10
						goto fail6
					ok7:
					}
					goto ok13
				fail6:
					node3 = ""
					pos = pos5
				ok13:
				}
				label0, node3 = label0+node3, ""
				// D+
				{
					var node16 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail
					} else {
						node16 = *n
						pos = p
					}
					node3 += node16
				}
				for {
					pos15 := pos
					var node16 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail17
					} else {
						node16 = *n
						pos = p
					}
					node3 += node16
					continue
				fail17:
					pos = pos15
					break
				}
				label0, node3 = label0+node3, ""
				// "."
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
					goto fail
				}
				node3 = parser.text[pos : pos+1]
				pos++
				label0, node3 = label0+node3, ""
				// D+
				{
					var node20 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail
					} else {
						node20 = *n
						pos = p
					}
					node3 += node20
				}
				for {
					pos19 := pos
					var node20 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail21
					} else {
						node20 = *n
						pos = p
					}
					node3 += node20
					continue
				fail21:
					pos = pos19
					break
				}
				label0, node3 = label0+node3, ""
				// ([eE] [+\-]? D+)?
				{
					pos23 := pos
					// ([eE] [+\-]? D+)
					// [eE] [+\-]? D+
					{
						var node25 string
						// [eE]
						if r, w := _next(parser, pos); r != 'e' && r != 'E' {
							goto fail24
						} else {
							node25 = parser.text[pos : pos+w]
							pos += w
						}
						node3, node25 = node3+node25, ""
						// [+\-]?
						{
							pos27 := pos
							// [+\-]
							if r, w := _next(parser, pos); r != '+' && r != '-' {
								goto fail28
							} else {
								node25 = parser.text[pos : pos+w]
								pos += w
							}
							goto ok29
						fail28:
							node25 = ""
							pos = pos27
						ok29:
						}
						node3, node25 = node3+node25, ""
						// D+
						{
							var node32 string
							// D
							if p, n := _DAction(parser, pos); n == nil {
								goto fail24
							} else {
								node32 = *n
								pos = p
							}
							node25 += node32
						}
						for {
							pos31 := pos
							var node32 string
							// D
							if p, n := _DAction(parser, pos); n == nil {
								goto fail33
							} else {
								node32 = *n
								pos = p
							}
							node25 += node32
							continue
						fail33:
							pos = pos31
							break
						}
						node3, node25 = node3+node25, ""
					}
					goto ok34
				fail24:
					node3 = ""
					pos = pos23
				ok34:
				}
				label0, node3 = label0+node3, ""
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, text string) Expr {
			return Expr(&FloatLit{Text: text, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _IdAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Id, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ !Reserved !TypeVar name:(("_"/L) ("_"/L/D)*)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// !Reserved
	{
		pos2 := pos
		perr4 := perr
		// Reserved
		if !_accept(parser, _ReservedAccepts, &pos, &perr) {
			goto ok1
		}
		pos = pos2
		perr = _max(perr4, pos)
		goto fail
	ok1:
		pos = pos2
		perr = perr4
	}
	// !TypeVar
	{
		pos6 := pos
		perr8 := perr
		// TypeVar
		if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
			goto ok5
		}
		pos = pos6
		perr = _max(perr8, pos)
		goto fail
	ok5:
		pos = pos6
		perr = perr8
	}
	// name:(("_"/L) ("_"/L/D)*)
	{
		pos9 := pos
		// (("_"/L) ("_"/L/D)*)
		// ("_"/L) ("_"/L/D)*
		// ("_"/L)
		// "_"/L
		{
			pos14 := pos
			// "_"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
				perr = _max(perr, pos)
				goto fail15
			}
			pos++
			goto ok11
		fail15:
			pos = pos14
			// L
			if !_accept(parser, _LAccepts, &pos, &perr) {
				goto fail16
			}
			goto ok11
		fail16:
			pos = pos14
			goto fail
		ok11:
		}
		// ("_"/L/D)*
		for {
			pos18 := pos
			// ("_"/L/D)
			// "_"/L/D
			{
				pos24 := pos
				// "_"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
					perr = _max(perr, pos)
					goto fail25
				}
				pos++
				goto ok21
			fail25:
				pos = pos24
				// L
				if !_accept(parser, _LAccepts, &pos, &perr) {
					goto fail26
				}
				goto ok21
			fail26:
				pos = pos24
				// D
				if !_accept(parser, _DAccepts, &pos, &perr) {
					goto fail27
				}
				goto ok21
			fail27:
				pos = pos24
				goto fail20
			ok21:
			}
			continue
		fail20:
			pos = pos18
			break
		}
		labels[0] = parser.text[pos9:pos]
	}
	perr = start
	return _memoize(parser, _Id, start, pos, perr)
fail:
	return _memoize(parser, _Id, start, -1, perr)
}

func _IdFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Id, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Id",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Id}
	// action
	// _ !Reserved !TypeVar name:(("_"/L) ("_"/L/D)*)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// !Reserved
	{
		pos2 := pos
		nkids3 := len(failure.Kids)
		// Reserved
		if !_fail(parser, _ReservedFail, errPos, failure, &pos) {
			goto ok1
		}
		pos = pos2
		failure.Kids = failure.Kids[:nkids3]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!Reserved",
			})
		}
		goto fail
	ok1:
		pos = pos2
		failure.Kids = failure.Kids[:nkids3]
	}
	// !TypeVar
	{
		pos6 := pos
		nkids7 := len(failure.Kids)
		// TypeVar
		if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
			goto ok5
		}
		pos = pos6
		failure.Kids = failure.Kids[:nkids7]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!TypeVar",
			})
		}
		goto fail
	ok5:
		pos = pos6
		failure.Kids = failure.Kids[:nkids7]
	}
	// name:(("_"/L) ("_"/L/D)*)
	{
		pos9 := pos
		// (("_"/L) ("_"/L/D)*)
		// ("_"/L) ("_"/L/D)*
		// ("_"/L)
		// "_"/L
		{
			pos14 := pos
			// "_"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"_\"",
					})
				}
				goto fail15
			}
			pos++
			goto ok11
		fail15:
			pos = pos14
			// L
			if !_fail(parser, _LFail, errPos, failure, &pos) {
				goto fail16
			}
			goto ok11
		fail16:
			pos = pos14
			goto fail
		ok11:
		}
		// ("_"/L/D)*
		for {
			pos18 := pos
			// ("_"/L/D)
			// "_"/L/D
			{
				pos24 := pos
				// "_"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"_\"",
						})
					}
					goto fail25
				}
				pos++
				goto ok21
			fail25:
				pos = pos24
				// L
				if !_fail(parser, _LFail, errPos, failure, &pos) {
					goto fail26
				}
				goto ok21
			fail26:
				pos = pos24
				// D
				if !_fail(parser, _DFail, errPos, failure, &pos) {
					goto fail27
				}
				goto ok21
			fail27:
				pos = pos24
				goto fail20
			ok21:
			}
			continue
		fail20:
			pos = pos18
			break
		}
		labels[0] = parser.text[pos9:pos]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "identifier"
	parser.fail[key] = failure
	return -1, failure
}

func _IdAction(parser *_Parser, start int) (int, *Ident) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Id]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Id}
	n := parser.act[key]
	if n != nil {
		n := n.(Ident)
		return start + int(dp-1), &n
	}
	var node Ident
	pos := start
	// action
	{
		start0 := pos
		// _ !Reserved !TypeVar name:(("_"/L) ("_"/L/D)*)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// !Reserved
		{
			pos3 := pos
			// Reserved
			if p, n := _ReservedAction(parser, pos); n == nil {
				goto ok2
			} else {
				pos = p
			}
			pos = pos3
			goto fail
		ok2:
			pos = pos3
		}
		// !TypeVar
		{
			pos7 := pos
			// TypeVar
			if p, n := _TypeVarAction(parser, pos); n == nil {
				goto ok6
			} else {
				pos = p
			}
			pos = pos7
			goto fail
		ok6:
			pos = pos7
		}
		// name:(("_"/L) ("_"/L/D)*)
		{
			pos10 := pos
			// (("_"/L) ("_"/L/D)*)
			// ("_"/L) ("_"/L/D)*
			{
				var node11 string
				// ("_"/L)
				// "_"/L
				{
					pos15 := pos
					var node14 string
					// "_"
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
						goto fail16
					}
					node11 = parser.text[pos : pos+1]
					pos++
					goto ok12
				fail16:
					node11 = node14
					pos = pos15
					// L
					if p, n := _LAction(parser, pos); n == nil {
						goto fail17
					} else {
						node11 = *n
						pos = p
					}
					goto ok12
				fail17:
					node11 = node14
					pos = pos15
					goto fail
				ok12:
				}
				label0, node11 = label0+node11, ""
				// ("_"/L/D)*
				for {
					pos19 := pos
					var node20 string
					// ("_"/L/D)
					// "_"/L/D
					{
						pos25 := pos
						var node24 string
						// "_"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
							goto fail26
						}
						node20 = parser.text[pos : pos+1]
						pos++
						goto ok22
					fail26:
						node20 = node24
						pos = pos25
						// L
						if p, n := _LAction(parser, pos); n == nil {
							goto fail27
						} else {
							node20 = *n
							pos = p
						}
						goto ok22
					fail27:
						node20 = node24
						pos = pos25
						// D
						if p, n := _DAction(parser, pos); n == nil {
							goto fail28
						} else {
							node20 = *n
							pos = p
						}
						goto ok22
					fail28:
						node20 = node24
						pos = pos25
						goto fail21
					ok22:
					}
					node11 += node20
					continue
				fail21:
					pos = pos19
					break
				}
				label0, node11 = label0+node11, ""
			}
			labels[0] = parser.text[pos10:pos]
		}
		node = func(
			start, end int, name string) Ident {
			return Ident{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeVarAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _TypeVar, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:[A-Z] !L !D
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:[A-Z]
	{
		pos1 := pos
		// [A-Z]
		if r, w := _next(parser, pos); r < 'A' || r > 'Z' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	// !L
	{
		pos3 := pos
		perr5 := perr
		// L
		if !_accept(parser, _LAccepts, &pos, &perr) {
			goto ok2
		}
		pos = pos3
		perr = _max(perr5, pos)
		goto fail
	ok2:
		pos = pos3
		perr = perr5
	}
	// !D
	{
		pos7 := pos
		perr9 := perr
		// D
		if !_accept(parser, _DAccepts, &pos, &perr) {
			goto ok6
		}
		pos = pos7
		perr = _max(perr9, pos)
		goto fail
	ok6:
		pos = pos7
		perr = perr9
	}
	perr = start
	return _memoize(parser, _TypeVar, start, pos, perr)
fail:
	return _memoize(parser, _TypeVar, start, -1, perr)
}

func _TypeVarFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _TypeVar, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "TypeVar",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _TypeVar}
	// action
	// _ name:[A-Z] !L !D
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:[A-Z]
	{
		pos1 := pos
		// [A-Z]
		if r, w := _next(parser, pos); r < 'A' || r > 'Z' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[A-Z]",
				})
			}
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	// !L
	{
		pos3 := pos
		nkids4 := len(failure.Kids)
		// L
		if !_fail(parser, _LFail, errPos, failure, &pos) {
			goto ok2
		}
		pos = pos3
		failure.Kids = failure.Kids[:nkids4]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!L",
			})
		}
		goto fail
	ok2:
		pos = pos3
		failure.Kids = failure.Kids[:nkids4]
	}
	// !D
	{
		pos7 := pos
		nkids8 := len(failure.Kids)
		// D
		if !_fail(parser, _DFail, errPos, failure, &pos) {
			goto ok6
		}
		pos = pos7
		failure.Kids = failure.Kids[:nkids8]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!D",
			})
		}
		goto fail
	ok6:
		pos = pos7
		failure.Kids = failure.Kids[:nkids8]
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
fail:
	failure.Kids = nil
	failure.Want = "type variable"
	parser.fail[key] = failure
	return -1, failure
}

func _TypeVarAction(parser *_Parser, start int) (int, *TypeVar) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_TypeVar]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _TypeVar}
	n := parser.act[key]
	if n != nil {
		n := n.(TypeVar)
		return start + int(dp-1), &n
	}
	var node TypeVar
	pos := start
	// action
	{
		start0 := pos
		// _ name:[A-Z] !L !D
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:[A-Z]
		{
			pos2 := pos
			// [A-Z]
			if r, w := _next(parser, pos); r < 'A' || r > 'Z' {
				goto fail
			} else {
				label0 = parser.text[pos : pos+w]
				pos += w
			}
			labels[0] = parser.text[pos2:pos]
		}
		// !L
		{
			pos4 := pos
			// L
			if p, n := _LAction(parser, pos); n == nil {
				goto ok3
			} else {
				pos = p
			}
			pos = pos4
			goto fail
		ok3:
			pos = pos4
		}
		// !D
		{
			pos8 := pos
			// D
			if p, n := _DAction(parser, pos); n == nil {
				goto ok7
			} else {
				pos = p
			}
			pos = pos8
			goto fail
		ok7:
			pos = pos8
		}
		node = func(
			start, end int, name string) TypeVar {
			return TypeVar{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ReservedAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Reserved, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// ("import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test") !("_"/L/D)
	// ("import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test")
	// "import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test"
	{
		pos4 := pos
		// "import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
			perr = _max(perr, pos)
			goto fail5
		}
		pos += 6
		goto ok1
	fail5:
		pos = pos4
		// "Import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos += 6
		goto ok1
	fail6:
		pos = pos4
		// "const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
			perr = _max(perr, pos)
			goto fail7
		}
		pos += 5
		goto ok1
	fail7:
		pos = pos4
		// "Const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos += 5
		goto ok1
	fail8:
		pos = pos4
		// "var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
			perr = _max(perr, pos)
			goto fail9
		}
		pos += 3
		goto ok1
	fail9:
		pos = pos4
		// "Var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
			perr = _max(perr, pos)
			goto fail10
		}
		pos += 3
		goto ok1
	fail10:
		pos = pos4
		// "type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
			perr = _max(perr, pos)
			goto fail11
		}
		pos += 4
		goto ok1
	fail11:
		pos = pos4
		// "Type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
			perr = _max(perr, pos)
			goto fail12
		}
		pos += 4
		goto ok1
	fail12:
		pos = pos4
		// "func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
			perr = _max(perr, pos)
			goto fail13
		}
		pos += 4
		goto ok1
	fail13:
		pos = pos4
		// "Func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
			perr = _max(perr, pos)
			goto fail14
		}
		pos += 4
		goto ok1
	fail14:
		pos = pos4
		// "test"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
			perr = _max(perr, pos)
			goto fail15
		}
		pos += 4
		goto ok1
	fail15:
		pos = pos4
		goto fail
	ok1:
	}
	// !("_"/L/D)
	{
		pos17 := pos
		perr19 := perr
		// ("_"/L/D)
		// "_"/L/D
		{
			pos23 := pos
			// "_"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
				perr = _max(perr, pos)
				goto fail24
			}
			pos++
			goto ok20
		fail24:
			pos = pos23
			// L
			if !_accept(parser, _LAccepts, &pos, &perr) {
				goto fail25
			}
			goto ok20
		fail25:
			pos = pos23
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail26
			}
			goto ok20
		fail26:
			pos = pos23
			goto ok16
		ok20:
		}
		pos = pos17
		perr = _max(perr19, pos)
		goto fail
	ok16:
		pos = pos17
		perr = perr19
	}
	return _memoize(parser, _Reserved, start, pos, perr)
fail:
	return _memoize(parser, _Reserved, start, -1, perr)
}

func _ReservedFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Reserved, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Reserved",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Reserved}
	// ("import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test") !("_"/L/D)
	// ("import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test")
	// "import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test"
	{
		pos4 := pos
		// "import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"import\"",
				})
			}
			goto fail5
		}
		pos += 6
		goto ok1
	fail5:
		pos = pos4
		// "Import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Import\"",
				})
			}
			goto fail6
		}
		pos += 6
		goto ok1
	fail6:
		pos = pos4
		// "const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"const\"",
				})
			}
			goto fail7
		}
		pos += 5
		goto ok1
	fail7:
		pos = pos4
		// "Const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Const\"",
				})
			}
			goto fail8
		}
		pos += 5
		goto ok1
	fail8:
		pos = pos4
		// "var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"var\"",
				})
			}
			goto fail9
		}
		pos += 3
		goto ok1
	fail9:
		pos = pos4
		// "Var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Var\"",
				})
			}
			goto fail10
		}
		pos += 3
		goto ok1
	fail10:
		pos = pos4
		// "type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"type\"",
				})
			}
			goto fail11
		}
		pos += 4
		goto ok1
	fail11:
		pos = pos4
		// "Type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Type\"",
				})
			}
			goto fail12
		}
		pos += 4
		goto ok1
	fail12:
		pos = pos4
		// "func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"func\"",
				})
			}
			goto fail13
		}
		pos += 4
		goto ok1
	fail13:
		pos = pos4
		// "Func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Func\"",
				})
			}
			goto fail14
		}
		pos += 4
		goto ok1
	fail14:
		pos = pos4
		// "test"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"test\"",
				})
			}
			goto fail15
		}
		pos += 4
		goto ok1
	fail15:
		pos = pos4
		goto fail
	ok1:
	}
	// !("_"/L/D)
	{
		pos17 := pos
		nkids18 := len(failure.Kids)
		// ("_"/L/D)
		// "_"/L/D
		{
			pos23 := pos
			// "_"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"_\"",
					})
				}
				goto fail24
			}
			pos++
			goto ok20
		fail24:
			pos = pos23
			// L
			if !_fail(parser, _LFail, errPos, failure, &pos) {
				goto fail25
			}
			goto ok20
		fail25:
			pos = pos23
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail26
			}
			goto ok20
		fail26:
			pos = pos23
			goto ok16
		ok20:
		}
		pos = pos17
		failure.Kids = failure.Kids[:nkids18]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!(\"_\"/L/D)",
			})
		}
		goto fail
	ok16:
		pos = pos17
		failure.Kids = failure.Kids[:nkids18]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _ReservedAction(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][_Reserved]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Reserved}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// ("import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test") !("_"/L/D)
	{
		var node0 string
		// ("import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test")
		// "import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test"
		{
			pos4 := pos
			var node3 string
			// "import"
			if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
				goto fail5
			}
			node0 = parser.text[pos : pos+6]
			pos += 6
			goto ok1
		fail5:
			node0 = node3
			pos = pos4
			// "Import"
			if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
				goto fail6
			}
			node0 = parser.text[pos : pos+6]
			pos += 6
			goto ok1
		fail6:
			node0 = node3
			pos = pos4
			// "const"
			if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
				goto fail7
			}
			node0 = parser.text[pos : pos+5]
			pos += 5
			goto ok1
		fail7:
			node0 = node3
			pos = pos4
			// "Const"
			if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
				goto fail8
			}
			node0 = parser.text[pos : pos+5]
			pos += 5
			goto ok1
		fail8:
			node0 = node3
			pos = pos4
			// "var"
			if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
				goto fail9
			}
			node0 = parser.text[pos : pos+3]
			pos += 3
			goto ok1
		fail9:
			node0 = node3
			pos = pos4
			// "Var"
			if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
				goto fail10
			}
			node0 = parser.text[pos : pos+3]
			pos += 3
			goto ok1
		fail10:
			node0 = node3
			pos = pos4
			// "type"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
				goto fail11
			}
			node0 = parser.text[pos : pos+4]
			pos += 4
			goto ok1
		fail11:
			node0 = node3
			pos = pos4
			// "Type"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
				goto fail12
			}
			node0 = parser.text[pos : pos+4]
			pos += 4
			goto ok1
		fail12:
			node0 = node3
			pos = pos4
			// "func"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
				goto fail13
			}
			node0 = parser.text[pos : pos+4]
			pos += 4
			goto ok1
		fail13:
			node0 = node3
			pos = pos4
			// "Func"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
				goto fail14
			}
			node0 = parser.text[pos : pos+4]
			pos += 4
			goto ok1
		fail14:
			node0 = node3
			pos = pos4
			// "test"
			if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
				goto fail15
			}
			node0 = parser.text[pos : pos+4]
			pos += 4
			goto ok1
		fail15:
			node0 = node3
			pos = pos4
			goto fail
		ok1:
		}
		node, node0 = node+node0, ""
		// !("_"/L/D)
		{
			pos17 := pos
			// ("_"/L/D)
			// "_"/L/D
			{
				pos23 := pos
				// "_"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "_" {
					goto fail24
				}
				pos++
				goto ok20
			fail24:
				pos = pos23
				// L
				if p, n := _LAction(parser, pos); n == nil {
					goto fail25
				} else {
					pos = p
				}
				goto ok20
			fail25:
				pos = pos23
				// D
				if p, n := _DAction(parser, pos); n == nil {
					goto fail26
				} else {
					pos = p
				}
				goto ok20
			fail26:
				pos = pos23
				goto ok16
			ok20:
			}
			pos = pos17
			goto fail
		ok16:
			pos = pos17
			node0 = ""
		}
		node, node0 = node+node0, ""
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _DAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _D, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// [0-9]
	if r, w := _next(parser, pos); r < '0' || r > '9' {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	return _memoize(parser, _D, start, pos, perr)
fail:
	return _memoize(parser, _D, start, -1, perr)
}

func _DFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _D, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "D",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _D}
	// [0-9]
	if r, w := _next(parser, pos); r < '0' || r > '9' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[0-9]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _DAction(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][_D]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _D}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// [0-9]
	if r, w := _next(parser, pos); r < '0' || r > '9' {
		goto fail
	} else {
		node = parser.text[pos : pos+w]
		pos += w
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _XAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _X, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// [a-fA-F0-9]
	if r, w := _next(parser, pos); (r < 'a' || r > 'f') && (r < 'A' || r > 'F') && (r < '0' || r > '9') {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	return _memoize(parser, _X, start, pos, perr)
fail:
	return _memoize(parser, _X, start, -1, perr)
}

func _XFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _X, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "X",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _X}
	// [a-fA-F0-9]
	if r, w := _next(parser, pos); (r < 'a' || r > 'f') && (r < 'A' || r > 'F') && (r < '0' || r > '9') {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[a-fA-F0-9]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _XAction(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][_X]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _X}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// [a-fA-F0-9]
	if r, w := _next(parser, pos); (r < 'a' || r > 'f') && (r < 'A' || r > 'F') && (r < '0' || r > '9') {
		goto fail
	} else {
		node = parser.text[pos : pos+w]
		pos += w
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _LAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _L, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// r:. &{…}
	// r:.
	{
		pos1 := pos
		// .
		if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	// pred code
	if ok := func(r string) bool { return isLetter(r) }(labels[0]); !ok {
		perr = _max(perr, pos)
		goto fail
	}
	return _memoize(parser, _L, start, pos, perr)
fail:
	return _memoize(parser, _L, start, -1, perr)
}

func _LFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _L, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "L",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _L}
	// r:. &{…}
	// r:.
	{
		pos1 := pos
		// .
		if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: ".",
				})
			}
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	// pred code
	if ok := func(r string) bool { return isLetter(r) }(labels[0]); !ok {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "&{" + "isLetter(r)" + "}",
			})
		}
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _LAction(parser *_Parser, start int) (int, *string) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_L]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _L}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// r:. &{…}
	{
		var node0 string
		// r:.
		{
			pos1 := pos
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				goto fail
			} else {
				label0 = parser.text[pos : pos+w]
				pos += w
			}
			node0 = label0
			labels[0] = parser.text[pos1:pos]
		}
		node, node0 = node+node0, ""
		// pred code
		if ok := func(r string) bool { return isLetter(r) }(labels[0]); !ok {
			goto fail
		}
		node0 = ""
		node, node0 = node+node0, ""
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _OAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _O, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// [*/%+\-\^=!<>&|~@$]
	if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' && r != '+' && r != '-' && r != '^' && r != '=' && r != '!' && r != '<' && r != '>' && r != '&' && r != '|' && r != '~' && r != '@' && r != '$' {
		perr = _max(perr, pos)
		goto fail
	} else {
		pos += w
	}
	return _memoize(parser, _O, start, pos, perr)
fail:
	return _memoize(parser, _O, start, -1, perr)
}

func _OFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _O, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "O",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _O}
	// [*/%+\-\^=!<>&|~@$]
	if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' && r != '+' && r != '-' && r != '^' && r != '=' && r != '!' && r != '<' && r != '>' && r != '&' && r != '|' && r != '~' && r != '@' && r != '$' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[*/%+\\-\\^=!<>&|~@$]",
			})
		}
		goto fail
	} else {
		pos += w
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _OAction(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][_O]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _O}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// [*/%+\-\^=!<>&|~@$]
	if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' && r != '+' && r != '-' && r != '^' && r != '=' && r != '!' && r != '<' && r != '>' && r != '&' && r != '|' && r != '~' && r != '@' && r != '$' {
		goto fail
	} else {
		node = parser.text[pos : pos+w]
		pos += w
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func __Accepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, __, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// (Space/Cmnt)*
	for {
		pos1 := pos
		// (Space/Cmnt)
		// Space/Cmnt
		{
			pos7 := pos
			// Space
			if !_accept(parser, _SpaceAccepts, &pos, &perr) {
				goto fail8
			}
			goto ok4
		fail8:
			pos = pos7
			// Cmnt
			if !_accept(parser, _CmntAccepts, &pos, &perr) {
				goto fail9
			}
			goto ok4
		fail9:
			pos = pos7
			goto fail3
		ok4:
		}
		continue
	fail3:
		pos = pos1
		break
	}
	perr = start
	return _memoize(parser, __, start, pos, perr)
}

func __Fail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, __, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "_",
		Pos:  int(start),
	}
	key := _key{start: start, rule: __}
	// (Space/Cmnt)*
	for {
		pos1 := pos
		// (Space/Cmnt)
		// Space/Cmnt
		{
			pos7 := pos
			// Space
			if !_fail(parser, _SpaceFail, errPos, failure, &pos) {
				goto fail8
			}
			goto ok4
		fail8:
			pos = pos7
			// Cmnt
			if !_fail(parser, _CmntFail, errPos, failure, &pos) {
				goto fail9
			}
			goto ok4
		fail9:
			pos = pos7
			goto fail3
		ok4:
		}
		continue
	fail3:
		pos = pos1
		break
	}
	failure.Kids = nil
	parser.fail[key] = failure
	return pos, failure
}

func __Action(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][__]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: __}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// (Space/Cmnt)*
	for {
		pos1 := pos
		var node2 string
		// (Space/Cmnt)
		// Space/Cmnt
		{
			pos7 := pos
			var node6 string
			// Space
			if p, n := _SpaceAction(parser, pos); n == nil {
				goto fail8
			} else {
				node2 = *n
				pos = p
			}
			goto ok4
		fail8:
			node2 = node6
			pos = pos7
			// Cmnt
			if p, n := _CmntAction(parser, pos); n == nil {
				goto fail9
			} else {
				node2 = *n
				pos = p
			}
			goto ok4
		fail9:
			node2 = node6
			pos = pos7
			goto fail3
		ok4:
		}
		node += node2
		continue
	fail3:
		pos = pos1
		break
	}
	parser.act[key] = node
	return pos, &node
}

func _SpaceAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Space, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// r:. &{…}
	// r:.
	{
		pos1 := pos
		// .
		if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	// pred code
	if ok := func(r string) bool { return isSpace(r) }(labels[0]); !ok {
		perr = _max(perr, pos)
		goto fail
	}
	return _memoize(parser, _Space, start, pos, perr)
fail:
	return _memoize(parser, _Space, start, -1, perr)
}

func _SpaceFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Space, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Space",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Space}
	// r:. &{…}
	// r:.
	{
		pos1 := pos
		// .
		if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: ".",
				})
			}
			goto fail
		} else {
			pos += w
		}
		labels[0] = parser.text[pos1:pos]
	}
	// pred code
	if ok := func(r string) bool { return isSpace(r) }(labels[0]); !ok {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "&{" + "isSpace(r)" + "}",
			})
		}
		goto fail
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _SpaceAction(parser *_Parser, start int) (int, *string) {
	var labels [1]string
	use(labels)
	var label0 string
	dp := parser.deltaPos[start][_Space]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Space}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// r:. &{…}
	{
		var node0 string
		// r:.
		{
			pos1 := pos
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				goto fail
			} else {
				label0 = parser.text[pos : pos+w]
				pos += w
			}
			node0 = label0
			labels[0] = parser.text[pos1:pos]
		}
		node, node0 = node+node0, ""
		// pred code
		if ok := func(r string) bool { return isSpace(r) }(labels[0]); !ok {
			goto fail
		}
		node0 = ""
		node, node0 = node+node0, ""
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CmntAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Cmnt, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// "//" (!"\n" .)*/"/*" (!"*/" .)* "*/"
	{
		pos3 := pos
		// "//" (!"\n" .)*
		// "//"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "//" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos += 2
		// (!"\n" .)*
		for {
			pos7 := pos
			// (!"\n" .)
			// !"\n" .
			// !"\n"
			{
				pos12 := pos
				perr14 := perr
				// "\n"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
					perr = _max(perr, pos)
					goto ok11
				}
				pos++
				pos = pos12
				perr = _max(perr14, pos)
				goto fail9
			ok11:
				pos = pos12
				perr = perr14
			}
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				perr = _max(perr, pos)
				goto fail9
			} else {
				pos += w
			}
			continue
		fail9:
			pos = pos7
			break
		}
		goto ok0
	fail4:
		pos = pos3
		// "/*" (!"*/" .)* "*/"
		// "/*"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "/*" {
			perr = _max(perr, pos)
			goto fail15
		}
		pos += 2
		// (!"*/" .)*
		for {
			pos18 := pos
			// (!"*/" .)
			// !"*/" .
			// !"*/"
			{
				pos23 := pos
				perr25 := perr
				// "*/"
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "*/" {
					perr = _max(perr, pos)
					goto ok22
				}
				pos += 2
				pos = pos23
				perr = _max(perr25, pos)
				goto fail20
			ok22:
				pos = pos23
				perr = perr25
			}
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				perr = _max(perr, pos)
				goto fail20
			} else {
				pos += w
			}
			continue
		fail20:
			pos = pos18
			break
		}
		// "*/"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "*/" {
			perr = _max(perr, pos)
			goto fail15
		}
		pos += 2
		goto ok0
	fail15:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Cmnt, start, pos, perr)
fail:
	return _memoize(parser, _Cmnt, start, -1, perr)
}

func _CmntFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Cmnt, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Cmnt",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Cmnt}
	// "//" (!"\n" .)*/"/*" (!"*/" .)* "*/"
	{
		pos3 := pos
		// "//" (!"\n" .)*
		// "//"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "//" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"//\"",
				})
			}
			goto fail4
		}
		pos += 2
		// (!"\n" .)*
		for {
			pos7 := pos
			// (!"\n" .)
			// !"\n" .
			// !"\n"
			{
				pos12 := pos
				nkids13 := len(failure.Kids)
				// "\n"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"\\n\"",
						})
					}
					goto ok11
				}
				pos++
				pos = pos12
				failure.Kids = failure.Kids[:nkids13]
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "!\"\\n\"",
					})
				}
				goto fail9
			ok11:
				pos = pos12
				failure.Kids = failure.Kids[:nkids13]
			}
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: ".",
					})
				}
				goto fail9
			} else {
				pos += w
			}
			continue
		fail9:
			pos = pos7
			break
		}
		goto ok0
	fail4:
		pos = pos3
		// "/*" (!"*/" .)* "*/"
		// "/*"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "/*" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"/*\"",
				})
			}
			goto fail15
		}
		pos += 2
		// (!"*/" .)*
		for {
			pos18 := pos
			// (!"*/" .)
			// !"*/" .
			// !"*/"
			{
				pos23 := pos
				nkids24 := len(failure.Kids)
				// "*/"
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "*/" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"*/\"",
						})
					}
					goto ok22
				}
				pos += 2
				pos = pos23
				failure.Kids = failure.Kids[:nkids24]
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "!\"*/\"",
					})
				}
				goto fail20
			ok22:
				pos = pos23
				failure.Kids = failure.Kids[:nkids24]
			}
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: ".",
					})
				}
				goto fail20
			} else {
				pos += w
			}
			continue
		fail20:
			pos = pos18
			break
		}
		// "*/"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "*/" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"*/\"",
				})
			}
			goto fail15
		}
		pos += 2
		goto ok0
	fail15:
		pos = pos3
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CmntAction(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][_Cmnt]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Cmnt}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// "//" (!"\n" .)*/"/*" (!"*/" .)* "*/"
	{
		pos3 := pos
		var node2 string
		// "//" (!"\n" .)*
		{
			var node5 string
			// "//"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "//" {
				goto fail4
			}
			node5 = parser.text[pos : pos+2]
			pos += 2
			node, node5 = node+node5, ""
			// (!"\n" .)*
			for {
				pos7 := pos
				var node8 string
				// (!"\n" .)
				// !"\n" .
				{
					var node10 string
					// !"\n"
					{
						pos12 := pos
						// "\n"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
							goto ok11
						}
						pos++
						pos = pos12
						goto fail9
					ok11:
						pos = pos12
						node10 = ""
					}
					node8, node10 = node8+node10, ""
					// .
					if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
						goto fail9
					} else {
						node10 = parser.text[pos : pos+w]
						pos += w
					}
					node8, node10 = node8+node10, ""
				}
				node5 += node8
				continue
			fail9:
				pos = pos7
				break
			}
			node, node5 = node+node5, ""
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// "/*" (!"*/" .)* "*/"
		{
			var node16 string
			// "/*"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "/*" {
				goto fail15
			}
			node16 = parser.text[pos : pos+2]
			pos += 2
			node, node16 = node+node16, ""
			// (!"*/" .)*
			for {
				pos18 := pos
				var node19 string
				// (!"*/" .)
				// !"*/" .
				{
					var node21 string
					// !"*/"
					{
						pos23 := pos
						// "*/"
						if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "*/" {
							goto ok22
						}
						pos += 2
						pos = pos23
						goto fail20
					ok22:
						pos = pos23
						node21 = ""
					}
					node19, node21 = node19+node21, ""
					// .
					if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
						goto fail20
					} else {
						node21 = parser.text[pos : pos+w]
						pos += w
					}
					node19, node21 = node19+node21, ""
				}
				node16 += node19
				continue
			fail20:
				pos = pos18
				break
			}
			node, node16 = node+node16, ""
			// "*/"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "*/" {
				goto fail15
			}
			node16 = parser.text[pos : pos+2]
			pos += 2
			node, node16 = node+node16, ""
		}
		goto ok0
	fail15:
		node = node2
		pos = pos3
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _EofAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _Eof, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// _ !.
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// !.
	{
		pos2 := pos
		perr4 := perr
		// .
		if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
			perr = _max(perr, pos)
			goto ok1
		} else {
			pos += w
		}
		pos = pos2
		perr = _max(perr4, pos)
		goto fail
	ok1:
		pos = pos2
		perr = perr4
	}
	return _memoize(parser, _Eof, start, pos, perr)
fail:
	return _memoize(parser, _Eof, start, -1, perr)
}

func _EofFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _Eof, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Eof",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Eof}
	// _ !.
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// !.
	{
		pos2 := pos
		nkids3 := len(failure.Kids)
		// .
		if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: ".",
				})
			}
			goto ok1
		} else {
			pos += w
		}
		pos = pos2
		failure.Kids = failure.Kids[:nkids3]
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "!.",
			})
		}
		goto fail
	ok1:
		pos = pos2
		failure.Kids = failure.Kids[:nkids3]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _EofAction(parser *_Parser, start int) (int, *string) {
	dp := parser.deltaPos[start][_Eof]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Eof}
	n := parser.act[key]
	if n != nil {
		n := n.(string)
		return start + int(dp-1), &n
	}
	var node string
	pos := start
	// _ !.
	{
		var node0 string
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			node0 = *n
			pos = p
		}
		node, node0 = node+node0, ""
		// !.
		{
			pos2 := pos
			// .
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' {
				goto ok1
			} else {
				pos += w
			}
			pos = pos2
			goto fail
		ok1:
			pos = pos2
			node0 = ""
		}
		node, node0 = node+node0, ""
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin__AsgnOp__AsgnArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin__AsgnOp__AsgnArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:AsgnArg calls:BinTail<AsgnOp, AsgnArg>+
	// arg0:AsgnArg
	{
		pos1 := pos
		// AsgnArg
		if !_accept(parser, _AsgnArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<AsgnOp, AsgnArg>+
	{
		pos2 := pos
		// BinTail<AsgnOp, AsgnArg>+
		// BinTail<AsgnOp, AsgnArg>
		if !_accept(parser, _BinTail__AsgnOp__AsgnArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<AsgnOp, AsgnArg>
			if !_accept(parser, _BinTail__AsgnOp__AsgnArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Bin__AsgnOp__AsgnArg, start, pos, perr)
fail:
	return _memoize(parser, _Bin__AsgnOp__AsgnArg, start, -1, perr)
}

func _Bin__AsgnOp__AsgnArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin__AsgnOp__AsgnArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin__AsgnOp__AsgnArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin__AsgnOp__AsgnArg}
	// action
	// arg0:AsgnArg calls:BinTail<AsgnOp, AsgnArg>+
	// arg0:AsgnArg
	{
		pos1 := pos
		// AsgnArg
		if !_fail(parser, _AsgnArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<AsgnOp, AsgnArg>+
	{
		pos2 := pos
		// BinTail<AsgnOp, AsgnArg>+
		// BinTail<AsgnOp, AsgnArg>
		if !_fail(parser, _BinTail__AsgnOp__AsgnArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<AsgnOp, AsgnArg>
			if !_fail(parser, _BinTail__AsgnOp__AsgnArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin__AsgnOp__AsgnArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Bin__AsgnOp__AsgnArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin__AsgnOp__AsgnArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:AsgnArg calls:BinTail<AsgnOp, AsgnArg>+
		// arg0:AsgnArg
		{
			pos2 := pos
			// AsgnArg
			if p, n := _AsgnArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// calls:BinTail<AsgnOp, AsgnArg>+
		{
			pos3 := pos
			// BinTail<AsgnOp, AsgnArg>+
			{
				var node6 *Call
				// BinTail<AsgnOp, AsgnArg>
				if p, n := _BinTail__AsgnOp__AsgnArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 *Call
				// BinTail<AsgnOp, AsgnArg>
				if p, n := _BinTail__AsgnOp__AsgnArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, calls []*Call) Expr {
			return Expr(bins(arg0, calls))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _NameArg__Kwd__KwArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _NameArg__Kwd__KwArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Kwd arg:KwArg
	// name:Kwd
	{
		pos1 := pos
		// Kwd
		if !_accept(parser, _KwdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg:KwArg
	{
		pos2 := pos
		// KwArg
		if !_accept(parser, _KwArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _NameArg__Kwd__KwArg, start, pos, perr)
fail:
	return _memoize(parser, _NameArg__Kwd__KwArg, start, -1, perr)
}

func _NameArg__Kwd__KwArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _NameArg__Kwd__KwArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "NameArg__Kwd__KwArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _NameArg__Kwd__KwArg}
	// action
	// name:Kwd arg:KwArg
	// name:Kwd
	{
		pos1 := pos
		// Kwd
		if !_fail(parser, _KwdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg:KwArg
	{
		pos2 := pos
		// KwArg
		if !_fail(parser, _KwArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _NameArg__Kwd__KwArgAction(parser *_Parser, start int) (int, *nameArg) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_NameArg__Kwd__KwArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _NameArg__Kwd__KwArg}
	n := parser.act[key]
	if n != nil {
		n := n.(nameArg)
		return start + int(dp-1), &n
	}
	var node nameArg
	pos := start
	// action
	{
		start0 := pos
		// name:Kwd arg:KwArg
		// name:Kwd
		{
			pos2 := pos
			// Kwd
			if p, n := _KwdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg:KwArg
		{
			pos3 := pos
			// KwArg
			if p, n := _KwArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg Expr, name Ident) nameArg {
			return nameArg{name: name, arg: arg}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _NameArg__CaseId__SwitchArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _NameArg__CaseId__SwitchArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:CaseId arg:SwitchArg
	// name:CaseId
	{
		pos1 := pos
		// CaseId
		if !_accept(parser, _CaseIdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg:SwitchArg
	{
		pos2 := pos
		// SwitchArg
		if !_accept(parser, _SwitchArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _NameArg__CaseId__SwitchArg, start, pos, perr)
fail:
	return _memoize(parser, _NameArg__CaseId__SwitchArg, start, -1, perr)
}

func _NameArg__CaseId__SwitchArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _NameArg__CaseId__SwitchArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "NameArg__CaseId__SwitchArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _NameArg__CaseId__SwitchArg}
	// action
	// name:CaseId arg:SwitchArg
	// name:CaseId
	{
		pos1 := pos
		// CaseId
		if !_fail(parser, _CaseIdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg:SwitchArg
	{
		pos2 := pos
		// SwitchArg
		if !_fail(parser, _SwitchArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _NameArg__CaseId__SwitchArgAction(parser *_Parser, start int) (int, *nameArg) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_NameArg__CaseId__SwitchArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _NameArg__CaseId__SwitchArg}
	n := parser.act[key]
	if n != nil {
		n := n.(nameArg)
		return start + int(dp-1), &n
	}
	var node nameArg
	pos := start
	// action
	{
		start0 := pos
		// name:CaseId arg:SwitchArg
		// name:CaseId
		{
			pos2 := pos
			// CaseId
			if p, n := _CaseIdAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg:SwitchArg
		{
			pos3 := pos
			// SwitchArg
			if p, n := _SwitchArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg Expr, name Ident) nameArg {
			return nameArg{name: name, arg: arg}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin__Bin5Op__Bin5ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin__Bin5Op__Bin5Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:Bin5Arg calls:BinTail<Bin5Op, Bin5Arg>+
	// arg0:Bin5Arg
	{
		pos1 := pos
		// Bin5Arg
		if !_accept(parser, _Bin5ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin5Op, Bin5Arg>+
	{
		pos2 := pos
		// BinTail<Bin5Op, Bin5Arg>+
		// BinTail<Bin5Op, Bin5Arg>
		if !_accept(parser, _BinTail__Bin5Op__Bin5ArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin5Op, Bin5Arg>
			if !_accept(parser, _BinTail__Bin5Op__Bin5ArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Bin__Bin5Op__Bin5Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin__Bin5Op__Bin5Arg, start, -1, perr)
}

func _Bin__Bin5Op__Bin5ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin__Bin5Op__Bin5Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin__Bin5Op__Bin5Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin__Bin5Op__Bin5Arg}
	// action
	// arg0:Bin5Arg calls:BinTail<Bin5Op, Bin5Arg>+
	// arg0:Bin5Arg
	{
		pos1 := pos
		// Bin5Arg
		if !_fail(parser, _Bin5ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin5Op, Bin5Arg>+
	{
		pos2 := pos
		// BinTail<Bin5Op, Bin5Arg>+
		// BinTail<Bin5Op, Bin5Arg>
		if !_fail(parser, _BinTail__Bin5Op__Bin5ArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin5Op, Bin5Arg>
			if !_fail(parser, _BinTail__Bin5Op__Bin5ArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin__Bin5Op__Bin5ArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Bin__Bin5Op__Bin5Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin__Bin5Op__Bin5Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:Bin5Arg calls:BinTail<Bin5Op, Bin5Arg>+
		// arg0:Bin5Arg
		{
			pos2 := pos
			// Bin5Arg
			if p, n := _Bin5ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// calls:BinTail<Bin5Op, Bin5Arg>+
		{
			pos3 := pos
			// BinTail<Bin5Op, Bin5Arg>+
			{
				var node6 *Call
				// BinTail<Bin5Op, Bin5Arg>
				if p, n := _BinTail__Bin5Op__Bin5ArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 *Call
				// BinTail<Bin5Op, Bin5Arg>
				if p, n := _BinTail__Bin5Op__Bin5ArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, calls []*Call) Expr {
			return Expr(bins(arg0, calls))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin__Bin4Op__Bin4ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin__Bin4Op__Bin4Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:Bin4Arg calls:BinTail<Bin4Op, Bin4Arg>+
	// arg0:Bin4Arg
	{
		pos1 := pos
		// Bin4Arg
		if !_accept(parser, _Bin4ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin4Op, Bin4Arg>+
	{
		pos2 := pos
		// BinTail<Bin4Op, Bin4Arg>+
		// BinTail<Bin4Op, Bin4Arg>
		if !_accept(parser, _BinTail__Bin4Op__Bin4ArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin4Op, Bin4Arg>
			if !_accept(parser, _BinTail__Bin4Op__Bin4ArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Bin__Bin4Op__Bin4Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin__Bin4Op__Bin4Arg, start, -1, perr)
}

func _Bin__Bin4Op__Bin4ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin__Bin4Op__Bin4Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin__Bin4Op__Bin4Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin__Bin4Op__Bin4Arg}
	// action
	// arg0:Bin4Arg calls:BinTail<Bin4Op, Bin4Arg>+
	// arg0:Bin4Arg
	{
		pos1 := pos
		// Bin4Arg
		if !_fail(parser, _Bin4ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin4Op, Bin4Arg>+
	{
		pos2 := pos
		// BinTail<Bin4Op, Bin4Arg>+
		// BinTail<Bin4Op, Bin4Arg>
		if !_fail(parser, _BinTail__Bin4Op__Bin4ArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin4Op, Bin4Arg>
			if !_fail(parser, _BinTail__Bin4Op__Bin4ArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin__Bin4Op__Bin4ArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Bin__Bin4Op__Bin4Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin__Bin4Op__Bin4Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:Bin4Arg calls:BinTail<Bin4Op, Bin4Arg>+
		// arg0:Bin4Arg
		{
			pos2 := pos
			// Bin4Arg
			if p, n := _Bin4ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// calls:BinTail<Bin4Op, Bin4Arg>+
		{
			pos3 := pos
			// BinTail<Bin4Op, Bin4Arg>+
			{
				var node6 *Call
				// BinTail<Bin4Op, Bin4Arg>
				if p, n := _BinTail__Bin4Op__Bin4ArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 *Call
				// BinTail<Bin4Op, Bin4Arg>
				if p, n := _BinTail__Bin4Op__Bin4ArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, calls []*Call) Expr {
			return Expr(bins(arg0, calls))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin__Bin3Op__Bin3ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin__Bin3Op__Bin3Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:Bin3Arg calls:BinTail<Bin3Op, Bin3Arg>+
	// arg0:Bin3Arg
	{
		pos1 := pos
		// Bin3Arg
		if !_accept(parser, _Bin3ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin3Op, Bin3Arg>+
	{
		pos2 := pos
		// BinTail<Bin3Op, Bin3Arg>+
		// BinTail<Bin3Op, Bin3Arg>
		if !_accept(parser, _BinTail__Bin3Op__Bin3ArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin3Op, Bin3Arg>
			if !_accept(parser, _BinTail__Bin3Op__Bin3ArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Bin__Bin3Op__Bin3Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin__Bin3Op__Bin3Arg, start, -1, perr)
}

func _Bin__Bin3Op__Bin3ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin__Bin3Op__Bin3Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin__Bin3Op__Bin3Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin__Bin3Op__Bin3Arg}
	// action
	// arg0:Bin3Arg calls:BinTail<Bin3Op, Bin3Arg>+
	// arg0:Bin3Arg
	{
		pos1 := pos
		// Bin3Arg
		if !_fail(parser, _Bin3ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin3Op, Bin3Arg>+
	{
		pos2 := pos
		// BinTail<Bin3Op, Bin3Arg>+
		// BinTail<Bin3Op, Bin3Arg>
		if !_fail(parser, _BinTail__Bin3Op__Bin3ArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin3Op, Bin3Arg>
			if !_fail(parser, _BinTail__Bin3Op__Bin3ArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin__Bin3Op__Bin3ArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Bin__Bin3Op__Bin3Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin__Bin3Op__Bin3Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:Bin3Arg calls:BinTail<Bin3Op, Bin3Arg>+
		// arg0:Bin3Arg
		{
			pos2 := pos
			// Bin3Arg
			if p, n := _Bin3ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// calls:BinTail<Bin3Op, Bin3Arg>+
		{
			pos3 := pos
			// BinTail<Bin3Op, Bin3Arg>+
			{
				var node6 *Call
				// BinTail<Bin3Op, Bin3Arg>
				if p, n := _BinTail__Bin3Op__Bin3ArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 *Call
				// BinTail<Bin3Op, Bin3Arg>
				if p, n := _BinTail__Bin3Op__Bin3ArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, calls []*Call) Expr {
			return Expr(bins(arg0, calls))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin__Bin2Op__Bin2ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin__Bin2Op__Bin2Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:Bin2Arg calls:BinTail<Bin2Op, Bin2Arg>+
	// arg0:Bin2Arg
	{
		pos1 := pos
		// Bin2Arg
		if !_accept(parser, _Bin2ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin2Op, Bin2Arg>+
	{
		pos2 := pos
		// BinTail<Bin2Op, Bin2Arg>+
		// BinTail<Bin2Op, Bin2Arg>
		if !_accept(parser, _BinTail__Bin2Op__Bin2ArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin2Op, Bin2Arg>
			if !_accept(parser, _BinTail__Bin2Op__Bin2ArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Bin__Bin2Op__Bin2Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin__Bin2Op__Bin2Arg, start, -1, perr)
}

func _Bin__Bin2Op__Bin2ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin__Bin2Op__Bin2Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin__Bin2Op__Bin2Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin__Bin2Op__Bin2Arg}
	// action
	// arg0:Bin2Arg calls:BinTail<Bin2Op, Bin2Arg>+
	// arg0:Bin2Arg
	{
		pos1 := pos
		// Bin2Arg
		if !_fail(parser, _Bin2ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin2Op, Bin2Arg>+
	{
		pos2 := pos
		// BinTail<Bin2Op, Bin2Arg>+
		// BinTail<Bin2Op, Bin2Arg>
		if !_fail(parser, _BinTail__Bin2Op__Bin2ArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin2Op, Bin2Arg>
			if !_fail(parser, _BinTail__Bin2Op__Bin2ArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin__Bin2Op__Bin2ArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Bin__Bin2Op__Bin2Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin__Bin2Op__Bin2Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:Bin2Arg calls:BinTail<Bin2Op, Bin2Arg>+
		// arg0:Bin2Arg
		{
			pos2 := pos
			// Bin2Arg
			if p, n := _Bin2ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// calls:BinTail<Bin2Op, Bin2Arg>+
		{
			pos3 := pos
			// BinTail<Bin2Op, Bin2Arg>+
			{
				var node6 *Call
				// BinTail<Bin2Op, Bin2Arg>
				if p, n := _BinTail__Bin2Op__Bin2ArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 *Call
				// BinTail<Bin2Op, Bin2Arg>
				if p, n := _BinTail__Bin2Op__Bin2ArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, calls []*Call) Expr {
			return Expr(bins(arg0, calls))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _Bin__Bin1Op__Bin1ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Bin__Bin1Op__Bin1Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// arg0:Bin1Arg calls:BinTail<Bin1Op, Bin1Arg>+
	// arg0:Bin1Arg
	{
		pos1 := pos
		// Bin1Arg
		if !_accept(parser, _Bin1ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin1Op, Bin1Arg>+
	{
		pos2 := pos
		// BinTail<Bin1Op, Bin1Arg>+
		// BinTail<Bin1Op, Bin1Arg>
		if !_accept(parser, _BinTail__Bin1Op__Bin1ArgAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin1Op, Bin1Arg>
			if !_accept(parser, _BinTail__Bin1Op__Bin1ArgAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Bin__Bin1Op__Bin1Arg, start, pos, perr)
fail:
	return _memoize(parser, _Bin__Bin1Op__Bin1Arg, start, -1, perr)
}

func _Bin__Bin1Op__Bin1ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Bin__Bin1Op__Bin1Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Bin__Bin1Op__Bin1Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Bin__Bin1Op__Bin1Arg}
	// action
	// arg0:Bin1Arg calls:BinTail<Bin1Op, Bin1Arg>+
	// arg0:Bin1Arg
	{
		pos1 := pos
		// Bin1Arg
		if !_fail(parser, _Bin1ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// calls:BinTail<Bin1Op, Bin1Arg>+
	{
		pos2 := pos
		// BinTail<Bin1Op, Bin1Arg>+
		// BinTail<Bin1Op, Bin1Arg>
		if !_fail(parser, _BinTail__Bin1Op__Bin1ArgFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// BinTail<Bin1Op, Bin1Arg>
			if !_fail(parser, _BinTail__Bin1Op__Bin1ArgFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _Bin__Bin1Op__Bin1ArgAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Bin__Bin1Op__Bin1Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Bin__Bin1Op__Bin1Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// action
	{
		start0 := pos
		// arg0:Bin1Arg calls:BinTail<Bin1Op, Bin1Arg>+
		// arg0:Bin1Arg
		{
			pos2 := pos
			// Bin1Arg
			if p, n := _Bin1ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// calls:BinTail<Bin1Op, Bin1Arg>+
		{
			pos3 := pos
			// BinTail<Bin1Op, Bin1Arg>+
			{
				var node6 *Call
				// BinTail<Bin1Op, Bin1Arg>
				if p, n := _BinTail__Bin1Op__Bin1ArgAction(parser, pos); n == nil {
					goto fail
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
			}
			for {
				pos5 := pos
				var node6 *Call
				// BinTail<Bin1Op, Bin1Arg>
				if p, n := _BinTail__Bin1Op__Bin1ArgAction(parser, pos); n == nil {
					goto fail7
				} else {
					node6 = *n
					pos = p
				}
				label1 = append(label1, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg0 Expr, calls []*Call) Expr {
			return Expr(bins(arg0, calls))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BinTail__AsgnOp__AsgnArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BinTail__AsgnOp__AsgnArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:AsgnOp arg1:AsgnArg
	// name:AsgnOp
	{
		pos1 := pos
		// AsgnOp
		if !_accept(parser, _AsgnOpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:AsgnArg
	{
		pos2 := pos
		// AsgnArg
		if !_accept(parser, _AsgnArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BinTail__AsgnOp__AsgnArg, start, pos, perr)
fail:
	return _memoize(parser, _BinTail__AsgnOp__AsgnArg, start, -1, perr)
}

func _BinTail__AsgnOp__AsgnArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BinTail__AsgnOp__AsgnArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BinTail__AsgnOp__AsgnArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BinTail__AsgnOp__AsgnArg}
	// action
	// name:AsgnOp arg1:AsgnArg
	// name:AsgnOp
	{
		pos1 := pos
		// AsgnOp
		if !_fail(parser, _AsgnOpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:AsgnArg
	{
		pos2 := pos
		// AsgnArg
		if !_fail(parser, _AsgnArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BinTail__AsgnOp__AsgnArgAction(parser *_Parser, start int) (int, **Call) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_BinTail__AsgnOp__AsgnArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BinTail__AsgnOp__AsgnArg}
	n := parser.act[key]
	if n != nil {
		n := n.(*Call)
		return start + int(dp-1), &n
	}
	var node *Call
	pos := start
	// action
	{
		start0 := pos
		// name:AsgnOp arg1:AsgnArg
		// name:AsgnOp
		{
			pos2 := pos
			// AsgnOp
			if p, n := _AsgnOpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg1:AsgnArg
		{
			pos3 := pos
			// AsgnArg
			if p, n := _AsgnArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg1 Expr, name Ident) *Call {
			return &Call{
				Fun:  name,
				Args: []Expr{nil, arg1},
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BinTail__Bin5Op__Bin5ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BinTail__Bin5Op__Bin5Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Bin5Op arg1:Bin5Arg
	// name:Bin5Op
	{
		pos1 := pos
		// Bin5Op
		if !_accept(parser, _Bin5OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin5Arg
	{
		pos2 := pos
		// Bin5Arg
		if !_accept(parser, _Bin5ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BinTail__Bin5Op__Bin5Arg, start, pos, perr)
fail:
	return _memoize(parser, _BinTail__Bin5Op__Bin5Arg, start, -1, perr)
}

func _BinTail__Bin5Op__Bin5ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BinTail__Bin5Op__Bin5Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BinTail__Bin5Op__Bin5Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BinTail__Bin5Op__Bin5Arg}
	// action
	// name:Bin5Op arg1:Bin5Arg
	// name:Bin5Op
	{
		pos1 := pos
		// Bin5Op
		if !_fail(parser, _Bin5OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin5Arg
	{
		pos2 := pos
		// Bin5Arg
		if !_fail(parser, _Bin5ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BinTail__Bin5Op__Bin5ArgAction(parser *_Parser, start int) (int, **Call) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_BinTail__Bin5Op__Bin5Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BinTail__Bin5Op__Bin5Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(*Call)
		return start + int(dp-1), &n
	}
	var node *Call
	pos := start
	// action
	{
		start0 := pos
		// name:Bin5Op arg1:Bin5Arg
		// name:Bin5Op
		{
			pos2 := pos
			// Bin5Op
			if p, n := _Bin5OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg1:Bin5Arg
		{
			pos3 := pos
			// Bin5Arg
			if p, n := _Bin5ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg1 Expr, name Ident) *Call {
			return &Call{
				Fun:  name,
				Args: []Expr{nil, arg1},
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BinTail__Bin4Op__Bin4ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BinTail__Bin4Op__Bin4Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Bin4Op arg1:Bin4Arg
	// name:Bin4Op
	{
		pos1 := pos
		// Bin4Op
		if !_accept(parser, _Bin4OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin4Arg
	{
		pos2 := pos
		// Bin4Arg
		if !_accept(parser, _Bin4ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BinTail__Bin4Op__Bin4Arg, start, pos, perr)
fail:
	return _memoize(parser, _BinTail__Bin4Op__Bin4Arg, start, -1, perr)
}

func _BinTail__Bin4Op__Bin4ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BinTail__Bin4Op__Bin4Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BinTail__Bin4Op__Bin4Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BinTail__Bin4Op__Bin4Arg}
	// action
	// name:Bin4Op arg1:Bin4Arg
	// name:Bin4Op
	{
		pos1 := pos
		// Bin4Op
		if !_fail(parser, _Bin4OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin4Arg
	{
		pos2 := pos
		// Bin4Arg
		if !_fail(parser, _Bin4ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BinTail__Bin4Op__Bin4ArgAction(parser *_Parser, start int) (int, **Call) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_BinTail__Bin4Op__Bin4Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BinTail__Bin4Op__Bin4Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(*Call)
		return start + int(dp-1), &n
	}
	var node *Call
	pos := start
	// action
	{
		start0 := pos
		// name:Bin4Op arg1:Bin4Arg
		// name:Bin4Op
		{
			pos2 := pos
			// Bin4Op
			if p, n := _Bin4OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg1:Bin4Arg
		{
			pos3 := pos
			// Bin4Arg
			if p, n := _Bin4ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg1 Expr, name Ident) *Call {
			return &Call{
				Fun:  name,
				Args: []Expr{nil, arg1},
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BinTail__Bin3Op__Bin3ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BinTail__Bin3Op__Bin3Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Bin3Op arg1:Bin3Arg
	// name:Bin3Op
	{
		pos1 := pos
		// Bin3Op
		if !_accept(parser, _Bin3OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin3Arg
	{
		pos2 := pos
		// Bin3Arg
		if !_accept(parser, _Bin3ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BinTail__Bin3Op__Bin3Arg, start, pos, perr)
fail:
	return _memoize(parser, _BinTail__Bin3Op__Bin3Arg, start, -1, perr)
}

func _BinTail__Bin3Op__Bin3ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BinTail__Bin3Op__Bin3Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BinTail__Bin3Op__Bin3Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BinTail__Bin3Op__Bin3Arg}
	// action
	// name:Bin3Op arg1:Bin3Arg
	// name:Bin3Op
	{
		pos1 := pos
		// Bin3Op
		if !_fail(parser, _Bin3OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin3Arg
	{
		pos2 := pos
		// Bin3Arg
		if !_fail(parser, _Bin3ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BinTail__Bin3Op__Bin3ArgAction(parser *_Parser, start int) (int, **Call) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_BinTail__Bin3Op__Bin3Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BinTail__Bin3Op__Bin3Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(*Call)
		return start + int(dp-1), &n
	}
	var node *Call
	pos := start
	// action
	{
		start0 := pos
		// name:Bin3Op arg1:Bin3Arg
		// name:Bin3Op
		{
			pos2 := pos
			// Bin3Op
			if p, n := _Bin3OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg1:Bin3Arg
		{
			pos3 := pos
			// Bin3Arg
			if p, n := _Bin3ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg1 Expr, name Ident) *Call {
			return &Call{
				Fun:  name,
				Args: []Expr{nil, arg1},
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BinTail__Bin2Op__Bin2ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BinTail__Bin2Op__Bin2Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Bin2Op arg1:Bin2Arg
	// name:Bin2Op
	{
		pos1 := pos
		// Bin2Op
		if !_accept(parser, _Bin2OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin2Arg
	{
		pos2 := pos
		// Bin2Arg
		if !_accept(parser, _Bin2ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BinTail__Bin2Op__Bin2Arg, start, pos, perr)
fail:
	return _memoize(parser, _BinTail__Bin2Op__Bin2Arg, start, -1, perr)
}

func _BinTail__Bin2Op__Bin2ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BinTail__Bin2Op__Bin2Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BinTail__Bin2Op__Bin2Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BinTail__Bin2Op__Bin2Arg}
	// action
	// name:Bin2Op arg1:Bin2Arg
	// name:Bin2Op
	{
		pos1 := pos
		// Bin2Op
		if !_fail(parser, _Bin2OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin2Arg
	{
		pos2 := pos
		// Bin2Arg
		if !_fail(parser, _Bin2ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BinTail__Bin2Op__Bin2ArgAction(parser *_Parser, start int) (int, **Call) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_BinTail__Bin2Op__Bin2Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BinTail__Bin2Op__Bin2Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(*Call)
		return start + int(dp-1), &n
	}
	var node *Call
	pos := start
	// action
	{
		start0 := pos
		// name:Bin2Op arg1:Bin2Arg
		// name:Bin2Op
		{
			pos2 := pos
			// Bin2Op
			if p, n := _Bin2OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg1:Bin2Arg
		{
			pos3 := pos
			// Bin2Arg
			if p, n := _Bin2ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg1 Expr, name Ident) *Call {
			return &Call{
				Fun:  name,
				Args: []Expr{nil, arg1},
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _BinTail__Bin1Op__Bin1ArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _BinTail__Bin1Op__Bin1Arg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Bin1Op arg1:Bin1Arg
	// name:Bin1Op
	{
		pos1 := pos
		// Bin1Op
		if !_accept(parser, _Bin1OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin1Arg
	{
		pos2 := pos
		// Bin1Arg
		if !_accept(parser, _Bin1ArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _BinTail__Bin1Op__Bin1Arg, start, pos, perr)
fail:
	return _memoize(parser, _BinTail__Bin1Op__Bin1Arg, start, -1, perr)
}

func _BinTail__Bin1Op__Bin1ArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _BinTail__Bin1Op__Bin1Arg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "BinTail__Bin1Op__Bin1Arg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _BinTail__Bin1Op__Bin1Arg}
	// action
	// name:Bin1Op arg1:Bin1Arg
	// name:Bin1Op
	{
		pos1 := pos
		// Bin1Op
		if !_fail(parser, _Bin1OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg1:Bin1Arg
	{
		pos2 := pos
		// Bin1Arg
		if !_fail(parser, _Bin1ArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _BinTail__Bin1Op__Bin1ArgAction(parser *_Parser, start int) (int, **Call) {
	var labels [2]string
	use(labels)
	var label0 Ident
	var label1 Expr
	dp := parser.deltaPos[start][_BinTail__Bin1Op__Bin1Arg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _BinTail__Bin1Op__Bin1Arg}
	n := parser.act[key]
	if n != nil {
		n := n.(*Call)
		return start + int(dp-1), &n
	}
	var node *Call
	pos := start
	// action
	{
		start0 := pos
		// name:Bin1Op arg1:Bin1Arg
		// name:Bin1Op
		{
			pos2 := pos
			// Bin1Op
			if p, n := _Bin1OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg1:Bin1Arg
		{
			pos3 := pos
			// Bin1Arg
			if p, n := _Bin1ArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg1 Expr, name Ident) *Call {
			return &Call{
				Fun:  name,
				Args: []Expr{nil, arg1},
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}
