package parser

import "github.com/eaburns/peggy/peg"

const (
	_File                     int = 0
	_Import                   int = 1
	_Def                      int = 2
	_ConstDef                 int = 3
	_VarDef                   int = 4
	_TypeDef                  int = 5
	_TypeVars                 int = 6
	_Type                     int = 7
	_Types                    int = 8
	_RefType                  int = 9
	_NamedType                int = 10
	_TypeArgs                 int = 11
	_QName                    int = 12
	_StructType               int = 13
	_Field                    int = 14
	_Fields                   int = 15
	_UnionType                int = 16
	_Case                     int = 17
	_Cases                    int = 18
	_FuncType                 int = 19
	_FuncDef                  int = 20
	_FuncName                 int = 21
	_IdxOp                    int = 22
	_FuncParms                int = 23
	_FuncParm                 int = 24
	_FuncDecl                 int = 25
	_FuncDecls                int = 26
	_TestDef                  int = 27
	_Exprs                    int = 28
	_Expr                     int = 29
	_Asgn                     int = 30
	_AsgnArg                  int = 31
	_AsgnOp                   int = 32
	_KwCall                   int = 33
	_Kw                       int = 34
	_Kwd                      int = 35
	_Kwds                     int = 36
	_KwArg                    int = 37
	_Bin5                     int = 38
	_Bin5Arg                  int = 39
	_Bin5Op                   int = 40
	_Bin4                     int = 41
	_Bin4Arg                  int = 42
	_Bin4Op                   int = 43
	_Bin3                     int = 44
	_Bin3Arg                  int = 45
	_Bin3Op                   int = 46
	_Bin2                     int = 47
	_Bin2Arg                  int = 48
	_Bin2Op                   int = 49
	_Bin1                     int = 50
	_Bin1Arg                  int = 51
	_Bin1Op                   int = 52
	_Cvt                      int = 53
	_Un                       int = 54
	_UnArg                    int = 55
	_Op                       int = 56
	_Idx                      int = 57
	_Idxs                     int = 58
	_IdxArg                   int = 59
	_Call                     int = 60
	_ArgList                  int = 61
	_CallArg                  int = 62
	_Sel                      int = 63
	_DotId                    int = 64
	_Pri                      int = 65
	_QualOp                   int = 66
	_QualKwds                 int = 67
	_CompLit                  int = 68
	_BlkLit                   int = 69
	_CharLit                  int = 70
	_StrLit                   int = 71
	_InterpStr                int = 72
	_Esc                      int = 73
	_RawStr                   int = 74
	_NumLit                   int = 75
	_DecLit                   int = 76
	_HexLit                   int = 77
	_FloatLit                 int = 78
	_Id                       int = 79
	_TypeVar                  int = 80
	_Reserved                 int = 81
	_D                        int = 82
	_X                        int = 83
	_L                        int = 84
	_O                        int = 85
	__                        int = 86
	_Space                    int = 87
	_Cmnt                     int = 88
	_Eof                      int = 89
	_Bin__AsgnOp__AsgnArg     int = 90
	_Bin__Bin5Op__Bin5Arg     int = 91
	_Bin__Bin4Op__Bin4Arg     int = 92
	_Bin__Bin3Op__Bin3Arg     int = 93
	_Bin__Bin2Op__Bin2Arg     int = 94
	_Bin__Bin1Op__Bin1Arg     int = 95
	_BinTail__AsgnOp__AsgnArg int = 96
	_BinTail__Bin5Op__Bin5Arg int = 97
	_BinTail__Bin4Op__Bin4Arg int = 98
	_BinTail__Bin3Op__Bin3Arg int = 99
	_BinTail__Bin2Op__Bin2Arg int = 100
	_BinTail__Bin1Op__Bin1Arg int = 101

	_N int = 102
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
	var label1 *Id
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
				label1 = new(Id)
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
			start, end int, id *Id, path Expr, reserved string) *Import {
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
	// _ reserved:("const"/"Const") name:Id t:Type? (_ "=" expr:Expr)?
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
	// t:Type?
	{
		pos9 := pos
		// Type?
		{
			pos11 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail12
			}
			goto ok13
		fail12:
			pos = pos11
		ok13:
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ "=" expr:Expr)?
	{
		pos15 := pos
		// (_ "=" expr:Expr)
		// _ "=" expr:Expr
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail16
		}
		// "="
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
			perr = _max(perr, pos)
			goto fail16
		}
		pos++
		// expr:Expr
		{
			pos18 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail16
			}
			labels[3] = parser.text[pos18:pos]
		}
		goto ok19
	fail16:
		pos = pos15
	ok19:
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
	// _ reserved:("const"/"Const") name:Id t:Type? (_ "=" expr:Expr)?
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
	// t:Type?
	{
		pos9 := pos
		// Type?
		{
			pos11 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail12
			}
			goto ok13
		fail12:
			pos = pos11
		ok13:
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ "=" expr:Expr)?
	{
		pos15 := pos
		// (_ "=" expr:Expr)
		// _ "=" expr:Expr
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail16
		}
		// "="
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"=\"",
				})
			}
			goto fail16
		}
		pos++
		// expr:Expr
		{
			pos18 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail16
			}
			labels[3] = parser.text[pos18:pos]
		}
		goto ok19
	fail16:
		pos = pos15
	ok19:
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
	var label1 Id
	var label2 *Type
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
		// _ reserved:("const"/"Const") name:Id t:Type? (_ "=" expr:Expr)?
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
		// t:Type?
		{
			pos10 := pos
			// Type?
			{
				pos12 := pos
				label2 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
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
		// (_ "=" expr:Expr)?
		{
			pos16 := pos
			// (_ "=" expr:Expr)
			// _ "=" expr:Expr
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail17
			} else {
				pos = p
			}
			// "="
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
				goto fail17
			}
			pos++
			// expr:Expr
			{
				pos19 := pos
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail17
				} else {
					label3 = *n
					pos = p
				}
				labels[3] = parser.text[pos19:pos]
			}
			goto ok20
		fail17:
			pos = pos16
		ok20:
		}
		node = func(
			start, end int, expr Expr, name Id, reserved string, t *Type) Def {
			var typ Type
			if t != nil {
				typ = *t
			}
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
	// _ reserved:("var"/"Var") name:Id t:Type? (_ "=" expr:Expr)?
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
	// t:Type?
	{
		pos9 := pos
		// Type?
		{
			pos11 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail12
			}
			goto ok13
		fail12:
			pos = pos11
		ok13:
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ "=" expr:Expr)?
	{
		pos15 := pos
		// (_ "=" expr:Expr)
		// _ "=" expr:Expr
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail16
		}
		// "="
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
			perr = _max(perr, pos)
			goto fail16
		}
		pos++
		// expr:Expr
		{
			pos18 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail16
			}
			labels[3] = parser.text[pos18:pos]
		}
		goto ok19
	fail16:
		pos = pos15
	ok19:
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
	// _ reserved:("var"/"Var") name:Id t:Type? (_ "=" expr:Expr)?
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
	// t:Type?
	{
		pos9 := pos
		// Type?
		{
			pos11 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail12
			}
			goto ok13
		fail12:
			pos = pos11
		ok13:
		}
		labels[2] = parser.text[pos9:pos]
	}
	// (_ "=" expr:Expr)?
	{
		pos15 := pos
		// (_ "=" expr:Expr)
		// _ "=" expr:Expr
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail16
		}
		// "="
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"=\"",
				})
			}
			goto fail16
		}
		pos++
		// expr:Expr
		{
			pos18 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail16
			}
			labels[3] = parser.text[pos18:pos]
		}
		goto ok19
	fail16:
		pos = pos15
	ok19:
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
	var label1 Id
	var label2 *Type
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
		// _ reserved:("var"/"Var") name:Id t:Type? (_ "=" expr:Expr)?
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
		// t:Type?
		{
			pos10 := pos
			// Type?
			{
				pos12 := pos
				label2 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
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
		// (_ "=" expr:Expr)?
		{
			pos16 := pos
			// (_ "=" expr:Expr)
			// _ "=" expr:Expr
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail17
			} else {
				pos = p
			}
			// "="
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
				goto fail17
			}
			pos++
			// expr:Expr
			{
				pos19 := pos
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail17
				} else {
					label3 = *n
					pos = p
				}
				labels[3] = parser.text[pos19:pos]
			}
			goto ok20
		fail17:
			pos = pos16
		ok20:
		}
		node = func(
			start, end int, expr Expr, name Id, reserved string, t *Type) Def {
			var typ Type
			if t != nil {
				typ = *t
			}
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
	var labels [4]string
	use(labels)
	if dp, de, ok := _memo(parser, _TypeDef, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ reserved:("type"/"Type") ps:TypeVars? name:Id t:Type?
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
	// t:Type?
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
	return _memoize(parser, _TypeDef, start, pos, perr)
fail:
	return _memoize(parser, _TypeDef, start, -1, perr)
}

func _TypeDefFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [4]string
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
	// _ reserved:("type"/"Type") ps:TypeVars? name:Id t:Type?
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
	// t:Type?
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
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _TypeDefAction(parser *_Parser, start int) (int, *Def) {
	var labels [4]string
	use(labels)
	var label0 string
	var label1 *[]TypeVar
	var label2 Id
	var label3 *Type
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
		// _ reserved:("type"/"Type") ps:TypeVars? name:Id t:Type?
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
		// t:Type?
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
		node = func(
			start, end int, name Id, ps *[]TypeVar, reserved string, t *Type) Def {
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
				TypeParms: parms,
				Name:      name,
				Type:      typ,
				L:         l(parser, start, end),
			})
		}(
			start0, pos, label2, label1, label0, label3)
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
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Type, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// RefType/NamedType/tvar:TypeVar {…}/_ "(" typ:Type _ ")" {…}/StructType/UnionType/FuncType
	{
		pos3 := pos
		// RefType
		if !_accept(parser, _RefTypeAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// NamedType
		if !_accept(parser, _NamedTypeAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// action
		// tvar:TypeVar
		{
			pos7 := pos
			// TypeVar
			if !_accept(parser, _TypeVarAccepts, &pos, &perr) {
				goto fail6
			}
			labels[0] = parser.text[pos7:pos]
		}
		goto ok0
	fail6:
		pos = pos3
		// action
		// _ "(" typ:Type _ ")"
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
		// typ:Type
		{
			pos10 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail8
			}
			labels[1] = parser.text[pos10:pos]
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
		// StructType
		if !_accept(parser, _StructTypeAccepts, &pos, &perr) {
			goto fail11
		}
		goto ok0
	fail11:
		pos = pos3
		// UnionType
		if !_accept(parser, _UnionTypeAccepts, &pos, &perr) {
			goto fail12
		}
		goto ok0
	fail12:
		pos = pos3
		// FuncType
		if !_accept(parser, _FuncTypeAccepts, &pos, &perr) {
			goto fail13
		}
		goto ok0
	fail13:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _Type, start, pos, perr)
fail:
	return _memoize(parser, _Type, start, -1, perr)
}

func _TypeFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
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
	// RefType/NamedType/tvar:TypeVar {…}/_ "(" typ:Type _ ")" {…}/StructType/UnionType/FuncType
	{
		pos3 := pos
		// RefType
		if !_fail(parser, _RefTypeFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// NamedType
		if !_fail(parser, _NamedTypeFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// action
		// tvar:TypeVar
		{
			pos7 := pos
			// TypeVar
			if !_fail(parser, _TypeVarFail, errPos, failure, &pos) {
				goto fail6
			}
			labels[0] = parser.text[pos7:pos]
		}
		goto ok0
	fail6:
		pos = pos3
		// action
		// _ "(" typ:Type _ ")"
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
		// typ:Type
		{
			pos10 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail8
			}
			labels[1] = parser.text[pos10:pos]
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
		// StructType
		if !_fail(parser, _StructTypeFail, errPos, failure, &pos) {
			goto fail11
		}
		goto ok0
	fail11:
		pos = pos3
		// UnionType
		if !_fail(parser, _UnionTypeFail, errPos, failure, &pos) {
			goto fail12
		}
		goto ok0
	fail12:
		pos = pos3
		// FuncType
		if !_fail(parser, _FuncTypeFail, errPos, failure, &pos) {
			goto fail13
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

func _TypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [2]string
	use(labels)
	var label0 TypeVar
	var label1 Type
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
	// RefType/NamedType/tvar:TypeVar {…}/_ "(" typ:Type _ ")" {…}/StructType/UnionType/FuncType
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
		// NamedType
		if p, n := _NamedTypeAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// action
		{
			start7 := pos
			// tvar:TypeVar
			{
				pos8 := pos
				// TypeVar
				if p, n := _TypeVarAction(parser, pos); n == nil {
					goto fail6
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos8:pos]
			}
			node = func(
				start, end int, tvar TypeVar) Type {
				return Type(tvar)
			}(
				start7, pos, label0)
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// action
		{
			start10 := pos
			// _ "(" typ:Type _ ")"
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
			// typ:Type
			{
				pos12 := pos
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail9
				} else {
					label1 = *n
					pos = p
				}
				labels[1] = parser.text[pos12:pos]
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
				start, end int, tvar TypeVar, typ Type) Type {
				return Type(typ)
			}(
				start10, pos, label0, label1)
		}
		goto ok0
	fail9:
		node = node2
		pos = pos3
		// StructType
		if p, n := _StructTypeAction(parser, pos); n == nil {
			goto fail13
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail13:
		node = node2
		pos = pos3
		// UnionType
		if p, n := _UnionTypeAction(parser, pos); n == nil {
			goto fail14
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail14:
		node = node2
		pos = pos3
		// FuncType
		if p, n := _FuncTypeAction(parser, pos); n == nil {
			goto fail15
		} else {
			node = *n
			pos = p
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
	// (args:TypeArgs)? names:QName+
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
	// names:QName+
	{
		pos6 := pos
		// QName+
		// QName
		if !_accept(parser, _QNameAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos8 := pos
			// QName
			if !_accept(parser, _QNameAccepts, &pos, &perr) {
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
	// (args:TypeArgs)? names:QName+
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
	// names:QName+
	{
		pos6 := pos
		// QName+
		// QName
		if !_fail(parser, _QNameFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos8 := pos
			// QName
			if !_fail(parser, _QNameFail, errPos, failure, &pos) {
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
	var label1 []qname
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
		// (args:TypeArgs)? names:QName+
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
		// names:QName+
		{
			pos7 := pos
			// QName+
			{
				var node10 qname
				// QName
				if p, n := _QNameAction(parser, pos); n == nil {
					goto fail
				} else {
					node10 = *n
					pos = p
				}
				label1 = append(label1, node10)
			}
			for {
				pos9 := pos
				var node10 qname
				// QName
				if p, n := _QNameAction(parser, pos); n == nil {
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
			start, end int, args []Type, names []qname) Type {
			return Type(namedType(l(parser, start, end), args, names))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _TypeArgsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _TypeArgs, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// tv:TypeVar {…}/_ "(" arg:Type _ ")" {…}/_ "(" args:Types _ ")" {…}
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
		// _ "(" arg:Type _ ")"
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
		// arg:Type
		{
			pos8 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail6
			}
			labels[1] = parser.text[pos8:pos]
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
		// action
		// _ "(" args:Types _ ")"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail9
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			perr = _max(perr, pos)
			goto fail9
		}
		pos++
		// args:Types
		{
			pos11 := pos
			// Types
			if !_accept(parser, _TypesAccepts, &pos, &perr) {
				goto fail9
			}
			labels[2] = parser.text[pos11:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail9
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			perr = _max(perr, pos)
			goto fail9
		}
		pos++
		goto ok0
	fail9:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _TypeArgs, start, pos, perr)
fail:
	return _memoize(parser, _TypeArgs, start, -1, perr)
}

func _TypeArgsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
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
	// tv:TypeVar {…}/_ "(" arg:Type _ ")" {…}/_ "(" args:Types _ ")" {…}
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
		// _ "(" arg:Type _ ")"
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
		// arg:Type
		{
			pos8 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail6
			}
			labels[1] = parser.text[pos8:pos]
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
		// action
		// _ "(" args:Types _ ")"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail9
		}
		// "("
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"(\"",
				})
			}
			goto fail9
		}
		pos++
		// args:Types
		{
			pos11 := pos
			// Types
			if !_fail(parser, _TypesFail, errPos, failure, &pos) {
				goto fail9
			}
			labels[2] = parser.text[pos11:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail9
		}
		// ")"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\")\"",
				})
			}
			goto fail9
		}
		pos++
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

func _TypeArgsAction(parser *_Parser, start int) (int, *[]Type) {
	var labels [3]string
	use(labels)
	var label0 TypeVar
	var label1 Type
	var label2 []Type
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
	// tv:TypeVar {…}/_ "(" arg:Type _ ")" {…}/_ "(" args:Types _ ")" {…}
	{
		pos3 := pos
		var node2 []Type
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
				start, end int, tv TypeVar) []Type {
				return []Type{tv}
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
			// _ "(" arg:Type _ ")"
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
			// arg:Type
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
				start, end int, arg Type, tv TypeVar) []Type {
				return []Type{arg}
			}(
				start8, pos, label1, label0)
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// action
		{
			start12 := pos
			// _ "(" args:Types _ ")"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail11
			} else {
				pos = p
			}
			// "("
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "(" {
				goto fail11
			}
			pos++
			// args:Types
			{
				pos14 := pos
				// Types
				if p, n := _TypesAction(parser, pos); n == nil {
					goto fail11
				} else {
					label2 = *n
					pos = p
				}
				labels[2] = parser.text[pos14:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail11
			} else {
				pos = p
			}
			// ")"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ")" {
				goto fail11
			}
			pos++
			node = func(
				start, end int, arg Type, args []Type, tv TypeVar) []Type {
				return []Type(args)
			}(
				start12, pos, label1, label2, label0)
		}
		goto ok0
	fail11:
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

func _QNameAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _QName, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// id0:Id id1:(_ "." id:Id {…})?
	// id0:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// id1:(_ "." id:Id {…})?
	{
		pos2 := pos
		// (_ "." id:Id {…})?
		{
			pos4 := pos
			// (_ "." id:Id {…})
			// action
			// _ "." id:Id
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail5
			}
			// "."
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
				perr = _max(perr, pos)
				goto fail5
			}
			pos++
			// id:Id
			{
				pos7 := pos
				// Id
				if !_accept(parser, _IdAccepts, &pos, &perr) {
					goto fail5
				}
				labels[1] = parser.text[pos7:pos]
			}
			goto ok8
		fail5:
			pos = pos4
		ok8:
		}
		labels[2] = parser.text[pos2:pos]
	}
	return _memoize(parser, _QName, start, pos, perr)
fail:
	return _memoize(parser, _QName, start, -1, perr)
}

func _QNameFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _QName, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "QName",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _QName}
	// action
	// id0:Id id1:(_ "." id:Id {…})?
	// id0:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// id1:(_ "." id:Id {…})?
	{
		pos2 := pos
		// (_ "." id:Id {…})?
		{
			pos4 := pos
			// (_ "." id:Id {…})
			// action
			// _ "." id:Id
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail5
			}
			// "."
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\".\"",
					})
				}
				goto fail5
			}
			pos++
			// id:Id
			{
				pos7 := pos
				// Id
				if !_fail(parser, _IdFail, errPos, failure, &pos) {
					goto fail5
				}
				labels[1] = parser.text[pos7:pos]
			}
			goto ok8
		fail5:
			pos = pos4
		ok8:
		}
		labels[2] = parser.text[pos2:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _QNameAction(parser *_Parser, start int) (int, *qname) {
	var labels [3]string
	use(labels)
	var label0 Id
	var label1 Id
	var label2 *Id
	dp := parser.deltaPos[start][_QName]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _QName}
	n := parser.act[key]
	if n != nil {
		n := n.(qname)
		return start + int(dp-1), &n
	}
	var node qname
	pos := start
	// action
	{
		start0 := pos
		// id0:Id id1:(_ "." id:Id {…})?
		// id0:Id
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
		// id1:(_ "." id:Id {…})?
		{
			pos3 := pos
			// (_ "." id:Id {…})?
			{
				pos5 := pos
				label2 = new(Id)
				// (_ "." id:Id {…})
				// action
				{
					start7 := pos
					// _ "." id:Id
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail6
					} else {
						pos = p
					}
					// "."
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
						goto fail6
					}
					pos++
					// id:Id
					{
						pos9 := pos
						// Id
						if p, n := _IdAction(parser, pos); n == nil {
							goto fail6
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos9:pos]
					}
					*label2 = func(
						start, end int, id Id, id0 Id) Id {
						return Id(id)
					}(
						start7, pos, label1, label0)
				}
				goto ok10
			fail6:
				label2 = nil
				pos = pos5
			ok10:
			}
			labels[2] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, id Id, id0 Id, id1 *Id) qname {
			return qname(makeQname(id0, id1))
		}(
			start0, pos, label1, label0, label2)
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
	// action
	// _ "[" fs:Fields? _ "]"
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
	// fs:Fields?
	{
		pos1 := pos
		// Fields?
		{
			pos3 := pos
			// Fields
			if !_accept(parser, _FieldsAccepts, &pos, &perr) {
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
	// "]"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
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
	// action
	// _ "[" fs:Fields? _ "]"
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
	// fs:Fields?
	{
		pos1 := pos
		// Fields?
		{
			pos3 := pos
			// Fields
			if !_fail(parser, _FieldsFail, errPos, failure, &pos) {
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

func _StructTypeAction(parser *_Parser, start int) (int, *Type) {
	var labels [1]string
	use(labels)
	var label0 *[]Field
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
	// action
	{
		start0 := pos
		// _ "[" fs:Fields? _ "]"
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
		// fs:Fields?
		{
			pos2 := pos
			// Fields?
			{
				pos4 := pos
				label0 = new([]Field)
				// Fields
				if p, n := _FieldsAction(parser, pos); n == nil {
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
		// "]"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "]" {
			goto fail
		}
		pos++
		node = func(
			start, end int, fs *[]Field) Type {
			var fields []Field
			if fs != nil {
				fields = *fs
			}
			return Type(&StructType{Fields: fields, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Field, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Id _ ":" typ:Type
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
	// ":"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// typ:Type
	{
		pos2 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Field, start, pos, perr)
fail:
	return _memoize(parser, _Field, start, -1, perr)
}

func _FieldFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Field, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Field",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Field}
	// action
	// name:Id _ ":" typ:Type
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

func _FieldAction(parser *_Parser, start int) (int, *Field) {
	var labels [2]string
	use(labels)
	var label0 Id
	var label1 Type
	dp := parser.deltaPos[start][_Field]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Field}
	n := parser.act[key]
	if n != nil {
		n := n.(Field)
		return start + int(dp-1), &n
	}
	var node Field
	pos := start
	// action
	{
		start0 := pos
		// name:Id _ ":" typ:Type
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
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			goto fail
		}
		pos++
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
			start, end int, name Id, typ Type) Field {
			return Field{
				Name: name,
				Type: typ,
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _FieldsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Fields, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// f0:Field fs:(_ "," f1:Field {…})* (_ ",")?
	// f0:Field
	{
		pos1 := pos
		// Field
		if !_accept(parser, _FieldAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fs:(_ "," f1:Field {…})*
	{
		pos2 := pos
		// (_ "," f1:Field {…})*
		for {
			pos4 := pos
			// (_ "," f1:Field {…})
			// action
			// _ "," f1:Field
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
			// f1:Field
			{
				pos8 := pos
				// Field
				if !_accept(parser, _FieldAccepts, &pos, &perr) {
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
	return _memoize(parser, _Fields, start, pos, perr)
fail:
	return _memoize(parser, _Fields, start, -1, perr)
}

func _FieldsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
	use(labels)
	pos, failure := _failMemo(parser, _Fields, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Fields",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Fields}
	// action
	// f0:Field fs:(_ "," f1:Field {…})* (_ ",")?
	// f0:Field
	{
		pos1 := pos
		// Field
		if !_fail(parser, _FieldFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// fs:(_ "," f1:Field {…})*
	{
		pos2 := pos
		// (_ "," f1:Field {…})*
		for {
			pos4 := pos
			// (_ "," f1:Field {…})
			// action
			// _ "," f1:Field
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
			// f1:Field
			{
				pos8 := pos
				// Field
				if !_fail(parser, _FieldFail, errPos, failure, &pos) {
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

func _FieldsAction(parser *_Parser, start int) (int, *[]Field) {
	var labels [3]string
	use(labels)
	var label0 Field
	var label1 Field
	var label2 []Field
	dp := parser.deltaPos[start][_Fields]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Fields}
	n := parser.act[key]
	if n != nil {
		n := n.([]Field)
		return start + int(dp-1), &n
	}
	var node []Field
	pos := start
	// action
	{
		start0 := pos
		// f0:Field fs:(_ "," f1:Field {…})* (_ ",")?
		// f0:Field
		{
			pos2 := pos
			// Field
			if p, n := _FieldAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// fs:(_ "," f1:Field {…})*
		{
			pos3 := pos
			// (_ "," f1:Field {…})*
			for {
				pos5 := pos
				var node6 Field
				// (_ "," f1:Field {…})
				// action
				{
					start8 := pos
					// _ "," f1:Field
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
					// f1:Field
					{
						pos10 := pos
						// Field
						if p, n := _FieldAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos10:pos]
					}
					node6 = func(
						start, end int, f0 Field, f1 Field) Field {
						return Field(f1)
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
			start, end int, f0 Field, f1 Field, fs []Field) []Field {
			return []Field(append([]Field{f0}, fs...))
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
	// _ "[" cases:Cases _ "]"
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
	// cases:Cases
	{
		pos1 := pos
		// Cases
		if !_accept(parser, _CasesAccepts, &pos, &perr) {
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
	// _ "[" cases:Cases _ "]"
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
	// cases:Cases
	{
		pos1 := pos
		// Cases
		if !_fail(parser, _CasesFail, errPos, failure, &pos) {
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
	var label0 []Case
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
		// _ "[" cases:Cases _ "]"
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
		// cases:Cases
		{
			pos2 := pos
			// Cases
			if p, n := _CasesAction(parser, pos); n == nil {
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
			start, end int, cases []Case) Type {
			return Type(&UnionType{Cases: cases, L: l(parser, start, end)})
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CaseAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Case, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Id (_ ":" typ:Type)?
	// name:Id
	{
		pos1 := pos
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// (_ ":" typ:Type)?
	{
		pos3 := pos
		// (_ ":" typ:Type)
		// _ ":" typ:Type
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// typ:Type
		{
			pos6 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail4
			}
			labels[1] = parser.text[pos6:pos]
		}
		goto ok7
	fail4:
		pos = pos3
	ok7:
	}
	return _memoize(parser, _Case, start, pos, perr)
fail:
	return _memoize(parser, _Case, start, -1, perr)
}

func _CaseFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Case, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Case",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Case}
	// action
	// name:Id (_ ":" typ:Type)?
	// name:Id
	{
		pos1 := pos
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// (_ ":" typ:Type)?
	{
		pos3 := pos
		// (_ ":" typ:Type)
		// _ ":" typ:Type
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\":\"",
				})
			}
			goto fail4
		}
		pos++
		// typ:Type
		{
			pos6 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail4
			}
			labels[1] = parser.text[pos6:pos]
		}
		goto ok7
	fail4:
		pos = pos3
	ok7:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CaseAction(parser *_Parser, start int) (int, *Case) {
	var labels [2]string
	use(labels)
	var label0 Id
	var label1 Type
	dp := parser.deltaPos[start][_Case]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Case}
	n := parser.act[key]
	if n != nil {
		n := n.(Case)
		return start + int(dp-1), &n
	}
	var node Case
	pos := start
	// action
	{
		start0 := pos
		// name:Id (_ ":" typ:Type)?
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
		// (_ ":" typ:Type)?
		{
			pos4 := pos
			// (_ ":" typ:Type)
			// _ ":" typ:Type
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail5
			} else {
				pos = p
			}
			// ":"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
				goto fail5
			}
			pos++
			// typ:Type
			{
				pos7 := pos
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail5
				} else {
					label1 = *n
					pos = p
				}
				labels[1] = parser.text[pos7:pos]
			}
			goto ok8
		fail5:
			pos = pos4
		ok8:
		}
		node = func(
			start, end int, name Id, typ Type) Case {
			return Case{
				Name: name,
				Type: typ,
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CasesAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Cases, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// (_ "|")? c0:Case cs:(_ "|" c1:Case {…})*
	// (_ "|")?
	{
		pos2 := pos
		// (_ "|")
		// _ "|"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail3
		}
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			perr = _max(perr, pos)
			goto fail3
		}
		pos++
		goto ok5
	fail3:
		pos = pos2
	ok5:
	}
	// c0:Case
	{
		pos6 := pos
		// Case
		if !_accept(parser, _CaseAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos6:pos]
	}
	// cs:(_ "|" c1:Case {…})*
	{
		pos7 := pos
		// (_ "|" c1:Case {…})*
		for {
			pos9 := pos
			// (_ "|" c1:Case {…})
			// action
			// _ "|" c1:Case
			// _
			if !_accept(parser, __Accepts, &pos, &perr) {
				goto fail11
			}
			// "|"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
				perr = _max(perr, pos)
				goto fail11
			}
			pos++
			// c1:Case
			{
				pos13 := pos
				// Case
				if !_accept(parser, _CaseAccepts, &pos, &perr) {
					goto fail11
				}
				labels[1] = parser.text[pos13:pos]
			}
			continue
		fail11:
			pos = pos9
			break
		}
		labels[2] = parser.text[pos7:pos]
	}
	return _memoize(parser, _Cases, start, pos, perr)
fail:
	return _memoize(parser, _Cases, start, -1, perr)
}

func _CasesFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
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
	// (_ "|")? c0:Case cs:(_ "|" c1:Case {…})*
	// (_ "|")?
	{
		pos2 := pos
		// (_ "|")
		// _ "|"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail3
		}
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"|\"",
				})
			}
			goto fail3
		}
		pos++
		goto ok5
	fail3:
		pos = pos2
	ok5:
	}
	// c0:Case
	{
		pos6 := pos
		// Case
		if !_fail(parser, _CaseFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos6:pos]
	}
	// cs:(_ "|" c1:Case {…})*
	{
		pos7 := pos
		// (_ "|" c1:Case {…})*
		for {
			pos9 := pos
			// (_ "|" c1:Case {…})
			// action
			// _ "|" c1:Case
			// _
			if !_fail(parser, __Fail, errPos, failure, &pos) {
				goto fail11
			}
			// "|"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "\"|\"",
					})
				}
				goto fail11
			}
			pos++
			// c1:Case
			{
				pos13 := pos
				// Case
				if !_fail(parser, _CaseFail, errPos, failure, &pos) {
					goto fail11
				}
				labels[1] = parser.text[pos13:pos]
			}
			continue
		fail11:
			pos = pos9
			break
		}
		labels[2] = parser.text[pos7:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CasesAction(parser *_Parser, start int) (int, *[]Case) {
	var labels [3]string
	use(labels)
	var label0 Case
	var label1 Case
	var label2 []Case
	dp := parser.deltaPos[start][_Cases]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Cases}
	n := parser.act[key]
	if n != nil {
		n := n.([]Case)
		return start + int(dp-1), &n
	}
	var node []Case
	pos := start
	// action
	{
		start0 := pos
		// (_ "|")? c0:Case cs:(_ "|" c1:Case {…})*
		// (_ "|")?
		{
			pos3 := pos
			// (_ "|")
			// _ "|"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "|"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
				goto fail4
			}
			pos++
			goto ok6
		fail4:
			pos = pos3
		ok6:
		}
		// c0:Case
		{
			pos7 := pos
			// Case
			if p, n := _CaseAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos7:pos]
		}
		// cs:(_ "|" c1:Case {…})*
		{
			pos8 := pos
			// (_ "|" c1:Case {…})*
			for {
				pos10 := pos
				var node11 Case
				// (_ "|" c1:Case {…})
				// action
				{
					start13 := pos
					// _ "|" c1:Case
					// _
					if p, n := __Action(parser, pos); n == nil {
						goto fail12
					} else {
						pos = p
					}
					// "|"
					if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
						goto fail12
					}
					pos++
					// c1:Case
					{
						pos15 := pos
						// Case
						if p, n := _CaseAction(parser, pos); n == nil {
							goto fail12
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos15:pos]
					}
					node11 = func(
						start, end int, c0 Case, c1 Case) Case {
						return Case(c1)
					}(
						start13, pos, label0, label1)
				}
				label2 = append(label2, node11)
				continue
			fail12:
				pos = pos10
				break
			}
			labels[2] = parser.text[pos8:pos]
		}
		node = func(
			start, end int, c0 Case, c1 Case, cs []Case) []Case {
			return []Case(append([]Case{c0}, cs...))
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
	// _ "{" _ "}" {…}/_ "{" ps:Types? _ "|" r:Type? _ "}" {…}
	{
		pos3 := pos
		// action
		// _ "{" _ "}"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail4
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "{" ps:Types? _ "|" r:Type? _ "}"
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		// ps:Types?
		{
			pos8 := pos
			// Types?
			{
				pos10 := pos
				// Types
				if !_accept(parser, _TypesAccepts, &pos, &perr) {
					goto fail11
				}
				goto ok12
			fail11:
				pos = pos10
			ok12:
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos++
		// r:Type?
		{
			pos13 := pos
			// Type?
			{
				pos15 := pos
				// Type
				if !_accept(parser, _TypeAccepts, &pos, &perr) {
					goto fail16
				}
				goto ok17
			fail16:
				pos = pos15
			ok17:
			}
			labels[1] = parser.text[pos13:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail6
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
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
	// _ "{" _ "}" {…}/_ "{" ps:Types? _ "|" r:Type? _ "}" {…}
	{
		pos3 := pos
		// action
		// _ "{" _ "}"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"{\"",
				})
			}
			goto fail4
		}
		pos++
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail4
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"}\"",
				})
			}
			goto fail4
		}
		pos++
		goto ok0
	fail4:
		pos = pos3
		// action
		// _ "{" ps:Types? _ "|" r:Type? _ "}"
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "{"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"{\"",
				})
			}
			goto fail6
		}
		pos++
		// ps:Types?
		{
			pos8 := pos
			// Types?
			{
				pos10 := pos
				// Types
				if !_fail(parser, _TypesFail, errPos, failure, &pos) {
					goto fail11
				}
				goto ok12
			fail11:
				pos = pos10
			ok12:
			}
			labels[0] = parser.text[pos8:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"|\"",
				})
			}
			goto fail6
		}
		pos++
		// r:Type?
		{
			pos13 := pos
			// Type?
			{
				pos15 := pos
				// Type
				if !_fail(parser, _TypeFail, errPos, failure, &pos) {
					goto fail16
				}
				goto ok17
			fail16:
				pos = pos15
			ok17:
			}
			labels[1] = parser.text[pos13:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail6
		}
		// "}"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"}\"",
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
	// _ "{" _ "}" {…}/_ "{" ps:Types? _ "|" r:Type? _ "}" {…}
	{
		pos3 := pos
		var node2 Type
		// action
		{
			start5 := pos
			// _ "{" _ "}"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "{"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
				goto fail4
			}
			pos++
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "}"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
				goto fail4
			}
			pos++
			node = func(
				start, end int) Type {
				return Type(&FuncType{L: l(parser, start, end)})
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
			// _ "{" ps:Types? _ "|" r:Type? _ "}"
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "{"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "{" {
				goto fail7
			}
			pos++
			// ps:Types?
			{
				pos10 := pos
				// Types?
				{
					pos12 := pos
					label0 = new([]Type)
					// Types
					if p, n := _TypesAction(parser, pos); n == nil {
						goto fail13
					} else {
						*label0 = *n
						pos = p
					}
					goto ok14
				fail13:
					label0 = nil
					pos = pos12
				ok14:
				}
				labels[0] = parser.text[pos10:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "|"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
				goto fail7
			}
			pos++
			// r:Type?
			{
				pos15 := pos
				// Type?
				{
					pos17 := pos
					label1 = new(Type)
					// Type
					if p, n := _TypeAction(parser, pos); n == nil {
						goto fail18
					} else {
						*label1 = *n
						pos = p
					}
					goto ok19
				fail18:
					label1 = nil
					pos = pos17
				ok19:
				}
				labels[1] = parser.text[pos15:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail7
			} else {
				pos = p
			}
			// "}"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "}" {
				goto fail7
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
				return Type(&FuncType{
					Parms: parms,
					Ret:   ret,
					L:     l(parser, start, end),
				})
			}(
				start8, pos, label0, label1)
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
	var label1 Id
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
			start, end int, es *[]Expr, iface []FuncDecl, name Id, ps *[]FuncParm, r *Type, reserved string) Def {
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

func _FuncNameAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _FuncName, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// DotId/IdxOp/Op/Id/Kwds
	{
		pos3 := pos
		// DotId
		if !_accept(parser, _DotIdAccepts, &pos, &perr) {
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
		// Id
		if !_accept(parser, _IdAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// Kwds
		if !_accept(parser, _KwdsAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
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
	// DotId/IdxOp/Op/Id/Kwds
	{
		pos3 := pos
		// DotId
		if !_fail(parser, _DotIdFail, errPos, failure, &pos) {
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
		// Id
		if !_fail(parser, _IdFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// Kwds
		if !_fail(parser, _KwdsFail, errPos, failure, &pos) {
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

func _FuncNameAction(parser *_Parser, start int) (int, *Id) {
	dp := parser.deltaPos[start][_FuncName]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _FuncName}
	n := parser.act[key]
	if n != nil {
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
	pos := start
	// DotId/IdxOp/Op/Id/Kwds
	{
		pos3 := pos
		var node2 Id
		// DotId
		if p, n := _DotIdAction(parser, pos); n == nil {
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
		// Id
		if p, n := _IdAction(parser, pos); n == nil {
			goto fail7
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// Kwds
		if p, n := _KwdsAction(parser, pos); n == nil {
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

func _IdxOpAction(parser *_Parser, start int) (int, *Id) {
	dp := parser.deltaPos[start][_IdxOp]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _IdxOp}
	n := parser.act[key]
	if n != nil {
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int) Id {
			return Id{Name: "[]", L: l(parser, start, end)}
		}(
			start0, pos)
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
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _FuncParm, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// name:Id typ:Type (_ "=" init:Expr)?
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
	// (_ "=" init:Expr)?
	{
		pos4 := pos
		// (_ "=" init:Expr)
		// _ "=" init:Expr
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail5
		}
		// "="
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
			perr = _max(perr, pos)
			goto fail5
		}
		pos++
		// init:Expr
		{
			pos7 := pos
			// Expr
			if !_accept(parser, _ExprAccepts, &pos, &perr) {
				goto fail5
			}
			labels[2] = parser.text[pos7:pos]
		}
		goto ok8
	fail5:
		pos = pos4
	ok8:
	}
	return _memoize(parser, _FuncParm, start, pos, perr)
fail:
	return _memoize(parser, _FuncParm, start, -1, perr)
}

func _FuncParmFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
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
	// name:Id typ:Type (_ "=" init:Expr)?
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
	// (_ "=" init:Expr)?
	{
		pos4 := pos
		// (_ "=" init:Expr)
		// _ "=" init:Expr
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail5
		}
		// "="
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"=\"",
				})
			}
			goto fail5
		}
		pos++
		// init:Expr
		{
			pos7 := pos
			// Expr
			if !_fail(parser, _ExprFail, errPos, failure, &pos) {
				goto fail5
			}
			labels[2] = parser.text[pos7:pos]
		}
		goto ok8
	fail5:
		pos = pos4
	ok8:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _FuncParmAction(parser *_Parser, start int) (int, *FuncParm) {
	var labels [3]string
	use(labels)
	var label0 Id
	var label1 Type
	var label2 Expr
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
		// name:Id typ:Type (_ "=" init:Expr)?
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
		// (_ "=" init:Expr)?
		{
			pos5 := pos
			// (_ "=" init:Expr)
			// _ "=" init:Expr
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail6
			} else {
				pos = p
			}
			// "="
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "=" {
				goto fail6
			}
			pos++
			// init:Expr
			{
				pos8 := pos
				// Expr
				if p, n := _ExprAction(parser, pos); n == nil {
					goto fail6
				} else {
					label2 = *n
					pos = p
				}
				labels[2] = parser.text[pos8:pos]
			}
			goto ok9
		fail6:
			pos = pos5
		ok9:
		}
		node = func(
			start, end int, init Expr, name Id, typ Type) FuncParm {
			return FuncParm{
				Name: name,
				Type: typ,
				Init: init,
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label2, label0, label1)
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
	// name:FuncName _ "(" parms:Types _ ")" r:Type?
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
	// parms:Types
	{
		pos2 := pos
		// Types
		if !_accept(parser, _TypesAccepts, &pos, &perr) {
			goto fail
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
		pos3 := pos
		// Type?
		{
			pos5 := pos
			// Type
			if !_accept(parser, _TypeAccepts, &pos, &perr) {
				goto fail6
			}
			goto ok7
		fail6:
			pos = pos5
		ok7:
		}
		labels[2] = parser.text[pos3:pos]
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
	// name:FuncName _ "(" parms:Types _ ")" r:Type?
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
	// parms:Types
	{
		pos2 := pos
		// Types
		if !_fail(parser, _TypesFail, errPos, failure, &pos) {
			goto fail
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
		pos3 := pos
		// Type?
		{
			pos5 := pos
			// Type
			if !_fail(parser, _TypeFail, errPos, failure, &pos) {
				goto fail6
			}
			goto ok7
		fail6:
			pos = pos5
		ok7:
		}
		labels[2] = parser.text[pos3:pos]
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
	var label0 Id
	var label1 []Type
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
		// name:FuncName _ "(" parms:Types _ ")" r:Type?
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
		// parms:Types
		{
			pos3 := pos
			// Types
			if p, n := _TypesAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
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
			pos4 := pos
			// Type?
			{
				pos6 := pos
				label2 = new(Type)
				// Type
				if p, n := _TypeAction(parser, pos); n == nil {
					goto fail7
				} else {
					*label2 = *n
					pos = p
				}
				goto ok8
			fail7:
				label2 = nil
				pos = pos6
			ok8:
			}
			labels[2] = parser.text[pos4:pos]
		}
		node = func(
			start, end int, name Id, parms []Type, r *Type) FuncDecl {
			var ret Type
			if r != nil {
				ret = *r
			}
			return FuncDecl{
				Name:  name,
				Parms: parms,
				Ret:   ret,
				L:     l(parser, start, end),
			}
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
	var label0 Id
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
			start, end int, es *[]Expr, name Id) Def {
			var exprs []Expr
			if es != nil {
				exprs = *es
			}
			return Def(&TestDef{
				Name:  name,
				Exprs: exprs,
				L:     l(parser, start, end),
			})
		}(
			start0, pos, label1, label0)
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

func _AsgnOpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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
	// kws:Kw+
	{
		pos0 := pos
		// Kw+
		// Kw
		if !_accept(parser, _KwAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos2 := pos
			// Kw
			if !_accept(parser, _KwAccepts, &pos, &perr) {
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
	// kws:Kw+
	{
		pos0 := pos
		// Kw+
		// Kw
		if !_fail(parser, _KwFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos2 := pos
			// Kw
			if !_fail(parser, _KwFail, errPos, failure, &pos) {
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
	var label0 []kw
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
		// kws:Kw+
		{
			pos1 := pos
			// Kw+
			{
				var node4 kw
				// Kw
				if p, n := _KwAction(parser, pos); n == nil {
					goto fail
				} else {
					node4 = *n
					pos = p
				}
				label0 = append(label0, node4)
			}
			for {
				pos3 := pos
				var node4 kw
				// Kw
				if p, n := _KwAction(parser, pos); n == nil {
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
			start, end int, kws []kw) Expr {
			return Expr(kwCall(l(parser, start, end), kws))
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _KwAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Kw, start); ok {
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
	return _memoize(parser, _Kw, start, pos, perr)
fail:
	return _memoize(parser, _Kw, start, -1, perr)
}

func _KwFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _Kw, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Kw",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Kw}
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

func _KwAction(parser *_Parser, start int) (int, *kw) {
	var labels [2]string
	use(labels)
	var label0 Id
	var label1 Expr
	dp := parser.deltaPos[start][_Kw]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Kw}
	n := parser.act[key]
	if n != nil {
		n := n.(kw)
		return start + int(dp-1), &n
	}
	var node kw
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
			start, end int, arg Expr, name Id) kw {
			return kw{name: name, arg: arg}
		}(
			start0, pos, label1, label0)
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

func _KwdAction(parser *_Parser, start int) (int, *Id) {
	var labels [1]string
	use(labels)
	var label0 Id
	dp := parser.deltaPos[start][_Kwd]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Kwd}
	n := parser.act[key]
	if n != nil {
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, id Id) Id {
			return Id{Name: id.Name + ":", L: l(parser, start, end)}
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

func _KwdsAction(parser *_Parser, start int) (int, *Id) {
	var labels [1]string
	use(labels)
	var label0 []Id
	dp := parser.deltaPos[start][_Kwds]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Kwds}
	n := parser.act[key]
	if n != nil {
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
				var node5 Id
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
				var node5 Id
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
			start, end int, keywords []Id) Id {
			var name string
			for _, k := range keywords {
				name += k.Name
			}
			return Id{Name: name, L: l(parser, start, end)}
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

func _Bin5OpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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

func _Bin4OpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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

func _Bin3OpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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

func _Bin2OpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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

func _Bin1OpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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
	// expr:UnArg _ ":" typ:Type
	// expr:UnArg
	{
		pos1 := pos
		// UnArg
		if !_accept(parser, _UnArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// ":"
	if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
		perr = _max(perr, pos)
		goto fail
	}
	pos++
	// typ:Type
	{
		pos2 := pos
		// Type
		if !_accept(parser, _TypeAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
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
	// expr:UnArg _ ":" typ:Type
	// expr:UnArg
	{
		pos1 := pos
		// UnArg
		if !_fail(parser, _UnArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
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

func _CvtAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 Type
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
		// expr:UnArg _ ":" typ:Type
		// expr:UnArg
		{
			pos2 := pos
			// UnArg
			if p, n := _UnArgAction(parser, pos); n == nil {
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
		// ":"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != ":" {
			goto fail
		}
		pos++
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
			start, end int, expr Expr, typ Type) Expr {
			return Expr(&Convert{
				Expr: expr,
				Type: typ,
				L:    l(parser, start, end),
			})
		}(
			start0, pos, label0, label1)
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
	// name:Op arg:UnArg
	// name:Op
	{
		pos1 := pos
		// Op
		if !_accept(parser, _OpAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg:UnArg
	{
		pos2 := pos
		// UnArg
		if !_accept(parser, _UnArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos2:pos]
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
	// name:Op arg:UnArg
	// name:Op
	{
		pos1 := pos
		// Op
		if !_fail(parser, _OpFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// arg:UnArg
	{
		pos2 := pos
		// UnArg
		if !_fail(parser, _UnArgFail, errPos, failure, &pos) {
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

func _UnAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Id
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
		// name:Op arg:UnArg
		// name:Op
		{
			pos2 := pos
			// Op
			if p, n := _OpAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// arg:UnArg
		{
			pos3 := pos
			// UnArg
			if p, n := _UnArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, arg Expr, name Id) Expr {
			return Expr(&Call{
				Fun:  name,
				Args: []Expr{arg},
				L:    l(parser, start, end),
			})
		}(
			start0, pos, label1, label0)
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
	// Un/Idx/Sel/Call/Pri
	{
		pos3 := pos
		// Un
		if !_accept(parser, _UnAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Idx
		if !_accept(parser, _IdxAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// Sel
		if !_accept(parser, _SelAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// Call
		if !_accept(parser, _CallAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// Pri
		if !_accept(parser, _PriAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
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
	// Un/Idx/Sel/Call/Pri
	{
		pos3 := pos
		// Un
		if !_fail(parser, _UnFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Idx
		if !_fail(parser, _IdxFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// Sel
		if !_fail(parser, _SelFail, errPos, failure, &pos) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		// Call
		if !_fail(parser, _CallFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// Pri
		if !_fail(parser, _PriFail, errPos, failure, &pos) {
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
	// Un/Idx/Sel/Call/Pri
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
		// Idx
		if p, n := _IdxAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// Sel
		if p, n := _SelAction(parser, pos); n == nil {
			goto fail6
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// Call
		if p, n := _CallAction(parser, pos); n == nil {
			goto fail7
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// Pri
		if p, n := _PriAction(parser, pos); n == nil {
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

func _OpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Op, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ name:[\-+*/%~!?\^<>=&|@$]+
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// name:[\-+*/%~!?\^<>=&|@$]+
	{
		pos1 := pos
		// [\-+*/%~!?\^<>=&|@$]+
		// [\-+*/%~!?\^<>=&|@$]
		if r, w := _next(parser, pos); r != '-' && r != '+' && r != '*' && r != '/' && r != '%' && r != '~' && r != '!' && r != '?' && r != '^' && r != '<' && r != '>' && r != '=' && r != '&' && r != '|' && r != '@' && r != '$' {
			perr = _max(perr, pos)
			goto fail
		} else {
			pos += w
		}
		for {
			pos3 := pos
			// [\-+*/%~!?\^<>=&|@$]
			if r, w := _next(parser, pos); r != '-' && r != '+' && r != '*' && r != '/' && r != '%' && r != '~' && r != '!' && r != '?' && r != '^' && r != '<' && r != '>' && r != '=' && r != '&' && r != '|' && r != '@' && r != '$' {
				perr = _max(perr, pos)
				goto fail5
			} else {
				pos += w
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
	// _ name:[\-+*/%~!?\^<>=&|@$]+
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// name:[\-+*/%~!?\^<>=&|@$]+
	{
		pos1 := pos
		// [\-+*/%~!?\^<>=&|@$]+
		// [\-+*/%~!?\^<>=&|@$]
		if r, w := _next(parser, pos); r != '-' && r != '+' && r != '*' && r != '/' && r != '%' && r != '~' && r != '!' && r != '?' && r != '^' && r != '<' && r != '>' && r != '=' && r != '&' && r != '|' && r != '@' && r != '$' {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "[\\-+*/%~!?\\^<>=&|@$]",
				})
			}
			goto fail
		} else {
			pos += w
		}
		for {
			pos3 := pos
			// [\-+*/%~!?\^<>=&|@$]
			if r, w := _next(parser, pos); r != '-' && r != '+' && r != '*' && r != '/' && r != '%' && r != '~' && r != '!' && r != '?' && r != '^' && r != '<' && r != '>' && r != '=' && r != '&' && r != '|' && r != '@' && r != '$' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "[\\-+*/%~!?\\^<>=&|@$]",
					})
				}
				goto fail5
			} else {
				pos += w
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

func _OpAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
	pos := start
	// action
	{
		start0 := pos
		// _ name:[\-+*/%~!?\^<>=&|@$]+
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// name:[\-+*/%~!?\^<>=&|@$]+
		{
			pos2 := pos
			// [\-+*/%~!?\^<>=&|@$]+
			{
				var node5 string
				// [\-+*/%~!?\^<>=&|@$]
				if r, w := _next(parser, pos); r != '-' && r != '+' && r != '*' && r != '/' && r != '%' && r != '~' && r != '!' && r != '?' && r != '^' && r != '<' && r != '>' && r != '=' && r != '&' && r != '|' && r != '@' && r != '$' {
					goto fail
				} else {
					node5 = parser.text[pos : pos+w]
					pos += w
				}
				label0 += node5
			}
			for {
				pos4 := pos
				var node5 string
				// [\-+*/%~!?\^<>=&|@$]
				if r, w := _next(parser, pos); r != '-' && r != '+' && r != '*' && r != '/' && r != '%' && r != '~' && r != '!' && r != '?' && r != '^' && r != '<' && r != '>' && r != '=' && r != '&' && r != '|' && r != '@' && r != '$' {
					goto fail6
				} else {
					node5 = parser.text[pos : pos+w]
					pos += w
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _IdxAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Idx, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// expr:IdxArg indexes:Idxs+
	// expr:IdxArg
	{
		pos1 := pos
		// IdxArg
		if !_accept(parser, _IdxArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// indexes:Idxs+
	{
		pos2 := pos
		// Idxs+
		// Idxs
		if !_accept(parser, _IdxsAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// Idxs
			if !_accept(parser, _IdxsAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Idx, start, pos, perr)
fail:
	return _memoize(parser, _Idx, start, -1, perr)
}

func _IdxFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
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
	// expr:IdxArg indexes:Idxs+
	// expr:IdxArg
	{
		pos1 := pos
		// IdxArg
		if !_fail(parser, _IdxArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// indexes:Idxs+
	{
		pos2 := pos
		// Idxs+
		// Idxs
		if !_fail(parser, _IdxsFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// Idxs
			if !_fail(parser, _IdxsFail, errPos, failure, &pos) {
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

func _IdxAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Idx]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Idx}
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
		// expr:IdxArg indexes:Idxs+
		// expr:IdxArg
		{
			pos2 := pos
			// IdxArg
			if p, n := _IdxArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// indexes:Idxs+
		{
			pos3 := pos
			// Idxs+
			{
				var node6 *Call
				// Idxs
				if p, n := _IdxsAction(parser, pos); n == nil {
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
				// Idxs
				if p, n := _IdxsAction(parser, pos); n == nil {
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
			start, end int, expr Expr, indexes []*Call) Expr {
			return Expr(bins(expr, indexes))
		}(
			start0, pos, label0, label1)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _IdxsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _Idxs, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "[" indices:Exprs _ "]"
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
	// indices:Exprs
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
	return _memoize(parser, _Idxs, start, pos, perr)
fail:
	return _memoize(parser, _Idxs, start, -1, perr)
}

func _IdxsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _Idxs, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "Idxs",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _Idxs}
	// action
	// _ "[" indices:Exprs _ "]"
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
	// indices:Exprs
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

func _IdxsAction(parser *_Parser, start int) (int, **Call) {
	var labels [1]string
	use(labels)
	var label0 []Expr
	dp := parser.deltaPos[start][_Idxs]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Idxs}
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
		// _ "[" indices:Exprs _ "]"
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
		// indices:Exprs
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
			start, end int, indices []Expr) *Call {
			return &Call{
				Fun:  Id{Name: "[]", L: l(parser, start, end)},
				Args: append([]Expr{nil}, indices...),
				L:    l(parser, start, end),
			}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _IdxArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _IdxArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Call/Sel/Pri
	{
		pos3 := pos
		// Call
		if !_accept(parser, _CallAccepts, &pos, &perr) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Sel
		if !_accept(parser, _SelAccepts, &pos, &perr) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// Pri
		if !_accept(parser, _PriAccepts, &pos, &perr) {
			goto fail6
		}
		goto ok0
	fail6:
		pos = pos3
		goto fail
	ok0:
	}
	return _memoize(parser, _IdxArg, start, pos, perr)
fail:
	return _memoize(parser, _IdxArg, start, -1, perr)
}

func _IdxArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _IdxArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "IdxArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _IdxArg}
	// Call/Sel/Pri
	{
		pos3 := pos
		// Call
		if !_fail(parser, _CallFail, errPos, failure, &pos) {
			goto fail4
		}
		goto ok0
	fail4:
		pos = pos3
		// Sel
		if !_fail(parser, _SelFail, errPos, failure, &pos) {
			goto fail5
		}
		goto ok0
	fail5:
		pos = pos3
		// Pri
		if !_fail(parser, _PriFail, errPos, failure, &pos) {
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

func _IdxArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_IdxArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _IdxArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Call/Sel/Pri
	{
		pos3 := pos
		var node2 Expr
		// Call
		if p, n := _CallAction(parser, pos); n == nil {
			goto fail4
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// Sel
		if p, n := _SelAction(parser, pos); n == nil {
			goto fail5
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// Pri
		if p, n := _PriAction(parser, pos); n == nil {
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

func _CallAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _Call, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// fun:CallArg argss:ArgList+
	// fun:CallArg
	{
		pos1 := pos
		// CallArg
		if !_accept(parser, _CallArgAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// argss:ArgList+
	{
		pos2 := pos
		// ArgList+
		// ArgList
		if !_accept(parser, _ArgListAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// ArgList
			if !_accept(parser, _ArgListAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
			break
		}
		labels[1] = parser.text[pos2:pos]
	}
	return _memoize(parser, _Call, start, pos, perr)
fail:
	return _memoize(parser, _Call, start, -1, perr)
}

func _CallFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
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
	// fun:CallArg argss:ArgList+
	// fun:CallArg
	{
		pos1 := pos
		// CallArg
		if !_fail(parser, _CallArgFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// argss:ArgList+
	{
		pos2 := pos
		// ArgList+
		// ArgList
		if !_fail(parser, _ArgListFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// ArgList
			if !_fail(parser, _ArgListFail, errPos, failure, &pos) {
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

func _CallAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 []*Call
	dp := parser.deltaPos[start][_Call]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Call}
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
		// fun:CallArg argss:ArgList+
		// fun:CallArg
		{
			pos2 := pos
			// CallArg
			if p, n := _CallArgAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// argss:ArgList+
		{
			pos3 := pos
			// ArgList+
			{
				var node6 *Call
				// ArgList
				if p, n := _ArgListAction(parser, pos); n == nil {
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
				// ArgList
				if p, n := _ArgListAction(parser, pos); n == nil {
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
			start, end int, argss []*Call, fun Expr) Expr {
			return Expr(calls(fun, argss))
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _ArgListAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _ArgList, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "(" as:Exprs? _ ")"
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
	// as:Exprs?
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
	return _memoize(parser, _ArgList, start, pos, perr)
fail:
	return _memoize(parser, _ArgList, start, -1, perr)
}

func _ArgListFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _ArgList, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "ArgList",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _ArgList}
	// action
	// _ "(" as:Exprs? _ ")"
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
	// as:Exprs?
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

func _ArgListAction(parser *_Parser, start int) (int, **Call) {
	var labels [1]string
	use(labels)
	var label0 *[]Expr
	dp := parser.deltaPos[start][_ArgList]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _ArgList}
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
		// _ "(" as:Exprs? _ ")"
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
		// as:Exprs?
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
			start, end int, as *[]Expr) *Call {
			return &Call{Args: args(as), L: l(parser, start, end)}
		}(
			start0, pos, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CallArgAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	if dp, de, ok := _memo(parser, _CallArg, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// Sel/Pri
	{
		pos3 := pos
		// Sel
		if !_accept(parser, _SelAccepts, &pos, &perr) {
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
		goto fail
	ok0:
	}
	return _memoize(parser, _CallArg, start, pos, perr)
fail:
	return _memoize(parser, _CallArg, start, -1, perr)
}

func _CallArgFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	pos, failure := _failMemo(parser, _CallArg, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CallArg",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CallArg}
	// Sel/Pri
	{
		pos3 := pos
		// Sel
		if !_fail(parser, _SelFail, errPos, failure, &pos) {
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
		goto fail
	ok0:
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _CallArgAction(parser *_Parser, start int) (int, *Expr) {
	dp := parser.deltaPos[start][_CallArg]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CallArg}
	n := parser.act[key]
	if n != nil {
		n := n.(Expr)
		return start + int(dp-1), &n
	}
	var node Expr
	pos := start
	// Sel/Pri
	{
		pos3 := pos
		var node2 Expr
		// Sel
		if p, n := _SelAction(parser, pos); n == nil {
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
		goto fail
	ok0:
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _SelAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [3]string
	use(labels)
	if dp, de, ok := _memo(parser, _Sel, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// expr:Pri ids:(id:DotId {…})+
	// expr:Pri
	{
		pos1 := pos
		// Pri
		if !_accept(parser, _PriAccepts, &pos, &perr) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ids:(id:DotId {…})+
	{
		pos2 := pos
		// (id:DotId {…})+
		// (id:DotId {…})
		// action
		// id:DotId
		{
			pos7 := pos
			// DotId
			if !_accept(parser, _DotIdAccepts, &pos, &perr) {
				goto fail
			}
			labels[1] = parser.text[pos7:pos]
		}
		for {
			pos4 := pos
			// (id:DotId {…})
			// action
			// id:DotId
			{
				pos8 := pos
				// DotId
				if !_accept(parser, _DotIdAccepts, &pos, &perr) {
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
	return _memoize(parser, _Sel, start, pos, perr)
fail:
	return _memoize(parser, _Sel, start, -1, perr)
}

func _SelFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [3]string
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
	// expr:Pri ids:(id:DotId {…})+
	// expr:Pri
	{
		pos1 := pos
		// Pri
		if !_fail(parser, _PriFail, errPos, failure, &pos) {
			goto fail
		}
		labels[0] = parser.text[pos1:pos]
	}
	// ids:(id:DotId {…})+
	{
		pos2 := pos
		// (id:DotId {…})+
		// (id:DotId {…})
		// action
		// id:DotId
		{
			pos7 := pos
			// DotId
			if !_fail(parser, _DotIdFail, errPos, failure, &pos) {
				goto fail
			}
			labels[1] = parser.text[pos7:pos]
		}
		for {
			pos4 := pos
			// (id:DotId {…})
			// action
			// id:DotId
			{
				pos8 := pos
				// DotId
				if !_fail(parser, _DotIdFail, errPos, failure, &pos) {
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
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _SelAction(parser *_Parser, start int) (int, *Expr) {
	var labels [3]string
	use(labels)
	var label0 Expr
	var label1 Id
	var label2 []Id
	dp := parser.deltaPos[start][_Sel]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _Sel}
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
		// expr:Pri ids:(id:DotId {…})+
		// expr:Pri
		{
			pos2 := pos
			// Pri
			if p, n := _PriAction(parser, pos); n == nil {
				goto fail
			} else {
				label0 = *n
				pos = p
			}
			labels[0] = parser.text[pos2:pos]
		}
		// ids:(id:DotId {…})+
		{
			pos3 := pos
			// (id:DotId {…})+
			{
				var node6 Id
				// (id:DotId {…})
				// action
				{
					start8 := pos
					// id:DotId
					{
						pos9 := pos
						// DotId
						if p, n := _DotIdAction(parser, pos); n == nil {
							goto fail
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos9:pos]
					}
					node6 = func(
						start, end int, expr Expr, id Id) Id {
						return Id(id)
					}(
						start8, pos, label0, label1)
				}
				label2 = append(label2, node6)
			}
			for {
				pos5 := pos
				var node6 Id
				// (id:DotId {…})
				// action
				{
					start10 := pos
					// id:DotId
					{
						pos11 := pos
						// DotId
						if p, n := _DotIdAction(parser, pos); n == nil {
							goto fail7
						} else {
							label1 = *n
							pos = p
						}
						labels[1] = parser.text[pos11:pos]
					}
					node6 = func(
						start, end int, expr Expr, id Id) Id {
						return Id(id)
					}(
						start10, pos, label0, label1)
				}
				label2 = append(label2, node6)
				continue
			fail7:
				pos = pos5
				break
			}
			labels[2] = parser.text[pos3:pos]
		}
		node = func(
			start, end int, expr Expr, id Id, ids []Id) Expr {
			return Expr(sel(expr, ids))
		}(
			start0, pos, label0, label1, label2)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _DotIdAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _DotId, start); ok {
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
	return _memoize(parser, _DotId, start, pos, perr)
fail:
	return _memoize(parser, _DotId, start, -1, perr)
}

func _DotIdFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _DotId, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "DotId",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _DotId}
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
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _DotIdAction(parser *_Parser, start int) (int, *Id) {
	var labels [1]string
	use(labels)
	var label0 Id
	dp := parser.deltaPos[start][_DotId]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _DotId}
	n := parser.act[key]
	if n != nil {
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, id Id) Id {
			return Id{Name: "." + id.Name, L: l(parser, start, end)}
		}(
			start0, pos, label0)
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
	// _ "(" expr:Expr _ ")" {…}/QualOp/QualKwds/CompLit/BlkLit/CharLit/StrLit/NumLit/id:Id {…}
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
		// QualOp
		if !_accept(parser, _QualOpAccepts, &pos, &perr) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// QualKwds
		if !_accept(parser, _QualKwdsAccepts, &pos, &perr) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		// CompLit
		if !_accept(parser, _CompLitAccepts, &pos, &perr) {
			goto fail9
		}
		goto ok0
	fail9:
		pos = pos3
		// BlkLit
		if !_accept(parser, _BlkLitAccepts, &pos, &perr) {
			goto fail10
		}
		goto ok0
	fail10:
		pos = pos3
		// CharLit
		if !_accept(parser, _CharLitAccepts, &pos, &perr) {
			goto fail11
		}
		goto ok0
	fail11:
		pos = pos3
		// StrLit
		if !_accept(parser, _StrLitAccepts, &pos, &perr) {
			goto fail12
		}
		goto ok0
	fail12:
		pos = pos3
		// NumLit
		if !_accept(parser, _NumLitAccepts, &pos, &perr) {
			goto fail13
		}
		goto ok0
	fail13:
		pos = pos3
		// action
		// id:Id
		{
			pos15 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail14
			}
			labels[1] = parser.text[pos15:pos]
		}
		goto ok0
	fail14:
		pos = pos3
		goto fail
	ok0:
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
	// _ "(" expr:Expr _ ")" {…}/QualOp/QualKwds/CompLit/BlkLit/CharLit/StrLit/NumLit/id:Id {…}
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
		// QualOp
		if !_fail(parser, _QualOpFail, errPos, failure, &pos) {
			goto fail7
		}
		goto ok0
	fail7:
		pos = pos3
		// QualKwds
		if !_fail(parser, _QualKwdsFail, errPos, failure, &pos) {
			goto fail8
		}
		goto ok0
	fail8:
		pos = pos3
		// CompLit
		if !_fail(parser, _CompLitFail, errPos, failure, &pos) {
			goto fail9
		}
		goto ok0
	fail9:
		pos = pos3
		// BlkLit
		if !_fail(parser, _BlkLitFail, errPos, failure, &pos) {
			goto fail10
		}
		goto ok0
	fail10:
		pos = pos3
		// CharLit
		if !_fail(parser, _CharLitFail, errPos, failure, &pos) {
			goto fail11
		}
		goto ok0
	fail11:
		pos = pos3
		// StrLit
		if !_fail(parser, _StrLitFail, errPos, failure, &pos) {
			goto fail12
		}
		goto ok0
	fail12:
		pos = pos3
		// NumLit
		if !_fail(parser, _NumLitFail, errPos, failure, &pos) {
			goto fail13
		}
		goto ok0
	fail13:
		pos = pos3
		// action
		// id:Id
		{
			pos15 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail14
			}
			labels[1] = parser.text[pos15:pos]
		}
		goto ok0
	fail14:
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

func _PriAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Expr
	var label1 Id
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
	// _ "(" expr:Expr _ ")" {…}/QualOp/QualKwds/CompLit/BlkLit/CharLit/StrLit/NumLit/id:Id {…}
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
				return Expr(&SubExpr{
					Expr: expr,
					L:    l(parser, start, end),
				})
			}(
				start5, pos, label0)
		}
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// QualOp
		if p, n := _QualOpAction(parser, pos); n == nil {
			goto fail8
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail8:
		node = node2
		pos = pos3
		// QualKwds
		if p, n := _QualKwdsAction(parser, pos); n == nil {
			goto fail9
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail9:
		node = node2
		pos = pos3
		// CompLit
		if p, n := _CompLitAction(parser, pos); n == nil {
			goto fail10
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail10:
		node = node2
		pos = pos3
		// BlkLit
		if p, n := _BlkLitAction(parser, pos); n == nil {
			goto fail11
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail11:
		node = node2
		pos = pos3
		// CharLit
		if p, n := _CharLitAction(parser, pos); n == nil {
			goto fail12
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail12:
		node = node2
		pos = pos3
		// StrLit
		if p, n := _StrLitAction(parser, pos); n == nil {
			goto fail13
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail13:
		node = node2
		pos = pos3
		// NumLit
		if p, n := _NumLitAction(parser, pos); n == nil {
			goto fail14
		} else {
			node = *n
			pos = p
		}
		goto ok0
	fail14:
		node = node2
		pos = pos3
		// action
		{
			start16 := pos
			// id:Id
			{
				pos17 := pos
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail15
				} else {
					label1 = *n
					pos = p
				}
				labels[1] = parser.text[pos17:pos]
			}
			node = func(
				start, end int, expr Expr, id Id) Expr {
				return Expr(id)
			}(
				start16, pos, label0, label1)
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

func _QualOpAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _QualOp, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// _ "(" (qual:Id _ ".")? name:(IdxOp/Op) _ ")"
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
	// (qual:Id _ ".")?
	{
		pos2 := pos
		// (qual:Id _ ".")
		// qual:Id _ "."
		// qual:Id
		{
			pos5 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail3
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail3
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			perr = _max(perr, pos)
			goto fail3
		}
		pos++
		goto ok6
	fail3:
		pos = pos2
	ok6:
	}
	// name:(IdxOp/Op)
	{
		pos7 := pos
		// (IdxOp/Op)
		// IdxOp/Op
		{
			pos11 := pos
			// IdxOp
			if !_accept(parser, _IdxOpAccepts, &pos, &perr) {
				goto fail12
			}
			goto ok8
		fail12:
			pos = pos11
			// Op
			if !_accept(parser, _OpAccepts, &pos, &perr) {
				goto fail13
			}
			goto ok8
		fail13:
			pos = pos11
			goto fail
		ok8:
		}
		labels[1] = parser.text[pos7:pos]
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
	return _memoize(parser, _QualOp, start, pos, perr)
fail:
	return _memoize(parser, _QualOp, start, -1, perr)
}

func _QualOpFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _QualOp, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "QualOp",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _QualOp}
	// action
	// _ "(" (qual:Id _ ".")? name:(IdxOp/Op) _ ")"
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
	// (qual:Id _ ".")?
	{
		pos2 := pos
		// (qual:Id _ ".")
		// qual:Id _ "."
		// qual:Id
		{
			pos5 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail3
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail3
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\".\"",
				})
			}
			goto fail3
		}
		pos++
		goto ok6
	fail3:
		pos = pos2
	ok6:
	}
	// name:(IdxOp/Op)
	{
		pos7 := pos
		// (IdxOp/Op)
		// IdxOp/Op
		{
			pos11 := pos
			// IdxOp
			if !_fail(parser, _IdxOpFail, errPos, failure, &pos) {
				goto fail12
			}
			goto ok8
		fail12:
			pos = pos11
			// Op
			if !_fail(parser, _OpFail, errPos, failure, &pos) {
				goto fail13
			}
			goto ok8
		fail13:
			pos = pos11
			goto fail
		ok8:
		}
		labels[1] = parser.text[pos7:pos]
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

func _QualOpAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Id
	var label1 Id
	dp := parser.deltaPos[start][_QualOp]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _QualOp}
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
		// _ "(" (qual:Id _ ".")? name:(IdxOp/Op) _ ")"
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
		// (qual:Id _ ".")?
		{
			pos3 := pos
			// (qual:Id _ ".")
			// qual:Id _ "."
			// qual:Id
			{
				pos6 := pos
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos6:pos]
			}
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
			goto ok7
		fail4:
			pos = pos3
		ok7:
		}
		// name:(IdxOp/Op)
		{
			pos8 := pos
			// (IdxOp/Op)
			// IdxOp/Op
			{
				pos12 := pos
				var node11 Id
				// IdxOp
				if p, n := _IdxOpAction(parser, pos); n == nil {
					goto fail13
				} else {
					label1 = *n
					pos = p
				}
				goto ok9
			fail13:
				label1 = node11
				pos = pos12
				// Op
				if p, n := _OpAction(parser, pos); n == nil {
					goto fail14
				} else {
					label1 = *n
					pos = p
				}
				goto ok9
			fail14:
				label1 = node11
				pos = pos12
				goto fail
			ok9:
			}
			labels[1] = parser.text[pos8:pos]
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
			start, end int, name Id, qual Id) Expr {
			return Expr(&Call{Fun: name, Args: []Expr{qual}, L: l(parser, start, end)})
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _QualKwdsAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [2]string
	use(labels)
	if dp, de, ok := _memo(parser, _QualKwds, start); ok {
		return dp, de
	}
	pos, perr := start, -1
	// action
	// (qual:Id _ ".")? name:Kwds
	// (qual:Id _ ".")?
	{
		pos2 := pos
		// (qual:Id _ ".")
		// qual:Id _ "."
		// qual:Id
		{
			pos5 := pos
			// Id
			if !_accept(parser, _IdAccepts, &pos, &perr) {
				goto fail3
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail3
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			perr = _max(perr, pos)
			goto fail3
		}
		pos++
		goto ok6
	fail3:
		pos = pos2
	ok6:
	}
	// name:Kwds
	{
		pos7 := pos
		// Kwds
		if !_accept(parser, _KwdsAccepts, &pos, &perr) {
			goto fail
		}
		labels[1] = parser.text[pos7:pos]
	}
	return _memoize(parser, _QualKwds, start, pos, perr)
fail:
	return _memoize(parser, _QualKwds, start, -1, perr)
}

func _QualKwdsFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [2]string
	use(labels)
	pos, failure := _failMemo(parser, _QualKwds, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "QualKwds",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _QualKwds}
	// action
	// (qual:Id _ ".")? name:Kwds
	// (qual:Id _ ".")?
	{
		pos2 := pos
		// (qual:Id _ ".")
		// qual:Id _ "."
		// qual:Id
		{
			pos5 := pos
			// Id
			if !_fail(parser, _IdFail, errPos, failure, &pos) {
				goto fail3
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail3
		}
		// "."
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\".\"",
				})
			}
			goto fail3
		}
		pos++
		goto ok6
	fail3:
		pos = pos2
	ok6:
	}
	// name:Kwds
	{
		pos7 := pos
		// Kwds
		if !_fail(parser, _KwdsFail, errPos, failure, &pos) {
			goto fail
		}
		labels[1] = parser.text[pos7:pos]
	}
	parser.fail[key] = failure
	return pos, failure
fail:
	parser.fail[key] = failure
	return -1, failure
}

func _QualKwdsAction(parser *_Parser, start int) (int, *Expr) {
	var labels [2]string
	use(labels)
	var label0 Id
	var label1 Id
	dp := parser.deltaPos[start][_QualKwds]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _QualKwds}
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
		// (qual:Id _ ".")? name:Kwds
		// (qual:Id _ ".")?
		{
			pos3 := pos
			// (qual:Id _ ".")
			// qual:Id _ "."
			// qual:Id
			{
				pos6 := pos
				// Id
				if p, n := _IdAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos6:pos]
			}
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
			goto ok7
		fail4:
			pos = pos3
		ok7:
		}
		// name:Kwds
		{
			pos8 := pos
			// Kwds
			if p, n := _KwdsAction(parser, pos); n == nil {
				goto fail
			} else {
				label1 = *n
				pos = p
			}
			labels[1] = parser.text[pos8:pos]
		}
		node = func(
			start, end int, name Id, qual Id) Expr {
			return Expr(&Call{Fun: name, Args: []Expr{qual}, L: l(parser, start, end)})
		}(
			start0, pos, label1, label0)
	}
	parser.act[key] = node
	return pos, &node
fail:
	return -1, nil
}

func _CompLitAccepts(parser *_Parser, start int) (deltaPos, deltaErr int) {
	var labels [1]string
	use(labels)
	if dp, de, ok := _memo(parser, _CompLit, start); ok {
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
	return _memoize(parser, _CompLit, start, pos, perr)
fail:
	return _memoize(parser, _CompLit, start, -1, perr)
}

func _CompLitFail(parser *_Parser, start, errPos int) (int, *peg.Fail) {
	var labels [1]string
	use(labels)
	pos, failure := _failMemo(parser, _CompLit, start, errPos)
	if failure != nil {
		return pos, failure
	}
	failure = &peg.Fail{
		Name: "CompLit",
		Pos:  int(start),
	}
	key := _key{start: start, rule: _CompLit}
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

func _CompLitAction(parser *_Parser, start int) (int, *Expr) {
	var labels [1]string
	use(labels)
	var label0 []Expr
	dp := parser.deltaPos[start][_CompLit]
	if dp < 0 {
		return -1, nil
	}
	key := _key{start: start, rule: _CompLit}
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
			start, end int, exprs []Expr) Expr {
			return Expr(&CompLit{Exprs: exprs, L: l(parser, start, end)})
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
	// _ "{" (parms:FuncParms _ "|")? es:Exprs? _ "}"
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
	// (parms:FuncParms _ "|")?
	{
		pos2 := pos
		// (parms:FuncParms _ "|")
		// parms:FuncParms _ "|"
		// parms:FuncParms
		{
			pos5 := pos
			// FuncParms
			if !_accept(parser, _FuncParmsAccepts, &pos, &perr) {
				goto fail3
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_accept(parser, __Accepts, &pos, &perr) {
			goto fail3
		}
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			perr = _max(perr, pos)
			goto fail3
		}
		pos++
		goto ok6
	fail3:
		pos = pos2
	ok6:
	}
	// es:Exprs?
	{
		pos7 := pos
		// Exprs?
		{
			pos9 := pos
			// Exprs
			if !_accept(parser, _ExprsAccepts, &pos, &perr) {
				goto fail10
			}
			goto ok11
		fail10:
			pos = pos9
		ok11:
		}
		labels[1] = parser.text[pos7:pos]
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
	// _ "{" (parms:FuncParms _ "|")? es:Exprs? _ "}"
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
	// (parms:FuncParms _ "|")?
	{
		pos2 := pos
		// (parms:FuncParms _ "|")
		// parms:FuncParms _ "|"
		// parms:FuncParms
		{
			pos5 := pos
			// FuncParms
			if !_fail(parser, _FuncParmsFail, errPos, failure, &pos) {
				goto fail3
			}
			labels[0] = parser.text[pos5:pos]
		}
		// _
		if !_fail(parser, __Fail, errPos, failure, &pos) {
			goto fail3
		}
		// "|"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"|\"",
				})
			}
			goto fail3
		}
		pos++
		goto ok6
	fail3:
		pos = pos2
	ok6:
	}
	// es:Exprs?
	{
		pos7 := pos
		// Exprs?
		{
			pos9 := pos
			// Exprs
			if !_fail(parser, _ExprsFail, errPos, failure, &pos) {
				goto fail10
			}
			goto ok11
		fail10:
			pos = pos9
		ok11:
		}
		labels[1] = parser.text[pos7:pos]
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
	var label0 []FuncParm
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
		// _ "{" (parms:FuncParms _ "|")? es:Exprs? _ "}"
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
		// (parms:FuncParms _ "|")?
		{
			pos3 := pos
			// (parms:FuncParms _ "|")
			// parms:FuncParms _ "|"
			// parms:FuncParms
			{
				pos6 := pos
				// FuncParms
				if p, n := _FuncParmsAction(parser, pos); n == nil {
					goto fail4
				} else {
					label0 = *n
					pos = p
				}
				labels[0] = parser.text[pos6:pos]
			}
			// _
			if p, n := __Action(parser, pos); n == nil {
				goto fail4
			} else {
				pos = p
			}
			// "|"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "|" {
				goto fail4
			}
			pos++
			goto ok7
		fail4:
			pos = pos3
		ok7:
		}
		// es:Exprs?
		{
			pos8 := pos
			// Exprs?
			{
				pos10 := pos
				label1 = new([]Expr)
				// Exprs
				if p, n := _ExprsAction(parser, pos); n == nil {
					goto fail11
				} else {
					*label1 = *n
					pos = p
				}
				goto ok12
			fail11:
				label1 = nil
				pos = pos10
			ok12:
			}
			labels[1] = parser.text[pos8:pos]
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
			start, end int, es *[]Expr, parms []FuncParm) Expr {
			var exprs []Expr
			if es != nil {
				exprs = *es
			}
			return Expr(&BlkLit{
				Parms: parms,
				Exprs: exprs,
				L:     l(parser, start, end),
			})
		}(
			start0, pos, label1, label0)
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
	// _ data:([\'] !"\n" Esc/"\\'"/[^\'] [\'])
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// data:([\'] !"\n" Esc/"\\'"/[^\'] [\'])
	{
		pos1 := pos
		// ([\'] !"\n" Esc/"\\'"/[^\'] [\'])
		// [\'] !"\n" Esc/"\\'"/[^\'] [\']
		{
			pos5 := pos
			// [\'] !"\n" Esc
			// [\']
			if r, w := _next(parser, pos); r != '\'' {
				perr = _max(perr, pos)
				goto fail6
			} else {
				pos += w
			}
			// !"\n"
			{
				pos9 := pos
				perr11 := perr
				// "\n"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
					perr = _max(perr, pos)
					goto ok8
				}
				pos++
				pos = pos9
				perr = _max(perr11, pos)
				goto fail6
			ok8:
				pos = pos9
				perr = perr11
			}
			// Esc
			if !_accept(parser, _EscAccepts, &pos, &perr) {
				goto fail6
			}
			goto ok2
		fail6:
			pos = pos5
			// "\\'"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\'" {
				perr = _max(perr, pos)
				goto fail12
			}
			pos += 2
			goto ok2
		fail12:
			pos = pos5
			// [^\'] [\']
			// [^\']
			if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '\'' {
				perr = _max(perr, pos)
				goto fail13
			} else {
				pos += w
			}
			// [\']
			if r, w := _next(parser, pos); r != '\'' {
				perr = _max(perr, pos)
				goto fail13
			} else {
				pos += w
			}
			goto ok2
		fail13:
			pos = pos5
			goto fail
		ok2:
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
	// _ data:([\'] !"\n" Esc/"\\'"/[^\'] [\'])
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// data:([\'] !"\n" Esc/"\\'"/[^\'] [\'])
	{
		pos1 := pos
		// ([\'] !"\n" Esc/"\\'"/[^\'] [\'])
		// [\'] !"\n" Esc/"\\'"/[^\'] [\']
		{
			pos5 := pos
			// [\'] !"\n" Esc
			// [\']
			if r, w := _next(parser, pos); r != '\'' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "[\\']",
					})
				}
				goto fail6
			} else {
				pos += w
			}
			// !"\n"
			{
				pos9 := pos
				nkids10 := len(failure.Kids)
				// "\n"
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "\"\\n\"",
						})
					}
					goto ok8
				}
				pos++
				pos = pos9
				failure.Kids = failure.Kids[:nkids10]
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "!\"\\n\"",
					})
				}
				goto fail6
			ok8:
				pos = pos9
				failure.Kids = failure.Kids[:nkids10]
			}
			// Esc
			if !_fail(parser, _EscFail, errPos, failure, &pos) {
				goto fail6
			}
			goto ok2
		fail6:
			pos = pos5
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
			goto ok2
		fail12:
			pos = pos5
			// [^\'] [\']
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
			// [\']
			if r, w := _next(parser, pos); r != '\'' {
				if pos >= errPos {
					failure.Kids = append(failure.Kids, &peg.Fail{
						Pos:  int(pos),
						Want: "[\\']",
					})
				}
				goto fail13
			} else {
				pos += w
			}
			goto ok2
		fail13:
			pos = pos5
			goto fail
		ok2:
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
		// _ data:([\'] !"\n" Esc/"\\'"/[^\'] [\'])
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// data:([\'] !"\n" Esc/"\\'"/[^\'] [\'])
		{
			pos2 := pos
			// ([\'] !"\n" Esc/"\\'"/[^\'] [\'])
			// [\'] !"\n" Esc/"\\'"/[^\'] [\']
			{
				pos6 := pos
				var node5 string
				// [\'] !"\n" Esc
				{
					var node8 string
					// [\']
					if r, w := _next(parser, pos); r != '\'' {
						goto fail7
					} else {
						node8 = parser.text[pos : pos+w]
						pos += w
					}
					label0, node8 = label0+node8, ""
					// !"\n"
					{
						pos10 := pos
						// "\n"
						if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\n" {
							goto ok9
						}
						pos++
						pos = pos10
						goto fail7
					ok9:
						pos = pos10
						node8 = ""
					}
					label0, node8 = label0+node8, ""
					// Esc
					if p, n := _EscAction(parser, pos); n == nil {
						goto fail7
					} else {
						node8 = *n
						pos = p
					}
					label0, node8 = label0+node8, ""
				}
				goto ok3
			fail7:
				label0 = node5
				pos = pos6
				// "\\'"
				if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\'" {
					goto fail13
				}
				label0 = parser.text[pos : pos+2]
				pos += 2
				goto ok3
			fail13:
				label0 = node5
				pos = pos6
				// [^\'] [\']
				{
					var node15 string
					// [^\']
					if r, w := _next(parser, pos); w == 0 || r == '\uFFFD' || r == '\'' {
						goto fail14
					} else {
						node15 = parser.text[pos : pos+w]
						pos += w
					}
					label0, node15 = label0+node15, ""
					// [\']
					if r, w := _next(parser, pos); r != '\'' {
						goto fail14
					} else {
						node15 = parser.text[pos : pos+w]
						pos += w
					}
					label0, node15 = label0+node15, ""
				}
				goto ok3
			fail14:
				label0 = node5
				pos = pos6
				goto fail
			ok3:
			}
			labels[0] = parser.text[pos2:pos]
		}
		node = func(
			start, end int, data string) Expr {
			return Expr(&CharLit{
				Rune: interpRune(data),
				L:    l(parser, start, end),
			})
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
			return Expr(&StrLit{
				Raw:  false,
				Data: data,
				L:    l(parser, start, end),
			})
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
	// "\\n" {…}/"\\t" {…}/"\\b" {…}/"\\\\" {…}/"\\" x0:(X X) {…}/"\\x" x1:(X X X X) {…}/"\\X" x2:(X X X X X X X X) {…}
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
		// "\\" x0:(X X)
		// "\\"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\\" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos++
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
		// "\\x" x1:(X X X X)
		// "\\x"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\x" {
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
		// "\\X" x2:(X X X X X X X X)
		// "\\X"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\X" {
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
	// "\\n" {…}/"\\t" {…}/"\\b" {…}/"\\\\" {…}/"\\" x0:(X X) {…}/"\\x" x1:(X X X X) {…}/"\\X" x2:(X X X X X X X X) {…}
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
		// "\\" x0:(X X)
		// "\\"
		if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\\" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\\"",
				})
			}
			goto fail8
		}
		pos++
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
		// "\\x" x1:(X X X X)
		// "\\x"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\x" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\x\"",
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
		// "\\X" x2:(X X X X X X X X)
		// "\\X"
		if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\X" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"\\\\X\"",
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
	// "\\n" {…}/"\\t" {…}/"\\b" {…}/"\\\\" {…}/"\\" x0:(X X) {…}/"\\x" x1:(X X X X) {…}/"\\X" x2:(X X X X X X X X) {…}
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
			// "\\" x0:(X X)
			// "\\"
			if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "\\" {
				goto fail12
			}
			pos++
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
			// "\\x" x1:(X X X X)
			// "\\x"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\x" {
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
				return string(interpHex(x0))
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
			// "\\X" x2:(X X X X X X X X)
			// "\\X"
			if len(parser.text[pos:]) < 2 || parser.text[pos:pos+2] != "\\X" {
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
				return string(interpHex(x0))
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
			return Expr(&StrLit{
				Raw:  true,
				Data: data,
				L:    l(parser, start, end),
			})
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
	// _ text:(D+)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// text:(D+)
	{
		pos1 := pos
		// (D+)
		// D+
		// D
		if !_accept(parser, _DAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos3 := pos
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
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
	// _ text:(D+)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// text:(D+)
	{
		pos1 := pos
		// (D+)
		// D+
		// D
		if !_fail(parser, _DFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos3 := pos
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail5
			}
			continue
		fail5:
			pos = pos3
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
		// _ text:(D+)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// text:(D+)
		{
			pos2 := pos
			// (D+)
			// D+
			{
				var node5 string
				// D
				if p, n := _DAction(parser, pos); n == nil {
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
				// D
				if p, n := _DAction(parser, pos); n == nil {
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
	// _ text:(D+ "." D+ ([eE] [+\-]? D+)?)
	// _
	if !_accept(parser, __Accepts, &pos, &perr) {
		goto fail
	}
	// text:(D+ "." D+ ([eE] [+\-]? D+)?)
	{
		pos1 := pos
		// (D+ "." D+ ([eE] [+\-]? D+)?)
		// D+ "." D+ ([eE] [+\-]? D+)?
		// D+
		// D
		if !_accept(parser, _DAccepts, &pos, &perr) {
			goto fail
		}
		for {
			pos4 := pos
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
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
			pos8 := pos
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail10
			}
			continue
		fail10:
			pos = pos8
			break
		}
		// ([eE] [+\-]? D+)?
		{
			pos12 := pos
			// ([eE] [+\-]? D+)
			// [eE] [+\-]? D+
			// [eE]
			if r, w := _next(parser, pos); r != 'e' && r != 'E' {
				perr = _max(perr, pos)
				goto fail13
			} else {
				pos += w
			}
			// [+\-]?
			{
				pos16 := pos
				// [+\-]
				if r, w := _next(parser, pos); r != '+' && r != '-' {
					perr = _max(perr, pos)
					goto fail17
				} else {
					pos += w
				}
				goto ok18
			fail17:
				pos = pos16
			ok18:
			}
			// D+
			// D
			if !_accept(parser, _DAccepts, &pos, &perr) {
				goto fail13
			}
			for {
				pos20 := pos
				// D
				if !_accept(parser, _DAccepts, &pos, &perr) {
					goto fail22
				}
				continue
			fail22:
				pos = pos20
				break
			}
			goto ok23
		fail13:
			pos = pos12
		ok23:
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
	// _ text:(D+ "." D+ ([eE] [+\-]? D+)?)
	// _
	if !_fail(parser, __Fail, errPos, failure, &pos) {
		goto fail
	}
	// text:(D+ "." D+ ([eE] [+\-]? D+)?)
	{
		pos1 := pos
		// (D+ "." D+ ([eE] [+\-]? D+)?)
		// D+ "." D+ ([eE] [+\-]? D+)?
		// D+
		// D
		if !_fail(parser, _DFail, errPos, failure, &pos) {
			goto fail
		}
		for {
			pos4 := pos
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail6
			}
			continue
		fail6:
			pos = pos4
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
			pos8 := pos
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail10
			}
			continue
		fail10:
			pos = pos8
			break
		}
		// ([eE] [+\-]? D+)?
		{
			pos12 := pos
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
				goto fail13
			} else {
				pos += w
			}
			// [+\-]?
			{
				pos16 := pos
				// [+\-]
				if r, w := _next(parser, pos); r != '+' && r != '-' {
					if pos >= errPos {
						failure.Kids = append(failure.Kids, &peg.Fail{
							Pos:  int(pos),
							Want: "[+\\-]",
						})
					}
					goto fail17
				} else {
					pos += w
				}
				goto ok18
			fail17:
				pos = pos16
			ok18:
			}
			// D+
			// D
			if !_fail(parser, _DFail, errPos, failure, &pos) {
				goto fail13
			}
			for {
				pos20 := pos
				// D
				if !_fail(parser, _DFail, errPos, failure, &pos) {
					goto fail22
				}
				continue
			fail22:
				pos = pos20
				break
			}
			goto ok23
		fail13:
			pos = pos12
		ok23:
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
		// _ text:(D+ "." D+ ([eE] [+\-]? D+)?)
		// _
		if p, n := __Action(parser, pos); n == nil {
			goto fail
		} else {
			pos = p
		}
		// text:(D+ "." D+ ([eE] [+\-]? D+)?)
		{
			pos2 := pos
			// (D+ "." D+ ([eE] [+\-]? D+)?)
			// D+ "." D+ ([eE] [+\-]? D+)?
			{
				var node3 string
				// D+
				{
					var node6 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail
					} else {
						node6 = *n
						pos = p
					}
					node3 += node6
				}
				for {
					pos5 := pos
					var node6 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
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
				// "."
				if len(parser.text[pos:]) < 1 || parser.text[pos:pos+1] != "." {
					goto fail
				}
				node3 = parser.text[pos : pos+1]
				pos++
				label0, node3 = label0+node3, ""
				// D+
				{
					var node10 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail
					} else {
						node10 = *n
						pos = p
					}
					node3 += node10
				}
				for {
					pos9 := pos
					var node10 string
					// D
					if p, n := _DAction(parser, pos); n == nil {
						goto fail11
					} else {
						node10 = *n
						pos = p
					}
					node3 += node10
					continue
				fail11:
					pos = pos9
					break
				}
				label0, node3 = label0+node3, ""
				// ([eE] [+\-]? D+)?
				{
					pos13 := pos
					// ([eE] [+\-]? D+)
					// [eE] [+\-]? D+
					{
						var node15 string
						// [eE]
						if r, w := _next(parser, pos); r != 'e' && r != 'E' {
							goto fail14
						} else {
							node15 = parser.text[pos : pos+w]
							pos += w
						}
						node3, node15 = node3+node15, ""
						// [+\-]?
						{
							pos17 := pos
							// [+\-]
							if r, w := _next(parser, pos); r != '+' && r != '-' {
								goto fail18
							} else {
								node15 = parser.text[pos : pos+w]
								pos += w
							}
							goto ok19
						fail18:
							node15 = ""
							pos = pos17
						ok19:
						}
						node3, node15 = node3+node15, ""
						// D+
						{
							var node22 string
							// D
							if p, n := _DAction(parser, pos); n == nil {
								goto fail14
							} else {
								node22 = *n
								pos = p
							}
							node15 += node22
						}
						for {
							pos21 := pos
							var node22 string
							// D
							if p, n := _DAction(parser, pos); n == nil {
								goto fail23
							} else {
								node22 = *n
								pos = p
							}
							node15 += node22
							continue
						fail23:
							pos = pos21
							break
						}
						node3, node15 = node3+node15, ""
					}
					goto ok24
				fail14:
					node3 = ""
					pos = pos13
				ok24:
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

func _IdAction(parser *_Parser, start int) (int, *Id) {
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
		n := n.(Id)
		return start + int(dp-1), &n
	}
	var node Id
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
			start, end int, name string) Id {
			return Id{Name: name, L: l(parser, start, end)}
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
	// "import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test"
	{
		pos3 := pos
		// "import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
			perr = _max(perr, pos)
			goto fail4
		}
		pos += 6
		goto ok0
	fail4:
		pos = pos3
		// "Import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
			perr = _max(perr, pos)
			goto fail5
		}
		pos += 6
		goto ok0
	fail5:
		pos = pos3
		// "const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
			perr = _max(perr, pos)
			goto fail6
		}
		pos += 5
		goto ok0
	fail6:
		pos = pos3
		// "Const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
			perr = _max(perr, pos)
			goto fail7
		}
		pos += 5
		goto ok0
	fail7:
		pos = pos3
		// "var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
			perr = _max(perr, pos)
			goto fail8
		}
		pos += 3
		goto ok0
	fail8:
		pos = pos3
		// "Var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
			perr = _max(perr, pos)
			goto fail9
		}
		pos += 3
		goto ok0
	fail9:
		pos = pos3
		// "type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
			perr = _max(perr, pos)
			goto fail10
		}
		pos += 4
		goto ok0
	fail10:
		pos = pos3
		// "Type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
			perr = _max(perr, pos)
			goto fail11
		}
		pos += 4
		goto ok0
	fail11:
		pos = pos3
		// "func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
			perr = _max(perr, pos)
			goto fail12
		}
		pos += 4
		goto ok0
	fail12:
		pos = pos3
		// "Func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
			perr = _max(perr, pos)
			goto fail13
		}
		pos += 4
		goto ok0
	fail13:
		pos = pos3
		// "test"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
			perr = _max(perr, pos)
			goto fail14
		}
		pos += 4
		goto ok0
	fail14:
		pos = pos3
		goto fail
	ok0:
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
	// "import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test"
	{
		pos3 := pos
		// "import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"import\"",
				})
			}
			goto fail4
		}
		pos += 6
		goto ok0
	fail4:
		pos = pos3
		// "Import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Import\"",
				})
			}
			goto fail5
		}
		pos += 6
		goto ok0
	fail5:
		pos = pos3
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
		goto ok0
	fail6:
		pos = pos3
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
		goto ok0
	fail7:
		pos = pos3
		// "var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"var\"",
				})
			}
			goto fail8
		}
		pos += 3
		goto ok0
	fail8:
		pos = pos3
		// "Var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Var\"",
				})
			}
			goto fail9
		}
		pos += 3
		goto ok0
	fail9:
		pos = pos3
		// "type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"type\"",
				})
			}
			goto fail10
		}
		pos += 4
		goto ok0
	fail10:
		pos = pos3
		// "Type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Type\"",
				})
			}
			goto fail11
		}
		pos += 4
		goto ok0
	fail11:
		pos = pos3
		// "func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"func\"",
				})
			}
			goto fail12
		}
		pos += 4
		goto ok0
	fail12:
		pos = pos3
		// "Func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"Func\"",
				})
			}
			goto fail13
		}
		pos += 4
		goto ok0
	fail13:
		pos = pos3
		// "test"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
			if pos >= errPos {
				failure.Kids = append(failure.Kids, &peg.Fail{
					Pos:  int(pos),
					Want: "\"test\"",
				})
			}
			goto fail14
		}
		pos += 4
		goto ok0
	fail14:
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
	// "import"/"Import"/"const"/"Const"/"var"/"Var"/"type"/"Type"/"func"/"Func"/"test"
	{
		pos3 := pos
		var node2 string
		// "import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "import" {
			goto fail4
		}
		node = parser.text[pos : pos+6]
		pos += 6
		goto ok0
	fail4:
		node = node2
		pos = pos3
		// "Import"
		if len(parser.text[pos:]) < 6 || parser.text[pos:pos+6] != "Import" {
			goto fail5
		}
		node = parser.text[pos : pos+6]
		pos += 6
		goto ok0
	fail5:
		node = node2
		pos = pos3
		// "const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "const" {
			goto fail6
		}
		node = parser.text[pos : pos+5]
		pos += 5
		goto ok0
	fail6:
		node = node2
		pos = pos3
		// "Const"
		if len(parser.text[pos:]) < 5 || parser.text[pos:pos+5] != "Const" {
			goto fail7
		}
		node = parser.text[pos : pos+5]
		pos += 5
		goto ok0
	fail7:
		node = node2
		pos = pos3
		// "var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "var" {
			goto fail8
		}
		node = parser.text[pos : pos+3]
		pos += 3
		goto ok0
	fail8:
		node = node2
		pos = pos3
		// "Var"
		if len(parser.text[pos:]) < 3 || parser.text[pos:pos+3] != "Var" {
			goto fail9
		}
		node = parser.text[pos : pos+3]
		pos += 3
		goto ok0
	fail9:
		node = node2
		pos = pos3
		// "type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "type" {
			goto fail10
		}
		node = parser.text[pos : pos+4]
		pos += 4
		goto ok0
	fail10:
		node = node2
		pos = pos3
		// "Type"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Type" {
			goto fail11
		}
		node = parser.text[pos : pos+4]
		pos += 4
		goto ok0
	fail11:
		node = node2
		pos = pos3
		// "func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "func" {
			goto fail12
		}
		node = parser.text[pos : pos+4]
		pos += 4
		goto ok0
	fail12:
		node = node2
		pos = pos3
		// "Func"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "Func" {
			goto fail13
		}
		node = parser.text[pos : pos+4]
		pos += 4
		goto ok0
	fail13:
		node = node2
		pos = pos3
		// "test"
		if len(parser.text[pos:]) < 4 || parser.text[pos:pos+4] != "test" {
			goto fail14
		}
		node = parser.text[pos : pos+4]
		pos += 4
		goto ok0
	fail14:
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
	// [*/%+\-\^=!<>&|~?@$]
	if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' && r != '+' && r != '-' && r != '^' && r != '=' && r != '!' && r != '<' && r != '>' && r != '&' && r != '|' && r != '~' && r != '?' && r != '@' && r != '$' {
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
	// [*/%+\-\^=!<>&|~?@$]
	if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' && r != '+' && r != '-' && r != '^' && r != '=' && r != '!' && r != '<' && r != '>' && r != '&' && r != '|' && r != '~' && r != '?' && r != '@' && r != '$' {
		if pos >= errPos {
			failure.Kids = append(failure.Kids, &peg.Fail{
				Pos:  int(pos),
				Want: "[*/%+\\-\\^=!<>&|~?@$]",
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
	// [*/%+\-\^=!<>&|~?@$]
	if r, w := _next(parser, pos); r != '*' && r != '/' && r != '%' && r != '+' && r != '-' && r != '^' && r != '=' && r != '!' && r != '<' && r != '>' && r != '&' && r != '|' && r != '~' && r != '?' && r != '@' && r != '$' {
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
	var label0 Id
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
			start, end int, arg1 Expr, name Id) *Call {
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
	var label0 Id
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
			start, end int, arg1 Expr, name Id) *Call {
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
	var label0 Id
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
			start, end int, arg1 Expr, name Id) *Call {
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
	var label0 Id
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
			start, end int, arg1 Expr, name Id) *Call {
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
	var label0 Id
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
			start, end int, arg1 Expr, name Id) *Call {
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
	var label0 Id
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
			start, end int, arg1 Expr, name Id) *Call {
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
