package parser

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/peggy/peg"
)

//go:generate peggy -t=false -o grammar.go grammar.peggy

// A Parser parses source code files.
type Parser struct {
	Files []*File
	offs  int
}

// NewParser returns a new parser.
func NewParser() *Parser {
	return &Parser{offs: 1}
}

// NewParserOffset returns a new parser with the given location offset.
func NewParserOffset(offs int) *Parser {
	return &Parser{offs: offs}
}

// Parse parses a file from an io.Reader.
// The first argument is the file path or "" if unspecified.
func (p *Parser) Parse(path string, r io.Reader) error {
	data, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}
	_p := _NewParser(string(data))
	_p.data = p
	if pos, perr := _FileAccepts(_p, 0); pos < 0 {
		_, t := _FileFail(_p, 0, perr)
		return parseError{path: path, loc: perr, text: _p.text, fail: t}
	}
	_, file := _FileAction(_p, 0)
	file.P = path
	for i, r := range data {
		if r == '\n' {
			file.NLs = append(file.NLs, i)
		}
	}
	file.Length = len(data)
	p.Files = append(p.Files, file)
	return nil
}

// ParseFile parses the source from a file path.
func (p *Parser) ParseFile(path string) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	return p.Parse(path, f)
}

func ParseExpr(str string) (Expr, error) {
	_p := _NewParser(str)
	_p.data = NewParser()
	if pos, perr := _ExprAccepts(_p, 0); pos < 0 {
		_, t := _ExprFail(_p, 0, perr)
		return nil, parseError{loc: perr, text: _p.text, fail: t}
	}
	_, expr := _ExprAction(_p, 0)
	return *expr, nil
}

type parseError struct {
	path string
	loc  int
	text string
	fail *peg.Fail
}

func (err parseError) Tree() *peg.Fail { return err.fail }

func (err parseError) Error() string {
	e := peg.SimpleError(err.text, err.fail)
	e.FilePath = err.path
	return e.Error()
}

func namedType(args []Type, names []*NamedType, l loc.Loc) Type {
	for _, name := range names {
		name.Args = args
		name.L[0] = l[0]
		args = []Type{name}
	}
	return args[0]
}

type nameArg struct {
	name Id
	arg  Expr
}

func nary(l loc.Loc, nameArgs []nameArg) *Call {
	var name Id
	name.L[0] = nameArgs[0].name.L[0]
	name.L[1] = nameArgs[len(nameArgs)-1].name.L[1]
	var args []Expr
	for _, nameArg := range nameArgs {
		name.Name += nameArg.name.Name
		args = append(args, nameArg.arg)
	}
	return &Call{Fun: name, Args: args, L: l}
}

func bins(expr Expr, calls []*Call) Expr {
	l0 := expr.Loc()[0]
	for _, call := range calls {
		call.Args[0] = expr
		call.L[0] = l0
		expr = call
	}
	return expr
}

// sel, call, or idx
type primary interface{}

type sel struct {
	name Id
	l    loc.Loc
}

type call struct {
	args []Expr
	l    loc.Loc
}

type idx struct {
	arg Expr
	l   loc.Loc
}

func primaries(head Expr, tail []primary) Expr {
	for _, primary := range tail {
		switch p := primary.(type) {
		case sel:
			head = &Call{Fun: p.name, Args: []Expr{head}, L: p.l}
		case call:
			head = &Call{Fun: head, Args: p.args, L: p.l}
		case idx:
			head = &Call{Fun: Id{Name: "[]", L: p.l}, Args: []Expr{head, p.arg}, L: p.l}
		default:
			panic(fmt.Sprintf("bad primary type: %T", primary))
		}
	}
	return head
}

func catNames(ids []Id) string {
	var s strings.Builder
	for _, id := range ids {
		s.WriteString(id.Name)
	}
	return s.String()
}

func interpRune(str string) rune {
	str = str[1 : len(str)-1] // remove 's
	r, _, tail, err := strconv.UnquoteChar(str, '\'')
	if err != nil {
		panic("impossible [" + str + "]: " + err.Error())
	}
	if tail != "" {
		panic("impossible tail: " + tail)
	}
	return r
}

func interpHex(x string) string {
	r, err := strconv.ParseInt(x, 16, 32)
	if err != nil {
		panic("impossible: " + err.Error())
	}
	return string([]rune{rune(r)})
}

func isLetter(s string) bool {
	r, _ := utf8.DecodeRuneInString(s)
	return unicode.IsLetter(r)
}

func isSpace(s string) bool {
	r, _ := utf8.DecodeRuneInString(s)
	return unicode.IsSpace(r)
}

func l(p *_Parser, s, e int) loc.Loc {
	for {
		r, w := utf8.DecodeRuneInString(p.text[s:])
		if !unicode.IsSpace(r) {
			break
		}
		s += w
	}
	for {
		r, w := utf8.DecodeLastRuneInString(p.text[:e])
		if !unicode.IsSpace(r) {
			break
		}
		e -= w
	}
	offs := p.data.(*Parser).offs
	return loc.Loc{offs + s, offs + e}
}
