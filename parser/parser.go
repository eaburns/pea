package parser

import (
	"io"
	"io/ioutil"
	"os"
	"strconv"
	"unicode"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
	. "github.com/eaburns/pea/tree"
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

func namedType(l loc.Loc, args []Type, qnames []qname) Type {
	for _, qn := range qnames {
		args = []Type{
			&NamedType{
				Args: args,
				Mod:  qn.Mod,
				Name: qn.Name,
				L:    loc.Loc{l[0], qn.Name.L[1]},
			},
		}
	}
	if len(args) != 1 {
		// There is at least 1 qnames, so we must have done one loop,
		// and therefore args has 1 element.
		panic("impossible")
	}
	return args[0]
}

type qname struct {
	Mod  *Id
	Name Id
}

func makeQname(id0 Id, id1 *Id) qname {
	if id1 == nil {
		return qname{Name: id0}
	}
	return qname{Mod: &id0, Name: *id1}
}

type kw struct {
	name Id
	arg  Expr
}

func kwCall(l loc.Loc, kws []kw) *Call {
	var name Id
	name.L[0] = kws[0].name.L[0]
	name.L[1] = kws[len(kws)-1].name.L[1]
	var args []Expr
	for _, kw := range kws {
		name.Name += kw.name.Name
		args = append(args, kw.arg)
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

func idxs(expr Expr, indexes []Expr) Expr {
	l0 := expr.Loc()[0]
	for _, idx := range indexes {
		expr = &Index{
			Expr:  expr,
			Index: idx,
			L:     loc.Loc{l0, idx.Loc()[1]},
		}
	}
	return expr
}

func calls(expr Expr, calls []*Call) Expr {
	l0 := expr.Loc()[0]
	for _, call := range calls {
		call.Fun = expr
		call.L[0] = l0
		expr = call
	}
	return expr
}

func args(as *[]Expr) []Expr {
	if as == nil {
		return nil
	}
	return *as
}

func sel(expr Expr, ids []Id) Expr {
	l0 := expr.Loc()[0]
	for _, id := range ids {
		expr = &Selector{Expr: expr, Id: id, L: loc.Loc{l0, id.L[1]}}
	}
	return expr
}

func interpRune(str string) rune {
	r, _, tail, err := strconv.UnquoteChar(str, '\'')
	if err != nil {
		panic("impossible: " + err.Error())
	}
	if tail != "" {
		panic("impossible tail: " + tail)
	}
	return r
}

func interpHex(x string) string {
	r, err := strconv.Atoi(x)
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
