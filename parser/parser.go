package parser

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
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
	// TrimPathPrefix is trimmed as the prefix
	// for any subsequenly parsed files.
	// This affects all locations created in those files.
	TrimPathPrefix string
	offs           int
}

// New returns a new parser.
func New() *Parser {
	return &Parser{offs: 1}
}

// NewWithOffset returns a new parser with the given location offset.
func NewWithOffset(offs int) *Parser {
	return &Parser{offs: offs}
}

// ImportsOnly returns just the paths imported by a source file.
// Imports only parses the imports and ignores the rest of the file;
// syntax errors after import statements are not reported.
func ImportsOnly(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	return importsOnly(path, f)
}

func importsOnly(path string, r io.Reader) ([]string, error) {
	data, err := ioutil.ReadAll(r)
	if err != nil {
		return nil, err
	}
	_p, err := _NewParser(string(data))
	if err != nil {
		return nil, err
	}
	_p.data = &Parser{offs: 1}
	if pos, perr := _ImportsOnlyAccepts(_p, 0); pos < 0 {
		_, t := _ImportsOnlyFail(_p, 0, perr)
		return nil, parseError{path: path, loc: perr, text: _p.text, fail: t}
	}
	_, imports := _ImportsOnlyAction(_p, 0)
	var paths []string
	for _, imp := range *imports {
		paths = append(paths, filepath.Clean(imp.Path))
	}
	return paths, nil
}

// Parse parses a file from an io.Reader.
// The first argument is the file path or "" if unspecified.
func (p *Parser) Parse(path string, r io.Reader) error {
	var err error
	if path, err = filepath.Abs(path); err != nil {
		return err
	}
	data, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}
	path = strings.TrimPrefix(path, p.TrimPathPrefix)
	_p, err := _NewParser(string(data))
	if err != nil {
		return err
	}
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
	p.offs += len(data)
	return nil
}

// ParseFile parses the source from a file path.
func (p *Parser) ParseFile(path string) error {
	var err error
	if path, err = filepath.Abs(path); err != nil {
		return err
	}
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	return p.Parse(path, f)
}

func ParseExpr(str string) (Expr, error) {
	_p, err := _NewParser(str)
	if err != nil {
		return nil, err
	}
	_p.data = New()
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
	name Ident
	arg  Expr
}

func nary(l loc.Loc, mod *Ident, nameArgs []nameArg) *Call {
	var name Ident
	name.L[0] = nameArgs[0].name.L[0]
	name.L[1] = nameArgs[len(nameArgs)-1].name.L[1]
	var args []Expr
	for _, nameArg := range nameArgs {
		name.Name += nameArg.name.Name
		args = append(args, nameArg.arg)
	}
	var fun Expr = name
	if mod != nil {
		fun = &ModSel{
			Mod:  *mod,
			Name: name,
			L:    loc.Loc{mod.L[0], name.L[1]},
		}
	}
	return &Call{Fun: fun, Args: args, L: l}
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
	mod  *Ident
	name Ident
	l    loc.Loc
}

type call struct {
	args []Expr
	l    loc.Loc
}

type idx struct {
	args []Expr
	l    loc.Loc
}

func primaries(head Expr, tail []primary) Expr {
	l0 := head.Loc()[0]
	for _, primary := range tail {
		switch p := primary.(type) {
		case sel:
			var fun Expr = p.name
			if p.mod != nil {
				fun = &ModSel{
					Mod:  *p.mod,
					Name: p.name,
					L:    p.l,
				}
			}
			head = &Call{
				Fun:  fun,
				Args: []Expr{head},
				L:    loc.Loc{l0, p.l[1]},
			}
		case call:
			head = &Call{
				Fun:  head,
				Args: p.args,
				L:    loc.Loc{l0, p.l[1]},
			}
		case idx:
			head = &Call{
				Fun:  Ident{Name: "[]", L: p.l},
				Args: append([]Expr{head}, p.args...),
				L:    loc.Loc{l0, p.l[1]},
			}
		default:
			panic(fmt.Sprintf("bad primary type: %T", primary))
		}
	}
	return head
}

func modTag(ids []Ident) Ident {
	var s strings.Builder
	for i, id := range ids {
		s.WriteString(id.Name)
		if i < len(ids)-1 {
			s.WriteRune('#')
		}
	}
	return Ident{
		Name: s.String(),
		L:    loc.Loc{ids[0].L[0], ids[len(ids)-1].L[1]},
	}
}

func catNames(ids []Ident) string {
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
