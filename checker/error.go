package checker

import (
	"fmt"
	"os"
	"strings"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type Error interface {
	error
	loc.Locer

	add(...note)
	done(*topScope)
}

func newError(locer loc.Locer, f string, vs ...interface{}) Error {
	l := locer.Loc()
	if l == (loc.Loc{}) {
		if pr, ok := locer.(printer); ok {
			print(os.Stderr, pr)
		}
		panic("impossible no location")
	}
	return &_error{msg: fmt.Sprintf(f, vs...), loc: l}
}

func ambigType(name string, locer loc.Locer, types []Type) Error {
	err := newError(locer, "type %s is ambiguous", name)
	for _, t := range types {
		err.add(newNote(t.String()).setLoc(t))
	}
	return err
}

func ambigIface(name string, locer loc.Locer, ifaces []*IfaceDef) Error {
	err := newError(locer, "interface %s is ambiguous", name)
	for _, iface := range ifaces {
		err.add(newNote(iface.Name).setLoc(iface))
	}
	return err
}

func redef(locer loc.Locer, name string, prev loc.Locer) Error {
	err := newError(locer, "%s redefined", name)
	if (prev.Loc() != loc.Loc{}) {
		err.add(newNote("previous").setLoc(prev))
	}
	return err
}

type note interface {
	add(...note)
	print(errorPrinter)
}

func newNote(f string, vs ...interface{}) *_error {
	return &_error{msg: fmt.Sprintf(f, vs...)}
}

type _error struct {
	msg   string
	loc   loc.Loc
	notes []note

	// v is whether this note should be displayed
	// only in verbose mode.
	v bool
}

func (e *_error) Error() string { return e.msg }
func (e *_error) Loc() loc.Loc  { return e.loc }

func (e *_error) add(notes ...note) {
	for _, n := range notes {
		if n != nil {
			e.notes = append(e.notes, n)
		}
	}
}

func (e *_error) setLoc(x interface{}) note {
	if locer, ok := x.(loc.Locer); ok {
		e.loc = locer.Loc()
	}
	return e
}

func (e *_error) done(c *topScope) {
	p := makeErrorPrinter(c)
	p.printf("%s: %s", locOf{e.loc}, e.msg)
	printNotes(p, e.notes)
	e.msg = p.String()
}

func (e *_error) print(p errorPrinter) {
	p.printf(e.msg)
	if e.loc != (loc.Loc{}) {
		p.printf(" (%s)", locOf{e.loc})
	}
	printNotes(p, e.notes)
}

// NotFoundError indicates an identifier whose definition is not found.
type NotFoundError struct {
	Ident parser.Ident
	scope scope
	notes
}

func notFound(x scope, ident parser.Ident) *NotFoundError {
	return &NotFoundError{Ident: ident, scope: x}
}

func notFoundTypeVar(x scope, tv parser.TypeVar) *NotFoundError {
	return &NotFoundError{Ident: parser.Ident(tv), scope: x}
}

func (err *NotFoundError) Loc() loc.Loc { return err.Ident.L }

func (err *NotFoundError) Error() string {
	p := makeErrorPrinter(top(err.scope))
	p.printf("%s: %s not found", locOf{err.Ident}, err.Ident.Name)
	printNotes(p, err.notes)
	return p.String()
}

type notes []note

func (notes *notes) add(ns ...note) {
	for _, n := range ns {
		if n != nil {
			*notes = append(*notes, n)
		}
	}
}

func (notes *notes) done(top *topScope) {}

type errorPrinter struct {
	bullet              int
	indent              string
	files               loc.Files
	trimErrorPathPrefix string
	w                   *strings.Builder
}

func makeErrorPrinter(top *topScope) errorPrinter {
	return errorPrinter{
		bullet:              0,
		indent:              "",
		files:               top.importer.Files(),
		trimErrorPathPrefix: top.trimErrorPathPrefix,
		w:                   new(strings.Builder),
	}
}

func (p *errorPrinter) String() string { return p.w.String() }

var bullets = [...]string{"•", "◦", "‣", "⁃"}

func (p errorPrinter) listItem() errorPrinter {
	p.w.WriteString("\n")
	p.w.WriteString(p.indent)
	p.w.WriteString(bullets[p.bullet])
	p.w.WriteString(" ")
	p.bullet = (p.bullet + 1) % len(bullets)
	p.indent += "  "
	return p
}

// locOf wraps a loc.Locer, and errorPrinter.fmt will format it as the location string.
type locOf struct{ loc.Locer }

func (p errorPrinter) printf(f string, vs ...interface{}) {
	for i := range vs {
		v, ok := vs[i].(locOf)
		if !ok {
			continue
		}
		if p.files.Len() == 0 {
			vs[i] = "no location info available"
			continue
		}
		location := p.files.Location(v.Loc())
		location.Path = strings.TrimPrefix(location.Path, p.trimErrorPathPrefix)
		vs[i] = location
	}
	s := fmt.Sprintf(f, vs...)
	if p.indent != "" {
		s = strings.Replace(s, "\n", "\n"+p.indent, -1)
	}
	p.w.WriteString(s)
}

func printNotes(p errorPrinter, notes notes) {
	for _, n := range notes {
		n.print(p.listItem())
	}
}
