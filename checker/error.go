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
	setLoc(x interface{}) note
	buildString(c *topScope, mustIdent bool, depth int, s *strings.Builder)
}

func newNote(f string, vs ...interface{}) note {
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
		if n == nil {
			panic("nil note")
		}
	}
	e.notes = append(e.notes, notes...)
}

func (e *_error) setLoc(x interface{}) note {
	if locer, ok := x.(loc.Locer); ok {
		e.loc = locer.Loc()
	}
	return e
}

func (e *_error) done(c *topScope) {
	var s strings.Builder
	l := c.importer.Files().Location(e.loc)
	l.Path = strings.TrimPrefix(l.Path, c.trimErrorPathPrefix)
	s.WriteString(l.String())
	s.WriteString(": ")
	s.WriteString(e.msg)
	for _, n := range e.notes {
		s.WriteRune('\n')
		n.buildString(c, true, 1, &s)
	}
	e.msg = s.String()
}

func (e *_error) buildString(c *topScope, mustIdent bool, depth int, s *strings.Builder) {
	s.WriteString(strings.Repeat("\t", depth))
	s.WriteString(e.msg)
	if e.loc != (loc.Loc{}) {
		s.WriteString(" (")
		l := c.importer.Files().Location(e.loc)
		l.Path = strings.TrimPrefix(l.Path, c.trimErrorPathPrefix)
		s.WriteString(l.String())
		s.WriteRune(')')
	}
	mustIdent = mustIdent || len(e.notes) > 1
	for _, n := range e.notes {
		s.WriteRune('\n')
		if mustIdent {
			n.buildString(c, false, depth+1, s)
		} else {
			n.buildString(c, true, depth, s)
		}
	}
}

// NotFoundError indicates an identifier whose definition is not found.
type NotFoundError struct {
	Ident parser.Ident
	scope scope
	notes []note
}

func notFound(x scope, ident parser.Ident) *NotFoundError {
	return &NotFoundError{Ident: ident, scope: x}
}

func notFoundTypeVar(x scope, tv parser.TypeVar) *NotFoundError {
	return &NotFoundError{Ident: parser.Ident(tv), scope: x}
}

func (err *NotFoundError) add(ns ...note)     { err.notes = append(err.notes, ns...) }
func (err *NotFoundError) done(top *topScope) {}
func (err *NotFoundError) Loc() loc.Loc       { return err.Ident.L }

func (err *NotFoundError) Error() string {
	var s strings.Builder
	writeLoc(err.scope, err.Loc(), &s)
	s.WriteString(fmt.Sprintf(": %s not found", err.Ident.Name))
	writeNotes(err.scope, err.notes, &s)
	return s.String()
}

func writeNotes(x scope, notes []note, s *strings.Builder) {
	t := top(x)
	for _, n := range notes {
		s.WriteRune('\n')
		n.buildString(t, true, 1, s)
	}
}

func writeLoc(x scope, l loc.Loc, s *strings.Builder) {
	t := top(x)
	if t.importer.Files().Len() == 0 {
		// This happens in some tests,
		// where the location info is not correctly tracked
		// across calls to parseTestPattern and such helpers.
		// TODO: tests to always have location information.
		return
	}
	location := t.importer.Files().Location(l)
	location.Path = strings.TrimPrefix(location.Path, t.trimErrorPathPrefix)
	s.WriteString(location.String())
}
