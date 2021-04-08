package checker

import (
	"fmt"
	"os"
	"strings"

	"github.com/eaburns/pea/loc"
)

type Error interface {
	error
	fmt.Stringer
	loc.Locer

	setNotes([]note)
	note(string, ...interface{}) note
	done(files loc.Files, maxDepth int)
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

func notFound(name string, locer loc.Locer) Error {
	return newError(locer, "%s: not found", name)
}

func ambigType(name string, locer loc.Locer, types []Type) Error {
	err := newError(locer, "type %s is ambiguous", name)
	for _, t := range types {
		err.note(t.String()).setLoc(t)
	}
	return err
}

func redef(locer loc.Locer, name string, prev loc.Locer) Error {
	err := newError(locer, "%s redefined", name)
	err.note("previous").setLoc(prev)
	return err
}

type note interface {
	setNotes([]note)
	setLoc(x interface{}) note
	buildString(files loc.Files, mustIdent bool, maxDepth, depth int, s *strings.Builder)
}

func newNote(f string, vs ...interface{}) note {
	return &_error{msg: fmt.Sprintf(f, vs...)}
}

type _error struct {
	msg   string
	loc   loc.Loc
	notes []note
}

func (e *_error) Error() string      { return e.msg }
func (e *_error) String() string     { return e.msg }
func (e *_error) Loc() loc.Loc       { return e.loc }
func (e *_error) setNotes(ns []note) { e.notes = ns }

func (e *_error) setLoc(x interface{}) note {
	if locer, ok := x.(loc.Locer); ok {
		e.loc = locer.Loc()
	}
	return e
}

func (e *_error) note(f string, vs ...interface{}) note {
	e.notes = append(e.notes, newNote(f, vs...))
	return e.notes[len(e.notes)-1]
}

func (e *_error) done(files loc.Files, maxDepth int) {
	var s strings.Builder
	s.WriteString(files.Location(e.loc).String())
	s.WriteString(": ")
	s.WriteString(e.msg)
	for _, n := range e.notes {
		s.WriteRune('\n')
		n.buildString(files, true, 1, maxDepth, &s)
	}
	e.msg = s.String()
}

func (e *_error) buildString(files loc.Files, mustIdent bool, depth, maxDepth int, s *strings.Builder) {
	if depth > maxDepth {
		return
	}
	s.WriteString(strings.Repeat("\t", depth))
	s.WriteString(e.msg)
	if e.loc != (loc.Loc{}) {
		s.WriteString(" (")
		s.WriteString(files.Location(e.loc).String())
		s.WriteRune(')')
	}
	mustIdent = mustIdent || len(e.notes) > 1
	for _, n := range e.notes {
		s.WriteRune('\n')
		if mustIdent {
			n.buildString(files, false, depth+1, maxDepth, s)
		} else {
			n.buildString(files, true, depth, maxDepth, s)
		}
	}
}
