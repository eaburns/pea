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
	done(*checker)
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
	verbose(bool)
	isVerbose() bool
	setNotes([]note)
	setLoc(x interface{}) note
	buildString(c *checker, mustIdent bool, depth int, s *strings.Builder)
}

func newNote(f string, vs ...interface{}) note {
	return &_error{msg: fmt.Sprintf(f, vs...)}
}

func markVerbose(notes []note) {
	for i := range notes {
		notes[i].verbose(true)
	}
}

type _error struct {
	msg   string
	loc   loc.Loc
	notes []note

	// v is whether this note should be displayed
	// only in verbose mode.
	v bool
}

func (e *_error) Error() string      { return e.msg }
func (e *_error) String() string     { return e.msg }
func (e *_error) Loc() loc.Loc       { return e.loc }
func (e *_error) verbose(b bool)     { e.v = b }
func (e *_error) isVerbose() bool    { return e.v }
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

func (e *_error) done(c *checker) {
	var s strings.Builder
	s.WriteString(c.importer.Files().Location(e.loc).String())
	s.WriteString(": ")
	s.WriteString(e.msg)
	i := 0
	for _, n := range e.notes {
		if n.isVerbose() && !c.verboseNotes {
			continue
		}
		s.WriteRune('\n')
		n.buildString(c, true, 1, &s)
		e.notes[i] = n
		i++
	}
	e.notes = e.notes[:i]
	e.msg = s.String()
}

func (e *_error) buildString(c *checker, mustIdent bool, depth int, s *strings.Builder) {
	s.WriteString(strings.Repeat("\t", depth))
	s.WriteString(e.msg)
	if e.loc != (loc.Loc{}) {
		s.WriteString(" (")
		s.WriteString(c.importer.Files().Location(e.loc).String())
		s.WriteRune(')')
	}
	mustIdent = mustIdent || len(e.notes) > 1
	if c.maxErrorDepth >= 0 && depth+1 > c.maxErrorDepth {
		s.WriteRune('\n')
		s.WriteString(strings.Repeat("\t", depth+1))
		s.WriteRune('…')
		return
	}
	for _, n := range e.notes {
		if n.isVerbose() && !c.verboseNotes {
			continue
		}
		s.WriteRune('\n')
		if mustIdent {
			n.buildString(c, false, depth+1, s)
		} else {
			n.buildString(c, true, depth, s)
		}
	}
}
