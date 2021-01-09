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

	addNotes([]note) Error
	note(string, ...interface{}) *note
	done(files loc.Files, maxDepth int)
}

type _error struct {
	msg   string
	loc   loc.Loc
	notes []note
}

func (e *_error) Error() string  { return e.msg }
func (e *_error) String() string { return e.msg }
func (e *_error) Loc() loc.Loc   { return e.loc }

func (e *_error) addNotes(ns []note) Error {
	e.notes = append(e.notes, ns...)
	return e
}

func (e *_error) note(f string, vs ...interface{}) *note {
	e.notes = append(e.notes, *newNote(f, vs...))
	return &e.notes[len(e.notes)-1]
}

func (e *_error) done(files loc.Files, maxDepth int) {
	var s strings.Builder
	s.WriteString(files.Location(e.loc).String())
	s.WriteString(": ")
	s.WriteString(e.msg)
	for _, n := range e.notes {
		s.WriteString("\n\t")
		s.WriteString(n.msg)
		if n.loc != (loc.Loc{}) {
			s.WriteString(" (")
			s.WriteString(files.Location(n.loc).String())
			s.WriteRune(')')
		}
	}
	e.msg = s.String()
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

type note struct {
	msg string
	loc loc.Loc // empty for built-in
}

func newNote(f string, vs ...interface{}) *note {
	return &note{msg: fmt.Sprintf(f, vs...)}
}

func notFound(name string, locer loc.Locer) Error {
	return newError(locer, "%s: not found", name)
}

func redef(locer loc.Locer, name string, prev loc.Locer) Error {
	err := newError(locer, "%s redefined", name)
	err.note("previous").loc = prev.Loc()
	return err
}
