package checker

import (
	"fmt"
	"os"
	"strings"

	"github.com/eaburns/pea/loc"
)

type Error interface {
	error
	loc.Locer
	Cause

	add(...note)
	done(*topScope)
}

func newError(locer loc.Locer, f string, vs ...interface{}) Error {
	l := locer.Loc()
	if l == (loc.Loc{}) {
		if pr, ok := locer.(treePrinter); ok {
			print(os.Stderr, pr)
		}
		panic("impossible no location")
	}
	return &_error{msg: fmt.Sprintf(f, vs...), loc: l}
}

func redef(locer loc.Locer, name string, prev loc.Locer) Error {
	err := newError(locer, "%s redefined", name)
	if (prev.Loc() != loc.Loc{}) {
		err.add(newNote("previous").setLoc(prev))
	}
	return err
}

type note interface {
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
		p.printf(" — %s", locOf{e.loc})
	}
	printNotes(p, e.notes)
}

// A Cause is additional information about an error.
type Cause interface {
	print(errorPrinter)
}

// NotFoundError indicates an identifier whose definition is not found.
type NotFoundError struct {
	Item interface {
		String() string
		Loc() loc.Loc
	}
	Candidates []CandidateError
	scope      scope

	// TODO: remove NotFoundError.notes once unused.
	notes
}

func (err *NotFoundError) Loc() loc.Loc { return err.Item.Loc() }

func (err *NotFoundError) Error() string {
	p := makeErrorPrinter(top(err.scope))
	p.printf("%s: ", locOf{err.Item})
	err.print(p)
	return p.String()
}

func (err *NotFoundError) print(p errorPrinter) {
	p.printf("%s not found", err.Item)
	for _, ce := range err.Candidates {
		ce.print(p.listItem())
	}
	printNotes(p, err.notes)
}

// A CandidateError is a candidate for ID resolution
// combined with an error message and cause
// for why that candidate was not, ultimately selected.
type CandidateError struct {
	// Both Func and id satisfy this.
	// If Candidate is also a loc.Locer,
	// its location is printed following
	// Candidate.String() + " – ".
	Candidate fmt.Stringer
	Msg       string
	// Possibly nil. If non-nil:
	// 	- if Msg is non-empty, Cause is printed after Msg + ": ".
	// 	- if Msg is empty, Cause is printed on its own.
	Cause Cause
}

func (err *CandidateError) print(p errorPrinter) {
	p.printf("%s", err.Candidate)
	if l, ok := err.Candidate.(loc.Locer); ok {
		p.printf(" — %s\n", locOf{l})
	} else {
		p.printf("\n")
	}
	if err.Msg != "" {
		p.printf("%s", err.Msg)
	}
	if err.Cause != nil {
		if err.Msg != "" {
			p.printf(": ")
		}
		err.Cause.print(p)
	}
}

// AmbiguousError is an error indicating that an identifier was ambiguous.
type AmbiguousError struct {
	Item interface {
		String() string
		Loc() loc.Loc
	}
	// Both Func and id satisfy this.
	// If Candidate is also a loc.Locer,
	// its location is printed following
	// Candidate.String() + " – ".
	Candidates []fmt.Stringer

	scope scope

	// TODO: remove NotFoundError.notes once unused.
	notes
}

func (err *AmbiguousError) Loc() loc.Loc { return err.Item.Loc() }

func (err *AmbiguousError) Error() string {
	p := makeErrorPrinter(top(err.scope))
	p.printf("%s: ", locOf{err.Item})
	err.print(p)
	return p.String()
}

func (err *AmbiguousError) print(p errorPrinter) {
	p.printf("%s is ambiguous", err.Item)
	for _, c := range err.Candidates {
		if l, ok := c.(loc.Locer); ok {
			p.listItem().printf("%s — %s", c, locOf{l})
		} else {
			p.listItem().printf("%s", c)
		}
	}
}

// ConvertExprError is an error indicating
// that an expression's type cannot be converted
// to the necessary pattern.
type ConvertExprError struct {
	Expr     Expr
	Dst      TypePattern
	Cause    Cause
	Explicit bool
	scope    scope
	// TODO: remove ConvertExprError.notes.
	notes
}

func (err *ConvertExprError) Loc() loc.Loc { return err.Expr.Loc() }

func (err *ConvertExprError) Error() string {
	p := makeErrorPrinter(top(err.scope))
	p.printf("%s: ", locOf{err.Expr})
	err.print(p)
	return p.String()
}

func (err *ConvertExprError) print(p errorPrinter) {
	var cause Cause
	if ce, ok := err.Cause.(*ConvertError); ok {
		// If verbose is false, ConvertError would not print its Cause,
		// and since this is supposed to behave similar to ConvertError,
		// it should also not print the ConvertError.Cause.
		if p.verbose {
			cause = ce.Cause
		}
	} else {
		cause = err.Cause
	}
	if err.Explicit {
		p.printf("cannot convert ")
	} else {
		p.printf("cannot implicitly convert ")
	}
	p.printf("%s (type %s) to %s", err.Expr, err.Expr.Type(), err.Dst)
	if cause != nil {
		p.printf("\n")
		cause.print(p)
	}
	printNotes(p, err.notes)
}

// ConvertError is an error describing
// failure to convert pattern Src to Dst.
type ConvertError struct {
	Src, Dst TypePattern
	// DefType is non-nil if either Src or Dst is a *DefType,
	// and this error is propagating a non-nil Casuse
	// for failure to convert the underlying type.
	DefType  *DefType
	Cause    Cause
	Explicit bool
}

func (err *ConvertError) print(p errorPrinter) {
	if err.Explicit {
		p.printf("cannot convert %s to %s", err.Src, err.Dst)
	} else {
		p.printf("cannot implicitly convert %s to %s", err.Src, err.Dst)
	}
	if p.verbose && err.Cause != nil {
		if dt := err.DefType; dt != nil {
			p.printf("\n%s is defined as %s at %s", dt, dt.Inst.Type, locOf{dt.Def})
		}
		p.printf("\n")
		err.Cause.print(p)
	}
}

// DiffTypeKindError implements PatternAlignError;
// it indicates that two patterns did not align,
// because their types were not the same kind.
type DiffTypeKindError struct{ A, B Type }

func (err *DiffTypeKindError) print(p errorPrinter) {
	p.printf("%s and %s are different kinds of types", err.A, err.B)
	p.printf("\n%s is %s", err.A, typeKindString(err.A))
	p.printf("\n%s is %s", err.B, typeKindString(err.B))
}

func typeKindString(t Type) string {
	switch t := t.(type) {
	case *DefType:
		return "a defined type"
	case *RefType:
		return "a reference literal type"
	case *ArrayType:
		return "an array literal type"
	case *StructType:
		return "a struct literal type"
	case *UnionType:
		return "a union literal type"
	case *FuncType:
		return "a function type"
	case *BasicType:
		return "the built-in " + t.String() + " type"
	case *TypeVar:
		return "a type variable"
	case nil:
		return "nil"
	default:
		panic(fmt.Sprintf("impossible Type type: %T", t))
	}
}

// DiffNamedTypeError implements PatternAlignError;
// it indicates that two patterns didn't align
// because their types are different named types.
type DiffNamedTypeError struct {
	// A and B are the differing types.
	// A.Type and B.Type are each either
	// *DefType, *BasicType.
	A, B Type
}

func (err *DiffNamedTypeError) print(p errorPrinter) {
	p.printf("%s and %s are different named types", defName{err.A}, defName{err.B})
	dtA, _ := err.A.(*DefType)
	dtB, _ := err.B.(*DefType)
	if dtA != nil && dtB != nil {
		p.printf("\n%s is defined at %s", defName{dtA}, locOf{dtA.Def})
		p.printf("\n%s is defined at %s", defName{dtB}, locOf{dtB.Def})
	}
}

// DiffTypeVarError implementsPatternAlignError;
// it indicates that two patterns didn't align
// because  their types were both type variables
// from different definitions.
type DiffTypeVarError struct{ A, B *TypeVar }

func (err *DiffTypeVarError) print(p errorPrinter) {
	p.printf("%s and %s are different type variables", err.A, err.B)
	p.printf("\n%s is defined at %s", err.A, locOf{err.A.Def})
	p.printf("\n%s is defined at %s", err.B, locOf{err.B.Def})
}

// DiffFieldsError implements PatternAlignError;
// it indicates that two patterns didn't align
// because their typse were struct literal types
// with differing fields.
type DiffFieldsError struct {
	A, B *StructType
	// If the difference is in the number of fields, Cause is nil.
	// If the first difference (from left-to-right) is in a field name, Cause is nil.
	// Cause is non-nil if the first difference is in the type of a field.
	Cause Cause
	// Field is the field name with the underlying error if Cause is non-nil.
	Field string
}

func (err *DiffFieldsError) print(p errorPrinter) {
	p.printf("%s and %s have different fields", err.A, err.B)
	switch {
	case len(err.A.Fields) != len(err.B.Fields):
		p.printf("\n%s has %d fields", err.A, len(err.A.Fields))
		p.printf("\n%s has %d fields", err.B, len(err.B.Fields))
	case err.Cause != nil:
		p.printf("\nfield %s: ", err.Field)
		err.Cause.print(p)
	}
}

// DiffCasesError implements PatternAlignError;
// it indicates that two patterns didn't align
// because their types were union literals types
// with different cases.
type DiffCasesError struct {
	A, B *UnionType
	// If the difference is in the number of cases, Cause is nil.
	// If the first difference (from left-to-right) is in a case name, Cause is nil.
	// If the first difference is in the typed-ness of a case, Cause is nil.
	// Cause is non-nil if the first difference is in the type of a case.
	Cause Cause
	// Case is the case name with the underlying error if Cause is non-nil.
	Case string
}

func (err *DiffCasesError) print(p errorPrinter) {
	p.printf("%s and %s have different cases", err.A, err.B)
	switch {
	case len(err.A.Cases) != len(err.B.Cases):
		p.printf("\n%s has %d cases", err.A, len(err.A.Cases))
		p.printf("\n%s has %d cases", err.B, len(err.B.Cases))
	case err.Cause != nil:
		p.printf("\ncase %s: ", err.Case)
		err.Cause.print(p)
	default:
		// It must be that there was a mis-match in case typedness.
		var a, b *CaseDef
		for i := range err.A.Cases {
			if err.A.Cases[i].Name == err.Case {
				a = &err.A.Cases[i]
				b = &err.B.Cases[i]
				if b.Name != err.Case {
					panic("impossible")
				}
			}
			break
		}
		switch {
		default:
			fallthrough
		case a == nil || b == nil:
			panic("impossible")
		case a.Type == nil:
			p.printf("\ncase %s: untyped and %s", err.Case, b.Type)
		case b.Type == nil:
			p.printf("\ncase %s: %s and untyped", err.Case, a.Type)
		}
	}
}

// DiffFuncError implements PatternAlignError;
// it indicates that two patterns didn't align
// because tehir types were function types
// with different signatures.
type DiffFuncError struct {
	A, B *FuncType

	// Cause is nil if the difference is in the arity,
	// otherwise if the difference is a parameter or return type
	// then Cause has the undrelying error.
	Cause Cause

	// Parm is the differing parameter index if a parameter type differs,
	// -1 if the return value differs, or
	// 0 if the arity differs, in which case Parm is to be ignored.
	Parm int
}

func (err *DiffFuncError) print(p errorPrinter) {
	switch {
	case err.Cause == nil:
		p.printf("%s and %s have different arity", err.A, err.B)
		p.printf("\n%s has %d parameters", err.A, len(err.A.Parms))
		p.printf("\n%s has %d parameters", err.B, len(err.B.Parms))
	case err.Parm >= 0:
		p.printf("%s and %s have different parameter types", err.A, err.B)
		p.printf("\nparameter %d: %s and %s", err.Parm,
			err.A.Parms[err.Parm],
			err.B.Parms[err.Parm])
	default:
		p.printf("%s and %s have different return types", err.A, err.B)
		p.printf("\nreturn: %s and %s", err.A.Ret, err.B.Ret)
	}
}

// PatternBindingError indicates an error binding type variables,
// because a type variable is bound to two different types.
type PatternBindingError struct {
	Parm      *TypeParm
	Prev, Cur Type
}

func (err *PatternBindingError) print(p errorPrinter) {
	p.printf("%s binds %s and %s", err.Parm.Name, err.Prev, err.Cur)
}

// PatternSubError indicates recursive substitution
// when substituting a type variable during pattern unification.
type PatternSubError struct {
	Loop []*TypeParm
}

func (err *PatternSubError) print(p errorPrinter) {
	p.printf("recursive binding: ")
	for i, parm := range err.Loop {
		if i > 0 {
			p.printf(" -> ")
		}
		p.printf("%s", parm.Name)
	}
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
	verbose             bool
	trimErrorPathPrefix string
	w                   *strings.Builder
}

func makeErrorPrinter(top *topScope) errorPrinter {
	return errorPrinter{
		bullet:              0,
		indent:              "",
		files:               top.importer.Files(),
		verbose:             top.verbose,
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

// defName wraps a Type that must be either a *DefType or *BasicType.
// errorPrinter.printf will format the type name from the definition,
// which can differ from the DefType.String(), since the definition
// does not have substituted type parameters.
type defName struct{ Type }

func (p errorPrinter) printf(f string, vs ...interface{}) {
	for i := range vs {
		switch v := vs[i].(type) {
		case locOf:
			if p.files.Len() == 0 {
				vs[i] = "no location info available"
				break
			}
			location := p.files.Location(v.Loc())
			location.Path = strings.TrimPrefix(location.Path, p.trimErrorPathPrefix)
			vs[i] = location
		case defName:
			if _, ok := v.Type.(*BasicType); ok {
				continue
			}
			dt := v.Type.(*DefType)
			var w strings.Builder
			switch {
			case len(dt.Def.Parms) == 1:
				w.WriteString(dt.Def.Parms[0].Name)
				w.WriteString(" ")
			case len(dt.Def.Parms) > 1:
				w.WriteString("(")
				w.WriteString(dt.Def.Parms[0].Name)
				for _, parm := range dt.Def.Parms[1:] {
					w.WriteString(", ")
					w.WriteString(parm.Name)
				}
				w.WriteString(") ")
			}
			w.WriteString(dt.Def.Name)
			vs[i] = w.String()
		}
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
