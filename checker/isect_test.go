package checker

import (
	"fmt"
	"strings"
	"testing"
)

func TestAlignPatternsTypeVars(t *testing.T) {
	tests := []alignPatternTest{
		{
			a:       "?",
			b:       "?",
			aa:      "?",
			bb:      "?",
			aligned: true,
		},
		{
			a:       "?1",
			b:       "?2",
			aa:      "?",
			bb:      "?",
			aligned: true,
		},
		{
			a:       "?",
			b:       "int",
			aa:      "?",
			bb:      "int",
			aligned: true,
		},
	}
	for _, test := range tests {
		test.Run(t)
	}
}

func TestAlignPatternsOpenUnions(t *testing.T) {
	tests := []alignPatternTest{
		{
			a:       "[some? int, …]",
			b:       "[some? int, …]",
			aa:      "[some? int, …]",
			bb:      "[some? int, …]",
			aligned: true,
		},
		{
			a:       "[some? int, …]",
			b:       "[some? ?, …]",
			aa:      "[some? int, …]",
			bb:      "[some? ?, …]",
			aligned: true,
		},
		{
			a:       "[some? int, …]",
			b:       "[none?, …]",
			aa:      "[none?, some? int, …]",
			bb:      "[none?, some? int, …]",
			aligned: true,
		},
		{
			a:       "[none?, some? int, …]",
			b:       "[none?, …]",
			aa:      "[none?, some? int, …]",
			bb:      "[none?, some? int, …]",
			aligned: true,
		},
		{
			a:       "[maybe?, some? int, …]",
			b:       "[none?, some? int, …]",
			aa:      "[maybe?, none?, some? int, …]",
			bb:      "[maybe?, none?, some? int, …]",
			aligned: true,
		},
		{
			a:       "[a?, common? [x? int, …], z?, …]",
			b:       "[aa?, common? [y?, …], zz?, …]",
			aa:      "[a?, aa?, common? [x? int, y?, …], z?, zz?, …]",
			bb:      "[a?, aa?, common? [x? int, y?, …], z?, zz?, …]",
			aligned: true,
		},
		{
			a:       "[a?, common? [x? int, …], z?, …]",
			b:       "[aa?, common? [x? ?, …], zz?, …]",
			aa:      "[a?, aa?, common? [x? int, …], z?, zz?, …]",
			bb:      "[a?, aa?, common? [x? ?, …], z?, zz?, …]",
			aligned: true,
		},
		{
			a:       "[some? int, …]",
			b:       "[some?, …]",
			aa:      "[some? int, …]",
			bb:      "[some?, …]",
			aligned: false,
		},
		{
			a:       "[some? int, …]",
			b:       "[some? float32, …]",
			aa:      "[some? int, …]",
			bb:      "[some? float32, …]",
			aligned: false,
		},
		{
			a:       "[a?, some?, z?, …]",
			b:       "[a?, some? int, z?, …]",
			aa:      "[a?, some?, z?, …]",
			bb:      "[a?, some? int, z?, …]",
			aligned: false,
		},
	}
	for _, test := range tests {
		test.Run(t)
	}
}

func TestAlignPatternsNamedType(t *testing.T) {
	tests := []alignPatternTest{
		{
			src: `
				type (X, Y) or [left? X, right? Y]
			`,
			a:       "(int, int) or",
			b:       "[left? int, right? int]",
			aa:      "(int, int) or",
			bb:      "(int, int) or",
			aligned: true,
		},
		{
			src: `
				type (X, Y) or [left? X, right? Y]
			`,
			a:       "(?0, ?1) or",
			b:       "[left? int, right? int]",
			aa:      "(?0, ?1) or",
			bb:      "(int, int) or",
			aligned: true,
		},
		{
			src: `
				type (X, Y) or [left? X, right? Y]
			`,
			a:       "(int, ?) or",
			b:       "[left? ?, right? int]",
			aa:      "(int, ?) or",
			bb:      "(?, int) or",
			aligned: true,
		},
		{
			src: `
				type (X, Y) or [left? X, right? Y]
				type (X, Y) swapped_or (Y, X) or
			`,
			a:       "(int, ?) swapped_or",
			b:       "[left? int, right? ?]",
			aa:      "(int, ?) swapped_or",
			bb:      "(?, int) swapped_or",
			aligned: true,
		},
	}
	for _, test := range tests {
		test.Run(t)
	}
}

type alignPatternTest struct {
	src     string
	a, b    string
	aa, bb  string
	aligned bool
}

func (test alignPatternTest) Run(t *testing.T) {
	t.Run(fmt.Sprintf("align(%s, %s)", test.a, test.b), func(t *testing.T) {
		mod, errs := check("test", []string{test.src}, nil)
		if len(errs) > 0 {
			t.Fatalf("failed to parse and check: %s", errs[0])
		}

		// Here's a hack to pretend we can parse open unions.
		// If we see "…" anywhere, we assume it's in a literal union type,
		// and we replace it with "OPEN?", making a type-less case.
		//
		// After parsing, we walk the type fixing any such union
		// to remove the OPEN? case and making it Open=true.
		aSrc := strings.ReplaceAll(test.a, "…", "OPEN?")
		bSrc := strings.ReplaceAll(test.b, "…", "OPEN?")

		a, b := parseTwoTestPatterns(t, mod, aSrc, bSrc)

		fixOpenUnion := func(t Type) bool {
			u, ok := t.(*UnionType)
			if !ok {
				return false
			}
			var i int
			for _, c := range u.Cases {
				if c.Name == "OPEN?" {
					u.Open = true
					continue
				}
				u.Cases[i] = c
				i++
			}
			u.Cases = u.Cases[:i]
			return false
		}
		walkType(a.Type, fixOpenUnion)
		walkType(b.Type, fixOpenUnion)

		// We call fixUnusedParms to remove unused type parameters.
		// They make the ?-representation of the actually used parameters
		// have arbitrary digits after them.

		aa, bb, ok := fixUnusedParms(alignPatterns2(a, b))
		if aa.String() != test.aa || bb.String() != test.bb || ok != test.aligned {
			t.Errorf("align(%s, %s)=%s, %s, %v, want %s, %s, %v",
				a, b, aa, bb, ok, test.aa, test.bb, test.aligned)
		}

		bb, aa, ok = fixUnusedParms(alignPatterns2(b, a))
		if aa.String() != test.aa || bb.String() != test.bb || ok != test.aligned {
			t.Errorf("align(%s, %s)=%s, %s, %v, want %s, %s, %v",
				a, b, aa, bb, ok, test.aa, test.bb, test.aligned)
		}
	})
}

func fixUnusedParms(a, b TypePattern, ok bool) (TypePattern, TypePattern, bool) {
	filterUnusedParms(&a)
	filterUnusedParms(&b)
	return a, b, ok
}

func filterUnusedParms(pat *TypePattern) {
	refed := make(map[*TypeParm]bool)
	walkType(pat.Type, func(t Type) bool {
		if tv, ok := t.(*TypeVar); ok {
			refed[tv.Def] = true
		}
		return false
	})
	pat.Parms = pat.Parms.Minus(func (p *TypeParm)bool { return !refed[p] })
}

func TestParseTwoTestPatterns(t *testing.T) {
	mod, errs := check("test", []string{""}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}

	// Should be equal.
	a, b := parseTwoTestPatterns(t, mod, "?0", "?0")
	if !eqType(a.Type, b.Type) {
		t.Errorf("%s != %s", a, b)
	}
	a, b = parseTwoTestPatterns(t, mod, "(?0, ?1, ?2){?3}", "(?0, ?1, ?2){?3}")
	if !eqType(a.Type, b.Type) {
		t.Errorf("%s != %s", a, b)
	}

	// Should not be equal
	a, b = parseTwoTestPatterns(t, mod, "?1", "?2")
	if eqType(a.Type, b.Type) {
		t.Errorf("%s == %s", a, b)
	}
	a, b = parseTwoTestPatterns(t, mod, "(?0, ?1, ?2){?10000}", "(?0, ?1, ?2){?3}")
	if eqType(a.Type, b.Type) {
		t.Errorf("%s != %s", a, b)
	}
}

// Parses two type patterns, using the same type parameter namespace.
func parseTwoTestPatterns(t *testing.T, m *Mod, a, b string) (TypePattern, TypePattern) {
	parmSet := make(map[string]*TypeParm)
	aPat, next := __parseTestPattern(t, m, 0, parmSet, a)
	bPat, _ := __parseTestPattern(t, m, next, parmSet, b)
	return aPat, bPat
}
