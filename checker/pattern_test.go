package checker

import (
	"strings"
	"testing"

	"github.com/eaburns/pea/parser"
)

func TestCommonPattern(t *testing.T) {
	tests := []struct {
		src  string
		pats []string
		want string
	}{
		{
			pats: []string{},
			want: "?",
		},
		{
			pats: []string{"int"},
			want: "int",
		},
		{
			src:  "type point [.x int, .y int]",
			pats: []string{"point"},
			want: "point",
		},
		{
			pats: []string{"T"},
			want: "T",
		},
		{
			pats: []string{"&float32"},
			want: "&float32",
		},
		{
			pats: []string{"[string]"},
			want: "[string]",
		},
		{
			pats: []string{"[.x int]"},
			want: "[.x int]",
		},
		{
			pats: []string{"[x? int]"},
			want: "[x? int]",
		},
		{
			pats: []string{"(int){}"},
			want: "(int){}",
		},
		{
			pats: []string{
				"int",
				"[x? int, y? string]",
			},
			want: "?",
		},
		{
			src: "type point [.x int, .y int]",
			pats: []string{
				"point",
				"point",
			},
			want: "point",
		},
		{
			src: `
				type point [.x int, .y int]
				type rect [.w int, .h int]
			`,
			pats: []string{
				"point",
				"rect",
			},
			want: "?",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(int, int) pair",
			},
			want: "(int, int) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(string, int) pair",
			},
			want: "(?, int) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(int, string) pair",
			},
			want: "(int, ?) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(string, string) pair",
			},
			want: "(?0, ?1) pair",
		},
		{
			pats: []string{
				"int",
				"int",
			},
			want: "int",
		},
		{
			pats: []string{
				"int",
				"[false?, true?]",
			},
			want: "?",
		},
		{
			pats: []string{
				"&int",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"&int",
				"&int",
			},
			want: "&int",
		},
		{
			pats: []string{
				"&int",
				"&string",
			},
			want: "&?",
		},
		{
			pats: []string{
				"[int]",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"[int]",
				"[int]",
			},
			want: "[int]",
		},
		{
			pats: []string{
				"[int]",
				"[string]",
			},
			want: "[?]",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.a int, .b string]",
			},
			want: "?",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.y string, .x int]",
			},
			want: "?",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.x int]",
			},
			want: "?",
		},
		{
			pats: []string{
				"[.]",
				"[.]",
			},
			want: "[.]",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.x int, .y string]",
			},
			want: "[.x int, .y string]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int32, .y int]",
			},
			want: "[.x ?, .y int]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int, .y int32]",
			},
			want: "[.x int, .y ?]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int32, .y int32]",
			},
			want: "[.x ?0, .y ?1]",
		},
		{
			pats: []string{
				"[.x int, .y float32, .z string]",
				"[.x int, .y [false?, true?], .z string]",
				"[.x [false?, true?], .y float32, .z string]",
			},
			want: "[.x ?0, .y ?1, .z string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?, z? string]",
			},
			want: "[x? int, y?, z? string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[a? int, b?, c? string]",
			},
			want: "?",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?]",
			},
			want: "?",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? [false?, true?], y?, z? string]",
			},
			want: "[x? ?, y?, z? string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?, z? [false?, true?]]",
			},
			want: "[x? int, y?, z? ?]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? [false?, true?], y?, z? [false?, true?]]",
			},
			want: "[x? ?0, y?, z? ?1]",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int){float64}",
			},
			want: "?",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string, float32){float64}",
			},
			want: "?",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){}",
			},
			want: "(int, string){?}",
		},
		{
			pats: []string{
				"(){}",
				"(){}",
			},
			want: "(){}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){float64}",
			},
			want: "(int, string){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"([false?, true?], string){float64}",
			},
			want: "(?, string){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, [false?, true?]){float64}",
			},
			want: "(int, ?){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){[false?, true?]}",
			},
			want: "(int, string){?}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int8, float32){[false?, true?]}",
			},
			want: "(?0, ?1){?2}",
		},
		{
			pats: []string{
				"T",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"T",
				"T",
			},
			want: "T",
		},
		{
			pats: []string{
				"?",
				"int",
			},
			want: "?",
		},
		{
			pats: []string{
				"?0",
				"?1",
			},
			want: "?",
		},
		{
			pats: []string{
				"?1",
				"?2",
			},
			want: "?",
		},
		{
			pats: []string{
				"(int, string){?}",
			},
			want: "(int, string){?}",
		},
	}
	for _, test := range tests {
		t.Run(strings.Join(test.pats, ","), func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			pats := make([]TypePattern, len(test.pats))
			for i, p := range test.pats {
				pats[i] = parseTestPattern(t, mod, p)
			}
			if u := common(pats...); u.String() != test.want {
				t.Errorf("union(%s)=%s, want %s", pats, u.String(), test.want)
			}
		})
	}
}

func parseTestPattern(t *testing.T, m *Mod, src string) TypePattern {
	t.Helper()
	p, err := parser.ParseTypePattern(src)
	if err != nil {
		t.Fatalf("failed to parse type %s: %s", src, err)
	}
	typ, errs := _makeType(m, p, true, true)
	if len(errs) > 0 {
		t.Fatalf("failed to make type: %s", errs[0])
	}
	parms := m.testTypeParms.Minus(func(p *TypeParm) bool {
		return !strings.HasPrefix(p.Name, "?")
	})
	return makeTypePattern(parms, typ)
}

func parseTestType(t *testing.T, m *Mod, src string) Type {
	t.Helper()
	p, err := parser.ParseType(src)
	if err != nil {
		t.Fatalf("failed to parse source: %s", err)
	}
	typ, errs := _makeType(m, p, true, true)
	if len(errs) > 0 {
		t.Fatalf("failed make type: %s", errs[0])
	}
	return typ
}
