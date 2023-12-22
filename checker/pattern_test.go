package checker

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"testing"
	"unicode"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
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
				"?",
				"?",
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

func TestPatternIntersection(t *testing.T) {
	tests := []struct {
		src  string
		a    string
		b    string
		want string
	}{
		// Sub ? with all the different kinds of types.
		{a: `?`, b: `?`, want: `?`},
		{a: `?`, b: `int`, want: `int`},
		{src: `type t int`, a: `?`, b: `t`, want: `t`},
		{src: `type (T, U) t int`, a: `?`, b: `(int, string) t`, want: `(int, string) t`},
		{a: `?`, b: `T`, want: `T`},
		{a: `?`, b: `&int`, want: `&int`},
		{a: `?`, b: `[int]`, want: `[int]`},
		{a: `?`, b: `[.x int]`, want: `[.x int]`},
		{a: `?`, b: `[x?]`, want: `[x?]`},
		{a: `?`, b: `[x? int]`, want: `[x? int]`},
		{a: `?`, b: `(int, string){float32}`, want: `(int, string){float32}`},

		// Sub ? with all the different kinds of types, leading to recursive sub error.
		{src: `type T t int`, a: `[.x ?0, .y ?0]`, b: `[.x ?1 t, .y ?1]`, want: ``},
		{a: `[.x ?0, .y ?0]`, b: `[.x &?1, .y ?1]`, want: ``},
		{a: `[.x ?0, .y ?0]`, b: `[.x [?1], .y ?1]`, want: ``},
		{a: `[.x ?0, .y ?0]`, b: `[.x [.y ?1], .y ?1]`, want: ``},
		{a: `[.x ?0, .y ?0]`, b: `[.x [y? ?1], .y ?1]`, want: ``},
		{a: `[.x ?0, .y ?0]`, b: `[.x (?1){}, .y ?1]`, want: ``},
		{a: `[.x ?0, .y ?0]`, b: `[.x (){?1}, .y ?1]`, want: ``},

		// Sub into all different spots.
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y ?0]`, want: `[.x int, .y int]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y &?0]`, want: `[.x int, .y &int]`},
		{src: `type T t int`, a: `[.x int, .y ?]`, b: `[.x ?0, .y ?0 t]`, want: `[.x int, .y int t]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y [?0]]`, want: `[.x int, .y [int]]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y [.a ?0]]`, want: `[.x int, .y [.a int]]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y [a? ?0]]`, want: `[.x int, .y [a? int]]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y (?0){}]`, want: `[.x int, .y (int){}]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y (){?0}]`, want: `[.x int, .y (){int}]`},
		{a: `[.x int, .y ?]`, b: `[.x ?0, .y (?0, ?0){?0}]`, want: `[.x int, .y (int, int){int}]`},

		{a: `int`, b: `int`, want: `int`},
		{a: `int`, b: `float32`, want: ``},
		{a: `int`, b: `&int`, want: ``},
		{a: `int`, b: `?`, want: `int`},
		{a: `?`, b: `int`, want: `int`},

		{src: `type t int 	type u string`, a: `t`, b: `t`, want: `t`},
		{src: `type t int 	type u string`, a: `t`, b: `[int]`, want: ``},
		{src: `type t int 	type u string`, a: `t`, b: `u`, want: ``},
		{src: `type t int 	type u string`, a: `t`, b: `?`, want: `t`},
		{src: `type t int 	type u string`, a: `?`, b: `u`, want: `u`},

		{src: `type T t T`, a: `int t`, b: `int t`, want: `int t`},
		{src: `type T t T`, a: `string t`, b: `int t`, want: ``},
		{src: `type T t T`, a: `int t`, b: `? t`, want: `int t`},
		{src: `type T t T`, a: `? t`, b: `int t`, want: `int t`},
		{src: `type T t T`, a: `? t`, b: `? t`, want: `? t`},
		{src: `type T t T 	type U u U`, a: `int t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `? t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `int t`, b: `? u`, want: ``},
		{src: `type T t T 	type U u U`, a: `? t`, b: `? u`, want: ``},

		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(?, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, ?) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(?, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, ?) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(?, ?) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(?, ?) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(?, int) t`, b: `(int, ?) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, ?) t`, b: `(?, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, ?) t`, b: `(int, ?) t`, want: `(int, ?) t`},
		{src: `type (T, U) t int`, a: `(?, int) t`, b: `(?, int) t`, want: `(?, int) t`},
		{src: `type (T, U) t int`, a: `(?, ?) t`, b: `(?, ?) t`, want: `(?0, ?1) t`},
		{src: `type (T, U) t int`, a: `(?, string) t`, b: `(int, ?) t`, want: `(int, string) t`},
		{src: `type (T, U) t int`, a: `(int, string) t`, b: `(?, int) t`, want: ``},
		{src: `type (T, U) t int`, a: `(int, ?) t`, b: `(string, ?) t`, want: ``},
		{src: `type (T, U) t int`, a: `(?0, ?0) t`, b: `(string, ?) t`, want: `(string, string) t`},
		{src: `type (T, U) t int`, a: `(?0, ?0) t`, b: `(string, int) t`, want: ``},

		{a: `T`, b: `T`, want: `T`},
		{a: `T`, b: `int`, want: ``},
		{a: `T`, b: `?`, want: `T`},
		{a: `?`, b: `T`, want: `T`},

		{a: `&int`, b: `&int`, want: `&int`},
		{a: `&int`, b: `int`, want: ``},
		{a: `&int`, b: `&float32`, want: ``},
		{a: `&int`, b: `&?`, want: `&int`},
		{a: `&?`, b: `&int`, want: `&int`},
		{a: `&?`, b: `&?`, want: `&?`},

		{a: `[int]`, b: `[int]`, want: `[int]`},
		{a: `[int]`, b: `int`, want: ``},
		{a: `[int]`, b: `string`, want: ``},
		{a: `[int]`, b: `[?]`, want: `[int]`},
		{a: `[?]`, b: `[int]`, want: `[int]`},
		{a: `[?]`, b: `[?]`, want: `[?]`},

		{a: `[.]`, b: `[.]`, want: `[.]`},
		{a: `[.x int]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int]`, b: `int`, want: ``},
		{a: `[.x int]`, b: `[.x int, .y string]`, want: ``},
		{a: `[.x int]`, b: `[.y int]`, want: ``},
		{a: `[.x int]`, b: `[.x ?]`, want: `[.x int]`},
		{a: `[.x ?]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x ?]`, b: `[.x ?]`, want: `[.x ?]`},
		{a: `[.x ?, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y ?]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x ?, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y ?]`, want: `[.x int, .y string]`},
		{a: `[.x ?, .y string]`, b: `[.x ?, .y string]`, want: `[.x ?, .y string]`},
		{a: `[.x int, .y ?]`, b: `[.x int, .y ?]`, want: `[.x int, .y ?]`},
		{a: `[.x ?, .y ?]`, b: `[.x ?, .y ?]`, want: `[.x ?0, .y ?1]`},
		{a: `[.x ?, .y ?]`, b: `[.x ?]`, want: ``},
		{a: `[.x ?, .y int]`, b: `[.x ?, .y string]`, want: ``},
		{a: `[.x ?, .y int]`, b: `[.x ?, .y T]`, want: ``},
		{a: `[.x ?, .y T]`, b: `[.x ?, .y string]`, want: ``},
		{a: `[.x ?, .y T]`, b: `[.x U, .y T]`, want: `[.x U, .y T]`},

		{a: `[x?]`, b: `[x?]`, want: `[x?]`},
		{a: `[x? float64]`, b: `[x? float64]`, want: `[x? float64]`},
		{a: `[x? int, y?, z? string]`, b: `[x? int, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? float64, y?]`, b: `int`, want: ``},
		{a: `[x? float64, y?]`, b: `[x? float64]`, want: ``},
		{a: `[x? float64, y?]`, b: `[a? float64, y?]`, want: ``},
		{a: `[x? float64, y?]`, b: `[x? float64, y? int]`, want: ``},
		{a: `[x? float64, y? string]`, b: `[x? float64, y? int]`, want: ``},
		{a: `[x? ?, y?, z? string]`, b: `[x? int, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? int, y?, z? string]`, b: `[x? ?, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? ?, y?, z? string]`, b: `[x? int]`, want: ``},
		{a: `[x? ?, y?, z? string]`, b: `[x? int, y?, z? int]`, want: ``},
		{a: `[x? ?, y?, z? string]`, b: `[x? int, y? string, z? string]`, want: ``},
		{a: `[x? ?0, y? ?0]`, b: `[x? int, y? ?]`, want: `[x? int, y? int]`},
		{a: `[x? ?0, y? ?0]`, b: `[x? ?, y? int]`, want: `[x? int, y? int]`},
		{a: `[x? ?0, y? ?0]`, b: `[x? string, y? int]`, want: ``},

		{a: `(){}`, b: `(){}`, want: `(){}`},
		{a: `(int){}`, b: `(int){}`, want: `(int){}`},
		{a: `(){int}`, b: `(){int}`, want: `(){int}`},
		{a: `(int){int}`, b: `(int){int}`, want: `(int){int}`},
		{a: `(int, string){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `int`, want: ``},
		{a: `(int, string){int}`, b: `(int, string, float32){int}`, want: ``},
		{a: `(int, string){int}`, b: `(string, int){int}`, want: ``},

		{a: `(?, string){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, ?){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){?}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(?, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(int, ?){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(int, string){?}`, want: `(int, string){int}`},
		{a: `(?, ?){?}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(?, string){int}`, b: `(int, ?){int}`, want: `(int, string){int}`},
		{a: `(int, ?){int}`, b: `(?, string){int}`, want: `(int, string){int}`},
		{a: `(int, ?){int}`, b: `(?){int}`, want: ``},
		{a: `(int, ?){int}`, b: `(string, int){int}`, want: ``},
		{a: `(int, ?){int}`, b: `(?, int){string}`, want: ``},
		{a: `(?0, ?0){int}`, b: `(int, ?){int}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(int, ?){?}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(?, int){?}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(?, ?){int}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(string, int){int}`, want: ``},
		{a: `(?0, ?0){?0}`, b: `(int, string){int}`, want: ``},
		{a: `(?0, ?0){?0}`, b: `(int, int){string}`, want: ``},
		{a: `(?0, ?0){?1}`, b: `(int, int){string}`, want: `(int, int){string}`},

		{a: `[.x ?0, .y ?0]`, b: `[.x ?, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x ?0, .y ?0]`, want: `[.x ?, .y ?]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x ?0, .y ?1]`, want: `[.x ?, .y ?]`},
		{a: `[.x ?0, .y ?1]`, b: `[.x ?0, .y ?0]`, want: `[.x ?, .y ?]`},
		{a: `[.x ?0, .y ?1]`, b: `[.x ?0, .y ?1]`, want: `[.x ?0, .y ?1]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x int, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x string, .y int]`, want: ``},
		{a: `[.x string, .y int]`, b: `[.x ?0, .y ?0]`, want: ``},
		{a: `[.x int, .y int]`, b: `[.x ?0, .y ?0]`, want: `[.x int, .y int]`},
		{a: `[.x ?0, .y ?0, .z int]`, b: `[.x string, .y ?0, .z ?0]`, want: ``},
		{a: `[.x ?0, .y ?1, .z int]`, b: `[.x string, .y ?0, .z ?0]`, want: `[.x string, .y int, .z int]`},
		{a: `[.x ?0, .y ?0, .z int]`, b: `[.x string, .y ?1, .z ?0]`, want: `[.x string, .y string, .z int]`},
		{
			a:    `[.a ?0,     .b ?0, .c ?1, .d ?1, .e ?2, .f ?2, .g int]`,
			b:    `[.a string, .b ?0, .c ?0, .d ?1, .e ?1, .f ?2, .g ?2]`,
			want: ``,
		},
		{
			a:    `[.a ?0, .b ?0, .c ?1, .d ?1, .e int, .f ?1]`,
			b:    `[.a ?0, .b ?0, .c ?1, .d ?1, .e ?0, .f string]`,
			want: `[.a int, .b int, .c string, .d string, .e int, .f string]`,
		},
		{a: `[.x [.y [.z ?]]]`, b: `[.x [.y [.z int]]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y [.z ?]]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z ?]]]`, b: `[.x [.y [.z ?]]]`, want: `[.x [.y [.z ?]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y ?]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x ?]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z ?]]]`, b: `[.x ?]`, want: `[.x [.y [.z ?]]]`},
		{a: `[.x [.y [.z ?]]]`, b: `?`, want: `[.x [.y [.z ?]]]`},
		{
			a: `[.a ?0, .x [.y [.z ?0]]]`,
			b: `[.a ?1, .x ?1]`,
			// This would be recursive.
			want: ``,
		},
		{
			a: `[.a [.y [.z ?0]], .x ?0]`,
			b: `[.a ?1, .x ?1]`,
			// This would be recursive.
			want: ``,
		},
		{
			a: `[.a ?0, .b [.y [.z ?0]], .c ?]`,
			b: `[.a int, .b ?2,              .c ?2]`,
			// Sub int into ?0, and then into [.x [.z ?0]] which was bound to ?2.
			want: `[.a int, .b [.y [.z int]], .c [.y [.z int]]]`,
		},
		{
			a: `[.a ?0, .b [.y [.z ?1]], .c ?1]`,
			b: `[.a ?2, .b ?2,              .c int]`,
			// Merge ?0 and ?2, then sub [.y. [.z ?1]] for ?2 (and ?0),
			// and then later sub int for ?1.
			want: `[.a [.y [.z int]], .b [.y [.z int]], .c int]`,
		},
		{
			a:    `[.a ?0,    .b [.x ?1], .c ?1, .d ?2]`,
			b:    `[.a ?3,    .b ?3,       .c int, .d ?3]`,
			want: `[.a [.x int], .b [.x int], .c int, .d [.x int]]`,
		},

		// Multi-step recursive substitution.
		{
			// ?0 = [.x ?10]
			// ?10 = [.x ?1]
			// ?1 = [.x ?11]
			// ?11 = [.x ?0]
			a:    `[.a ?0,		.b [.x ?1],	.c ?1,		.d [.x ?0]]`,
			b:    `[.a [.x ?10],	.b ?10,	.c [.x ?11],	.d ?11]`,
			want: ``,
		},
	}
	for _, test := range tests {
		t.Run("unify("+test.a+", "+test.b+")", func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			a := parseTestPattern(t, mod, test.a)
			b := parseTestPattern(t, mod, test.b)

			// Given unique names.
			var i int
			a.Parms.ForEach(func(p *TypeParm) {
				p.Name = fmt.Sprintf("A%d", i)
				i++
			})
			i = 0
			b.Parms.ForEach(func(p *TypeParm) {
				p.Name = fmt.Sprintf("B%d", i)
				i++
			})

			var bind map[*TypeParm]Type
			switch u, err := unify(a, b, &bind); {
			case test.want == "" && u != nil:
				t.Fatalf("unify(%s, %s)=%s, want nil", test.a, test.b, u)
			case test.want == "":
				return // OK
			case test.want != "" && u == nil:
				var n string
				if err != nil {
					p := makeErrorPrinter(mod.topScope)
					err.print(p)
					n = ", " + p.String() + ""
				}
				t.Fatalf("unify(%s, %s)=nil%s, want %s", test.a, test.b, n, test.want)
			case test.want != "" && u.String() != test.want:
				t.Fatalf("unify(%s, %s)=%s, want %s", test.a, test.b, u, test.want)
			default:
				if aPrime := subType(bind, a.Type); !eqType(aPrime, u.Type) {
					t.Errorf("sub(bind, %s)=%s, want %s", a.Type, aPrime, u.Type)
				}
				if bPrime := subType(bind, b.Type); !eqType(bPrime, u.Type) {
					t.Errorf("sub(bind, %s)=%s, want %s", b.Type, bPrime, u.Type)
				}
			}
		})
	}
}

func TestPatternIntersectionOfTwoVariables(t *testing.T) {
	mod, errs := check("test", []string{""}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	a := parseTestPattern(t, mod, "?")
	b := parseTestPattern(t, mod, "?")
	var bind map[*TypeParm]Type
	u, _ := unify(a, b, &bind)
	if u == nil {
		t.Fatalf("unify(%s, %s)=nil, wanted ?", a, b)
	}
	if u.isGroundType() {
		t.Fatalf("unify(%s, %s).isGroundType()=true, want false", a, b)
	}
}

func TestPatternIntersectionIgnoreBoundTypeParameters(t *testing.T) {
	const src = `
		type T list [node? [.data T, .next T list], nil?]
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	pat0 := parseTestPattern(t, mod, "? list")
	if pat0.Parms.Len() != 1 {
		t.Fatalf("pat0.Len()=%d, want 1", pat0.Parms.Len())
	}
	var parm0 *TypeParm
	pat0.Parms.ForEach(func(p *TypeParm){ parm0 = p })

	pat0.Parms = pat0.Parms.Union(NewTypeParmSet(
		&TypeParm{Name: "U0"},
		&TypeParm{Name: "U1"},
		&TypeParm{Name: "U2"},
	))

	pat1 := parseTestPattern(t, mod, "int list")

	var bind map[*TypeParm]Type
	u, note := unify(pat0, pat1, &bind)
	if u == nil {
		t.Fatalf("unify(%s, %s) failed %s", pat0, pat1, note)
	}
	if bind == nil {
		t.Fatalf("unify(%s, %s) got bindings nil, expected a binding to %s",
			pat0, pat1, parm0.Name)
	}
	if _, ok := bind[parm0]; !ok || len(bind) != 1 {
		t.Fatalf("unify(%s, %s) got bindings %v, expected a binding to %s",
			pat0, pat1, bindAsSlice(bind), parm0.Name)
	}
}

func TestPatternSelfIntersection(t *testing.T) {
	const src = `
		type T list [node? [.data T, .next T list], nil?]
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	pat := parseTestPattern(t, mod, "? list")

	var bind map[*TypeParm]Type
	u, note := unify(pat, pattern(pat.Type), &bind)
	if u == nil {
		t.Fatalf("unify(%s, %s) failed %s", pat, pattern(pat.Type), note)
	}
	if !eqType(u.Type, pat.Type) || u.Parms.Len() != 0 {
		t.Fatalf("unify(%s, %s)=%s, wanted %s\n", pat, pattern(pat.Type), u, pat.Type)
	}

	bind = nil
	u, note = unify(pattern(pat.Type), pat, &bind)
	if u == nil {
		t.Fatalf("unify(%s, %s) failed %s", pattern(pat.Type), pat, note)
	}
	if !eqType(u.Type, pat.Type) || u.Parms.Len() != 0 {
		t.Fatalf("unify(%s, %s)=%s, wanted %s\n", pattern(pat.Type), pat, u, pat.Type)
	}
}

// These are the old unify() tests.
// unify() is gone, but the same tests should work with convertType(),
// so we have repurposed them.
func TestPatternUnify(t *testing.T) {
	tests := []struct {
		src  string
		typ  string
		pat  string
		want string
	}{
		{typ: "int", pat: "string", want: ""},
		{typ: "int", pat: "int", want: "int"},
		{typ: "int", pat: "T", want: ""},
		{typ: "int", pat: "?", want: "int"},
		{typ: "int", pat: "&string", want: ""},
		{typ: "int", pat: "&int", want: "&int"},
		{typ: "int", pat: "[int]", want: ""},
		{typ: "int", pat: "[.x int, .y int]", want: ""},
		{typ: "int", pat: "[x? int, y?]", want: ""},
		{typ: "int", pat: "(int){float32}", want: ""},

		{src: "type t [.x int]", typ: "t", pat: "string", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "t", want: "t"},
		{src: "type t [.x int]", typ: "t", pat: "[.x int]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[.x ?]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "T", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "?", want: "t"},
		{src: "type t [.x int]", typ: "t", pat: "&string", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "&t", want: "&t"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x int]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x ?]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[int]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[.x int, .y int]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[x? int, y?]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "(int){float32}", want: ""},

		{typ: "T", pat: "string", want: ""},
		{typ: "T", pat: "int", want: ""},
		{typ: "T", pat: "T", want: "T"},
		{typ: "T", pat: "?", want: "T"},
		{typ: "T", pat: "&string", want: ""},
		{typ: "T", pat: "&T", want: "&T"},
		{typ: "T", pat: "[int]", want: ""},
		{typ: "T", pat: "[.x int, .y int]", want: ""},
		{typ: "T", pat: "[x? int, y?]", want: ""},
		{typ: "T", pat: "(int){float32}", want: ""},

		{typ: "&int", pat: "string", want: ""},
		{typ: "&int", pat: "int", want: "int"},
		{typ: "&int", pat: "T", want: ""},
		{typ: "&int", pat: "?", want: "&int"},
		{typ: "&int", pat: "&int", want: "&int"},
		{typ: "&int", pat: "[int]", want: ""},
		{typ: "&int", pat: "[.x int, .y int]", want: ""},
		{typ: "&int", pat: "[x? int, y?]", want: ""},
		{typ: "&int", pat: "(int){float32}", want: ""},

		{typ: "[int]", pat: "string", want: ""},
		{typ: "[int]", pat: "int", want: ""},
		{typ: "[int]", pat: "T", want: ""},
		{typ: "[int]", pat: "?", want: "[int]"},
		{typ: "[int]", pat: "&int", want: ""},
		{typ: "[int]", pat: "&[int]", want: "&[int]"},
		{typ: "[int]", pat: "[int]", want: "[int]"},
		{typ: "[int]", pat: "[.x int, .y int]", want: ""},
		{typ: "[int]", pat: "[x? int, y?]", want: ""},
		{typ: "[int]", pat: "(int){float32}", want: ""},
		{src: "type t [int]", typ: "t", pat: "[int]", want: "[int]"},
		{src: "type t [int]", typ: "t", pat: "&[int]", want: "&[int]"},
		{src: "type t [int]", typ: "t", pat: "[?]", want: "[int]"},
		{src: "type t [int]", typ: "t", pat: "&[?]", want: "&[int]"},

		{typ: "[.x int, .y int]", pat: "string", want: ""},
		{typ: "[.x int, .y int]", pat: "int", want: ""},
		{typ: "[.x int, .y int]", pat: "T", want: ""},
		{typ: "[.x int, .y int]", pat: "?", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "&int", want: ""},
		{typ: "[.x int, .y int]", pat: "&[.x int, .y int]", want: "&[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int, .z int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.y int, .x int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int, .y int]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x ?, .y int]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x int, .y ?]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x ?, .y ?]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x ?, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y ?]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x ?, .y ?]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y int]", pat: "[x? int, y?]", want: ""},
		{typ: "[.x int, .y int]", pat: "(int){float32}", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[.x int]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x int]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[.x ?]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x ?]", want: "&[.x int]"},

		{typ: "[x? int, y?, z? string]", pat: "string", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "int", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "T", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "?", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "&int", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "&[x? int, y?, z? string]", want: "&[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[int]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[.x int, .y int]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? string, y?, z? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? int]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y? float32, z? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z?]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x?, y?, z? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[z? int, y?, x? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? ?, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? ?]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? ?, y?, z? ?]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "(int){float32}", want: ""},
		{src: "type t [x? int]", typ: "t", pat: "[x? int]", want: "[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "&[x? int]", want: "&[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "[x? ?]", want: "[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "&[x? ?]", want: "&[x? int]"},

		{typ: "(int){float32}", pat: "string", want: ""},
		{typ: "(int){float32}", pat: "int", want: ""},
		{typ: "(int){float32}", pat: "T", want: ""},
		{typ: "(int){float32}", pat: "?", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "&int", want: ""},
		{typ: "(int){float32}", pat: "&(int){float32}", want: "&(int){float32}"},
		{typ: "(int){float32}", pat: "[int]", want: ""},
		{typ: "(int){float32}", pat: "[.x int, .y int]", want: ""},
		{typ: "(int){float32}", pat: "[x? int, y?]", want: ""},
		{typ: "(int){float32}", pat: "(int){float32}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(int){}", want: "(int){}"},
		{typ: "(int){float32}", pat: "(){float32}", want: ""},
		{typ: "(int){float32}", pat: "(int){float64}", want: ""},
		{typ: "(int){float32}", pat: "(int32){float32}", want: ""},
		{typ: "(int){float32}", pat: "(int, string){float32}", want: ""},
		{typ: "(int){float32}", pat: "(?){float32}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(int){?}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(?){?}", want: "(int){float32}"},
		{typ: "(int, string){float32}", pat: "(?, ?){?}", want: "(int, string){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(?){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){?}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(?){?}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(?){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){?}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(?){?}", want: "&(int){float32}"},

		{
			src: `
				type a b
				type b c
				type c d
				type d [.x int]
			`,
			typ:  "a",
			pat:  "[.x ?]",
			want: "[.x int]",
		},
		{
			src: `
				type a b
				type b c
				type c d
				type d [.x int]
			`,
			typ:  "[.x int]",
			pat:  "a",
			want: "a",
		},

		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t string, .u int]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t string, .u ?]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t ?, .u int]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t ?, .u ?]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a", // args are reversed, so binding fails
			pat:  "[.t int, .u string]",
			want: "",
		},

		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t string, .u int]",
			want: "[.t string, .u int]",
		},

		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(int, string) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(?, string) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(int, ?) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(?, ?) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(string, int) a", // reversed parameters, so binding fails
			want: "",
		},
	}
	for _, test := range tests {
		t.Run(fmt.Sprintf("convertType(%s, %s)", test.typ, test.pat), func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			pat := parseTestPattern(t, mod, test.pat)
			typ := parseTestType(t, mod, test.typ)
			bind, err := convertType(pattern(typ), pat, implicit)
			if test.want == "" {
				if err == nil {
					t.Errorf("got %s %v, want error", subType(bind, typ), bindAsSlice(bind))
				}
			} else {
				if err != nil {
					p := makeErrorPrinter(mod.topScope)
					err.print(p)
					t.Errorf("got error %s, want %s", p.String(), test.want)
				} else if sub := subType(bind, pat.Type); sub.String() != test.want {
					t.Errorf("got %s %v, want %s", sub, bindAsSlice(bind), test.want)
				}
			}
		})
	}
}

func bindAsSlice(bind map[*TypeParm]Type) [][2]string {
	a := make([][2]string, 0, len(bind))
	for k, v := range bind {
		a = append(a, [2]string{k.Name, v.String()})
	}
	sort.Slice(a, func(i, j int) bool { return a[i][0] < a[j][0] })
	return a
}

type typeParmScope struct {
	mod   *Mod
	parms []*TypeParm
}

func (s *typeParmScope) up() scope { return s.mod }

func (s *typeParmScope) findType(args []Type, name string, l loc.Loc) []Type {
	if len(args) != 0 {
		return nil
	}
	for _, parm := range s.parms {
		if parm.Name == name {
			return []Type{&TypeVar{SourceName: name, Def: parm, L: l}}
		}
	}
	return nil
}

func parseTestPattern(t *testing.T, m *Mod, src string) TypePattern {
	pat, _ := _parseTestPattern(t, m, 0, src)
	return pat
}

func _parseTestPattern(t *testing.T, m *Mod, nextName int, src string) (TypePattern, int) {
	return __parseTestPattern(t, m, nextName, make(map[string]*TypeParm), src)
}

// TODO(eaburns): Fix __parseTestPattern mess.
func __parseTestPattern(t *testing.T, m *Mod, nextName int, parmSet map[string]*TypeParm, src string) (TypePattern, int) {
	t.Helper()
	var parms []*TypeParm
	src2 := ""
	inParms := make(map[*TypeParm]bool)
	var prev rune
	for len(src) > 0 {
		r, w := utf8.DecodeRuneInString(src)
		src = src[w:]
		if r != '?' || unicode.IsLetter(prev) || unicode.IsNumber(prev) || prev == '?' {
			src2 += string([]rune{r})
			prev = r
			continue
		}
		var name string
		for {
			r, w := utf8.DecodeRuneInString(src)
			if r < '0' || '9' < r {
				break
			}
			name += src[:w]
			src = src[w:]
		}
		if name == "" {
			name += "uniq" + strconv.Itoa(len(parmSet))
		}
		p, ok := parmSet[name]
		if !ok {
			p = &TypeParm{Name: fmt.Sprintf("Z%d", nextName)}
			nextName++
			parmSet[name] = p
		}
		if !inParms[p] {
			parms = append(parms, p)
			inParms[p] = true
		}
		src2 += parmSet[name].Name
		prev = r
	}
	p, err := parser.ParseType(src2)
	if err != nil {
		t.Fatalf("failed to parse type %s: %s", src2, err)
	}
	typ, errs := _makeType(&typeParmScope{mod: m, parms: parms}, p, true, true)
	if len(errs) > 0 {
		t.Fatalf("failed to make type: %s", errs[0])
	}
	return TypePattern{Parms: NewTypeParmSet(parms...), Type: typ}, nextName
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
