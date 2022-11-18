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
			want: "_",
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
			want: "_",
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
			want: "_",
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
			want: "(_, int) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(int, string) pair",
			},
			want: "(int, _) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(string, string) pair",
			},
			want: "(_, _) pair",
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
			want: "_",
		},
		{
			pats: []string{
				"&int",
				"int",
			},
			want: "_",
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
			want: "&_",
		},
		{
			pats: []string{
				"[int]",
				"int",
			},
			want: "_",
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
			want: "[_]",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.a int, .b string]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.y string, .x int]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.x int]",
			},
			want: "_",
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
			want: "[.x _, .y int]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int, .y int32]",
			},
			want: "[.x int, .y _]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int32, .y int32]",
			},
			want: "[.x _, .y _]",
		},
		{
			pats: []string{
				"[.x int, .y float32, .z string]",
				"[.x int, .y [false?, true?], .z string]",
				"[.x [false?, true?], .y float32, .z string]",
			},
			want: "[.x _, .y _, .z string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"int",
			},
			want: "_",
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
			want: "_",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? [false?, true?], y?, z? string]",
			},
			want: "[x? _, y?, z? string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?, z? [false?, true?]]",
			},
			want: "[x? int, y?, z? _]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? [false?, true?], y?, z? [false?, true?]]",
			},
			want: "[x? _, y?, z? _]",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int){float64}",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string, float32){float64}",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){}",
			},
			want: "(int, string){_}",
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
			want: "(_, string){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, [false?, true?]){float64}",
			},
			want: "(int, _){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){[false?, true?]}",
			},
			want: "(int, string){_}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int8, float32){[false?, true?]}",
			},
			want: "(_, _){_}",
		},
		{
			pats: []string{
				"T",
				"int",
			},
			want: "_",
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
				"_",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"_",
				"_",
			},
			want: "_",
		},
		{
			pats: []string{
				"_1",
				"_2",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){_}",
			},
			want: "(int, string){_}",
		},
	}
	for _, test := range tests {
		t.Run(strings.Join(test.pats, ","), func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			pats := make([]typePattern, len(test.pats))
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
		// Sub _ with all the different kinds of types.
		{a: `_`, b: `_`, want: `_`},
		{a: `_`, b: `int`, want: `int`},
		{src: `type t int`, a: `_`, b: `t`, want: `t`},
		{src: `type (T, U) t int`, a: `_`, b: `(int, string) t`, want: `(int, string) t`},
		{a: `_`, b: `T`, want: `T`},
		{a: `_`, b: `&int`, want: `&int`},
		{a: `_`, b: `[int]`, want: `[int]`},
		{a: `_`, b: `[.x int]`, want: `[.x int]`},
		{a: `_`, b: `[x?]`, want: `[x?]`},
		{a: `_`, b: `[x? int]`, want: `[x? int]`},
		{a: `_`, b: `(int, string){float32}`, want: `(int, string){float32}`},

		// Sub _ with all the different kinds of types, leading to recursive sub error.
		{src: `type T t int`, a: `[.x _0, .y _0]`, b: `[.x _1 t, .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x &_1, .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x [_1], .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x [.y _1], .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x [y? _1], .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x (_1){}, .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x (){_1}, .y _1]`, want: ``},

		// Sub into all different spots.
		{a: `[.x int, .y _]`, b: `[.x _0, .y _0]`, want: `[.x int, .y int]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y &_0]`, want: `[.x int, .y &int]`},
		{src: `type T t int`, a: `[.x int, .y _]`, b: `[.x _0, .y _0 t]`, want: `[.x int, .y int t]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y [_0]]`, want: `[.x int, .y [int]]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y [.a _0]]`, want: `[.x int, .y [.a int]]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y [a? _0]]`, want: `[.x int, .y [a? int]]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y (_0){}]`, want: `[.x int, .y (int){}]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y (){_0}]`, want: `[.x int, .y (){int}]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y (_0, _0){_0}]`, want: `[.x int, .y (int, int){int}]`},

		{a: `int`, b: `int`, want: `int`},
		{a: `int`, b: `float32`, want: ``},
		{a: `int`, b: `&int`, want: ``},
		{a: `int`, b: `_`, want: `int`},
		{a: `_`, b: `int`, want: `int`},

		{src: `type t int 	type u string`, a: `t`, b: `t`, want: `t`},
		{src: `type t int 	type u string`, a: `t`, b: `[int]`, want: ``},
		{src: `type t int 	type u string`, a: `t`, b: `u`, want: ``},
		{src: `type t int 	type u string`, a: `t`, b: `_`, want: `t`},
		{src: `type t int 	type u string`, a: `_`, b: `u`, want: `u`},

		{src: `type T t T`, a: `int t`, b: `int t`, want: `int t`},
		{src: `type T t T`, a: `string t`, b: `int t`, want: ``},
		{src: `type T t T`, a: `int t`, b: `_ t`, want: `int t`},
		{src: `type T t T`, a: `_ t`, b: `int t`, want: `int t`},
		{src: `type T t T`, a: `_ t`, b: `_ t`, want: `_ t`},
		{src: `type T t T 	type U u U`, a: `int t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `_ t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `int t`, b: `_ u`, want: ``},
		{src: `type T t T 	type U u U`, a: `_ t`, b: `_ u`, want: ``},

		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(_, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(_, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, _) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(_, _) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(_, _) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(_, int) t`, b: `(int, _) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(_, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(int, _) t`, want: `(int, _) t`},
		{src: `type (T, U) t int`, a: `(_, int) t`, b: `(_, int) t`, want: `(_, int) t`},
		{src: `type (T, U) t int`, a: `(_, _) t`, b: `(_, _) t`, want: `(_, _) t`},
		{src: `type (T, U) t int`, a: `(_, string) t`, b: `(int, _) t`, want: `(int, string) t`},
		{src: `type (T, U) t int`, a: `(int, string) t`, b: `(_, int) t`, want: ``},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(string, _) t`, want: ``},
		{src: `type (T, U) t int`, a: `(_0, _0) t`, b: `(string, _) t`, want: `(string, string) t`},
		{src: `type (T, U) t int`, a: `(_0, _0) t`, b: `(string, int) t`, want: ``},

		{a: `T`, b: `T`, want: `T`},
		{a: `T`, b: `U`, want: `T`},
		{a: `T`, b: `int`, want: ``},
		{a: `T`, b: `_`, want: `T`},
		{a: `_`, b: `T`, want: `T`},

		{a: `&int`, b: `&int`, want: `&int`},
		{a: `&int`, b: `int`, want: ``},
		{a: `&int`, b: `&float32`, want: ``},
		{a: `&int`, b: `&_`, want: `&int`},
		{a: `&_`, b: `&int`, want: `&int`},
		{a: `&_`, b: `&_`, want: `&_`},

		{a: `[int]`, b: `[int]`, want: `[int]`},
		{a: `[int]`, b: `int`, want: ``},
		{a: `[int]`, b: `string`, want: ``},
		{a: `[int]`, b: `[_]`, want: `[int]`},
		{a: `[_]`, b: `[int]`, want: `[int]`},
		{a: `[_]`, b: `[_]`, want: `[_]`},

		{a: `[.]`, b: `[.]`, want: `[.]`},
		{a: `[.x int]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int]`, b: `int`, want: ``},
		{a: `[.x int]`, b: `[.x int, .y string]`, want: ``},
		{a: `[.x int]`, b: `[.y int]`, want: ``},
		{a: `[.x int]`, b: `[.x _]`, want: `[.x int]`},
		{a: `[.x _]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x _]`, b: `[.x _]`, want: `[.x _]`},
		{a: `[.x _, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y _]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x _, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y _]`, want: `[.x int, .y string]`},
		{a: `[.x _, .y string]`, b: `[.x _, .y string]`, want: `[.x _, .y string]`},
		{a: `[.x int, .y _]`, b: `[.x int, .y _]`, want: `[.x int, .y _]`},
		{a: `[.x _, .y _]`, b: `[.x _, .y _]`, want: `[.x _, .y _]`},
		{a: `[.x _, .y _]`, b: `[.x _]`, want: ``},
		{a: `[.x _, .y int]`, b: `[.x _, .y string]`, want: ``},
		{a: `[.x _, .y int]`, b: `[.x _, .y T]`, want: ``},
		{a: `[.x _, .y T]`, b: `[.x _, .y string]`, want: ``},
		{a: `[.x _, .y T]`, b: `[.x U, .y T]`, want: `[.x U, .y T]`},

		{a: `[x?]`, b: `[x?]`, want: `[x?]`},
		{a: `[x? float64]`, b: `[x? float64]`, want: `[x? float64]`},
		{a: `[x? int, y?, z? string]`, b: `[x? int, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? float64, y?]`, b: `int`, want: ``},
		{a: `[x? float64, y?]`, b: `[x? float64]`, want: ``},
		{a: `[x? float64, y?]`, b: `[a? float64, y?]`, want: ``},
		{a: `[x? float64, y?]`, b: `[x? float64, y? int]`, want: ``},
		{a: `[x? float64, y? string]`, b: `[x? float64, y? int]`, want: ``},
		{a: `[x? _, y?, z? string]`, b: `[x? int, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? int, y?, z? string]`, b: `[x? _, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? _, y?, z? string]`, b: `[x? int]`, want: ``},
		{a: `[x? _, y?, z? string]`, b: `[x? int, y?, z? int]`, want: ``},
		{a: `[x? _, y?, z? string]`, b: `[x? int, y? string, z? string]`, want: ``},
		{a: `[x? _0, y? _0]`, b: `[x? int, y? _]`, want: `[x? int, y? int]`},
		{a: `[x? _0, y? _0]`, b: `[x? _, y? int]`, want: `[x? int, y? int]`},
		{a: `[x? _0, y? _0]`, b: `[x? string, y? int]`, want: ``},

		{a: `(){}`, b: `(){}`, want: `(){}`},
		{a: `(int){}`, b: `(int){}`, want: `(int){}`},
		{a: `(){int}`, b: `(){int}`, want: `(){int}`},
		{a: `(int){int}`, b: `(int){int}`, want: `(int){int}`},
		{a: `(int, string){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `int`, want: ``},
		{a: `(int, string){int}`, b: `(int, string, float32){int}`, want: ``},
		{a: `(int, string){int}`, b: `(string, int){int}`, want: ``},

		{a: `(_, string){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, _){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){_}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(_, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(int, _){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(int, string){_}`, want: `(int, string){int}`},
		{a: `(_, _){_}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(_, string){int}`, b: `(int, _){int}`, want: `(int, string){int}`},
		{a: `(int, _){int}`, b: `(_, string){int}`, want: `(int, string){int}`},
		{a: `(int, _){int}`, b: `(_){int}`, want: ``},
		{a: `(int, _){int}`, b: `(string, int){int}`, want: ``},
		{a: `(int, _){int}`, b: `(_, int){string}`, want: ``},
		{a: `(_0, _0){int}`, b: `(int, _){int}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(int, _){_}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(_, int){_}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(_, _){int}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(string, int){int}`, want: ``},
		{a: `(_0, _0){_0}`, b: `(int, string){int}`, want: ``},
		{a: `(_0, _0){_0}`, b: `(int, int){string}`, want: ``},
		{a: `(_0, _0){_1}`, b: `(int, int){string}`, want: `(int, int){string}`},

		{a: `[.x _0, .y _0]`, b: `[.x _, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x _0, .y _0]`, b: `[.x _0, .y _0]`, want: `[.x _0, .y _0]`},
		{a: `[.x _0, .y _0]`, b: `[.x _0, .y _1]`, want: `[.x _0, .y _0]`},
		{a: `[.x _0, .y _1]`, b: `[.x _0, .y _0]`, want: `[.x _0, .y _0]`},
		{a: `[.x _0, .y _1]`, b: `[.x _0, .y _1]`, want: `[.x _, .y _]`},
		{a: `[.x _0, .y _0]`, b: `[.x int, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x _0, .y _0]`, b: `[.x string, .y int]`, want: ``},
		{a: `[.x string, .y int]`, b: `[.x _0, .y _0]`, want: ``},
		{a: `[.x int, .y int]`, b: `[.x _0, .y _0]`, want: `[.x int, .y int]`},
		{a: `[.x _0, .y _0, .z int]`, b: `[.x string, .y _0, .z _0]`, want: ``},
		{a: `[.x _0, .y _1, .z int]`, b: `[.x string, .y _0, .z _0]`, want: `[.x string, .y int, .z int]`},
		{a: `[.x _0, .y _0, .z int]`, b: `[.x string, .y _1, .z _0]`, want: `[.x string, .y string, .z int]`},
		{
			a:    `[.a _0,     .b _0, .c _1, .d _1, .e _2, .f _2, .g int]`,
			b:    `[.a string, .b _0, .c _0, .d _1, .e _1, .f _2, .g _2]`,
			want: ``,
		},
		{
			a:    `[.a _0, .b _0, .c _1, .d _1, .e int, .f _1]`,
			b:    `[.a _0, .b _0, .c _1, .d _1, .e _0, .f string]`,
			want: `[.a int, .b int, .c string, .d string, .e int, .f string]`,
		},
		{a: `[.x [.y [.z _]]]`, b: `[.x [.y [.z int]]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y [.z _]]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z _]]]`, b: `[.x [.y [.z _]]]`, want: `[.x [.y [.z _]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y _]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x _]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z _]]]`, b: `[.x _]`, want: `[.x [.y [.z _]]]`},
		{a: `[.x [.y [.z _]]]`, b: `_`, want: `[.x [.y [.z _]]]`},
		{
			a: `[.a _0, .x [.y [.z _0]]]`,
			b: `[.a _1, .x _1]`,
			// This would be recursive.
			want: ``,
		},
		{
			a: `[.a [.y [.z _0]], .x _0]`,
			b: `[.a _1, .x _1]`,
			// This would be recursive.
			want: ``,
		},
		{
			a: `[.a _0, .b [.y [.z _0]], .c _]`,
			b: `[.a int, .b _2,              .c _2]`,
			// Sub int into _0, and then into [.x [.z _0]] which was bound to _2.
			want: `[.a int, .b [.y [.z int]], .c [.y [.z int]]]`,
		},
		{
			a: `[.a _0, .b [.y [.z _1]], .c _1]`,
			b: `[.a _2, .b _2,              .c int]`,
			// Merge _0 and _2, then sub [.y. [.z _1]] for _2 (and _0),
			// and then later sub int for _1.
			want: `[.a [.y [.z int]], .b [.y [.z int]], .c int]`,
		},
		{
			a:    `[.a _0,    .b [.x _1], .c _1, .d _2]`,
			b:    `[.a _3,    .b _3,       .c int, .d _3]`,
			want: `[.a [.x int], .b [.x int], .c int, .d [.x int]]`,
		},

		// Multi-step recursive substitution.
		{
			// _0 = [.x _10]
			// _10 = [.x _1]
			// _1 = [.x _11]
			// _11 = [.x _0]
			a:    `[.a _0,		.b [.x _1],	.c _1,		.d [.x _0]]`,
			b:    `[.a [.x _10],	.b _10,	.c [.x _11],	.d _11]`,
			want: ``,
		},
	}
	for _, test := range tests {
		t.Run("intersect("+test.a+", "+test.b+")", func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			a := parseTestPattern(t, mod, test.a)
			b := parseTestPattern(t, mod, test.b)

			// Given unique names.
			for i := range a.parms {
				a.parms[i].Name = fmt.Sprintf("A%d", i)
			}
			copyTypeParmNamesToVars(a.typ)
			for i := range b.parms {
				b.parms[i].Name = fmt.Sprintf("B%d", i)
			}
			copyTypeParmNamesToVars(b.typ)

			var bind map[*TypeParm]Type
			switch isect, note := intersection(a, b, &bind); {
			case test.want == "" && isect != nil:
				t.Fatalf("intersect(%s, %s)=%s, want nil", test.a, test.b, isect)
			case test.want == "":
				return // OK
			case test.want != "" && isect == nil:
				var n string
				if note != nil {
					n = " (" + note.(*_error).msg + ")"
				}
				t.Fatalf("intersect(%s, %s)=nil%s, want %s", test.a, test.b, n, test.want)
			case test.want != "" && isect.String() != test.want:
				t.Fatalf("intersect(%s, %s)=%s, want %s", test.a, test.b, isect, test.want)
			default:
				if aPrime := subType(bind, a.typ); !eqType(aPrime, isect.typ) {
					t.Errorf("sub(bind, %s)=%s, want %s", a.typ, aPrime, isect.typ)
				}
				if bPrime := subType(bind, b.typ); !eqType(bPrime, isect.typ) {
					t.Errorf("sub(bind, %s)=%s, want %s", b.typ, bPrime, isect.typ)
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
	a := parseTestPattern(t, mod, "_")
	b := parseTestPattern(t, mod, "_")
	var bind map[*TypeParm]Type
	isect, _ := intersection(a, b, &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s)=nil, wanted _", a, b)
	}
	if isect.isGroundType() {
		t.Fatalf("intersection(%s, %s).isGroundType()=true, want false", a, b)
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
	pat0 := parseTestPattern(t, mod, "_ list")
	pat0.parms = append(pat0.parms,
		&TypeParm{Name: "U0"},
		&TypeParm{Name: "U1"},
		&TypeParm{Name: "U2"})

	pat1 := parseTestPattern(t, mod, "int list")

	var bind map[*TypeParm]Type
	isect, note := intersection(pat0, pat1, &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s) failed %s", pat0, pat1, note)
	}
	if bind == nil {
		t.Fatalf("intersection(%s, %s) got bindings nil, expected a binding to %s",
			pat0, pat1, pat0.parms[0].Name)
	}
	if _, ok := bind[pat0.parms[0]]; !ok || len(bind) != 1 {
		t.Fatalf("intersection(%s, %s) got bindings %v, expected a binding to %s",
			pat0, pat1, bindAsSlice(bind), pat0.parms[0].Name)
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
	pat := parseTestPattern(t, mod, "_ list")

	var bind map[*TypeParm]Type
	isect, note := intersection(pat, pattern(pat.typ), &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s) failed %s", pat, pattern(pat.typ), note)
	}
	if !eqType(isect.typ, pat.typ) || len(isect.parms) != 0 {
		t.Fatalf("intersection(%s, %s)=%s, wanted %s\n", pat, pattern(pat.typ), isect, pat.typ)
	}

	bind = nil
	isect, note = intersection(pattern(pat.typ), pat, &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s) failed %s", pattern(pat.typ), pat, note)
	}
	if !eqType(isect.typ, pat.typ) || len(isect.parms) != 0 {
		t.Fatalf("intersection(%s, %s)=%s, wanted %s\n", pattern(pat.typ), pat, isect, pat.typ)
	}
}

func copyTypeParmNamesToVars(t Type) {
	switch t := t.(type) {
	case *DefType:
		for i := range t.Args {
			copyTypeParmNamesToVars(t.Args[i])
		}
	case *RefType:
		copyTypeParmNamesToVars(t.Type)
	case *ArrayType:
		copyTypeParmNamesToVars(t.ElemType)
	case *StructType:
		for i := range t.Fields {
			copyTypeParmNamesToVars(t.Fields[i].Type)
		}
	case *UnionType:
		for i := range t.Cases {
			copyTypeParmNamesToVars(t.Cases[i].Type)
		}
	case *FuncType:
		for i := range t.Parms {
			copyTypeParmNamesToVars(t.Parms[i])
		}
		copyTypeParmNamesToVars(t.Ret)
	case *TypeVar:
		if t.Def != nil {
			t.Name = t.Def.Name
		}
	case *BasicType:
	case nil:
	default:
		panic(fmt.Sprintf("bad type type: %T", t))
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
		{typ: "int", pat: "_", want: "int"},
		{typ: "int", pat: "&string", want: ""},
		{typ: "int", pat: "&int", want: "&int"},
		{typ: "int", pat: "[int]", want: ""},
		{typ: "int", pat: "[.x int, .y int]", want: ""},
		{typ: "int", pat: "[x? int, y?]", want: ""},
		{typ: "int", pat: "(int){float32}", want: ""},

		{src: "type t [.x int]", typ: "t", pat: "string", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "t", want: "t"},
		{src: "type t [.x int]", typ: "t", pat: "[.x int]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[.x _]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "T", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "_", want: "t"},
		{src: "type t [.x int]", typ: "t", pat: "&string", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "&t", want: "&t"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x int]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x _]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[int]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[.x int, .y int]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[x? int, y?]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "(int){float32}", want: ""},

		{typ: "T", pat: "string", want: ""},
		{typ: "T", pat: "int", want: ""},
		{typ: "T", pat: "T", want: "T"},
		{typ: "T", pat: "_", want: "T"},
		{typ: "T", pat: "&string", want: ""},
		{typ: "T", pat: "&T", want: "&T"},
		{typ: "T", pat: "[int]", want: ""},
		{typ: "T", pat: "[.x int, .y int]", want: ""},
		{typ: "T", pat: "[x? int, y?]", want: ""},
		{typ: "T", pat: "(int){float32}", want: ""},

		{typ: "&int", pat: "string", want: ""},
		{typ: "&int", pat: "int", want: "int"},
		{typ: "&int", pat: "T", want: ""},
		{typ: "&int", pat: "_", want: "&int"},
		{typ: "&int", pat: "&int", want: "&int"},
		{typ: "&int", pat: "[int]", want: ""},
		{typ: "&int", pat: "[.x int, .y int]", want: ""},
		{typ: "&int", pat: "[x? int, y?]", want: ""},
		{typ: "&int", pat: "(int){float32}", want: ""},

		{typ: "[int]", pat: "string", want: ""},
		{typ: "[int]", pat: "int", want: ""},
		{typ: "[int]", pat: "T", want: ""},
		{typ: "[int]", pat: "_", want: "[int]"},
		{typ: "[int]", pat: "&int", want: ""},
		{typ: "[int]", pat: "&[int]", want: "&[int]"},
		{typ: "[int]", pat: "[int]", want: "[int]"},
		{typ: "[int]", pat: "[.x int, .y int]", want: ""},
		{typ: "[int]", pat: "[x? int, y?]", want: ""},
		{typ: "[int]", pat: "(int){float32}", want: ""},
		{src: "type t [int]", typ: "t", pat: "[int]", want: "[int]"},
		{src: "type t [int]", typ: "t", pat: "&[int]", want: "&[int]"},
		{src: "type t [int]", typ: "t", pat: "[_]", want: "[int]"},
		{src: "type t [int]", typ: "t", pat: "&[_]", want: "&[int]"},

		{typ: "[.x int, .y int]", pat: "string", want: ""},
		{typ: "[.x int, .y int]", pat: "int", want: ""},
		{typ: "[.x int, .y int]", pat: "T", want: ""},
		{typ: "[.x int, .y int]", pat: "_", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "&int", want: ""},
		{typ: "[.x int, .y int]", pat: "&[.x int, .y int]", want: "&[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int, .z int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.y int, .x int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int, .y int]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x _, .y int]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x int, .y _]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x _, .y _]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x _, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y _]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x _, .y _]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y int]", pat: "[x? int, y?]", want: ""},
		{typ: "[.x int, .y int]", pat: "(int){float32}", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[.x int]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x int]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[.x _]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x _]", want: "&[.x int]"},

		{typ: "[x? int, y?, z? string]", pat: "string", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "int", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "T", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "_", want: "[x? int, y?, z? string]"},
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
		{typ: "[x? int, y?, z? string]", pat: "[x? _, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? _]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? _, y?, z? _]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "(int){float32}", want: ""},
		{src: "type t [x? int]", typ: "t", pat: "[x? int]", want: "[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "&[x? int]", want: "&[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "[x? _]", want: "[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "&[x? _]", want: "&[x? int]"},

		{typ: "(int){float32}", pat: "string", want: ""},
		{typ: "(int){float32}", pat: "int", want: ""},
		{typ: "(int){float32}", pat: "T", want: ""},
		{typ: "(int){float32}", pat: "_", want: "(int){float32}"},
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
		{typ: "(int){float32}", pat: "(_){float32}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(int){_}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(_){_}", want: "(int){float32}"},
		{typ: "(int, string){float32}", pat: "(_, _){_}", want: "(int, string){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(_){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){_}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(_){_}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(_){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){_}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(_){_}", want: "&(int){float32}"},

		{
			src: `
				type a b
				type b c
				type c d
				type d [.x int]
			`,
			typ:  "a",
			pat:  "[.x _]",
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
			pat:  "[.t string, .u _]",
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
			pat:  "[.t _, .u int]",
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
			pat:  "[.t _, .u _]",
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
			pat:  "(_, string) a",
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
			pat:  "(int, _) a",
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
			pat:  "(_, _) a",
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
			bind, note := convertType(pattern(typ), pat, implicit)
			if test.want == "" {
				if note == nil {
					t.Errorf("got %s %v, want error", subType(bind, typ), bindAsSlice(bind))
				}
			} else {
				if note != nil {
					t.Errorf("got error %s, want %s", note, test.want)
				} else if sub := subType(bind, pat.typ); sub.String() != test.want {
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

func TestTypePatternString(t *testing.T) {
	tests := []struct {
		src  string
		pat  string
		want string
	}{
		{pat: "int", want: "int"},
		{pat: "&int", want: "&int"},
		{pat: "[int]", want: "[int]"},
		{pat: "[.x int, .y int]", want: "[.x int, .y int]"},
		{pat: "[x?, y?, z?]", want: "[x?, y?, z?]"},
		{pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{pat: "(){}", want: "(){}"},
		{pat: "(int){}", want: "(int){}"},
		{pat: "(){int}", want: "(){int}"},
		{pat: "_", want: "_"},
		{pat: "_0", want: "_"},
		{pat: "&_", want: "&_"},
		{pat: "[_]", want: "[_]"},
		{pat: "[.x _, .y int]", want: "[.x _, .y int]"},
		{pat: "[.x int, .y _]", want: "[.x int, .y _]"},
		{pat: "[x? _, y?, z? string]", want: "[x? _, y?, z? string]"},
		{pat: "[x? int, y?, z? _]", want: "[x? int, y?, z? _]"},
		{pat: "(_){}", want: "(_){}"},
		{pat: "(){_}", want: "(){_}"},
		{pat: "(int, _){}", want: "(int, _){}"},
		{pat: "[.x _, .y _0, .z _0]", want: "[.x _, .y _0, .z _0]"},
		{pat: "[.x _, .y _1, .z _1]", want: "[.x _, .y _0, .z _0]"},
		{pat: "[.x _, .y U, .z _]", want: "[.x _, .y U, .z _]"},
		{pat: "[.high [.med [.low _]]]", want: "[.high [.med [.low _]]]"},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(string, int) pair",
			want: "(string, int) pair",
		},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(_, _) pair",
			want: "(_, _) pair",
		},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(_0, _0) pair",
			want: "(_0, _0) pair",
		},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(_, int) pair",
			want: "(_, int) pair",
		},
	}
	for _, test := range tests {
		t.Run(test.pat, func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			pat := parseTestPattern(t, mod, test.pat)
			if got := pat.String(); got != test.want {
				t.Errorf("(%s).String()=%s, want %s", test.pat, got, test.want)
			}
		})
	}
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
			return []Type{&TypeVar{Name: name, Def: parm, L: l}}
		}
	}
	return nil
}

func parseTestPattern(t *testing.T, m *Mod, src string) typePattern {
	t.Helper()
	var parms []*TypeParm
	parmSet := make(map[string]*TypeParm)
	nextName := 0
	src2 := ""
	var prev rune
	for len(src) > 0 {
		r, w := utf8.DecodeRuneInString(src)
		src = src[w:]
		if r != '_' || unicode.IsLetter(prev) || unicode.IsNumber(prev) || prev == '_' {
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
		if _, ok := parmSet[name]; !ok {
			p := &TypeParm{Name: fmt.Sprintf("_%d", nextName)}
			nextName++
			parmSet[name] = p
			parms = append(parms, p)
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
	return typePattern{parms: parms, typ: typ}
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
