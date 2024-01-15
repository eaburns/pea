package checker

import (
	"fmt"
	"sort"
	"testing"
)

func TestUnify(t *testing.T) {
	tests := []struct {
		src  string
		a    string
		b    string
		want string
	}{
		// Expand open and open structs.
		{a: `[.x int, ...]`, b: `[.x int, ...]`, want: `[.x int, ...]`},
		{a: `[.x int, ...]`, b: `[.y int, ...]`, want: `[.x int, .y int, ...]`},
		{a: `[.x ?, ...]`, b: `[.y int, ...]`, want: `[.x ?, .y int, ...]`},
		{a: `[.x int, ...]`, b: `[.y ?, ...]`, want: `[.x int, .y ?, ...]`},
		{a: `[.x int, ...]`, b: `[.x ?, ...]`, want: `[.x int, ...]`},
		{a: `[.x int, ...]`, b: `[.x float32, ...]`, want: ``},
		{a: `[.x int, .y ?1, ...]`, b: `[.x ?1, .y float32, ...]`, want: ``},
		{a: `[.x int, ...]`, b: `int`, want: ``},

		// Expand open and closed structs.
		{a: `[.x int, ...]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x int]`, b: `[.x int, ...]`, want: `[.x int]`},
		{a: `[.x int, ...]`, b: `[.x int, .y float32]`, want: `[.x int, .y float32]`},
		{a: `[.y float32, ...]`, b: `[.x int, .y float32]`, want: `[.x int, .y float32]`},
		{a: `[.y float32, .x int, ...]`, b: `[.x int, .y float32]`, want: `[.x int, .y float32]`},
		{a: `[.y ?, .x int, ...]`, b: `[.x int, .y float32]`, want: `[.x int, .y float32]`},
		{a: `[.y float32, .x ?, ...]`, b: `[.x int, .y float32]`, want: `[.x int, .y float32]`},
		{a: `[.y ?1, .x ?1, ...]`, b: `[.x int, .y int]`, want: `[.x int, .y int]`},
		{a: `[.y ?1, .x ?1, ...]`, b: `[.x int, .y float32]`, want: ``},
		{a: `[.y int, .x int, ...]`, b: `[.x int]`, want: ``},
		{a: `[.y int, ...]`, b: `[.x int]`, want: ``},

		// Expand open and open unions.
		{a: `[x? int, ...]`, b: `[x? int, ...]`, want: `[x? int, ...]`},
		{a: `[x?, ...]`, b: `[x?, ...]`, want: `[x?, ...]`},
		{a: `[x?, ...]`, b: `[y?, ...]`, want: `[x?, y?, ...]`},
		{a: `[x? int, ...]`, b: `[y? int, ...]`, want: `[x? int, y? int, ...]`},
		{a: `[x? ?, ...]`, b: `[y? int, ...]`, want: `[x? ?, y? int, ...]`},
		{a: `[x? int, ...]`, b: `[y? ?, ...]`, want: `[x? int, y? ?, ...]`},
		{a: `[x? int, ...]`, b: `[x? ?, ...]`, want: `[x? int, ...]`},
		{a: `[x? int, ...]`, b: `[x? float32, ...]`, want: ``},
		{a: `[x? int, y? ?1, ...]`, b: `[x? ?1, y? float32, ...]`, want: ``},
		{a: `[x?, ...]`, b: `[x? int, ...]`, want: ``},
		{a: `[x? int, ...]`, b: `int`, want: ``},

		// Expand open and closed unions.
		{a: `[x? int, ...]`, b: `[x? int]`, want: `[x? int]`},
		{a: `[x? int]`, b: `[x? int, ...]`, want: `[x? int]`},
		{a: `[x? int, ...]`, b: `[x? int, y? float32]`, want: `[x? int, y? float32]`},
		{a: `[y? float32, ...]`, b: `[x? int, y? float32]`, want: `[x? int, y? float32]`},
		{a: `[y? float32, x? int, ...]`, b: `[x? int, y? float32]`, want: `[x? int, y? float32]`},
		{a: `[y? ?, x? int, ...]`, b: `[x? int, y? float32]`, want: `[x? int, y? float32]`},
		{a: `[y? float32, x? ?, ...]`, b: `[x? int, y? float32]`, want: `[x? int, y? float32]`},
		{a: `[y? ?1, x? ?1, ...]`, b: `[x? int, y? int]`, want: `[x? int, y? int]`},
		{a: `[y? ?1, x? ?1, ...]`, b: `[x? int, y? float32]`, want: ``},
		{a: `[y? ?0, x? ?1, ...]`, b: `[x? int, y?]`, want: `[x? int, y?]`},
		{a: `[y? ?1, x? ?1, ...]`, b: `[x? int, y?]`, want: ``},
		{a: `[y? int, ...]`, b: `[x? int]`, want: ``},
		{a: `[y? int, x? int, ...]`, b: `[x? int]`, want: ``},
		{a: `[y? int, ...]`, b: `[x? int]`, want: ``},

		// Expand, recur into various literal types
		{a: `&[.x int, ...]`, b: `&[.x int, .y int]`, want: `&[.x int, .y int]`},
		{a: `&[.x int, ...]`, b: `int`, want: ``},
		{a: `[[.x int, ...]]`, b: `[[.x int, .y int]]`, want: `[[.x int, .y int]]`},
		{a: `[[.x int, ...]]`, b: `int`, want: ``},
		{a: `[.a [.x int, ...]]`, b: `[.a [.x int, .y int]]`, want: `[.a [.x int, .y int]]`},
		{a: `[.a [.x int, ...], .b int]`, b: `[.a [.x int, .y int]]`, want: ``},
		{a: `[.b [.x int, ...]]`, b: `[.a [.x int, .y int]]`, want: ``},
		{a: `[a? [.x int, ...]]`, b: `[a? [.x int, .y int]]`, want: `[a? [.x int, .y int]]`},
		{a: `[a? [.x int, ...], b?]`, b: `[a? [.x int, .y int]]`, want: ``},
		{a: `[b? [.x int, ...]]`, b: `[a? [.x int, .y int]]`, want: ``},
		{a: `([.x int, ...]){}`, b: `([.x int, .y int]){}`, want: `([.x int, .y int]){}`},
		{a: `([.x int, ...], int){}`, b: `([.x int, .y int]){}`, want: ``},
		{a: `(){[.x int, ...]}`, b: `(){[.x int, .y int]}`, want: `(){[.x int, .y int]}`},
		{a: `int`, b: `[.x int, ...]`, want: ``},
		{src: `type t int`, a: `t`, b: `[.x int, ...]`, want: ``},
		{a: `T`, b: `[.x int, ...]`, want: ``},

		// Sub ? with all the different kinds of types.
		{a: `?0`, b: `?1`, want: `?`},
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
		{src: `type T t T`, a: `?0 t`, b: `?1 t`, want: `? t`},
		{src: `type T t T 	type U u U`, a: `int t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `? t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `int t`, b: `? u`, want: ``},
		{src: `type T t T 	type U u U`, a: `?0 t`, b: `?1 u`, want: ``},

		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(?, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, ?) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(?, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, ?) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(?1, ?2) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(?1, ?2) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(?1, int) t`, b: `(int, ?2) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, ?1) t`, b: `(?2, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, ?1) t`, b: `(int, ?2) t`, want: `(int, ?) t`},
		{src: `type (T, U) t int`, a: `(?1, int) t`, b: `(?2, int) t`, want: `(?, int) t`},
		{src: `type (T, U) t int`, a: `(?1, ?2) t`, b: `(?3, ?4) t`, want: `(?0, ?1) t`},
		{src: `type (T, U) t int`, a: `(?1, string) t`, b: `(int, ?2) t`, want: `(int, string) t`},
		{src: `type (T, U) t int`, a: `(int, string) t`, b: `(?1, int) t`, want: ``},
		{src: `type (T, U) t int`, a: `(int, ?1) t`, b: `(string, ?2) t`, want: ``},
		{src: `type (T, U) t int`, a: `(?0, ?0) t`, b: `(string, ?1) t`, want: `(string, string) t`},
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
		{a: `&?0`, b: `&?1`, want: `&?`},

		{a: `[int]`, b: `[int]`, want: `[int]`},
		{a: `[int]`, b: `int`, want: ``},
		{a: `[int]`, b: `string`, want: ``},
		{a: `[int]`, b: `[?]`, want: `[int]`},
		{a: `[?]`, b: `[int]`, want: `[int]`},
		{a: `[?0]`, b: `[?1]`, want: `[?]`},

		{a: `[.]`, b: `[.]`, want: `[.]`},
		{a: `[.x int]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int]`, b: `int`, want: ``},
		{a: `[.x int]`, b: `[.x int, .y string]`, want: ``},
		{a: `[.x int]`, b: `[.y int]`, want: ``},
		{a: `[.x int]`, b: `[.x ?]`, want: `[.x int]`},
		{a: `[.x ?]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x ?0]`, b: `[.x ?1]`, want: `[.x ?]`},
		{a: `[.x ?, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y ?]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x ?, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y ?]`, want: `[.x int, .y string]`},
		{a: `[.x ?0, .y string]`, b: `[.x ?1, .y string]`, want: `[.x ?, .y string]`},
		{a: `[.x int, .y ?0]`, b: `[.x int, .y ?1]`, want: `[.x int, .y ?]`},
		{a: `[.x ?1, .y ?2]`, b: `[.x ?3, .y ?4]`, want: `[.x ?0, .y ?1]`},
		{a: `[.x ?1, .y ?2]`, b: `[.x ?3]`, want: ``},
		{a: `[.x ?1, .y int]`, b: `[.x ?2, .y string]`, want: ``},
		{a: `[.x ?1, .y int]`, b: `[.x ?2, .y T]`, want: ``},
		{a: `[.x ?1, .y T]`, b: `[.x ?2, .y string]`, want: ``},
		{a: `[.x ?1, .y T]`, b: `[.x U, .y T]`, want: `[.x U, .y T]`},

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
		{a: `(?1, ?2){?3}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(?1, string){int}`, b: `(int, ?2){int}`, want: `(int, string){int}`},
		{a: `(int, ?1){int}`, b: `(?2, string){int}`, want: `(int, string){int}`},
		{a: `(int, ?1){int}`, b: `(?2){int}`, want: ``},
		{a: `(int, ?1){int}`, b: `(string, int){int}`, want: ``},
		{a: `(int, ?1){int}`, b: `(?2, int){string}`, want: ``},
		{a: `(?0, ?0){int}`, b: `(int, ?1){int}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(int, ?1){?}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(?1, int){?}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(?1, ?2){int}`, want: `(int, int){int}`},
		{a: `(?0, ?0){?0}`, b: `(string, int){int}`, want: ``},
		{a: `(?0, ?0){?0}`, b: `(int, string){int}`, want: ``},
		{a: `(?0, ?0){?0}`, b: `(int, int){string}`, want: ``},
		{a: `(?0, ?0){?1}`, b: `(int, int){string}`, want: `(int, int){string}`},

		{a: `[.x ?0, .y ?0]`, b: `[.x ?, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x ?1, .y ?1]`, want: `[.x ?, .y ?]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x ?1, .y ?2]`, want: `[.x ?, .y ?]`},
		{a: `[.x ?0, .y ?1]`, b: `[.x ?2, .y ?2]`, want: `[.x ?, .y ?]`},
		{a: `[.x ?0, .y ?1]`, b: `[.x ?2, .y ?3]`, want: `[.x ?0, .y ?1]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x int, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x ?0, .y ?0]`, b: `[.x string, .y int]`, want: ``},
		{a: `[.x string, .y int]`, b: `[.x ?0, .y ?0]`, want: ``},
		{a: `[.x int, .y int]`, b: `[.x ?0, .y ?0]`, want: `[.x int, .y int]`},
		{a: `[.x ?0, .y ?0, .z int]`, b: `[.x string, .y ?1, .z ?1]`, want: ``},
		{a: `[.x ?0, .y ?1, .z int]`, b: `[.x string, .y ?2, .z ?2]`, want: `[.x string, .y int, .z int]`},
		{a: `[.x ?0, .y ?0, .z int]`, b: `[.x string, .y ?1, .z ?2]`, want: `[.x string, .y string, .z int]`},
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
		{a: `[.x [.y [.z ?0]]]`, b: `[.x [.y [.z ?1]]]`, want: `[.x [.y [.z ?]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y ?]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x ?]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z ?0]]]`, b: `[.x ?1]`, want: `[.x [.y [.z ?]]]`},
		{a: `[.x [.y [.z ?0]]]`, b: `?1`, want: `[.x [.y [.z ?]]]`},
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
				aType, bType := expandOpenLits(a.Type, b.Type)
				if aPrime := subType(bind, aType); !eqType(aPrime, u.Type) {
					t.Errorf("sub(bind, %s)=%s, want %s", aType, aPrime, u.Type)
				}
				if bPrime := subType(bind, bType); !eqType(bPrime, u.Type) {
					t.Errorf("sub(bind, %s)=%s, want %s", bType, bPrime, u.Type)
				}
			}
		})
	}
}

func TestUnifyOfTwoVariables(t *testing.T) {
	mod, errs := check("test", []string{""}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	a := parseTestPattern(t, mod, "?0")
	b := parseTestPattern(t, mod, "?1")
	var bind map[*TypeParm]Type
	u, _ := unify(a, b, &bind)
	if u == nil {
		t.Fatalf("unify(%s, %s)=nil, wanted ?", a, b)
	}
	if u.isGroundType() {
		t.Fatalf("unify(%s, %s).isGroundType()=true, want false", a, b)
	}
}

func TestUnifyIgnoreBoundTypeParameters(t *testing.T) {
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
	pat0.Parms.ForEach(func(p *TypeParm) { parm0 = p })

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
// But the same tests should work with convertType(),
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
		{typ: "[.x int, .y int]", pat: "[.x ?0, .y ?1]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x ?, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y ?]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x ?0, .y ?1]", want: "[.x int, .y string]"},
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
		{typ: "[x? int, y?, z? string]", pat: "[x? ?0, y?, z? ?1]", want: "[x? int, y?, z? string]"},
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
		{typ: "(int){float32}", pat: "(?0){?1}", want: "(int){float32}"},
		{typ: "(int, string){float32}", pat: "(?0, ?1){?2}", want: "(int, string){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(?){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){?}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(?0){?1}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(?){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){?}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(?0){?1}", want: "&(int){float32}"},

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
			pat:  "[.t ?0, .u ?1]",
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
			pat:  "(?0, ?1) a",
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
