package checker

import "testing"

func TestString(t *testing.T) {
	tests := []struct {
		src, want string
		otherMod  testMod
	}{
		{
			src:  "type t &int",
			want: "&int",
		},
		{
			src:  "type t &&int",
			want: "&&int",
		},
		{
			src:  "type t &[int]",
			want: "&[int]",
		},
		{
			src:  "type t int",
			want: "int",
		},
		{
			src:  "type t [int]",
			want: "[int]",
		},
		{
			src:  "type T t [T]",
			want: "[T]",
		},
		{
			src:  "type t [x: int, y: int]",
			want: "[x: int, y: int]",
		},
		{
			src:  "type (X, Y) t [x: X, y: Y]",
			want: "[x: X, y: Y]",
		},
		{
			src:  "type t [x: [foo: string], y: int]",
			want: "[x: [foo: string], y: int]",
		},
		{
			src:  "type t [x: int | y: int]",
			want: "[x: int | y: int]",
		},
		{
			src:  "type t [x | y: int]",
			want: "[x | y: int]",
		},
		{
			src:  "type t [x | y]",
			want: "[x | y]",
		},
		{
			src:  "type t [|x]",
			want: "[|x]",
		},
		{
			src:  "type t (){}",
			want: "(){}",
		},
		{
			src:  "type t (int){}",
			want: "(int){}",
		},
		{
			src:  "type t (int, string){}",
			want: "(int, string){}",
		},
		{
			src:  "type t (){int}",
			want: "(){int}",
		},
		{
			src:  "type t (string){int}",
			want: "(string){int}",
		},
		{
			src:  "type t (string, int){int}",
			want: "(string, int){int}",
		},
		{
			src:  "type t ([foo: string], int){int}",
			want: "([foo: string], int){int}",
		},
		{
			src:  "type t ((int){string}, int){int}",
			want: "((int){string}, int){int}",
		},
		{
			src:  "type T t T",
			want: "T",
		},
		{
			src: `
				type u int
				type t u
			`,
			want: "u",
		},
		{
			src: `
				type X u [x: X]
				type t int u
			`,
			want: "int u",
		},
		{
			src: `
				type X u [x: X]
				type t [foo: string] u
			`,
			want: "[foo: string] u",
		},
		{
			src: `
				type (X, Y) u [x: X, y: Y]
				type t ([foo: string], int) u
			`,
			want: "([foo: string], int) u",
		},
		{
			src: `
				import "foo/bar/pair"
				type t (int, string) pair#pair
			`,
			otherMod: testMod{
				path: "foo/bar/pair",
				src:  "type (X, Y) pair [x: X, y: Y]",
			},
			want: "(int, string) pair#pair",
		},
		{
			src: `
				type X list
				type t &int list
			`,
			want: "&int list",
		},
		{
			src: `
				type X list
				type t (&int) list
			`,
			want: "(&int) list",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.want, func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, []testMod{test.otherMod})
			if len(errs) > 0 {
				t.Fatalf("%s", errs[0])
			}
			typ := findTypeDef(t, "t", mod)
			if str := typ.Type.String(); str != test.want {
				t.Errorf("got %s, want %s", str, test.want)
			}
		})
	}
}
