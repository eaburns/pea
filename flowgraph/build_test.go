package flowgraph

import (
	"fmt"
	"strings"
	"testing"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/parser"
)

func TestBuildType(t *testing.T) {
	tests := []struct {
		src  string
		typ  string
		want string
	}{
		{typ: "int", want: "int64"},
		{typ: "int32", want: "int32"},
		{typ: "uint", want: "uint64"},
		{typ: "uint32", want: "uint32"},
		{typ: "&uint32", want: "*uint32"},
		{typ: "[uint32]", want: "array<uint32>"},
		{typ: "[[uint32]]", want: "array<array<uint32>>"},
		{typ: "string", want: "string"},
		{typ: "[.x int, .y int]", want: "struct{x int64; y int64}"},
		{src: "type u [.x int]", typ: "[.nest u]", want: "struct{nest test.u}"},
		{src: "type u [.x int]", typ: "[.nest [.x int]]", want: "struct{nest struct{x int64}}"},
		{src: "type t [.loop &t]", typ: "t", want: "test.t"},
		{typ: "(int, string){uint8}", want: "block<func(*struct{}, int64, string)uint8>"},
		{typ: "[one?, two?, three?]", want: "int64"},
		{typ: "[none?, some? &int]", want: "*int64"},
		{typ: "[none?, some? int]", want: "struct{tag int64; data union{some int64}}"},
		{
			typ:  "[a? int, b? float32, c? string]",
			want: "struct{tag int64; data union{a int64; b float32; c string}}",
		},
		{
			src:  "type t &[none?, some? [.x int, .next t]]",
			typ:  "t",
			want: "*test.t",
		},
		{
			src:  "type t [none?, some? &[.x int, .next t]]",
			typ:  "t",
			want: "*test.t",
		},
		{
			src:  "type t [none?, some? [.x int, .next &t]]",
			typ:  "t",
			want: "test.t",
		},
		{
			src: "type t &u		type u [.next t]",
			typ:  "t",
			want: "*test.u",
		},
		{
			src: "type t &u		type u &v		type v &t",
			typ: "t",
			// t = *u
			//   = **v
			//   = ***t
			//   = *** *struct{} <-- t replaced with &struct{}
			want: "****struct{}",
		},
		{
			src: "type t &u		type u [.u &u, .t t]",
			typ:  "t",
			want: "*test.u",
		},
		{
			src: "type t &[foo? t, bar?]",
			typ: "t",
			// [foo? t, bar?], with pointer t is a pointer to t.
			// & of a pointer is a pointer.
			// This is a double pointer.
			want: "**struct{}",
		},
		{
			src:  "type t [foo? t, bar?]",
			typ:  "t",
			want: "test.t",
		},
		{
			src:  "type t [foo? &t, bar? &t, baz?]",
			typ:  "t",
			want: "test.t",
		},
		{
			typ:  "(int){string}",
			want: "block<func(*struct{}, int64)string>",
		},
		{
			src:  "type t (t){t}",
			typ:  "t",
			want: "test.t<func(*struct{}, test.t<…>)test.t<…>>",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			src := fmt.Sprintf("%s\nFunc foo(_ %s)\n", test.src, test.typ)
			t.Log(src)
			mod := check(t, src)
			fg := Build(mod)
			var foo *FuncDef
			for _, f := range fg.Funcs {
				if f.Name == "foo" {
					foo = f
					break
				}
			}
			typ := foo.Parms[0].Type
			if foo.Parms[0].ByValue {
				typ = typ.(*AddrType).Elem
			}
			if got := typ.String(); got != test.want {
				t.Errorf("got %s, want %s", got, test.want)
			}
		})
	}
}

func check(t *testing.T, src string) *checker.Mod {
	p := parser.New()
	if err := p.Parse("", strings.NewReader(src)); err != nil {
		t.Fatalf("failed to parse: %s", err)
	}
	mod, _, errs := checker.Check("test", p.Files)
	if len(errs) > 0 {
		t.Fatalf("failed to checke: %s", errs[0])
	}
	return mod
}

func findTypeDef(t *testing.T, name string, mod *checker.Mod) *checker.TypeDef {
	for _, def := range mod.Defs {
		if d, ok := def.(*checker.TypeDef); ok && d.Name == name {
			return d
		}
	}
	t.Fatalf("failed to find type definition %s", name)
	panic("impossible")
}
