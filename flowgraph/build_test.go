package flowgraph

import (
	"strings"
	"testing"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/parser"
)

func TestBuildType(t *testing.T) {
	tests := []struct {
		src  string
		want string
	}{
		{src: "type t int", want: "int64"},
		{src: "type t int32", want: "int32"},
		{src: "type t uint", want: "uint64"},
		{src: "type t uint32", want: "uint32"},
		{src: "type t &uint32", want: "*uint32"},
		{src: "type t &&&uint32", want: "***uint32"},
		{src: "type t [uint32]", want: "struct{length int64; data []uint32}"},
		{src: "type t [[uint32]]", want: "struct{length int64; data []struct{length int64; data []uint32}}"},
		{src: "type t string", want: "struct{length int64; data []uint8}"},
		{src: "type t [.x int, .y int]", want: "struct{x int64; y int64}"},
		{src: "type t [.nest u] type u [.x int]", want: "struct{nest test.u}"},
		{src: "type t [.nest [.x int]] type u [.x int]", want: "struct{nest struct{x int64}}"},
		{src: "type t [.loop &t]", want: "struct{loop *test.t}"},
		{src: "type t (int, float32){uint8}", want: "struct{func func(*struct{}, int64, float32)uint8; caps *struct{}}"},
		{src: "type t [?one, ?two, ?three]", want: "int64"},
		{src: "type t [?none, ?some &int]", want: "*int64"},
		{src: "type t [?none, ?some int]", want: "struct{tag int64; data union{some int64}}"},
		{
			src:  "type t [?a int, ?b float32, ?c string]",
			want: "struct{tag int64; data union{a int64; b float32; c struct{length int64; data []uint8}}}",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			mod := check(t, test.src)
			typ := findTypeDef(t, "t", mod)
			got := newModBuilder("test").buildType(typ.Type)
			if got.String() != test.want {
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
	mod, _, errs := checker.Check("test", p.Files, nil)
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
