package checker

import (
	"fmt"
	"strings"
	"testing"
	"text/template"
)

func TestEq(t *testing.T) {
	tests := []struct {
		Src      string
		Typ      string
		Same     []string
		Diff     []string
		otherMod testMod
	}{
		{
			Typ:  "int",
			Same: []string{"int"},
			Diff: []string{
				"&int",
				"int32",
				"float32",
				"[x: int, y: int]",
				"[none | some: int]",
				"(int){float32}",
			},
		},
		{
			Src:  "type named_type",
			Typ:  "named_type",
			Same: []string{"named_type"},
			Diff: []string{
				"&named_type",
				"int32",
				"float32",
				"[x: int, y: int]",
				"[none | some: int]",
				"(int){float32}",
			},
		},
		{
			Src:  "type named_int int",
			Typ:  "named_int",
			Same: []string{"named_int"},
			Diff: []string{"int"},
		},
		{
			Src: `
				type named_struct [x: int]
				type named_struct_alias := named_struct
				type bar [x: int]
			`,
			Typ: "named_struct",
			Same: []string{
				"named_struct",
				"named_struct_alias",
			},
			Diff: []string{
				"bar",
				"[x: int]",
			},
		},
		{
			Src:  "type X param_named_type",
			Typ:  "int param_named_type",
			Same: []string{"int param_named_type"},
			Diff: []string{
				"&int param_named_type",
				"float32 param_named_type",
				"int32",
				"float32",
				"[x: int, y: int]",
				"[none | some: int]",
				"(int){float32}",
			},
		},
		{
			Typ:  "[int]",
			Same: []string{"[int]"},
			Diff: []string{
				"[|int]",
				"&[int]",
				"[float32]",
				"[y: int]",
				"int32",
			},
		},
		{
			Typ:  "[x: int]",
			Same: []string{"[x: int]"},
			Diff: []string{
				"&[x: int]",
				"[x: int, y: int]",
				"[y: int]",
				"int32",
				"float32",
				"[x: int, y: int]",
				"[none | some: int]",
				"(int){float32}",
			},
		},
		{
			Typ:  "[x: int, y: int]",
			Same: []string{"[x: int, y: int]"},
			Diff: []string{"[y: int, x: int]"},
		},
		{
			Typ:  "[none | some: int]",
			Same: []string{"[none | some: int]"},
			Diff: []string{
				"&[none | some: int]",
				"[none: int | some]",
				"[some: int | none]",
				"[none: int, some: int]",
				"[y: int]",
				"int32",
				"float32",
				"[x: int, y: int]",
				"(int){float32}",
			},
		},
		{
			Typ:  "[a: int | b: int]",
			Diff: []string{"[a: int, b: int]"},
		},
		{
			Typ:  "(){}",
			Same: []string{"(){}"},
			Diff: []string{
				"&(){}",
				"(int){}",
				"(){int}",
				"(int){int}",
				"[y: int]",
				"int32",
				"[x: int, y: int]",
			},
		},
		{
			Typ:  "(int){}",
			Same: []string{"(int){}"},
			Diff: []string{
				"&(int){}",
				"(){}",
				"(){int}",
				"(int){int}",
				"[y: int]",
				"int32",
				"[x: int, y: int]",
			},
		},
		{
			Typ:  "(){int}",
			Same: []string{"(){int}"},
			Diff: []string{
				"&(){int}",
				"(){}",
				"(int){}",
				"(int){int}",
				"[y: int]",
				"int32",
				"[x: int, y: int]",
			},
		},
		{
			Typ:  "(int){int}",
			Same: []string{"(int){int}"},
			Diff: []string{
				"&(int){int}",
				"(){}",
				"(int){}",
				"(){int}",
				"[y: int]",
				"int32",
				"[x: int, y: int]",
			},
		},
		{
			Typ:  "(int, float32){int}",
			Same: []string{"(int, float32){int}"},
			Diff: []string{
				"&(int, float32){int}",
				"(int){}",
				"(float32){}",
				"(float32, int){}",
				"(){int}",
				"[y: int]",
				"int32",
				"[x: int, y: int]",
			},
		},
		{
			Src: "type T array := [T]",
			Typ: "int array",
			Same: []string{
				"int array",
				"[int]",
			},
			Diff: []string{
				"[float32]",
				"float32 array",
			},
		},
		{
			Src: `
				type one := two
				type two := three
				type three := [x: int, y: int]
			`,
			Typ: "one",
			Same: []string{
				"two",
				"three",
				"[x: int, y: int]",
			},
		},
		{
			Src: `
				type (X, Y) reverse := (Y, X) forward
				type (X, Y) forward := [x: X, y: Y]
			`,
			Typ: "(string, int) reverse",
			Same: []string{
				"(string, int) reverse",
				"(int, string) forward",
				"[x: int, y: string]",
			},
			Diff: []string{
				"(int, string) reverse",
				"(string, int) forward",
				"[x: string, y: int]",
			},
		},
		{
			Src: `
				type (K, V) map [k: K, v: V]
				type V string_map := (string, V) map
			`,
			Typ: "int string_map",
			Same: []string{
				"(string, int) map",
				"int string_map",
			},
			Diff: []string{
				"[k: string, v: int]",
			},
		},
		{
			Src: `
				type foo := baz
				type bar := [x: foo, y: [z: foo]]
				type baz := int
			`,
			Typ: "bar",
			Same: []string{
				"bar",
				"[x: foo, y: [z: foo]]",
				"[x: baz, y: [z: baz]]",
				"[x: int, y: [z: int]]",
			},
		},
		{
			Src: `
				type T foo := T bar
				type T baz := [x: T]
				type T bar := T baz
				type V qux := [x: V foo]
			`,
			Typ: "int qux",
			Same: []string{
				"int qux",
				"[x: int foo]",
				"[x: int bar]",
				"[x: int baz]",
				"[x: [x: int]]",
			},
		},
		{
			Src: `
				type T foo := [x: T]
				type T bar [x: T]
			`,
			Typ: "int foo bar",
			Same: []string{
				"int foo bar",
				"[x: int] bar",
			},
		},
		{
			Src: `
				import "other"
				type different_mods int
			`,
			otherMod: testMod{
				path: "other",
				src:  "type different_mods int",
			},
			Typ:  "different_mods",
			Diff: []string{"other#different_mods"},
		},
		{
			Src: `
				import "other"
				type different_mods_alias := other#different_mods
			`,
			otherMod: testMod{
				path: "other",
				src:  "type different_mods int",
			},
			Typ:  "different_mods_alias",
			Same: []string{"other#different_mods"},
		},
		{
			Src: `
				import "other"
				type T cross_mod_alas := T other#other_type
			`,
			otherMod: testMod{
				path: "other",
				src:  "type T other_type := [x: T]",
			},
			Typ: "int cross_mod_alas",
			Same: []string{
				"int other#other_type",
				"[x: int]",
			},
		},
	}
	const eqTestTemplate = `
		{{.Src}}
		var x {{.Typ}}
		{{range $i, $typ := .Same -}}
		var s{{$i}} {{$typ}}
		{{end -}}
		{{range $i, $typ := .Diff -}}
		var d{{$i}} {{$typ}}
		{{end}}
	`
	tmp, err := template.New("").Parse(eqTestTemplate)
	if err != nil {
		t.Fatalf("failed to parse template: %s", err)
	}
	for _, test := range tests {
		test := test
		t.Run(test.Typ, func(t *testing.T) {
			var src strings.Builder
			if err := tmp.Execute(&src, test); err != nil {
				t.Fatalf("failed to execute template: %s", err)
			}
			mod, errs := check("test", []string{src.String()}, []testMod{test.otherMod})
			if len(errs) > 0 {
				t.Log(src.String())
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			typ := findVarDef(t, "x", mod).Type
			for i := range test.Same {
				s := findVarDef(t, fmt.Sprintf("s%d", i), mod).Type
				if !typ.eq(s) {
					t.Errorf("%s != %s", typ, s)
				}
			}
			for i := range test.Diff {
				d := findVarDef(t, fmt.Sprintf("d%d", i), mod).Type
				if typ.eq(d) {
					t.Errorf("%s = %s", typ, d)
				}
			}
		})
	}
}
