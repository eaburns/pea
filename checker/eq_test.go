package checker

import (
	"fmt"
	"strings"
	"testing"
	"text/template"
)

func TestEq(t *testing.T) {
	tests := []struct {
		Src string
		Typ      string
		Same     []string
		Diff     []string
		otherMod testMod
	}{
		{
			Typ: "int",
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
			Src: "type foo",
			Typ: "foo",
			Same: []string{"foo"},
			Diff: []string{
				"&foo",
				"int32",
				"float32",
				"[x: int, y: int]",
				"[none | some: int]",
				"(int){float32}",
			},
		},
		{
			Src: "type X foo",
			Typ: "int foo",
			Same: []string{"int foo"},
			Diff: []string{
				"&int foo",
				"float32 foo",
				"int32",
				"float32",
				"[x: int, y: int]",
				"[none | some: int]",
				"(int){float32}",
			},
		},
		{
			Typ: "[int]",
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
			Typ: "[x: int]",
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
			Typ: "[x: int, y: int]",
			Same: []string{"[x: int, y: int]"},
			Diff: []string{"[y: int, x: int]"},
		},
		{
			Typ: "[none | some: int]",
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
			Typ: "[a: int | b: int]",
			Diff: []string{"[a: int, b: int]"},
		},
		{
			Typ: "(){}",
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
			Typ: "(int){}",
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
			Typ: "(){int}",
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
			Typ: "(int){int}",
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
			Typ: "(int, float32){int}",
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
