package parser

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestExpr(t *testing.T) {
	tests := []struct {
		expr string
		want interface{}
	}{
		{"a", Ident{Name: "a"}},
		{"a123", Ident{Name: "a123"}},
		{"_a123", Ident{Name: "_a123"}},
		{"α", Ident{Name: "α"}},
		{"test_foo", Ident{Name: "test_foo"}}, // reserved word prefix

		{"0.0", &FloatLit{Text: "0.0"}},
		{"1.0", &FloatLit{Text: "1.0"}},
		{"123.456", &FloatLit{Text: "123.456"}},
		{"-1.0", &FloatLit{Text: "-1.0"}},
		{"+1.0", &FloatLit{Text: "+1.0"}},
		{"1.0e5", &FloatLit{Text: "1.0e5"}},
		{"1.0E5", &FloatLit{Text: "1.0E5"}},
		{"1.0e-5", &FloatLit{Text: "1.0e-5"}},
		{"1.0E-5", &FloatLit{Text: "1.0E-5"}},

		{"0", &IntLit{Text: "0"}},
		{"1", &IntLit{Text: "1"}},
		{"-1", &IntLit{Text: "-1"}},
		{"+1", &IntLit{Text: "+1"}},
		{"123456", &IntLit{Text: "123456"}},

		{"0x1", &IntLit{Text: "0x1"}},
		{"0x123abcdef", &IntLit{Text: "0x123abcdef"}},
		{"0x123ABCDEF", &IntLit{Text: "0x123ABCDEF"}},

		{"`abc\nxyz`", &StrLit{Raw: true, Data: "abc\nxyz"}},

		{`"abc\"\t\n\bxyz"`, &StrLit{Raw: false, Data: "abc\"\t\n\bxyz"}},
		{`"\x0F"`, &StrLit{Raw: false, Data: "\x0F"}},
		{`"\u01ab"`, &StrLit{Raw: false, Data: "\u01ab"}},
		{`"\U000065e5"`, &StrLit{Raw: false, Data: "\U000065e5"}},

		{`'x'`, &CharLit{Rune: 'x'}},
		{`'\\'`, &CharLit{Rune: '\\'}},
		{`'\''`, &CharLit{Rune: '\''}},
		{`'\t'`, &CharLit{Rune: '\t'}},
		{`'\n'`, &CharLit{Rune: '\n'}},
		{`'\b'`, &CharLit{Rune: '\b'}},
		{`'\x01'`, &CharLit{Rune: '\x01'}},
		{`'\u01ab'`, &CharLit{Rune: '\u01ab'}},
		{`'\U000065e5'`, &CharLit{Rune: '\U000065e5'}},

		{
			`(){x}`,
			&BlockLit{Exprs: []Expr{Ident{Name: "x"}}},
		},
		{
			`(i int){x}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{Name: Ident{Name: "int"}},
					},
				},
				Exprs: []Expr{Ident{Name: "x"}},
			},
		},
		{
			`(i int, j int){x}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{Name: Ident{Name: "int"}},
					},
					{
						Name: Ident{Name: "j"},
						Type: &NamedType{Name: Ident{Name: "int"}},
					},
				},
				Exprs: []Expr{Ident{Name: "x"}},
			},
		},
		{
			`(i, j int){x}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{Name: Ident{Name: "int"}},
					},
					{
						Name: Ident{Name: "j"},
						Type: &NamedType{Name: Ident{Name: "int"}},
					},
				},
				Exprs: []Expr{Ident{Name: "x"}},
			},
		},
		{
			`(i, j){x}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
					},
					{
						Name: Ident{Name: "j"},
					},
				},
				Exprs: []Expr{Ident{Name: "x"}},
			},
		},
		{
			`(t T){}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Ident{Name: "t"},
						Type: TypeVar{Name: "T"},
					},
				},
				Exprs: nil,
			},
		},
		{
			`(t T123){}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Ident{Name: "t"},
						Type: TypeVar{Name: "T123"},
					},
				},
				Exprs: nil,
			},
		},

		{
			`[none?]`,
			&UnionLit{
				CaseVal: CaseVal{Name: Ident{Name: "none?"}},
			},
		},
		{
			`[some? 5]`,
			&UnionLit{
				CaseVal: CaseVal{
					Name: Ident{Name: "some?"},
					Val:  &IntLit{Text: "5"},
				},
			},
		},

		{
			`[.]`,
			&StructLit{},
		},
		{
			`[.x 5]`,
			&StructLit{
				FieldVals: []FieldVal{
					{
						Name: Ident{Name: ".x"},
						Val:  &IntLit{Text: "5"},
					},
				},
			},
		},
		{
			`[.x 5, .y 7]`,
			&StructLit{
				FieldVals: []FieldVal{
					{
						Name: Ident{Name: ".x"},
						Val:  &IntLit{Text: "5"},
					},
					{
						Name: Ident{Name: ".y"},
						Val:  &IntLit{Text: "7"},
					},
				},
			},
		},

		{
			`[]`,
			&ArrayLit{},
		},
		{
			`[1]`,
			&ArrayLit{Exprs: []Expr{&IntLit{Text: "1"}}},
		},
		{
			`[1, 2]`,
			&ArrayLit{
				Exprs: []Expr{
					&IntLit{Text: "1"},
					&IntLit{Text: "2"},
				},
			},
		},
		{
			`[x foo? 6]`,
			&ArrayLit{
				Exprs: []Expr{
					&Call{
						Fun: Ident{Name: "foo?"},
						Args: []Expr{
							Ident{Name: "x"},
							&IntLit{Text: "6"},
						},
					},
				},
			},
		},
		{
			`[x .y]`,
			&ArrayLit{
				Exprs: []Expr{
					&Call{
						Fun: Ident{Name: ".y"},
						Args: []Expr{
							Ident{Name: "x"},
						},
					},
				},
			},
		},

		{
			`1.y`,
			&Call{
				Fun: Ident{Name: ".y"},
				Args: []Expr{
					&IntLit{Text: "1"},
				},
			},
		},
		{
			`x.y`,
			&Call{
				Fun: Ident{Name: ".y"},
				Args: []Expr{
					Ident{Name: "x"},
				},
			},
		},
		{
			`x foo#.y`,
			&Call{
				Fun: &ModSel{
					Mod:  Ident{Name: "foo"},
					Name: Ident{Name: ".y"},
				},
				Args: []Expr{
					Ident{Name: "x"},
				},
			},
		},
		{
			`x.y.z`,
			&Call{
				Fun: Ident{Name: ".z"},
				Args: []Expr{
					&Call{
						Fun:  Ident{Name: ".y"},
						Args: []Expr{Ident{Name: "x"}},
					},
				},
			},
		},

		{
			`int :: foo + bar`,
			&Convert{
				Expr: &Call{
					Fun: Ident{Name: "+"},
					Args: []Expr{
						Ident{Name: "foo"},
						Ident{Name: "bar"},
					},
				},
				Type: &NamedType{Name: Ident{Name: "int"}},
			},
		},
		{
			`int::foo`,
			&Convert{
				Expr: Ident{Name: "foo"},
				Type: &NamedType{Name: Ident{Name: "int"}},
			},
		},
		{
			`int :: foo`,
			&Convert{
				Expr: Ident{Name: "foo"},
				Type: &NamedType{Name: Ident{Name: "int"}},
			},
		},
		{
			`[.x int] :: foo`,
			&Convert{
				Expr: Ident{Name: "foo"},
				Type: &StructType{
					Fields: []FieldDef{{
						Name: Ident{Name: ".x"},
						Type: &NamedType{Name: Ident{Name: "int"}},
					}},
				},
			},
		},

		{
			`a*b+c=d&&true||false`,
			&Call{
				Fun: Ident{Name: "||"},
				Args: []Expr{
					&Call{
						Fun: Ident{Name: "&&"},
						Args: []Expr{
							&Call{
								Fun: Ident{Name: "="},
								Args: []Expr{
									&Call{
										Fun: Ident{Name: "+"},
										Args: []Expr{
											&Call{
												Fun: Ident{Name: "*"},
												Args: []Expr{
													Ident{Name: "a"},
													Ident{Name: "b"},
												},
											},
											Ident{Name: "c"},
										},
									},
									Ident{Name: "d"},
								},
							},
							Ident{Name: "true"},
						},
					},
					Ident{Name: "false"},
				},
			},
		},

		{
			`x foo? y`,
			&Call{
				Fun: Ident{Name: "foo?"},
				Args: []Expr{
					Ident{Name: "x"},
					Ident{Name: "y"},
				},
			},
		},
		{
			`x bar#foo? y`,
			&Call{
				Fun: &ModSel{
					Mod:  Ident{Name: "bar"},
					Name: Ident{Name: "foo?"},
				},
				Args: []Expr{
					Ident{Name: "x"},
					Ident{Name: "y"},
				},
			},
		},
		{
			`x foo? y bar? z baz? a`,
			&Call{
				Fun: Ident{Name: "foo?bar?baz?"},
				Args: []Expr{
					Ident{Name: "x"},
					Ident{Name: "y"},
					Ident{Name: "z"},
					Ident{Name: "a"},
				},
			},
		},
		{
			`x() foo? y`,
			&Call{
				Fun: Ident{Name: "foo?"},
				Args: []Expr{
					&Call{Fun: Ident{Name: "x"}},
					Ident{Name: "y"},
				},
			},
		},
		{
			`a foo: x`,
			&Call{
				Fun: Ident{Name: "foo:"},
				Args: []Expr{
					Ident{Name: "a"},
					Ident{Name: "x"},
				},
			},
		},
		{
			`a bar#foo: x`,
			&Call{
				Fun: &ModSel{
					Mod:  Ident{Name: "bar"},
					Name: Ident{Name: "foo:"},
				},
				Args: []Expr{
					Ident{Name: "a"},
					Ident{Name: "x"},
				},
			},
		},
		{
			`a foo: x bar: y baz: z`,
			&Call{
				Fun: Ident{Name: "foo:bar:baz:"},
				Args: []Expr{
					Ident{Name: "a"},
					Ident{Name: "x"},
					Ident{Name: "y"},
					Ident{Name: "z"},
				},
			},
		},

		{
			`foo: x`,
			&Call{
				Fun: Ident{Name: "foo:"},
				Args: []Expr{
					Ident{Name: "x"},
				},
			},
		},
		{
			`bar#foo: x`,
			&Call{
				Fun: &ModSel{
					Mod:  Ident{Name: "bar"},
					Name: Ident{Name: "foo:"},
				},
				Args: []Expr{
					Ident{Name: "x"},
				},
			},
		},
		{
			`foo: x bar: y baz: z`,
			&Call{
				Fun: Ident{Name: "foo:bar:baz:"},
				Args: []Expr{
					Ident{Name: "x"},
					Ident{Name: "y"},
					Ident{Name: "z"},
				},
			},
		},
		{
			`foo: x bar: (y baz: z)`,
			&Call{
				Fun: Ident{Name: "foo:bar:"},
				Args: []Expr{
					Ident{Name: "x"},
					&SubExpr{
						Expr: &Call{
							Fun: Ident{Name: "baz:"},
							Args: []Expr{
								Ident{Name: "y"},
								Ident{Name: "z"},
							},
						},
					},
				},
			},
		},
		{
			`foo().bar`,
			&Call{
				Fun: Ident{Name: ".bar"},
				Args: []Expr{
					&Call{Fun: Ident{Name: "foo"}},
				},
			},
		},
		{
			`foo()[bar]`,
			&Call{
				Fun: Ident{Name: "[]"},
				Args: []Expr{
					&Call{Fun: Ident{Name: "foo"}},
					Ident{Name: "bar"},
				},
			},
		},
		{
			`foo()()`,
			&Call{
				Fun: &Call{Fun: Ident{Name: "foo"}},
			},
		},
		{
			`foo[bar].baz`,
			&Call{
				Fun: Ident{Name: ".baz"},
				Args: []Expr{
					&Call{
						Fun: Ident{Name: "[]"},
						Args: []Expr{
							Ident{Name: "foo"},
							Ident{Name: "bar"},
						},
					},
				},
			},
		},
		{
			`foo[bar]()`,
			&Call{
				Fun: &Call{
					Fun: Ident{Name: "[]"},
					Args: []Expr{
						Ident{Name: "foo"},
						Ident{Name: "bar"},
					},
				},
			},
		},
		{
			`foo[bar][baz]`,
			&Call{
				Fun: Ident{Name: "[]"},
				Args: []Expr{
					&Call{
						Fun: Ident{Name: "[]"},
						Args: []Expr{
							Ident{Name: "foo"},
							Ident{Name: "bar"},
						},
					},
					Ident{Name: "baz"},
				},
			},
		},
		{
			`foo.bar()`,
			&Call{
				Fun: &Call{
					Fun:  Ident{Name: ".bar"},
					Args: []Expr{Ident{Name: "foo"}},
				},
			},
		},
		{
			`foo.bar[baz]`,
			&Call{
				Fun: Ident{Name: "[]"},
				Args: []Expr{
					&Call{
						Fun:  Ident{Name: ".bar"},
						Args: []Expr{Ident{Name: "foo"}},
					},
					Ident{Name: "baz"},
				},
			},
		},
		{
			`foo.bar.baz`,
			&Call{
				Fun: Ident{Name: ".baz"},
				Args: []Expr{
					&Call{
						Fun:  Ident{Name: ".bar"},
						Args: []Expr{Ident{Name: "foo"}},
					},
				},
			},
		},
		{
			`foo.bar(x, y, z)`,
			&Call{
				Fun: &Call{
					Fun:  Ident{Name: ".bar"},
					Args: []Expr{Ident{Name: "foo"}},
				},
				Args: []Expr{
					Ident{Name: "x"},
					Ident{Name: "y"},
					Ident{Name: "z"},
				},
			},
		},

		{
			"foo#bar#baz#qux()",
			&Call{
				Fun: &ModSel{
					Mod:  Ident{Name: "foo#bar#baz"},
					Name: Ident{Name: "qux"},
				},
			},
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.expr, func(t *testing.T) {
			src := fmt.Sprintf("var _ := (){} :: {%s}", test.expr)
			p := New()
			if err := p.Parse("", strings.NewReader(src)); err != nil {
				t.Log(src)
				t.Fatalf("failed to parse: %s", err.Error())
			}
			got := p.Files[0].Defs[0].(*VarDef).Expr.(*BlockLit).Exprs[0]
			opts := []cmp.Option{
				cmp.FilterPath(isLoc, cmp.Ignore()),
			}
			diff := cmp.Diff(test.want, got, opts...)
			if diff != "" {
				t.Error(diff)
			}
		})
	}
}

func TestDistributeFuncParmType(t *testing.T) {
	tests := []struct {
		src  string
		want *FuncDef
	}{
		{
			src: "func f()",
			want: &FuncDef{
				Name: Ident{Name: "f"},
			},
		},
		{
			src: "func f(i int)",
			want: &FuncDef{
				Name: Ident{Name: "f"},
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
				},
			},
		},
		{
			src: "func f(i int, j int)",
			want: &FuncDef{
				Name: Ident{Name: "f"},
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "j"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
				},
			},
		},
		{
			src: "func f(i, j int)",
			want: &FuncDef{
				Name: Ident{Name: "f"},
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "j"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
				},
			},
		},
		{
			src: "func f(i, j, k, l int)",
			want: &FuncDef{
				Name: Ident{Name: "f"},
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "j"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "k"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "l"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
				},
			},
		},
		{
			src: "func f(i, j int, k, l int)",
			want: &FuncDef{
				Name: Ident{Name: "f"},
				Parms: []FuncParm{
					{
						Name: Ident{Name: "i"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "j"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "k"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
					{
						Name: Ident{Name: "l"},
						Type: &NamedType{
							Name: Ident{Name: "int"},
						},
					},
				},
			},
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			p := New()
			if err := p.Parse("", strings.NewReader(test.src)); err != nil {
				t.Fatalf("got error: %s", err)
			}
			got := p.Files[0].Defs[0]
			opts := []cmp.Option{
				cmp.FilterPath(isLoc, cmp.Ignore()),
			}
			diff := cmp.Diff(test.want, got, opts...)
			if diff != "" {
				t.Error(diff)
			}
		})
	}
}

func TestIfaceDef(t *testing.T) {
	tests := []struct {
		src  string
		want *IfaceDef
	}{
		{
			src: "iface empty {}",
			want: &IfaceDef{
				Name:  Ident{Name: "empty"},
				Iface: nil,
			},
		},
		{
			src: "iface fooer { foo() }",
			want: &IfaceDef{
				Name:  Ident{Name: "fooer"},
				Iface: []interface{}{&FuncDecl{Name: Ident{Name: "foo"}}},
			},
		},
		{
			src: "Iface fooer { foo() }",
			want: &IfaceDef{
				Exp:   true,
				Name:  Ident{Name: "fooer"},
				Iface: []interface{}{&FuncDecl{Name: Ident{Name: "foo"}}},
			},
		},
		{
			src: "iface fooer ({ foo() })",
			want: &IfaceDef{
				Opaque: true,
				Name:   Ident{Name: "fooer"},
				Iface:  []interface{}{&FuncDecl{Name: Ident{Name: "foo"}}},
			},
		},
		{
			src: "Iface fooer ({ foo() })",
			want: &IfaceDef{
				Exp:    true,
				Opaque: true,
				Name:   Ident{Name: "fooer"},
				Iface:  []interface{}{&FuncDecl{Name: Ident{Name: "foo"}}},
			},
		},
		{
			src: "iface fooer { foo(int, string)float64 }",
			want: &IfaceDef{
				Name: Ident{Name: "fooer"},
				Iface: []interface{}{
					&FuncDecl{
						Name: Ident{Name: "foo"},
						Parms: []Type{
							&NamedType{Name: Ident{Name: "int"}},
							&NamedType{Name: Ident{Name: "string"}},
						},
						Ret: &NamedType{Name: Ident{Name: "float64"}},
					},
				},
			},
		},
		{
			src: "iface fooer { foo(), bar() }",
			want: &IfaceDef{
				Name: Ident{Name: "fooer"},
				Iface: []interface{}{
					&FuncDecl{
						Name: Ident{Name: "foo"},
					},
					&FuncDecl{
						Name: Ident{Name: "bar"},
					},
				},
			},
		},
		{
			src: "iface fooer { foo(int, string)float64, bar()[int8] }",
			want: &IfaceDef{
				Name: Ident{Name: "fooer"},
				Iface: []interface{}{
					&FuncDecl{
						Name: Ident{Name: "foo"},
						Parms: []Type{
							&NamedType{Name: Ident{Name: "int"}},
							&NamedType{Name: Ident{Name: "string"}},
						},
						Ret: &NamedType{Name: Ident{Name: "float64"}},
					},
					&FuncDecl{
						Name: Ident{Name: "bar"},
						Ret: &ArrayType{
							ElemType: &NamedType{Name: Ident{Name: "int8"}},
						},
					},
				},
			},
		},
		{
			src: "iface T set { contains(T)bool }",
			want: &IfaceDef{
				TypeParms: []TypeVar{{Name: "T"}},
				Name:      Ident{Name: "set"},
				Iface: []interface{}{
					&FuncDecl{
						Name:  Ident{Name: "contains"},
						Parms: []Type{TypeVar{Name: "T"}},
						Ret:   &NamedType{Name: Ident{Name: "bool"}},
					},
				},
			},
		},
		{
			src: "iface (K, V) map { get(K)V }",
			want: &IfaceDef{
				TypeParms: []TypeVar{
					{Name: "K"},
					{Name: "V"},
				},
				Name: Ident{Name: "map"},
				Iface: []interface{}{
					&FuncDecl{
						Name:  Ident{Name: "get"},
						Parms: []Type{TypeVar{Name: "K"}},
						Ret:   TypeVar{Name: "V"},
					},
				},
			},
		},
		{
			src: "iface a { b }",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Iface: []interface{}{
					&NamedType{
						Name:  Ident{Name: "b"},
					},
				},
			},
		},
		{
			src: "iface a { b, c, d }",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Iface: []interface{}{
					&NamedType{
						Name:  Ident{Name: "b"},
					},
					&NamedType{
						Name:  Ident{Name: "c"},
					},
					&NamedType{
						Name:  Ident{Name: "d"},
					},
				},
			},
		},
		{
			src: "iface a { int b }",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Iface: []interface{}{
					&NamedType{
						Args: []Type{
							&NamedType{Name: Ident{Name: "int"}},
						},
						Name:  Ident{Name: "b"},
					},
				},
			},
		},
		{
			src: "iface a { mod#b }",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Iface: []interface{}{
					&NamedType{
						Mod: &Ident{Name: "mod"},
						Name:  Ident{Name: "b"},
					},
				},
			},
		},
		{
			src: "iface a { int mod#b }",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Iface: []interface{}{
					&NamedType{
						Args: []Type{
							&NamedType{Name: Ident{Name: "int"}},
						},
						Mod: &Ident{Name: "mod"},
						Name:  Ident{Name: "b"},
					},
				},
			},
		},
		{
			src: "iface a := b",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Alias: &NamedType{
					Name: Ident{Name: "b"},
				},
			},
		},
		{
			src: "iface a := int b",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Alias: &NamedType{
					Args: []Type{
						&NamedType{Name: Ident{Name: "int"}},
					},
					Name: Ident{Name: "b"},
				},
			},
		},
		{
			src: "iface a := mod#b",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Alias: &NamedType{
					Mod: &Ident{Name: "mod"},
					Name: Ident{Name: "b"},
				},
			},
		},
		{
			src: "iface a := int mod#b",
			want: &IfaceDef{
				Name:      Ident{Name: "a"},
				Alias: &NamedType{
					Args: []Type{
						&NamedType{Name: Ident{Name: "int"}},
					},
					Mod: &Ident{Name: "mod"},
					Name: Ident{Name: "b"},
				},
			},
		},
		{
			src: "iface T set := (T, bool) map",
			want: &IfaceDef{
				TypeParms: []TypeVar{{Name: "T"}},
				Name:      Ident{Name: "set"},
				Alias: &NamedType{
					Args: []Type{
						TypeVar{Name: "T"},
						&NamedType{Name: Ident{Name: "bool"}},
					},
					Name: Ident{Name: "map"},
				},
			},
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			p := New()
			if err := p.Parse("", strings.NewReader(test.src)); err != nil {
				t.Fatalf("got error: %s", err)
			}
			got := p.Files[0].Defs[0]
			opts := []cmp.Option{
				cmp.FilterPath(isLoc, cmp.Ignore()),
			}
			diff := cmp.Diff(test.want, got, opts...)
			if diff != "" {
				t.Error(diff)
			}
		})
	}

}

func TestNoDoubleRef(t *testing.T) {
	const src = `
		type t &&int
	`
	if err := New().Parse("", strings.NewReader(src)); err == nil {
		t.Log(src)
		t.Fatalf("got successful parse, want error")
	}
}

func TestTypeThenIface(t *testing.T) {
	const src = `
		type T option [none?, some? T]

		iface (M, K, V) map {
			add(M, K, V)bool
		}
	`
	if err := New().Parse("", strings.NewReader(src)); err != nil {
		t.Log(src)
		t.Fatalf("got %s parse, want nil", err)
	}
}

func TestImportsOnly_NoImports(t *testing.T) {
	const src = `
		func main() {}
	`
	got, err := importsOnly("test.pea", strings.NewReader(src))
	if err != nil {
		t.Errorf("got error %s, want no error", err)
	}
	if len(got) != 0 {
		t.Errorf("got %d imports, want 0:\n%v", len(got), got)
	}
}

func TestImportsOnly(t *testing.T) {
	const src = `
		import "bar"
		Import "baz"
		import renamed "qux"
		func main() {}
	`
	got, err := importsOnly("test.pea", strings.NewReader(src))
	if err != nil {
		t.Errorf("got error %s, want no error", err)
	}
	sort.Strings(got)
	if !reflect.DeepEqual(got, []string{"bar", "baz", "qux"}) {
		t.Errorf(`got imports %v, want {"bar", "baz", "qux"}`, got)
	}
}

func isLoc(path cmp.Path) bool {
	for _, s := range path {
		if s.String() == ".L" {
			return true
		}
	}
	return false
}
