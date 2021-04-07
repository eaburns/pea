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
			`foo: x`,
			&Call{
				Fun: Ident{Name: "foo:"},
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
