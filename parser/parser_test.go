package parser

import (
	"fmt"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestExpr(t *testing.T) {
	tests := []struct {
		expr string
		want interface{}
	}{
		{"a", Id{Name: "a"}},
		{"a123", Id{Name: "a123"}},
		{"_a123", Id{Name: "_a123"}},
		{"α", Id{Name: "α"}},
		{"test_foo", Id{Name: "test_foo"}}, // reserved word prefix

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
			&BlockLit{Exprs: []Expr{Id{Name: "x"}}},
		},
		{
			`(i int){x}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Id{Name: "i"},
						Type: &NamedType{Name: Id{Name: "int"}},
					},
				},
				Exprs: []Expr{Id{Name: "x"}},
			},
		},
		{
			`(i int, j int){x}`,
			&BlockLit{
				Parms: []FuncParm{
					{
						Name: Id{Name: "i"},
						Type: &NamedType{Name: Id{Name: "int"}},
					},
					{
						Name: Id{Name: "j"},
						Type: &NamedType{Name: Id{Name: "int"}},
					},
				},
				Exprs: []Expr{Id{Name: "x"}},
			},
		},

		{
			`[?none]`,
			&UnionLit{
				CaseVal: CaseVal{Name: Id{Name: "?none"}},
			},
		},
		{
			`[?some 5]`,
			&UnionLit{
				CaseVal: CaseVal{
					Name: Id{Name: "?some"},
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
						Name: Id{Name: ".x"},
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
						Name: Id{Name: ".x"},
						Val:  &IntLit{Text: "5"},
					},
					{
						Name: Id{Name: ".y"},
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
			`[x ?foo 6]`,
			&ArrayLit{
				Exprs: []Expr{
					&Call{
						Fun: Id{Name: "?foo"},
						Args: []Expr{
							Id{Name: "x"},
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
						Fun: Id{Name: ".y"},
						Args: []Expr{
							Id{Name: "x"},
						},
					},
				},
			},
		},

		{
			`1.y`,
			&Call{
				Fun: Id{Name: ".y"},
				Args: []Expr{
					&IntLit{Text: "1"},
				},
			},
		},
		{
			`x.y`,
			&Call{
				Fun: Id{Name: ".y"},
				Args: []Expr{
					Id{Name: "x"},
				},
			},
		},
		{
			`x.y.z`,
			&Call{
				Fun: Id{Name: ".z"},
				Args: []Expr{
					&Call{
						Fun:  Id{Name: ".y"},
						Args: []Expr{Id{Name: "x"}},
					},
				},
			},
		},

		{
			`int : foo`,
			&Convert{
				Expr: Id{Name: "foo"},
				Type: &NamedType{Name: Id{Name: "int"}},
			},
		},
		{
			`[.x int] : foo`,
			&Convert{
				Expr: Id{Name: "foo"},
				Type: &StructType{
					Fields: []FieldDef{{
						Name: Id{Name: ".x"},
						Type: &NamedType{Name: Id{Name: "int"}},
					}},
				},
			},
		},

		{
			`a*b+c=d&&true||false`,
			&Call{
				Fun: Id{Name: "||"},
				Args: []Expr{
					&Call{
						Fun: Id{Name: "&&"},
						Args: []Expr{
							&Call{
								Fun: Id{Name: "="},
								Args: []Expr{
									&Call{
										Fun: Id{Name: "+"},
										Args: []Expr{
											&Call{
												Fun: Id{Name: "*"},
												Args: []Expr{
													Id{Name: "a"},
													Id{Name: "b"},
												},
											},
											Id{Name: "c"},
										},
									},
									Id{Name: "d"},
								},
							},
							Id{Name: "true"},
						},
					},
					Id{Name: "false"},
				},
			},
		},

		{
			`x ?foo y`,
			&Call{
				Fun: Id{Name: "?foo"},
				Args: []Expr{
					Id{Name: "x"},
					Id{Name: "y"},
				},
			},
		},
		{
			`x ?foo y ?bar z ?baz a`,
			&Call{
				Fun: Id{Name: "?foo?bar?baz"},
				Args: []Expr{
					Id{Name: "x"},
					Id{Name: "y"},
					Id{Name: "z"},
					Id{Name: "a"},
				},
			},
		},
		{
			`x() ?foo y`,
			&Call{
				Fun: Id{Name: "?foo"},
				Args: []Expr{
					&Call{Fun: Id{Name: "x"}},
					Id{Name: "y"},
				},
			},
		},

		{
			`foo: x`,
			&Call{
				Fun: Id{Name: "foo:"},
				Args: []Expr{
					Id{Name: "x"},
				},
			},
		},
		{
			`foo: x bar: y baz: z`,
			&Call{
				Fun: Id{Name: "foo:bar:baz:"},
				Args: []Expr{
					Id{Name: "x"},
					Id{Name: "y"},
					Id{Name: "z"},
				},
			},
		},
		{
			`foo().bar`,
			&Call{
				Fun: Id{Name: ".bar"},
				Args: []Expr{
					&Call{Fun: Id{Name: "foo"}},
				},
			},
		},
		{
			`foo()[bar]`,
			&Call{
				Fun: Id{Name: "[]"},
				Args: []Expr{
					&Call{Fun: Id{Name: "foo"}},
					Id{Name: "bar"},
				},
			},
		},
		{
			`foo()()`,
			&Call{
				Fun: &Call{Fun: Id{Name: "foo"}},
			},
		},
		{
			`foo[bar].baz`,
			&Call{
				Fun: Id{Name: ".baz"},
				Args: []Expr{
					&Call{
						Fun: Id{Name: "[]"},
						Args: []Expr{
							Id{Name: "foo"},
							Id{Name: "bar"},
						},
					},
				},
			},
		},
		{
			`foo[bar]()`,
			&Call{
				Fun: &Call{
					Fun: Id{Name: "[]"},
					Args: []Expr{
						Id{Name: "foo"},
						Id{Name: "bar"},
					},
				},
			},
		},
		{
			`foo[bar][baz]`,
			&Call{
				Fun: Id{Name: "[]"},
				Args: []Expr{
					&Call{
						Fun: Id{Name: "[]"},
						Args: []Expr{
							Id{Name: "foo"},
							Id{Name: "bar"},
						},
					},
					Id{Name: "baz"},
				},
			},
		},
		{
			`foo.bar()`,
			&Call{
				Fun: &Call{
					Fun:  Id{Name: ".bar"},
					Args: []Expr{Id{Name: "foo"}},
				},
			},
		},
		{
			`foo.bar[baz]`,
			&Call{
				Fun: Id{Name: "[]"},
				Args: []Expr{
					&Call{
						Fun:  Id{Name: ".bar"},
						Args: []Expr{Id{Name: "foo"}},
					},
					Id{Name: "baz"},
				},
			},
		},
		{
			`foo.bar.baz`,
			&Call{
				Fun: Id{Name: ".baz"},
				Args: []Expr{
					&Call{
						Fun:  Id{Name: ".bar"},
						Args: []Expr{Id{Name: "foo"}},
					},
				},
			},
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.expr, func(t *testing.T) {
			src := fmt.Sprintf("var x := %s", test.expr)
			p := NewParser()
			if err := p.Parse("", strings.NewReader(src)); err != nil {
				t.Log(src)
				t.Fatalf("failed to parse: %s", err.Error())
			}
			got := p.Files[0].Defs[0].(*VarDef).Expr
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

func isLoc(path cmp.Path) bool {
	for _, s := range path {
		if s.String() == ".L" {
			return true
		}
	}
	return false
}
