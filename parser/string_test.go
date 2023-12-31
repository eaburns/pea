package parser

import "testing"

func TestExprString(t *testing.T) {
	tests := []struct {
		src, want string
	}{
		{"a", "a"},
		{"3.14", "3.14"},
		{"-3.14", "-3.14"},
		{"+3.14", "+3.14"},
		{"+3.0e14", "+3.0e14"},
		{"3.0e-14", "3.0e-14"},
		{"3.0e+14", "3.0e+14"},
		{"3.0E-14", "3.0E-14"},
		{"3.0E+14", "3.0E+14"},
		{"+3.0E-14", "+3.0E-14"},
		{"-3.0E+14", "-3.0E+14"},
		{"123", "123"},
		{"-123", "-123"},
		{"+123", "+123"},
		{"`hello`", "`hello`"},
		{`"hello"`, `"hello"`},
		{"'a'", "'a'"},
		{"(){}", "(){}"},
		{"(i){}", "(i){}"},
		{"(i, j, k){}", "(i, j, k){}"},
		{"(i string, j int, k a b c){}", "(i string, j int, k a b c){}"},
		{"(i, j, k){k}", "(i, j, k){k}"},
		{"(i, j, k){i, j, k}", "(i, j, k){…, k}"},
		{"[x?]", "[x?]"},
		{"[x? 5]", "[x? 5]"},
		{"[.]", "[.]"},
		{"[.x 5]", "[.x 5]"},
		{"[.x 5, .y 6, .z (){}]", "[.x 5, .y 6, .z (){}]"},
		{"[]", "[]"},
		{"[1]", "[1]"},
		{"[1, 2, 3]", "[1, 2, 3]"},
		{"a#b#c", "a#b#c"},
		{"(1)", "(1)"},
		{"(((1)))", "(((1)))"},
		{"uint32 :: 5", "uint32 :: 5"},
		{"(a, b) hash_table :: new()", "(a, b) hash_table :: new()"},
		{"foo()", "foo()"},
		{"foo(1)", "foo(1)"},
		{"foo(1, 2, 3)", "foo(1, 2, 3)"},
		{"a#b#c#foo(1, 2, 3)", "a#b#c#foo(1, 2, 3)"},
		{"(x)(1, 2, 3)", "(x)(1, 2, 3)"},
		{"x[1]", "x[1]"},
		{"x[1, 2, 3]", "x[1, 2, 3]"},
		{"(x)[1, 2, 3]", "(x)[1, 2, 3]"},
		{"x.foo", "x.foo"},
		{"(x).foo", "(x).foo"},
		{"x a? y", "x a? y"},
		{"x a: y", "x a: y"},
		{"x a? y b? z", "x a? y b? z"},
		{"x a: y b: z", "x a: y b: z"},
		{"a: x", "a: x"},
		{"a: x b: y", "a: x b: y"},
		{"x l#m#n#a? y b? z", "x l#m#n#a? y b? z"},
		{"x l#m#n#a: y b: z", "x l#m#n#a: y b: z"},
		{"l#m#n#a: x b: y", "l#m#n#a: x b: y"},
		{"-x", "- x"},
		{"- -x", "- - x"},
		{"x + 1", "x + 1"},
		{"x + 1 + -b", "x + 1 + - b"},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			expr, err := ParseExpr(test.src)
			if err != nil {
				t.Fatalf("failed to parse: %s", err.Error())
			}
			if got := expr.String(); got != test.want {
				t.Errorf("«%s».String()=%s, want %s\n", test.src, got, test.want)
			}
		})
	}
}

func TestTypeString(t *testing.T) {
	tests := []struct {
		src, want string
	}{
		{"a", "a"},
		{"&a", "&a"},
		{"x a", "x a"},
		{"x y z a", "x y z a"},
		{"&x a", "&x a"},
		{"(&x) a", "(&x) a"},
		{"(x, y, z) a", "(x, y, z) a"},
		{"[int]", "[int]"},
		{"[.]", "[.]"},
		{"[.x int]", "[.x int]"},
		{"[.x int, .y a b c]", "[.x int, .y a b c]"},
		{"[x?]", "[x?]"},
		{"[x? int]", "[x? int]"},
		{"[x? int, y?, z? a b c]", "[x? int, y?, z? a b c]"},
		{"(){}", "(){}"},
		{"(i){}", "(i){}"},
		{"(int, string, a b c){}", "(int, string, a b c){}"},
		{"(){int}", "(){int}"},
		{"(){a b c}", "(){a b c}"},
		{"(int, string, a b c){a b c}", "(int, string, a b c){a b c}"},
		{"T", "T"},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			expr, err := ParseType(test.src)
			if err != nil {
				t.Fatalf("failed to parse: %s", err.Error())
			}
			if got := expr.String(); got != test.want {
				t.Errorf("«%s».String()=%s, want %s\n", test.src, got, test.want)
			}
		})
	}
}
