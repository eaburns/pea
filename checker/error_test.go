package checker

import (
	"fmt"
	"regexp"
	"testing"
)

// TestConvertError test conversion error messages.
// This is somewhat of a change detector test,
// but the only way I can think of to see that
// all of the error messages are reasonable.
func TestConvertError(t *testing.T) {
	t.Parallel()
	tests := []struct {
		src  string
		a, b string
		mode convertMode
		err  string
	}{
		{
			src: `
				type a int
				type b int
			`,
			a:    "a",
			b:    "b",
			mode: implicit,
			err: "cannot implicitly convert a to b\n" +
				"a and b are different named types\n" +
				"a is defined at .*\n" +
				"b is defined at .*$",
		},
		{
			src:  "type a int",
			a:    "a",
			b:    "[int]",
			mode: implicit,
			err: "cannot implicitly convert a to .*\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert int to .*\n" +
				"int and .* are different kinds of types\n" +
				"int is the built-in int type\n" +
				".* is an array literal type$",
		},
		{
			src:  "type a int",
			a:    "a",
			b:    "[.x int]",
			mode: implicit,
			err: "cannot implicitly convert a to .*\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert int to .*\n" +
				"int and .* are different kinds of types\n" +
				"int is the built-in int type\n" +
				".* is a struct literal type$",
		},
		{
			src:  "type a int",
			a:    "a",
			b:    "[x? int]",
			mode: implicit,
			err: "cannot implicitly convert a to .*\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert int to .*\n" +
				"int and .* are different kinds of types\n" +
				"int is the built-in int type\n" +
				".* is a union literal type$",
		},
		{
			src:  "type a int",
			a:    "a",
			b:    "(){}",
			mode: implicit,
			err: "cannot implicitly convert a to .*\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert int to .*\n" +
				"int and .* are different kinds of types\n" +
				"int is the built-in int type\n" +
				".* is a function type$",
		},
		{
			src:  "type a int",
			a:    "a",
			b:    "int",
			mode: implicit,
			err: "cannot implicitly convert a to .*\n" +
				"a and int are different named types$",
		},
		{
			src:  "type a int",
			a:    "a",
			b:    "T",
			mode: implicit,
			err: "cannot implicitly convert a to .*\n" +
				"a and T are different kinds of types\n" +
				"a is a defined type\n" +
				"T is a type variable$",
		},
		{
			src:  "type a int",
			a:    "[int]",
			b:    "a",
			mode: implicit,
			err: "cannot implicitly convert \\[int\\] to a\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert \\[int\\] to int\n" +
				"\\[int\\] and int are different kinds of types\n" +
				"\\[int\\] is an array literal type\n" +
				"int is the built-in int type$",
		},
		{
			a:    "[int]",
			b:    "[.x int]",
			mode: implicit,
			err: "cannot implicitly convert \\[int\\] to .*\n" +
				"\\[int\\] and .* are different kinds of types\n" +
				"\\[int\\] is an array literal type\n" +
				".* is a struct literal type$",
		},
		{
			a:    "[int]",
			b:    "[x? int]",
			mode: implicit,
			err: "cannot implicitly convert \\[int\\] to .*\n" +
				"\\[int\\] and .* are different kinds of types\n" +
				"\\[int\\] is an array literal type\n" +
				".* is a union literal type$",
		},
		{
			a:    "[int]",
			b:    "(){}",
			mode: implicit,
			err: "cannot implicitly convert \\[int\\] to .*\n" +
				"\\[int\\] and .* are different kinds of types\n" +
				"\\[int\\] is an array literal type\n" +
				".* is a function type$",
		},
		{
			a:    "[int]",
			b:    "int",
			mode: implicit,
			err: "cannot implicitly convert \\[int\\] to .*\n" +
				"\\[int\\] and .* are different kinds of types\n" +
				"\\[int\\] is an array literal type\n" +
				".* is the built-in int type$",
		},
		{
			a:    "[int]",
			b:    "T",
			mode: implicit,
			err: "cannot implicitly convert \\[int\\] to .*\n" +
				"\\[int\\] and .* are different kinds of types\n" +
				"\\[int\\] is an array literal type\n" +
				".* is a type variable$",
		},
		{
			src:  "type a int",
			a:    "[.x int]",
			b:    "a",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to a\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert \\[.x int\\] to int\n" +
				"\\[.x int\\] and int are different kinds of types\n" +
				"\\[.x int\\] is a struct literal type\n" +
				"int is the built-in int type$",
		},
		{
			a:    "[.x int]",
			b:    "[int]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* are different kinds of types\n" +
				"\\[.x int\\] is a struct literal type\n" +
				".* is an array literal type$",
		},
		{
			a:    "[.x int]",
			b:    "[.y int]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* have different fields$",
		},
		{
			a:    "[.x int]",
			b:    "[.x int, .y float32]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* have different fields\n" +
				"\\[.x int\\] has 1 fields\n" +
				".* has 2 fields$",
		},
		{
			a:    "[.x int]",
			b:    "[.x string]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* have different fields\n" +
				"field .x: int and string are different named types$",
		},
		{
			a:    "[.x int, .y string]",
			b:    "[.x int, .y float32]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int, .y string\\] to .*\n" +
				"\\[.x int, .y string\\] and .* have different fields\n" +
				"field .y: string and float32 are different named types$",
		},
		{
			a:    "[.x int, .y [.z int]]",
			b:    "[.x int, .y [.z string]]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int, .y \\[.z int\\]\\] to .*\n" +
				"\\[.x int, .y \\[.z int\\]\\] and .* have different fields\n" +
				"field .y: \\[.z int\\] and \\[.z string\\] have different fields\n" +
				"field .z: int and string are different named types$",
		},
		{
			a:    "[.x int]",
			b:    "[x? int]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* are different kinds of types\n" +
				"\\[.x int\\] is a struct literal type\n" +
				".* is a union literal type$",
		},
		{
			a:    "[.x int]",
			b:    "(){}",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* are different kinds of types\n" +
				"\\[.x int\\] is a struct literal type\n" +
				".* is a function type$",
		},
		{
			a:    "[.x int]",
			b:    "int",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* are different kinds of types\n" +
				"\\[.x int\\] is a struct literal type\n" +
				".* is the built-in int type$",
		},
		{
			a:    "[.x int]",
			b:    "T",
			mode: implicit,
			err: "cannot implicitly convert \\[.x int\\] to .*\n" +
				"\\[.x int\\] and .* are different kinds of types\n" +
				"\\[.x int\\] is a struct literal type\n" +
				".* is a type variable$",
		},
		{
			src:  "type a int",
			a:    "[x? int]",
			b:    "a",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to a\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert \\[x\\? int\\] to int\n" +
				"\\[x\\? int\\] and int are different kinds of types\n" +
				"\\[x\\? int\\] is a union literal type\n" +
				"int is the built-in int type$",
		},
		{
			a:    "[x? int]",
			b:    "[int]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* are different kinds of types\n" +
				"\\[x\\? int\\] is a union literal type\n" +
				".* is an array literal type$",
		},
		{
			a:    "[x? int]",
			b:    "[.x int]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* are different kinds of types\n" +
				"\\[x\\? int\\] is a union literal type\n" +
				".* is a struct literal type$",
		},
		{
			a:    "[x? int]",
			b:    "[x? int, y?]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* have different cases\n" +
				"\\[x\\? int\\] has 1 cases\n" +
				".* has 2 cases$",
		},
		{
			a:    "[x? int]",
			b:    "[x? string]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* have different cases\n" +
				"case x\\?: int and string are different named types$",
		},
		{
			a:    "[x? [y? int]]",
			b:    "[x? [y? string]]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? \\[y\\? int\\]\\] to .*\n" +
				"\\[x\\? \\[y\\? int\\]\\] and .* have different cases\n" +
				"case x\\?: \\[y\\? int\\] and .* have different cases\n" +
				"case y\\?: int and string are different named types$",
		},
		{
			a:    "[x? int]",
			b:    "[x?]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* have different cases\n" +
				"case x\\?: int and untyped$",
		},
		{
			a:    "[x?]",
			b:    "[x? int]",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\?\\] to .*\n" +
				"\\[x\\?\\] and .* have different cases\n" +
				"case x\\?: untyped and int$",
		},
		{
			a:    "[x? int]",
			b:    "(){}",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* are different kinds of types\n" +
				"\\[x\\? int\\] is a union literal type\n" +
				".* is a function type$",
		},
		{
			a:    "[x? int]",
			b:    "int",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* are different kinds of types\n" +
				"\\[x\\? int\\] is a union literal type\n" +
				".* is the built-in int type$",
		},
		{
			a:    "[x? int]",
			b:    "T",
			mode: implicit,
			err: "cannot implicitly convert \\[x\\? int\\] to .*\n" +
				"\\[x\\? int\\] and .* are different kinds of types\n" +
				"\\[x\\? int\\] is a union literal type\n" +
				".* is a type variable$",
		},
		{
			src:  "type a int",
			a:    "(){}",
			b:    "a",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){} to a\n" +
				"a is defined as int at .*\n" +
				"cannot implicitly convert \\(\\){} to int\n" +
				"\\(\\){} and int are different kinds of types\n" +
				"\\(\\){} is a function type\n" +
				"int is the built-in int type$",
		},
		{
			a:    "(){}",
			b:    "[int]",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){} to .*\n" +
				"\\(\\){} and .* are different kinds of types\n" +
				"\\(\\){} is a function type\n" +
				".* is an array literal type$",
		},
		{
			a:    "(){}",
			b:    "[.x int]",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){} to .*\n" +
				"\\(\\){} and .* are different kinds of types\n" +
				"\\(\\){} is a function type\n" +
				".* is a struct literal type$",
		},
		{
			a:    "(){}",
			b:    "[x? int]",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){} to .*\n" +
				"\\(\\){} and .* are different kinds of types\n" +
				"\\(\\){} is a function type\n" +
				".* is a union literal type$",
		},
		{
			a:    "(){string}",
			b:    "(){int}",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){string} to .*\n" +
				"\\(\\){string} and .* have different return types\n" +
				"return: string and int$",
		},
		{
			a:    "(int){}",
			b:    "(int, int){}",
			mode: implicit,
			err: "cannot implicitly convert \\(int\\){} to .*\n" +
				"\\(int\\){} and .* have different arity\n" +
				"\\(int\\){} has 1 parameters\n" +
				".* has 2 parameters$",
		},
		{
			a:    "(string){}",
			b:    "(int){}",
			mode: implicit,
			err: "cannot implicitly convert \\(string\\){} to .*\n" +
				"\\(string\\){} and .* have different parameter types\n" +
				"parameter 0: string and int$",
		},
		{
			a:    "(){}",
			b:    "int",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){} to .*\n" +
				"\\(\\){} and .* are different kinds of types\n" +
				"\\(\\){} is a function type\n" +
				".* is the built-in int type$",
		},
		{
			a:    "(){}",
			b:    "T",
			mode: implicit,
			err: "cannot implicitly convert \\(\\){} to .*\n" +
				"\\(\\){} and .* are different kinds of types\n" +
				"\\(\\){} is a function type\n" +
				".* is a type variable$",
		},
		{
			src:  "type a int",
			a:    "T",
			b:    "a",
			mode: implicit,
			err: "cannot implicitly convert T to a\n" +
				"T and a are different kinds of types\n" +
				"T is a type variable\n" +
				"a is a defined type$",
		},
		{
			a:    "T",
			b:    "[int]",
			mode: implicit,
			err: "cannot implicitly convert T to .*\n" +
				"T and .* are different kinds of types\n" +
				"T is a type variable\n" +
				".* is an array literal type$",
		},
		{
			a:    "T",
			b:    "[.x int]",
			mode: implicit,
			err: "cannot implicitly convert T to .*\n" +
				"T and .* are different kinds of types\n" +
				"T is a type variable\n" +
				".* is a struct literal type$",
		},
		{
			a:    "T",
			b:    "[x? int]",
			mode: implicit,
			err: "cannot implicitly convert T to .*\n" +
				"T and .* are different kinds of types\n" +
				"T is a type variable\n" +
				".* is a union literal type$",
		},
		{
			a:    "T",
			b:    "int",
			mode: implicit,
			err: "cannot implicitly convert T to .*\n" +
				"T and .* are different kinds of types\n" +
				"T is a type variable\n" +
				".* is the built-in int type$",
		},
		{
			a:    "T",
			b:    "(){}",
			mode: implicit,
			err: "cannot implicitly convert T to .*\n" +
				"T and .* are different kinds of types\n" +
				"T is a type variable\n" +
				".* is a function type$",
		},
		{
			a:    "T",
			b:    "U",
			mode: implicit,
			err: "cannot implicitly convert T to U\n" +
				"T and U are different type variables\n" +
				"T is defined at .*\n" +
				"U is defined at .*$",
		},

		// Test ref literal failed conversions.
		// This requires the ref literal to be not the top-level type,
		// since the top-level type can implicitly ref-convert.
		// We test it by making it a non-top-level type.
		// In this case, we choose an array element.
		{
			src:  "type a int",
			a:    "[&int]",
			b:    "[a]",
			mode: implicit,
			err: "cannot implicitly convert \\[&int\\] to .*\n" +
				"&int and .* are different kinds of types\n" +
				"&int is a reference literal type\n" +
				".* is a defined type$",
		},
		{
			a:    "[&int]",
			b:    "[[.x int]]",
			mode: implicit,
			err: "cannot implicitly convert \\[&int\\] to .*\n" +
				"&int and .* are different kinds of types\n" +
				"&int is a reference literal type\n" +
				".* is a struct literal type$",
		},
		{
			a:    "[&int]",
			b:    "[[x? int]]",
			mode: implicit,
			err: "cannot implicitly convert \\[&int\\] to .*\n" +
				"&int and .* are different kinds of types\n" +
				"&int is a reference literal type\n" +
				".* is a union literal type$",
		},
		{
			a:    "[&int]",
			b:    "[(){}]",
			mode: implicit,
			err: "cannot implicitly convert \\[&int\\] to .*\n" +
				"&int and .* are different kinds of types\n" +
				"&int is a reference literal type\n" +
				".* is a function type$",
		},
		{
			a:    "[&int]",
			b:    "[int]",
			mode: implicit,
			err: "cannot implicitly convert \\[&int\\] to .*\n" +
				"&int and .* are different kinds of types\n" +
				"&int is a reference literal type\n" +
				".* is the built-in int type$",
		},
		{
			a:    "[&int]",
			b:    "[T]",
			mode: implicit,
			err: "cannot implicitly convert \\[&int\\] to .*\n" +
				"&int and .* are different kinds of types\n" +
				"&int is a reference literal type\n" +
				".* is a type variable$",
		},

		// Multiple binding error.
		{
			a:    "[.x ?1, .y ?1]",
			b:    "[.x int, .y string]",
			mode: implicit,
			err: "cannot implicitly convert \\[.x \\?, .y \\?] to .*\n" +
				// Z0 is what the test framework internally names ?1.
				"Z0 binds int and string$",
		},

		// Recursive substitution error.
		{
			a:    "[.a ?0,		.b [.x ?1],	.c ?1,		.d [.x ?0]]",
			b:    "[.a [.x ?10],	.b ?10,	.c [.x ?11],	.d ?11]",
			mode: implicit,
			err: "cannot implicitly convert .* to .*\n" +
				// ZN is what the test framework internally names ?N.
				// TODO: stop leaking internal type variable names in tests.
				"recursive binding: Z2 -> Z1 -> Z3 -> Z0 -> Z2$",
		},

		// Test an explicit conversion.
		{
			src: `
				type a int
				type b string
			`,
			a:    "a",
			b:    "b",
			mode: explicit,
			err: "cannot convert a to b\n" +
				"a is defined as int at .*\n" +
				"cannot convert int to .*\n" +
				".* is defined as string at .*\n" +
				"cannot convert int to string\n" +
				"int and string are different named types$",
		},
	}
	for _, test := range tests {
		test := test
		name := fmt.Sprintf("convertPattern(%s, %s, %s)", test.a, test.b, test.mode)
		t.Run(name, func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			a, n := _parseTestPattern(t, mod, 0, test.a)
			b, _ := _parseTestPattern(t, mod, n, test.b)
			var bind map[*TypeParm]Type
			_, _, err := convertPattern(nil, a, b, test.mode, &bind)
			if err == nil {
				t.Fatalf("convertPattern(%s, %s, %s)=nil, want matching %s",
					a, b, test.mode, test.err)
			}
			p := makeErrorPrinter(mod.topScope)
			err.print(p)
			if got := p.String(); !regexp.MustCompile(test.err).MatchString(got) {
				t.Errorf("convertPattern(%s, %s, %s)=%s, want matching %s",
					a, b, test.mode, got, test.err)
			}
		})
	}
}
