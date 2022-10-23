package flowgraph

import (
	"errors"
	"fmt"
	"regexp"
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
			src:  "type t &u		type u [.next t]",
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
			src:  "type t &u		type u [.u &u, .t t]",
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

var comments = regexp.MustCompile("[ 	]*//.*")

// TestSimplifyCmpSwitchToComparison tests that complete switch calls
// on the built-in <=> operator can be simplified to conditional expressions
// in the case that their arguments are boolean literal blocks.
//
// This test is somewhat of a change-detector test, unfortunately.
// But also, it's job is to catch regressing changes where this crutial
// simplification is not correctly performed.
func TestSimplifyCmpSwitchToComparison(t *testing.T) {
	tests := []struct {
		otherDefs string
		src       string
		op        string
	}{
		// Various combos of const true, const false, [true?], and [false?] all work.
		{src: "1 <=> 2 less? { [false?, true?] :: [true?] } _? { [false?] }", op: "<"},
		{src: "1 <=> 2 less? { true } _? { [false?] }", op: "<"},
		{src: "1 <=> 2 less? { [false?, true?] :: [true?] } _? { false }", op: "<"},
		{src: "1 <=> 2 less? { true } _? { false }", op: "<"},

		// Non-const true and false variables do not simplify to conditionals.
		{
			otherDefs: "var varTrue := [false?, true?] :: [true?]",
			src:       "1 <=> 2 less? { varTrue } _? { false }",
			op:        "",
		},
		{
			otherDefs: "var varFalse := [false?, true?] :: [false?]",
			src:       "1 <=> 2 less? { true } _? { varFalse }",
			op:        "",
		},

		{src: "1 <=> 2 less? { true } _? { false }", op: "<"},
		{src: "1 <=> 2 _? { false } less? { true }", op: "<"},
		{src: "1 <=> 2 less? { true } equal? { false } greater? { false }", op: "<"},
		{src: "1 <=> 2 greater? { false } less? { true } equal? { false }", op: "<"},
		{src: "1 <=> 2 greater? { false } less? { true } _? { false }", op: "<"},
		{src: "1 <=> 2 greater? { false } _? { true } equal? { false }", op: "<"},

		{src: "1 <=> 2 greater? { false } _? { true }", op: "<="},
		{src: "1 <=> 2 _? { true } greater? { false }", op: "<="},
		{src: "1 <=> 2 less? { true } equal? { true } greater? { false }", op: "<="},
		{src: "1 <=> 2 greater? { false } less? { true } equal? { true }", op: "<="},
		{src: "1 <=> 2 less? { true } equal? { true } _? { false }", op: "<="},

		{src: "1 <=> 2 equal? { true } _? { false }", op: "=="},
		{src: "1 <=> 2 equal? { true } less? { false } greater? { false }", op: "=="},
		{src: "1 <=> 2 greater? { false } equal? { true } less? { false }", op: "=="},
		{src: "1 <=> 2 less? { false } greater? { false } equal? { true }", op: "=="},
		{src: "1 <=> 2 less? { false } _? { false } equal? { true }", op: "=="},
		{src: "1 <=> 2 less? { false } greater? { false } _? { true }", op: "=="},

		{src: "1 <=> 2 equal? { false } _? { true }", op: "!="},
		{src: "1 <=> 2 _? { true } equal? { false }", op: "!="},
		{src: "1 <=> 2 equal? { false } greater? { true } less? { true }", op: "!="},
		{src: "1 <=> 2 equal? { false } greater? { true } _? { true }", op: "!="},
		{src: "1 <=> 2greater? { true } _? { true } equal? { false } ", op: "!="},
		{src: "1 <=> 2 _? { false } greater? { true } less? { true }", op: "!="},

		{src: "1 <=> 2 greater? { true } _? { false }", op: ">"},
		{src: "1 <=> 2 _? { false } greater? { true }", op: ">"},
		{src: "1 <=> 2 greater? { true } equal? { false } less? { false }", op: ">"},
		{src: "1 <=> 2 less? { false } greater? { true } equal? { false }", op: ">"},
		{src: "1 <=> 2 less? { false } greater? { true } _? { false }", op: ">"},
		{src: "1 <=> 2 less? { false } _? { true } equal? { false }", op: ">"},

		{src: "1 <=> 2 less? { false } _? { true }", op: ">="},
		{src: "1 <=> 2 _? { true } less? { false }", op: ">="},
		{src: "1 <=> 2 greater? { true } equal? { true } less? { false }", op: ">="},
		{src: "1 <=> 2 less? { false } greater? { true } equal? { true }", op: ">="},
		{src: "1 <=> 2 greater? { true } equal? { true } _? { false }", op: ">="},
	}

	const template = `func "main#main()"() {
0:	in=[], out=[1]
    jump 1
1:	in=[0], out=[]
    x0 := 1
    x1 := 2
    x2 := x0 %s x1
    x3 := alloc(int64)
    store(*x3, x2)
    return
}`
	for _, test := range tests {
		t.Run(test.src, func(t *testing.T) {
			src := fmt.Sprintf(`
				const true := [false?, true?] :: [true?]
				const false := [false?, true?] :: [false?]
				%s
				func main() { %s }
			`, test.otherDefs, test.src)
			fg, err := build(src, NoOptimize)
			if err != nil {
				t.Log(src)
				t.Fatalf("failed to compile %s\n", err)
			}
			// Strip type name comments.
			fg = comments.ReplaceAllLiteralString(fg, "")
			var op string
			for _, o := range [...]string{"<", "<=", "==", "!=", ">", ">="} {
				if fg == fmt.Sprintf(template, o) {
					op = o
					break
				}
			}
			if op != test.op {
				t.Log(test.src)
				t.Log(fg)
				t.Errorf("got %s, wanted %s\n", op, test.op)
			}
		})
	}
}

// TestSimplificationOfComparisonFunctions tests that
// comparisons, <, <=, >, and >= can be implemented
// as generic functions using <=>, but still be simplified
// to single, non-branching instructions for built-in <=>
//
// It is a change-detector test, but for an important optimization
// that needs to be changed only with care.
func TestSimplificationOfComparisonFunctions(t *testing.T) {
	const src = `
		func main() {
			use(1 < 2),
			use(2 <= 3),
			use(3 > 4),
			use(4 >= 5),
		}
		Func <(a, b T) [false?, true?] : <=>(T, T)[less?, equal?, greater?] {
			return: a <=> b less? { [false?, true?] :: [true?] } _? { [false?] }
		}
		Func <=(a, b T) [false?, true?] : <=>(T, T)[less?, equal?, greater?] {
			return: a <=> b greater? { [false?, true?] :: [false?] } _? { [true?] }
		}
		Func >(a, b T) [false?, true?] : <=>(T, T)[less?, equal?, greater?] {
			return: a <=> b greater? { [false?, true?] :: [true?] } _? { [false?] }
		}
		Func >=(a, b T) [false?, true?] : <=>(T, T)[less?, equal?, greater?] {
			return: a <=> b less? { [false?, true?] :: [false?] } _? { [true?] }
		}
		func use(_ [false?, true?])
	`

	const expected = `func "main#main()"() {
0:	in=[], out=[]
    x0 := 1
    x1 := 2
    x2 := x0 < x1
    x3 := &"main#use([false?, true?])"
    x3(x2)
    x4 := 2
    x5 := 3
    x6 := x4 <= x5
    x7 := &"main#use([false?, true?])"
    x7(x6)
    x8 := 3
    x9 := 4
    x10 := x8 > x9
    x11 := &"main#use([false?, true?])"
    x11(x10)
    x12 := 4
    x13 := 5
    x14 := x12 >= x13
    x15 := &"main#use([false?, true?])"
    x15(x14)
    return
}`
	fg, err := build(src)
	if err != nil {
		t.Log(src)
		t.Fatalf("failed to compile %s\n", err)
	}
	// Strip type name comments.
	fg = comments.ReplaceAllLiteralString(fg, "")
	if fg != expected {
		t.Errorf("got\n%s\nwanted\n%s", fg, expected)
	}
}

func build(src string, opts ...Option) (string, error) {
	p := parser.New()
	if err := p.Parse("", strings.NewReader(src)); err != nil {
		return "", err
	}
	c, _, errs := checker.Check("main", p.Files)
	if len(errs) > 0 {
		return "", errs[0]
	}
	var main *FuncDef
	for _, fun := range Build(c, opts...).Funcs {
		if fun.Name == "main" {
			main = fun
			break
		}
	}
	if main == nil {
		return "", errors.New("no main")
	}
	return main.String(), nil
}
