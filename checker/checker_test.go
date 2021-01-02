package checker

import (
	"fmt"
	"regexp"
	"strings"
	"testing"
	"text/template"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type testMod struct {
	path string
	src  string
}

type testImporter struct {
	files  loc.Files
	mods   []testMod
	loaded map[string]*Mod
}

func newTestImporter(mods []testMod, files []*parser.File) *testImporter {
	var locFiles []loc.File
	for _, file := range files {
		locFiles = append(locFiles, file)
	}
	return &testImporter{
		files:  locFiles,
		mods:   mods,
		loaded: make(map[string]*Mod),
	}
}

func (imp *testImporter) Files() loc.Files { return imp.files }

func (imp *testImporter) Load(path string) (*Mod, error) {
	if mod, ok := imp.loaded[path]; ok {
		return mod, nil
	}
	var testMod *testMod
	for i := range imp.mods {
		if imp.mods[i].path == path {
			testMod = &imp.mods[i]
			break
		}
	}
	if testMod == nil {
		return nil, fmt.Errorf("%s: not found", path)
	}
	p := parser.NewParserOffset(imp.files.Len() + 1)
	err := p.Parse(testMod.path, strings.NewReader(testMod.src))
	if err != nil {
		return nil, err
	}
	imp.files = append(imp.files, p.Files[0])
	mod, _, errs := Check(testMod.path, p.Files, imp)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	mod.Imported = true
	imp.loaded[path] = mod
	return mod, nil
}

func check(path string, files []string, mods []testMod) (*Mod, []error) {
	p := parser.NewParser()
	for i, file := range files {
		r := strings.NewReader(file)
		if err := p.Parse(fmt.Sprintf("%s%d", path, i), r); err != nil {
			return nil, []error{err}
		}
	}
	imp := newTestImporter(mods, p.Files)
	mod, _, errs := Check(path, p.Files, imp)
	return mod, errs
}

func findTypeDef(t *testing.T, name string, mod *Mod) *TypeDef {
	for _, def := range mod.Defs {
		if td, ok := def.(*TypeDef); ok && td.Name == name {
			return td
		}
	}
	t.Fatalf("failed to find type definition %s", name)
	panic("impossible")
}

func findVarDef(t *testing.T, name string, mod *Mod) *VarDef {
	for _, def := range mod.Defs {
		if vd, ok := def.(*VarDef); ok && vd.Name == name {
			return vd
		}
	}
	t.Fatalf("failed to find variable definition %s", name)
	panic("impossible")
}

func TestRedef(t *testing.T) {
	tests := []struct {
		name string
		src  string
		err  string
	}{
		{
			name: "var redef",
			src: "var a := 1		var a := 1",
			err: "redefined",
		},
		{
			name: "var _ not redef",
			src: "var _ := 1		var _ := 1",
			err: "",
		},
		{
			name: "func parm redef",
			src:  "func f(a int, a float64)",
			err:  "redefined",
		},
		{
			name: "func parm _ not redef",
			src:  "func f(_ int, _ float64)",
			err:  "",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.name, func(t *testing.T) {
			t.Log(test.src)
			switch _, errs := check("test", []string{test.src}, nil); {
			case test.err == "" && len(errs) == 0:
				break
			case test.err == "" && len(errs) > 0:
				t.Errorf("unexpected error: %s", errs[0])
			case test.err != "" && len(errs) == 0:
				t.Errorf("expected error matching %s, got nil", test.err)
			case !regexp.MustCompile(test.err).MatchString(errStr(errs)):
				t.Errorf("expected error matching %s, got\n%s", test.err, errStr(errs))
			}
		})
	}
}

func TestOverloadResolution(t *testing.T) {
	tests := []struct {
		name string
		src  string
		call string
		ret  string // or ""
		want string
		err  string
	}{
		{
			name: "not callable",
			src:  "var x int",
			ret:  "int",
			call: "x()",
			err:  "not callable",
		},
		{
			name: "no functions found",
			call: "x()",
			err:  "not found",
		},
		{
			name: "arity mismatch",
			src:  "func x(i int)",
			call: "x()",
			err:  "not found",
		},
		{
			name: "expected return type mismatch",
			src:  "func x() int",
			call: "x()",
			ret:  "string",
			err:  "not found",
		},
		{
			name: "argument type mismatch",
			src:  "func x(i int)",
			call: "x(\"hello\")",
			err:  "type mismatch: got string, want int",
		},
		{
			name: "0-ary no return function found",
			src:  "func x()",
			call: "x()",
			want: "x()",
		},
		{
			name: "infer return type",
			src:  "func x() int",
			call: "x()",
			want: "x()int",
		},
		{
			name: "pick function with correct arity: 0",
			src: "func x()	func x(_ int)",
			call: "x()",
			want: "x()",
		},
		{
			name: "pick function with correct arity: 1",
			src: "func x()	func x(_ int)",
			call: "x(1)",
			want: "x(int)",
		},
		{
			name: "call a 0-ary function variable",
			src:  "var x (){}",
			call: "x()",
			want: "x",
		},
		{
			name: "call a 1-ary function variable",
			src: "var x (int){}	func x()",
			call: "x()",
			want: "x()",
		},
		{
			name: "choose matching func variable over mismatching func",
			src: "var x (int){}	func x()",
			call: "x(1)",
			want: "x",
		},
		{
			name: "ambiguous call: same exact signatures",
			src: "func x()	func x()",
			call: "x()",
			err:  "ambiguous",
		},
		{
			name: "ambiguous call: variable and function",
			src: "var x(){}	func x()",
			call: "x()",
			err:  "ambiguous",
		},
		{
			name: "arg 1 matches; pick based on arg 2",
			src: "func x(u int, s string)		func x(i int, j int)",
			call: "x(1, 2)",
			want: "x(int, int)",
		},
		{
			name: "arg 1 matches; pick based on arg 2—again",
			src: "func x(u int, s string)		func x(i int, j int)",
			call: "x(1, \"hello\")",
			want: "x(int, string)",
		},
		{
			src: "func x(u int8, s string)		func x(i int8, j int)",
			name: "arg 1 common type matches, pick based on arg 2",
			call: "x(1, 2)",
			want: "x(int8, int)",
		},
		{
			name: "arg 1 common type matches, pick based on arg 2—again",
			src: "func x(u int8, s string)		func x(i int8, j int)",
			call: "x(1, \"hello\")",
			want: "x(int8, string)",
		},
		{
			name: "arg 1 converts, pick based on arg 2?",
			src: "func x(u int, s string)		func x(i &int, j int)",
			call: "x(&int : 1, 2)",
			want: "x(&int, int)",
		},
		{
			name: "reference convert matching arg",
			src: "func x(u int, s string)		func x(i &int, j int)",
			call: "x(&int : 1, \"hello\")",
			want: "x(int, string)",
		},
		{
			name: "pick based on matching return type",
			src: "func x()int		func x()string",
			ret:  "int",
			call: "x()",
			want: "x()int",
		},
		{
			name: "pick based on matching return type—again",
			src: "func x()int		func x()string",
			ret:  "string",
			call: "x()",
			want: "x()string",
		},
		{
			name: "return type converts",
			src:  "func x()&int",
			ret:  "int",
			call: "x()",
			want: "x()&int",
		},
		{
			name: "built-in selector",
			src:  "type point [.x float64, .y float64]",
			call: "(point : [.x 4, .y 4]).x",
			want: "built-in .x(&[.x float64, .y float64])&float64",
		},
		{
			name: "built-in selector, other field",
			src:  "type point [.x float64, .y float64]",
			call: "(point : [.x 4, .y 4]).y",
			want: "built-in .y(&[.x float64, .y float64])&float64",
		},
		{
			name: "built-in selector, not a struct",
			src:  "type point int",
			call: "(point : 1).z",
			err:  "point is not a struct type",
		},
		{
			name: "built-in selector, no field",
			src:  "type point [.x float64, .y float64]",
			call: "(point : [.x 4, .y 4]).z",
			err:  "point has no field .z",
		},
		{
			name: "built-in selector, wrong return type",
			src:  "type point [.x float64, .y float64]",
			call: "(point : [.x 4, .y 4]).y",
			ret:  "string",
			err:  "type mismatch: got &float64, want string",
		},
		{
			name: "built-in selector mismatches, but func def matches",
			src: `
				type point [.x float64, .y float64]
				func .z(_ point)float64
			`,
			call: "(point : [.x 4, .y 4]).z",
			want: ".z(point)float64",
		},
		{
			name: "built-in selector matches, and func def mismatches",
			src: `
				type point [.x float64, .y float64]
				func .x(_ point)string
			`,
			call: "(point : [.x 4, .y 4]).x",
			ret:  "float64",
			want: "built-in .x(&[.x float64, .y float64])&float64",
		},
		{
			name: "built-in selector and func def ambiguity",
			src: `
				type point [.x float64, .y float64]
				func .x(_ point)float64
			`,
			call: "(point : [.x 4, .y 4]).x",
			ret:  "float64",
			err:  "ambiguous",
		},
		{
			name: "no convert between non-literal types",
			src: `
				type point_a [.x float64, .y float64]
				type point_b [.x float64, .y float64]
				func f(_ point_b)
			`,
			call: "f(point_a : [.x 1, .y 1])",
			err:  "type mismatch: got point_a, want point_b",
		},
		{
			name: "convert defined to literal type",
			src: `
				type point [.x float64, .y float64]
				func f(_ [.x float64, .y float64])
			`,
			call: "f(point : [.x 1, .y 1])",
			want: "f([.x float64, .y float64])",
		},
		{
			name: "convert literal to defined type",
			src: `
				type point [.x float64, .y float64]
				func f(_ point)
			`,
			call: "f([.x float64, .y float64] : [.x 1, .y 1])",
			want: "f(point)",
		},
		{
			name: "\"convert\" type alias to its type",
			src: `
				type point [.x float64, .y float64]
				type point_alias := point
				func f(_ point)
			`,
			call: "f(point_alias : [.x 1, .y 1])",
			want: "f(point)",
		},
		{
			name: "\"convert\" type to an alias",
			src: `
				type point [.x float64, .y float64]
				type point_alias := point
				func f(_ point_alias)
			`,
			call: "f(point : [.x 1, .y 1])",
			want: "f(point)",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.name, func(t *testing.T) {
			src := fmt.Sprintf("%s\nvar zz %s :=%s\n", test.src, test.ret, test.call)
			t.Log(src)
			mod, errs := check("test", []string{src}, nil)
			switch {
			case test.err == "" && len(errs) == 0:
				expr := findVarDef(t, "zz", mod).Expr
				call := findCall(expr)
				if call == nil {
					t.Fatalf("no call: %s", expr)
				}
				got := call.Fun.String()
				if got != test.want {
					t.Errorf("got %s, want %s", got, test.want)
				}
			case test.err == "" && len(errs) > 0:
				t.Errorf("unexpected error: %s", errs[0])
			case test.err != "" && len(errs) == 0:
				t.Errorf("expected error matching %s, got nil", test.err)
			case !regexp.MustCompile(test.err).MatchString(errStr(errs)):
				t.Errorf("expected error matching %s, got\n%s", test.err, errStr(errs))
			}
		})
	}
}

func findCall(e Expr) *Call {
	switch e := e.(type) {
	case *Call:
		return e
	case *Deref:
		return findCall(e.Expr)
	default:
		fmt.Printf("%T unimplemented", e)
		return nil
	}
}

func TestNumLiteralErrors(t *testing.T) {
	tests := []struct {
		src  string
		err  string
		mods []testMod
	}{
		{src: "var x int8 := -128", err: ""},
		{src: "var x int8 := -129", err: "underflow"},
		{src: "var x int8 := 127", err: ""},
		{src: "var x int8 := 128", err: "overflow"},
		{src: "var x int16 := -32768", err: ""},
		{src: "var x int16 := -327690", err: "underflow"},
		{src: "var x int16 := 32767", err: ""},
		{src: "var x int16 := 32768", err: "overflow"},
		{src: "var x int32 := -2147483648", err: ""},
		{src: "var x int32 := -2147483649", err: "underflow"},
		{src: "var x int32 := 2147483647", err: ""},
		{src: "var x int32 := 2147483648", err: "overflow"},
		{src: "var x int64 := -9223372036854775808", err: ""},
		{src: "var x int64 := -9223372036854775809", err: "underflow"},
		{src: "var x int64 := 9223372036854775807", err: ""},
		{src: "var x int64 := 9223372036854775808", err: "overflow"},
		{src: "var x uint8 := 0", err: ""},
		{src: "var x uint8 := -1", err: "underflow"},
		{src: "var x uint8 := 255", err: ""},
		{src: "var x uint8 := 256", err: "overflow"},
		{src: "var x uint16 := 0", err: ""},
		{src: "var x uint16 := -1", err: "underflow"},
		{src: "var x uint16 := 65535", err: ""},
		{src: "var x uint16 := 65536", err: "overflow"},
		{src: "var x uint32 := 0", err: ""},
		{src: "var x uint32 := -1", err: "underflow"},
		{src: "var x uint32 := 4294967295", err: ""},
		{src: "var x uint32 := 4294967296", err: "overflow"},
		{src: "var x uint64 := 0", err: ""},
		{src: "var x uint64 := -1", err: "underflow"},
		{src: "var x uint64 := 18446744073709551615", err: ""},
		{src: "var x uint64 := 18446744073709551616", err: "overflow"},
		{src: "var x int := 1.00", err: ""},
		{src: "var x int := 1.01", err: "truncates"},
		{src: "var x float32 := 0.0", err: ""},
		{src: "var x float32 := 3.1415926535", err: ""},
		{src: "var x float32 := 123", err: ""},
		{src: "var x float64 := 0.0", err: ""},
		{src: "var x float64 := 3.1415926535", err: ""},
		{src: "var x float64 := 123", err: ""},
		{src: "type t int var x t := 1", err: ""},
		{src: "type t int var x t := 1.00", err: ""},
		{src: "type t uint8 var x t := 256", err: "overflow"},
		{src: "type t float32 var x t := 3.14", err: ""},
		{src: "type t float32 var x t := 123", err: ""},
	}
	for _, test := range tests {
		test := test
		t.Run(test.src, func(t *testing.T) {
			_, errs := check("test", []string{test.src}, test.mods)
			switch {
			case test.err == "" && len(errs) == 0:
				break
			case test.err == "" && len(errs) > 0:
				t.Errorf("unexpected error: %s", errs[0])
			case test.err != "" && len(errs) == 0:
				t.Errorf("expected error matching %s, got nil", test.err)
			case !regexp.MustCompile(test.err).MatchString(errStr(errs)):
				t.Errorf("expected error matching %s, got\n%s", test.err, errStr(errs))
			}
		})
	}
}

func TestLiteralInference(t *testing.T) {
	tests := []struct {
		expr  string
		infer string
		want  string
		src   string
	}{
		{expr: "1.0", want: "float64"},
		{expr: "1.0", infer: "float64", want: "float64"},
		{expr: "1.0", infer: "float32", want: "float32"},
		{expr: "1.0", infer: "int", want: "int"},
		{expr: "1.0", infer: "int32", want: "int32"},
		{expr: "1.0", infer: "&float64", want: "&float64"},
		{expr: "1.0", infer: "&float32", want: "&float32"},
		{expr: "1.0", infer: "&int", want: "&int"},
		{expr: "1.0", infer: "t", want: "t", src: "type t float64"},
		{expr: "1.0", infer: "&t", want: "&t", src: "type t float64"},
		{expr: "1.0", infer: "t", want: "t", src: "type t &float64"},
		{expr: "1.0", infer: "t", want: "t", src: "type t int"},
		{expr: "1.0", infer: "&t", want: "&t", src: "type t int"},
		{expr: "1.0", infer: "t", want: "t", src: "type t &int"},
		{expr: "1.0", infer: "[int]", want: "float64"},
		{expr: "1.0", infer: "string", want: "float64"},
		{expr: "1.0", infer: "&&float32", want: "float64"},
		{expr: "1.0", infer: "&t", want: "float64", src: "type t &float32"},
		{expr: "1.0", infer: "t", want: "float64", src: "type t &&float32"},
		{expr: "1.0", infer: "float64 t", want: "float64 t", src: "type T t T"},
		{expr: "1.0", infer: "int32 t", want: "int32 t", src: "type T t T"},
		{expr: "1.0", infer: "float32 t", want: "float32 t", src: "type T t T"},
		{expr: "1.0", infer: "&float64 t", want: "&float64 t", src: "type T t T"},
		{expr: "1.0", infer: "&&float64 t", want: "float64", src: "type T t T"},
		{expr: "1.0", infer: "float64 t", want: "float64 t", src: "type T t &T"},
		{expr: "1.0", infer: "&float64 t", want: "float64", src: "type T t &T"},
		{expr: "1.0", infer: "string t", want: "float64", src: "type T t T"},
		{expr: "1.0", infer: "int t", want: "float64", src: "type T t [T]"},

		{expr: "1", want: "int"},
		{expr: "1", infer: "int32", want: "int32"},
		{expr: "1", infer: "uint32", want: "uint32"},
		{expr: "1", infer: "float32", want: "float32"},
		{expr: "1", infer: "&int", want: "&int"},
		{expr: "1", infer: "&float32", want: "&float32"},
		{expr: "1", infer: "t", want: "t", src: "type t int16"},
		{expr: "1", infer: "&t", want: "&t", src: "type t int16"},
		{expr: "1", infer: "t", want: "t", src: "type t &int16"},
		{expr: "1", infer: "t", want: "t", src: "type t float32"},
		{expr: "1", infer: "&t", want: "&t", src: "type t float32"},
		{expr: "1", infer: "t", want: "t", src: "type t &float32"},
		{expr: "1", infer: "&t", want: "int", src: "type t &int16"},
		{expr: "1", infer: "&&int32", want: "int"},
		{expr: "1", infer: "&&float32", want: "int"},
		{expr: "1", infer: "string", want: "int"},
		{expr: "1", infer: "[int]", want: "int"},
		{expr: "1", infer: "int t", want: "int t", src: "type T t T"},
		{expr: "1", infer: "int32 t", want: "int32 t", src: "type T t T"},
		{expr: "1", infer: "float32 t", want: "float32 t", src: "type T t T"},
		{expr: "1", infer: "&int32 t", want: "&int32 t", src: "type T t T"},
		{expr: "1", infer: "&&int32 t", want: "int", src: "type T t T"},
		{expr: "1", infer: "int32 t", want: "int32 t", src: "type T t &T"},
		{expr: "1", infer: "&int32 t", want: "int", src: "type T t &T"},
		{expr: "1", infer: "string t", want: "int", src: "type T t T"},
		{expr: "1", infer: "int t", want: "int", src: "type T t [T]"},

		{expr: "'a'", want: "int"}, // TODO: should be int32
		{expr: "'a'", infer: "int32", want: "int32"},

		{expr: `"abc"`, want: "string"},
		{expr: `"abc"`, infer: "&string", want: "&string"},
		{expr: `"abc"`, infer: "t", want: "t", src: "type t string"},
		{expr: `"abc"`, infer: "&t", want: "&t", src: "type t string"},
		{expr: `"abc"`, infer: "t", want: "t", src: "type t &string"},
		{expr: `"abc"`, infer: "&t", want: "string", src: "type t &string"},
		{expr: `"abc"`, infer: "int", want: "string"},
		{expr: `"abc"`, infer: "string t", want: "string t", src: "type T t T"},
		{expr: `"abc"`, infer: "&string t", want: "&string t", src: "type T t T"},
		{expr: `"abc"`, infer: "&&string t", want: "string", src: "type T t T"},
		{expr: `"abc"`, infer: "string t", want: "string t", src: "type T t &T"},
		{expr: `"abc"`, infer: "&string t", want: "string", src: "type T t &T"},
		{expr: `"abc"`, infer: "string t", want: "string", src: "type T t [T]"},

		{expr: "(){}", want: "(){}"},
		{expr: "(i int){}", want: "(int){}"},
		{expr: "(){1}", want: "(){int}"},
		{expr: "(i int){1}", want: "(int){int}"},
		{expr: "(i int, s string){1}", want: "(int, string){int}"},
		{expr: "(){}", infer: "(){}", want: "(){}"},
		{expr: "(){}", infer: "&(){}", want: "&(){}"},
		{expr: "(i int, s string){1}", infer: "(int, string){int}", want: "(int, string){int}"},
		{expr: "(i int, s string){1}", infer: "&(int, string){int}", want: "&(int, string){int}"},
		{expr: "(){1}", infer: "(){int32}", want: "(){int32}"},
		{expr: "(){1}", infer: "(){float32}", want: "(){float32}"},
		{expr: "(){1}", infer: "(){&int}", want: "(){&int}"},
		{expr: "(){1}", infer: "(){&float32}", want: "(){&float32}"},
		{expr: "(i int){1}", infer: "&(int){int}", want: "&(int){int}"},
		{expr: "(i int){1}", infer: "&&(int){int}", want: "(int){int}"},
		{expr: "(){1}", infer: "(){t}", want: "(){t}", src: "type t int"},
		{expr: "(){1}", infer: "(){&t}", want: "(){&t}", src: "type t int"},
		{expr: "(){1}", infer: "(){t}", want: "(){t}", src: "type t &int"},
		{expr: "(){1}", infer: "(){t}", want: "(){t}", src: "type t [int]"},
		{expr: "(){1}", infer: "t", want: "t", src: "type t (){int}"},
		{expr: "(){1}", infer: "t", want: "t", src: "type t (){int32}"},
		{expr: "(){1}", infer: "&t", want: "&t", src: "type t (){int}"},
		{expr: "(){1}", infer: "t", want: "t", src: "type t &(){int}"},
		{expr: "(){1}", infer: "&t", want: "(){int}", src: "type t &(){int}"},
		{expr: "(){1}", infer: "string", want: "(){int}"},
		{expr: "(){1}", infer: "int t", want: "int t", src: "type T t (){T}"},
		{expr: "(){1}", infer: "int32 t", want: "int32 t", src: "type T t (){T}"},
		{expr: "(){1}", infer: "float64 t", want: "float64 t", src: "type T t (){T}"},
		{expr: "(){1}", infer: "&float64 t", want: "&float64 t", src: "type T t (){T}"},
		{expr: "(){1}", infer: "&&float64 t", want: "(){int}", src: "type T t (){T}"},
		{expr: "(){1}", infer: "float64 t", want: "float64 t", src: "type T t &(){T}"},
		{expr: "(){1}", infer: "&float64 t", want: "(){int}", src: "type T t &(){T}"},
		{expr: "(i int){}", infer: "int t", want: "int t", src: "type T t (T){}"},
		{expr: "(i int){}", infer: "int32 t", want: "(int){}", src: "type T t (T){}"},
		{expr: "(i int){}", infer: "&int t", want: "&int t", src: "type T t (T){}"},
		{expr: "(i int){}", infer: "&&int t", want: "(int){}", src: "type T t (T){}"},
		{expr: "(i int){}", infer: "int t", want: "int t", src: "type T t &(T){}"},
		{expr: "(i int){}", infer: "&int t", want: "(int){}", src: "type T t &(T){}"},
		{expr: "(){1}", infer: "int t", want: "(){int}", src: "type T t (T){T}"},
		{expr: "(i int){1}", infer: "int t", want: "int t", src: "type T t (T){T}"},
		{expr: "(s string){1}", infer: "int t", want: "(string){int}", src: "type T t (T){T}"},
		{expr: "(i int, s string){1}", infer: "int t", want: "(int, string){int}", src: "type T t (T){T}"},
		{expr: `(i int){"foo"}`, infer: "int t", want: "int t", src: "type T t (T){T}"},

		{expr: "[?none]", want: "[?none]"},
		{expr: "[?none]", infer: "&&[?none]", want: "[?none]"},
		{expr: "[?none]", infer: "&[?none]", want: "&[?none]"},
		{expr: "[?none]", infer: "t", want: "t", src: "type t [?none, ?some int]"},
		{expr: "[?none]", infer: "&t", want: "&t", src: "type t [?none, ?some int]"},
		{expr: "[?none]", infer: "&&t", want: "[?none]", src: "type t [?none, ?some int]"},
		{expr: "[?none]", infer: "t", want: "t", src: "type t &[?none, ?some int]"},
		{expr: "[?none]", infer: "&t", want: "[?none]", src: "type t &[?none, ?some int]"},
		{expr: "[?some 1]", want: "[?some int]"},
		{expr: "[?some (i int){1.0}]", want: "[?some (int){float64}]"},
		{expr: "[?a 1]", infer: "[?a int32]", want: "[?a int32]"},
		{expr: "[?a 1]", infer: "&[?a int32]", want: "&[?a int32]"},
		{expr: "[?a 1]", infer: "&&[?a int32]", want: "[?a int]"},
		{expr: "[?a 1]", infer: "[?a int32, ?b, ?c int]", want: "[?a int32, ?b, ?c int]"},
		{expr: "[?a 1]", infer: "[?b, ?a int32, ?c int]", want: "[?b, ?a int32, ?c int]"},
		{expr: "[?a 1]", infer: "[?b, ?c int, ?a int32]", want: "[?b, ?c int, ?a int32]"},
		{expr: "[?a 1]", infer: "&[?b, ?c int, ?a int32]", want: "&[?b, ?c int, ?a int32]"},
		{expr: "[?a 1]", infer: "&&[?b, ?c int, ?a int32]", want: "[?a int]"},
		{expr: "[?a 1]", infer: "[?b, ?c int]", want: "[?a int]"},
		{expr: "[?a 1]", infer: "[?b, ?c int, ?a]", want: "[?a int]"},
		{expr: "[?a 1]", infer: "[?b, ?c int, ?a string]", want: "[?b, ?c int, ?a string]"},
		{expr: "[?a 1]", infer: "t", want: "t", src: "type t [?a int]"},
		{expr: "[?a 1]", infer: "t", want: "t", src: "type t [?a int, ?b]"},
		{expr: "[?a 1]", infer: "t", want: "t", src: "type t [?a int, ?b, ?c int]"},
		{expr: "[?a 1]", infer: "t", want: "t", src: "type t [?b, ?a int, ?c int]"},
		{expr: "[?a 1]", infer: "t", want: "t", src: "type t [?a int32, ?b]"},
		{expr: "[?a 1]", infer: "&t", want: "&t", src: "type t [?a int, ?b]"},
		{expr: "[?a 1]", infer: "&&t", want: "[?a int]", src: "type t [?a int, ?b]"},
		{expr: "[?a 1]", infer: "t", want: "t", src: "type t &[?a int, ?b]"},
		{expr: "[?a 1]", infer: "&t", want: "[?a int]", src: "type t &[?a int, ?b]"},
		{expr: "[?a 1]", infer: "t", want: "[?a int]", src: "type t [?c int, ?b]"},
		{expr: "[?a 1]", infer: "t", want: "[?a int]", src: "type t [?a, ?b]"},
		{expr: "[?some 1]", infer: "int opt", want: "int opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", infer: "int32 opt", want: "int32 opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", infer: "&int32 opt", want: "&int32 opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", infer: "&&int32 opt", want: "[?some int]", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", infer: "string opt", want: "string opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some]", infer: "int opt", want: "[?some]", src: "type T opt [?none, ?some T]"},
		{expr: "[?none]", infer: "int opt", want: "int opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?none]", infer: "&int opt", want: "&int opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?none]", infer: "&&int opt", want: "[?none]", src: "type T opt [?none, ?some T]"},

		{expr: `[.x "hello"]`, want: "[.x string]"},
		{expr: "[.x 5]", want: "[.x int]"},
		{expr: "[.x [.y 5]]", want: "[.x [.y int]]"},
		{expr: "[.x 5]", infer: "[.x int32]", want: "[.x int32]"},
		{expr: "[.x 5]", infer: "[.x string]", want: "[.x string]"},
		{expr: "[.x 5]", infer: "&[.x int32]", want: "&[.x int32]"},
		{expr: "[.x 5]", infer: "&&[.x int32]", want: "[.x int]"},
		{expr: "[.x 5]", infer: "[.x &int]", want: "[.x &int]"},
		{expr: "[.x 5, .y 1]", infer: "[.x int8, .y float32]", want: "[.x int8, .y float32]"},
		{expr: "[.x 5, .z 1]", infer: "[.x int8, .y float32]", want: "[.x int, .z int]"},
		{expr: "[.x 5, .y 1]", infer: "[.y int8, .x float32]", want: "[.x int, .y int]"},
		{expr: "[.x 5]", infer: "[.x int8, .y float32]", want: "[.x int]"},
		{expr: "[.x 5, .y 1]", infer: "[.x int8]", want: "[.x int, .y int]"},
		{expr: "[.x 5, .y 1]", infer: "[.x int8, .y string]", want: "[.x int8, .y string]"},
		{expr: "[.x 5]", infer: "t", want: "t", src: "type t [.x int]"},
		{expr: "[.x 5]", infer: "t", want: "t", src: "type t [.x int32]"},
		{expr: "[.x 5]", infer: "&t", want: "&t", src: "type t [.x int32]"},
		{expr: "[.x 5]", infer: "&&t", want: "[.x int]", src: "type t [.x int32]"},
		{expr: "[.x 5]", infer: "t", want: "[.x int]", src: "type t [.x int, .y int]"},
		{expr: "[.x 5, .y 4]", infer: "t", want: "[.x int, .y int]", src: "type t [.x int]"},
		{expr: "[.x 5, .y 4]", infer: "t", want: "[.x int, .y int]", src: "type t [.y int, .x int]"},
		{expr: "[.x 5]", infer: "t", want: "t", src: "type t &[.x int32]"},
		{expr: "[.x 5]", infer: "&t", want: "[.x int]", src: "type t &[.x int32]"},
		{expr: "[.x 5]", infer: "int8 t", want: "int8 t", src: "type T t [.x T]"},
		{expr: "[.x 5]", infer: "&int8 t", want: "&int8 t", src: "type T t [.x T]"},
		{expr: "[.x 5]", infer: "&&int8 t", want: "[.x int]", src: "type T t [.x T]"},
		{expr: "[.x 5]", infer: "int8 t", want: "int8 t", src: "type T t &[.x T]"},
		{expr: "[.x 5]", infer: "&int8 t", want: "[.x int]", src: "type T t &[.x T]"},
		{expr: `[.x 5, .y "x"]`, infer: "(int, string) pair", want: "(int, string) pair", src: "type (X, Y) pair [.x X, .y Y]"},
		{expr: `[.x 5, .y "x"]`, infer: "(int8, string) pair", want: "(int8, string) pair", src: "type (X, Y) pair [.x X, .y Y]"},

		{expr: "[5]", want: "[int]"},
		{expr: `["hello"]`, want: "[string]"},
		{expr: "[[[5]]]", want: "[[[int]]]"},
		{expr: "[[[.x 4]]]", want: "[[[.x int]]]"},
		{expr: "[5]", infer: "[int8]", want: "[int8]"},
		{expr: "[5]", infer: "int8", want: "[int]"},
		{expr: "[]", infer: "[float32]", want: "[float32]"},
		{expr: "[5]", infer: "[int]", want: "[int]"},
		{expr: "[5]", infer: "&[int]", want: "&[int]"},
		{expr: "[5]", infer: "&&[int]", want: "[int]"},
		{expr: "[5]", infer: "t", want: "t", src: "type t [int]"},
		{expr: "[5]", infer: "&t", want: "&t", src: "type t [int]"},
		{expr: "[5]", infer: "t", want: "t", src: "type t &[int]"},
		{expr: "[5]", infer: "&t", want: "[int]", src: "type t &[int]"},
		{expr: "[5]", infer: "int t", want: "int t", src: "type T t [T]"},
	}
	for _, test := range tests {
		test := test
		name := test.expr
		if test.infer != "" {
			name = test.infer + " : " + name
		}
		t.Run(name, func(t *testing.T) {
			src := fmt.Sprintf("%s\ntype infer %s\ntype want %s\n",
				test.src, test.infer, test.want)
			t.Log(src)
			mod, errs := check("test", []string{src}, nil)
			if len(errs) > 0 {
				t.Fatal("failed to parse and check:", errStr(errs))
			}
			parserExpr, err := parser.ParseExpr(test.expr)
			if err != nil {
				t.Fatalf("failed to parse [%s]: %s", test.expr, err)
			}
			infer := findTypeDef(t, "infer", mod).Type
			// Ignore the error. Many of the test cases are type mismatches.
			// That's fine. Here we are testing the resulting literal type,
			// not correct reporting of type mismatch.
			expr, _ := checkExpr(mod.Files[0], parserExpr, infer)
			want := findTypeDef(t, "want", mod).Type
			if !eq(expr.Type(), want) {
				t.Errorf("got %s, want %s", expr.Type(), want)
			}
		})
	}
}

func TestLiteralType(t *testing.T) {
	tests := []struct {
		src string
		typ string
		lit string
	}{
		{typ: "int", lit: "int"},
		{typ: "[int]", lit: "[int]"},
		{typ: "[.x int]", lit: "[.x int]"},
		{typ: "[?x int]", lit: "[?x int]"},
		{typ: "(int){int}", lit: "(int){int}"},
		{typ: "&int", lit: "&int"},
		{typ: "&&int", lit: "&&int"},
		{src: "type t int", typ: "t", lit: "int"},
		{src: "type t int", typ: "&t", lit: "&int"},
		{src: "type t &int", typ: "t", lit: "&int"},
	}
	for _, test := range tests {
		test := test
		t.Run(test.typ, func(t *testing.T) {
			src := fmt.Sprintf("%s\ntype got %s\ntype lit %s\n",
				test.src, test.typ, test.lit)
			t.Log(src)
			mod, errs := check("test", []string{src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to check: %s", errStr(errs))
			}
			got := literal(findTypeDef(t, "got", mod).Type)
			lit := findTypeDef(t, "lit", mod).Type
			if !eq(got, lit) {
				t.Errorf("got literal %s, want %s", got, lit)
			}
		})
	}
}

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
				"[.x int, .y int]",
				"[?none, ?some int]",
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
				"[.x int, .y int]",
				"[?none, ?some int]",
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
				type named_struct [.x int]
				type named_struct_alias := named_struct
				type bar [.x int]
			`,
			Typ: "named_struct",
			Same: []string{
				"named_struct",
				"named_struct_alias",
			},
			Diff: []string{
				"bar",
				"[.x int]",
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
				"[.x int, .y int]",
				"[?none, ?some int]",
				"(int){float32}",
			},
		},
		{
			Typ:  "[int]",
			Same: []string{"[int]"},
			Diff: []string{
				"[?int]",
				"&[int]",
				"[float32]",
				"[.y int]",
				"int32",
			},
		},
		{
			Typ:  "[.x int]",
			Same: []string{"[.x int]"},
			Diff: []string{
				"&[.x int]",
				"[.x int, .y int]",
				"[.y int]",
				"int32",
				"float32",
				"[.x int, .y int]",
				"[?none, ?some int]",
				"(int){float32}",
			},
		},
		{
			Typ:  "[.x int, .y int]",
			Same: []string{"[.x int, .y int]"},
			Diff: []string{"[.y int, .x int]"},
		},
		{
			Typ:  "[?none, ?some int]",
			Same: []string{"[?none, ?some int]"},
			Diff: []string{
				"&[?none, ?some int]",
				"[?none int, ?some]",
				"[?some int, ?none]",
				"[.none int, .some int]",
				"[.y int]",
				"int32",
				"float32",
				"[.x int, .y int]",
				"(int){float32}",
			},
		},
		{
			Typ:  "[?a int, ?b int]",
			Diff: []string{"[.a int, .b int]"},
		},
		{
			Typ:  "(){}",
			Same: []string{"(){}"},
			Diff: []string{
				"&(){}",
				"(int){}",
				"(){int}",
				"(int){int}",
				"[.y int]",
				"int32",
				"[.x int, .y int]",
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
				"[.y int]",
				"int32",
				"[.x int, .y int]",
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
				"[.y int]",
				"int32",
				"[.x int, .y int]",
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
				"[.y int]",
				"int32",
				"[.x int, .y int]",
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
				"[.y int]",
				"int32",
				"[.x int, .y int]",
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
				type three := [.x int, .y int]
			`,
			Typ: "one",
			Same: []string{
				"two",
				"three",
				"[.x int, .y int]",
			},
		},
		{
			Src: `
				type (X, Y) reverse := (Y, X) forward
				type (X, Y) forward := [.x X, .y Y]
			`,
			Typ: "(string, int) reverse",
			Same: []string{
				"(string, int) reverse",
				"(int, string) forward",
				"[.x int, .y string]",
			},
			Diff: []string{
				"(int, string) reverse",
				"(string, int) forward",
				"[.x string, .y int]",
			},
		},
		{
			Src: `
				type (K, V) map [.k K, .v V]
				type V string_map := (string, V) map
			`,
			Typ: "int string_map",
			Same: []string{
				"(string, int) map",
				"int string_map",
			},
			Diff: []string{
				"[.k string, .v int]",
			},
		},
		{
			Src: `
				type foo := baz
				type bar := [.x foo, .y [.z foo]]
				type baz := int
			`,
			Typ: "bar",
			Same: []string{
				"bar",
				"[.x foo, .y [.z foo]]",
				"[.x baz, .y [.z baz]]",
				"[.x int, .y [.z int]]",
			},
		},
		{
			Src: `
				type T foo := T bar
				type T baz := [.x T]
				type T bar := T baz
				type V qux := [.x V foo]
			`,
			Typ: "int qux",
			Same: []string{
				"int qux",
				"[.x int foo]",
				"[.x int bar]",
				"[.x int baz]",
				"[.x [.x int]]",
			},
		},
		{
			Src: `
				type T foo := [.x T]
				type T bar [.x T]
			`,
			Typ: "int foo bar",
			Same: []string{
				"int foo bar",
				"[.x int] bar",
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
				src:  "type T other_type := [.x T]",
			},
			Typ: "int cross_mod_alas",
			Same: []string{
				"int other#other_type",
				"[.x int]",
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
			typ := findVarDef(t, "x", mod).T
			for i := range test.Same {
				s := findVarDef(t, fmt.Sprintf("s%d", i), mod).T
				if !eq(typ, s) {
					t.Errorf("%s != %s", typ, s)
				}
			}
			for i := range test.Diff {
				d := findVarDef(t, fmt.Sprintf("d%d", i), mod).T
				if eq(typ, d) {
					t.Errorf("%s = %s", typ, d)
				}
			}
		})
	}
}

func errStr(errs []error) string {
	var s strings.Builder
	for i, err := range errs {
		if i > 0 {
			s.WriteRune('\n')
		}
		s.WriteString(err.Error())
	}
	return s.String()
}
