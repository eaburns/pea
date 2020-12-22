package checker

import (
	"fmt"
	"regexp"
	"strings"
	"testing"

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

func TestErrors(t *testing.T) {
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
		expr string
		sug string
		want string
		src string
	}{
		{expr: "1.0", want: "float64"},
		{expr: "1.0", sug: "float64", want: "float64"},
		{expr: "1.0", sug: "float32", want: "float32"},
		{expr: "1.0", sug: "int", want: "int"},
		{expr: "1.0", sug: "int32", want: "int32"},
		{expr: "1.0", sug: "&float64", want: "&float64"},
		{expr: "1.0", sug: "&float32", want: "&float32"},
		{expr: "1.0", sug: "&int", want: "&int"},
		{expr: "1.0", sug: "t", want: "t", src: "type t float64"},
		{expr: "1.0", sug: "&t", want: "&t", src: "type t float64"},
		{expr: "1.0", sug: "t", want: "t", src: "type t &float64"},
		{expr: "1.0", sug: "t", want: "t", src: "type t int"},
		{expr: "1.0", sug: "&t", want: "&t", src: "type t int"},
		{expr: "1.0", sug: "t", want: "t", src: "type t &int"},
		{expr: "1.0", sug: "[int]", want: "float64"},
		{expr: "1.0", sug: "string", want: "float64"},
		{expr: "1.0", sug: "&&float32", want: "float64"},
		{expr: "1.0", sug: "&t", want: "float64", src: "type t &float32"},
		{expr: "1.0", sug: "t", want: "float64", src: "type t &&float32"},
		{expr: "1.0", sug: "float64 t", want: "float64 t", src: "type T t T"},
		{expr: "1.0", sug: "int32 t", want: "int32 t", src: "type T t T"},
		{expr: "1.0", sug: "float32 t", want: "float32 t", src: "type T t T"},
		{expr: "1.0", sug: "&float64 t", want: "&float64 t", src: "type T t T"},
		{expr: "1.0", sug: "&&float64 t", want: "float64", src: "type T t T"},
		{expr: "1.0", sug: "float64 t", want: "float64 t", src: "type T t &T"},
		{expr: "1.0", sug: "&float64 t", want: "float64", src: "type T t &T"},
		{expr: "1.0", sug: "string t", want: "float64", src: "type T t T"},
		{expr: "1.0", sug: "int t", want: "float64", src: "type T t [T]"},

		{expr: "1", want: "int"},
		{expr: "1", sug: "int32", want: "int32"},
		{expr: "1", sug: "uint32", want: "uint32"},
		{expr: "1", sug: "float32", want: "float32"},
		{expr: "1", sug: "&int", want: "&int"},
		{expr: "1", sug: "&float32", want: "&float32"},
		{expr: "1", sug: "t", want: "t", src: "type t int16"},
		{expr: "1", sug: "&t", want: "&t", src: "type t int16"},
		{expr: "1", sug: "t", want: "t", src: "type t &int16"},
		{expr: "1", sug: "t", want: "t", src: "type t float32"},
		{expr: "1", sug: "&t", want: "&t", src: "type t float32"},
		{expr: "1", sug: "t", want: "t", src: "type t &float32"},
		{expr: "1", sug: "&t", want: "int", src: "type t &int16"},
		{expr: "1", sug: "&&int32", want: "int"},
		{expr: "1", sug: "&&float32", want: "int"},
		{expr: "1", sug: "string", want: "int"},
		{expr: "1", sug: "[int]", want: "int"},
		{expr: "1", sug: "int t", want: "int t", src: "type T t T"},
		{expr: "1", sug: "int32 t", want: "int32 t", src: "type T t T"},
		{expr: "1", sug: "float32 t", want: "float32 t", src: "type T t T"},
		{expr: "1", sug: "&int32 t", want: "&int32 t", src: "type T t T"},
		{expr: "1", sug: "&&int32 t", want: "int", src: "type T t T"},
		{expr: "1", sug: "int32 t", want: "int32 t", src: "type T t &T"},
		{expr: "1", sug: "&int32 t", want: "int", src: "type T t &T"},
		{expr: "1", sug: "string t", want: "int", src: "type T t T"},
		{expr: "1", sug: "int t", want: "int", src: "type T t [T]"},

		{expr: "'a'", want: "int"}, // TODO: should be int32
		{expr: "'a'", sug: "int32", want: "int32"},

		{expr: `"abc"`, want: "string"},
		{expr: `"abc"`, sug: "&string", want: "&string"},
		{expr: `"abc"`, sug: "t", want: "t", src: "type t string"},
		{expr: `"abc"`, sug: "&t", want: "&t", src: "type t string"},
		{expr: `"abc"`, sug: "t", want: "t", src: "type t &string"},
		{expr: `"abc"`, sug: "&t", want: "string", src: "type t &string"},
		{expr: `"abc"`, sug: "int", want: "string"},
		{expr: `"abc"`, sug: "string t", want: "string t", src: "type T t T"},
		{expr: `"abc"`, sug: "&string t", want: "&string t", src: "type T t T"},
		{expr: `"abc"`, sug: "&&string t", want: "string", src: "type T t T"},
		{expr: `"abc"`, sug: "string t", want: "string t", src: "type T t &T"},
		{expr: `"abc"`, sug: "&string t", want: "string", src: "type T t &T"},
		{expr: `"abc"`, sug: "string t", want: "string", src: "type T t [T]"},

		{expr: "(){}", want: "(){}"},
		{expr: "(i int){}", want: "(int){}"},
		{expr: "(){1}", want: "(){int}"},
		{expr: "(i int){1}", want: "(int){int}"},
		{expr: "(i int, s string){1}", want: "(int, string){int}"},
		{expr: "(){}", sug: "(){}", want: "(){}"},
		{expr: "(){}", sug: "&(){}", want: "&(){}"},
		{expr: "(i int, s string){1}", sug: "(int, string){int}", want: "(int, string){int}"},
		{expr: "(i int, s string){1}", sug: "&(int, string){int}", want: "&(int, string){int}"},
		{expr: "(){1}", sug: "(){int32}", want: "(){int32}"},
		{expr: "(){1}", sug: "(){float32}", want: "(){float32}"},
		{expr: "(){1}", sug: "(){&int}", want: "(){&int}"},
		{expr: "(){1}", sug: "(){&float32}", want: "(){&float32}"},
		{expr: "(i int){1}", sug: "&(int){int}", want: "&(int){int}"},
		{expr: "(i int){1}", sug: "&&(int){int}", want: "(int){int}"},
		{expr: "(){1}", sug: "(){t}", want: "(){t}", src: "type t int"},
		{expr: "(){1}", sug: "(){&t}", want: "(){&t}", src: "type t int"},
		{expr: "(){1}", sug: "(){t}", want: "(){t}", src: "type t &int"},
		{expr: "(){1}", sug: "(){t}", want: "(){int}", src: "type t [int]"},
		{expr: "(){1}", sug: "t", want: "t", src: "type t (){int}"},
		{expr: "(){1}", sug: "t", want: "t", src: "type t (){int32}"},
		{expr: "(){1}", sug: "&t", want: "&t", src: "type t (){int}"},
		{expr: "(){1}", sug: "t", want: "t", src: "type t &(){int}"},
		{expr: "(){1}", sug: "&t", want: "(){int}", src: "type t &(){int}"},
		{expr: "(){1}", sug: "string", want: "(){int}"},
		{expr: "(){1}", sug: "int t", want: "int t", src: "type T t (){T}"},
		{expr: "(){1}", sug: "int32 t", want: "int32 t", src: "type T t (){T}"},
		{expr: "(){1}", sug: "float64 t", want: "float64 t", src: "type T t (){T}"},
		{expr: "(){1}", sug: "&float64 t", want: "&float64 t", src: "type T t (){T}"},
		{expr: "(){1}", sug: "&&float64 t", want: "(){int}", src: "type T t (){T}"},
		{expr: "(){1}", sug: "float64 t", want: "float64 t", src: "type T t &(){T}"},
		{expr: "(){1}", sug: "&float64 t", want: "(){int}", src: "type T t &(){T}"},
		{expr: "(i int){}", sug: "int t", want: "int t", src: "type T t (T){}"},
		{expr: "(i int){}", sug: "int32 t", want: "(int){}", src: "type T t (T){}"},
		{expr: "(i int){}", sug: "&int t", want: "&int t", src: "type T t (T){}"},
		{expr: "(i int){}", sug: "&&int t", want: "(int){}", src: "type T t (T){}"},
		{expr: "(i int){}", sug: "int t", want: "int t", src: "type T t &(T){}"},
		{expr: "(i int){}", sug: "&int t", want: "(int){}", src: "type T t &(T){}"},
		{expr: "(i int){}", sug: "int t", want: "(int){}", src: "type T t (T){T}"},
		{expr: "(){1}", sug: "int t", want: "(){int}", src: "type T t (T){T}"},
		{expr: "(i int){}", sug: "int t", want: "(int){}", src: "type T t (T){T}"},
		{expr: "(i int){1}", sug: "int t", want: "int t", src: "type T t (T){T}"},
		{expr: "(s string){1}", sug: "int t", want: "(string){int}", src: "type T t (T){T}"},
		{expr: "(i int, s string){1}", sug: "int t", want: "(int, string){int}", src: "type T t (T){T}"},
		{expr: `(i int){"foo"}`, sug: "int t", want: "(int){string}", src: "type T t (T){T}"},

		{expr: "[?none]", want: "[?none]"},
		{expr: "[?none]", sug: "&&[?none]", want: "[?none]"},
		{expr: "[?none]", sug: "&[?none]", want: "&[?none]"},
		{expr: "[?none]", sug: "t", want: "t", src: "type t [?none, ?some int]"},
		{expr: "[?none]", sug: "&t", want: "&t", src: "type t [?none, ?some int]"},
		{expr: "[?none]", sug: "&&t", want: "[?none]", src: "type t [?none, ?some int]"},
		{expr: "[?none]", sug: "t", want: "t", src: "type t &[?none, ?some int]"},
		{expr: "[?none]", sug: "&t", want: "[?none]", src: "type t &[?none, ?some int]"},
		{expr: "[?some 1]", want: "[?some int]"},
		{expr: "[?some (i int){1.0}]", want: "[?some (int){float64}]"},
		{expr: "[?a 1]", sug: "[?a int32]", want: "[?a int32]"},
		{expr: "[?a 1]", sug: "&[?a int32]", want: "&[?a int32]"},
		{expr: "[?a 1]", sug: "&&[?a int32]", want: "[?a int]"},
		{expr: "[?a 1]", sug: "[?a int32, ?b, ?c int]", want: "[?a int32, ?b, ?c int]"},
		{expr: "[?a 1]", sug: "[?b, ?a int32, ?c int]", want: "[?b, ?a int32, ?c int]"},
		{expr: "[?a 1]", sug: "[?b, ?c int, ?a int32]", want: "[?b, ?c int, ?a int32]"},
		{expr: "[?a 1]", sug: "&[?b, ?c int, ?a int32]", want: "&[?b, ?c int, ?a int32]"},
		{expr: "[?a 1]", sug: "&&[?b, ?c int, ?a int32]", want: "[?a int]"},
		{expr: "[?a 1]", sug: "[?b, ?c int]", want: "[?a int]"},
		{expr: "[?a 1]", sug: "[?b, ?c int, ?a]", want: "[?a int]"},
		{expr: "[?a 1]", sug: "[?b, ?c int, ?a string]", want: "[?a int]"},
		{expr: "[?a 1]", sug: "t", want: "t", src: "type t [?a int]"},
		{expr: "[?a 1]", sug: "t", want: "t", src: "type t [?a int, ?b]"},
		{expr: "[?a 1]", sug: "t", want: "t", src: "type t [?a int, ?b, ?c int]"},
		{expr: "[?a 1]", sug: "t", want: "t", src: "type t [?b, ?a int, ?c int]"},
		{expr: "[?a 1]", sug: "t", want: "t", src: "type t [?a int32, ?b]"},
		{expr: "[?a 1]", sug: "&t", want: "&t", src: "type t [?a int, ?b]"},
		{expr: "[?a 1]", sug: "&&t", want: "[?a int]", src: "type t [?a int, ?b]"},
		{expr: "[?a 1]", sug: "t", want: "t", src: "type t &[?a int, ?b]"},
		{expr: "[?a 1]", sug: "&t", want: "[?a int]", src: "type t &[?a int, ?b]"},
		{expr: "[?a 1]", sug: "t", want: "[?a int]", src: "type t [?c int, ?b]"},
		{expr: "[?a 1]", sug: "t", want: "[?a int]", src: "type t [?a, ?b]"},
		{expr: "[?some 1]", sug: "int opt", want: "int opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", sug: "int32 opt", want: "int32 opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", sug: "&int32 opt", want: "&int32 opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", sug: "&&int32 opt", want: "[?some int]", src: "type T opt [?none, ?some T]"},
		{expr: "[?some 1]", sug: "string opt", want: "[?some int]", src: "type T opt [?none, ?some T]"},
		{expr: "[?some]", sug: "int opt", want: "[?some]", src: "type T opt [?none, ?some T]"},
		{expr: "[?none]", sug: "int opt", want: "int opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?none]", sug: "&int opt", want: "&int opt", src: "type T opt [?none, ?some T]"},
		{expr: "[?none]", sug: "&&int opt", want: "[?none]", src: "type T opt [?none, ?some T]"},
	}
	for _, test := range tests {
		test := test
		name := test.expr
		if test.sug != "" {
			name = test.sug + " : " + name
		}
		t.Run(name, func(t *testing.T) {
			src := fmt.Sprintf("%s\ntype sug %s\ntype want %s\n",
				test.src, test.sug, test.want)
			t.Log(src)
			mod, errs := check("test", []string{src}, nil)
			if len(errs) > 0 {
				t.Fatal("failed to parse and check:", errStr(errs))
			}
			parserExpr, err := parser.ParseExpr(test.expr)
			if err != nil {
				t.Fatalf("failed to parse [%s]: %s", test.expr, err)
			}
			sug := findTypeDef(t, "sug", mod).Type
			_, expr, fails := checkExpr(mod.Files[0], parserExpr, sug)
			if len(fails) > 0 {
				t.Fatalf("failed to check [%s]: %s", test.expr, fails[0].msg)
			}
			want := findTypeDef(t, "want", mod).Type
			if !expr.Type().eq(want) {
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
			if !got.eq(lit) {
				t.Errorf("got literal %s, want %s", got, lit)
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
