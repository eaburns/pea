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
