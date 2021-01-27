package checker

import (
	"fmt"
	"regexp"
	"strings"
	"testing"
	"text/template"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
	"github.com/google/go-cmp/cmp"
)

var diffOpts = []cmp.Option{
	cmp.FilterPath(isLoc, cmp.Ignore()),
}

func isLoc(path cmp.Path) bool {
	for _, s := range path {
		if s.String() == ".L" {
			return true
		}
	}
	return false
}

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
	p := parser.NewWithOffset(imp.files.Len() + 1)
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
	p := parser.New()
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
		if d, ok := def.(*TypeDef); ok && d.Name == name {
			return d
		}
	}
	t.Fatalf("failed to find type definition %s", name)
	panic("impossible")
}

func findVarDef(t *testing.T, name string, mod *Mod) *VarDef {
	for _, def := range mod.Defs {
		if d, ok := def.(*VarDef); ok && d.Name == name {
			return d
		}
	}
	t.Fatalf("failed to find variable definition %s", name)
	panic("impossible")
}

func findFuncDef(t *testing.T, name string, mod *Mod) *FuncDef {
	for _, def := range mod.Defs {
		if d, ok := def.(*FuncDef); ok && d.Name == name {
			return d
		}
	}
	t.Fatalf("failed to find function definition %s", name)
	panic("impossible")
}

func findTestDef(t *testing.T, name string, mod *Mod) *TestDef {
	for _, def := range mod.Defs {
		if d, ok := def.(*TestDef); ok && d.Name == name {
			return d
		}
	}
	t.Fatalf("failed to find test definition %s", name)
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
			src: "var a int := 1		var a itn := 1",
			err: "redefined",
		},
		{
			name: "var _ not redef",
			src: "var _ int := 1		var _ int := 1",
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

func TestVarCycle(t *testing.T) {
	tests := []struct {
		name string
		src  string
		err  string
	}{
		{
			name: "self cycle",
			src:  "var a int := a",
			err:  "cyclic initialization",
		},
		{
			name: "simple funcall cycle",
			src: `
				var a int := foo()
				func foo() int {return: a}
			`,
			err: "cyclic initialization",
		},
		{
			name: "multi-var cycle",
			src: `
				var a int := b
				var b int := c
				var c int := d
				var d int := a
			`,
			err: "cyclic initialization",
		},
		{
			name: "multi-var-and-call cycle",
			src: `
				var a int := foo()
				func foo() int { return: b }
				var b int := bar()
				func bar() int { return: c }
				var c int := baz()
				func baz() int { return: a }
			`,
			err: "cyclic initialization",
		},
		{
			name: "ident fun cycle",
			src: `
				var a int := foo()
				func foo() int { x := bar, return: x() }
				func bar() int { return: a }
			`,
			err: "cyclic initialization",
		},
		{
			name: "iface call cycle",
			src: `
				var a int := foo(5)
				func foo(t T) T : bar(T)T { return: bar(t) }
				func bar(i int)int { return: i + a }
			`,
			err: "cyclic initialization",
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

func TestVarSorting(t *testing.T) {
	const src = `
		var x int := foo(z) + y
		var xx int := z
		var y int := 1
		var z int := 2
		func foo(t T) T : bar(T)T {return: bar(t)}
		func bar(_ int) int
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	var i int
	ord := make(map[string]int)
	for _, def := range mod.Defs {
		if vr, ok := def.(*VarDef); ok {
			ord[vr.Name] = i
			i++
		}
	}
	t.Logf("%v\n", ord)
	// x depends on z and y.
	if ord["x"] < ord["z"] {
		t.Errorf("x comes before z")
	}
	if ord["x"] < ord["y"] {
		t.Errorf("x comes before y")
	}
	// xx depends on z
	if ord["xx"] < ord["z"] {
		t.Errorf("xx comes before z")
	}
}

func TestFuncNewLocal(t *testing.T) {
	const src = `
		// x and y are locals of the func.
		// z is a local of the block.
		// All other variables are not locals.
		var a int := 1
		func testFunc(b int){
			x := 1,
			{z := x},
			y := "hello",
			x := 3,
			a := 5,
			b := 6,
		}
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	fun := findFuncDef(t, "testFunc", mod)
	want := []*FuncLocal{
		{Name: "x", T: &BasicType{Kind: Int}},
		{Name: "y", T: &BasicType{Kind: String}},
	}
	if diff := cmp.Diff(want, fun.Locals, diffOpts...); diff != "" {
		t.Errorf("func locals differ: %s", diff)
	}

	block := fun.Exprs[1].(*Convert).Expr.(*BlockLit)
	want = []*FuncLocal{
		{Name: "z", T: &BasicType{Kind: Int}},
	}
	if diff := cmp.Diff(want, block.Locals, diffOpts...); diff != "" {
		t.Errorf("block locals differ: %s", diff)
	}
}

func TestTestNewLocal(t *testing.T) {
	const src = `
		// x and y are locals of the test.
		// z is a local of the block.
		// All other variables are not locals.
		var a int := 1
		test testDef {
			x := 1,
			{z := x},
			y := "hello",
			x := 3,
			a := 5,
		}
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	test := findTestDef(t, "testDef", mod)
	want := []*FuncLocal{
		{Name: "x", T: &BasicType{Kind: Int}},
		{Name: "y", T: &BasicType{Kind: String}},
	}
	if diff := cmp.Diff(want, test.Locals, diffOpts...); diff != "" {
		t.Errorf("test locals differ: %s", diff)
	}

	block := test.Exprs[1].(*Convert).Expr.(*BlockLit)
	want = []*FuncLocal{
		{Name: "z", T: &BasicType{Kind: Int}},
	}
	if diff := cmp.Diff(want, block.Locals, diffOpts...); diff != "" {
		t.Errorf("block locals differ: %s", diff)
	}
}

func TestBlockNewLocal(t *testing.T) {
	const src = `
		// x and y are locals of the outer block.
		// z is a local of the inner block.
		// All other variables are not locals.
		var a int := 1
		var testVar (int){} := (b int){
			x := 1,
			{z := x},
			y := "hello",
			x := 3,
			a := 5,
			b := 6,
		}
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	block := findVarDef(t, "testVar", mod).Expr.(*Convert).Expr.(*BlockLit)
	want := []*FuncLocal{
		{Name: "x", T: &BasicType{Kind: Int}},
		{Name: "y", T: &BasicType{Kind: String}},
	}
	if diff := cmp.Diff(want, block.Locals, diffOpts...); diff != "" {
		t.Errorf("outer-block locals differ: %s", diff)
	}

	block2 := block.Exprs[1].(*Convert).Expr.(*BlockLit)
	want = []*FuncLocal{
		{Name: "z", T: &BasicType{Kind: Int}},
	}
	if diff := cmp.Diff(want, block2.Locals, diffOpts...); diff != "" {
		t.Errorf("inner-block locals differ: %s", diff)
	}
}

func TestNoNewLocalInNestedExpr(t *testing.T) {
	const src = `
		var testVar int := 1 + (x := 2)
	`
	switch _, errs := check("test", []string{src}, nil); {
	case len(errs) != 1:
		t.Errorf("expected 1 error, got %d", len(errs))
	case !strings.Contains(errs[0].Error(), "x: not found"):
		t.Errorf("expected not found error, got %s", errs[0])
	}
}

func TestNoNewLocalInArrayExprs(t *testing.T) {
	const src = `
		var testVar [int] := [1, x := 2]
	`
	switch _, errs := check("test", []string{src}, nil); {
	case len(errs) != 1:
		t.Errorf("expected 1 error, got %d", len(errs))
	case !strings.Contains(errs[0].Error(), "x: not found"):
		t.Errorf("expected not found error, got %s", errs[0])
	}
}

func TestNewLocalTypes(t *testing.T) {
	tests := []struct {
		src  string
		expr string
		want string
		err  string
	}{
		{
			src:  "var int_array [int]",
			expr: "int_array",
			want: "[int]",
		},
		{
			src:  "var int_array [int]",
			expr: "&[int] : int_array",
			want: "&[int]",
		},
		{
			src:  "var int_array [int]",
			expr: "&&[int] : int_array",
			err:  "cannot convert int_array \\(\\[int\\]\\) to type &&\\[int\\]",
		},
		{
			src:  "var int_array [int]",
			expr: "int_array[0]",
			want: "int",
		},
		{
			src:  "var int_array [int]",
			expr: "&int : int_array[0]",
			want: "&int",
		},
		{
			src:  "var int_array [int]",
			expr: "&&int : int_array[0]",
			want: "&&int",
		},
		{
			src:  "var int_array [int]",
			expr: "&&&int : int_array[0]",
			err:  "cannot convert returned &int to &&&int",
		},
		{
			src:  "var int_ref_array [&int]",
			expr: "int_ref_array[0]",
			want: "int",
		},
		{
			src:  "var int_ref_array [&int]",
			expr: "&int : int_ref_array[0]",
			want: "&int",
		},
		{
			src:  "var int_ref_array [&int]",
			expr: "&&int : int_ref_array[0]",
			want: "&&int",
		},
		{
			src:  "var int_ref_array [&int]",
			expr: "&&&int : int_ref_array[0]",
			want: "&&&int",
		},
		{
			src:  "var int_ref_array [&int]",
			expr: "&&&&int : int_ref_array[0]",
			err:  "cannot convert returned &&int to &&&&int",
		},
		{
			src:  "var int_array_ref &[int]",
			expr: "int_array_ref[0]",
			want: "int",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "make_point()",
			want: "point",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&point : make_point()",
			want: "&point",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&&point : make_point()",
			err:  "cannot convert returned point to &&point",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "make_point().x",
			want: "int",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&int : make_point().x",
			want: "&int",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&&int : make_point().x",
			want: "&&int",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&&&int : make_point().x",
			err:  "cannot convert returned &int to &&&int",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "make_point()",
			want: "point",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&point : make_point()",
			want: "&point",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			expr: "&&point : make_point()",
			err:  "cannot convert returned point to &&point",
		},
		{
			src: `
				type point_ref &[.x int, .y int]
				func make_point_ref() point_ref
			`,
			expr: "make_point_ref()",
			want: "point_ref",
		},
		{
			src: `
				type point_ref &[.x int, .y int]
				func new_point_ref() &point_ref
			`,
			expr: "new_point_ref()",
			want: "point_ref",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.expr, func(t *testing.T) {
			src := fmt.Sprintf("%s\nvar xxx (){} := {got := %s}\n", test.src, test.expr)
			if test.want != "" {
				src += fmt.Sprintf("var want %s\n", test.want)
			}
			t.Log(src)
			switch mod, errs := check("test", []string{src}, nil); {
			case test.err == "" && len(errs) == 0:
				xxx := findVarDef(t, "xxx", mod)
				got := xxx.Expr.(*Convert).Expr.(*BlockLit).Locals[0].Type()
				want := findVarDef(t, "want", mod).Type()
				if !eqType(got, want) {
					t.Errorf("got %s, want %s", got, want)
				}
			case test.err == "" && len(errs) > 0:
				t.Fatalf("unexpected error: %s", errs[0])
			case test.err != "" && len(errs) == 0:
				t.Fatalf("expected error matching %s, got nil", test.err)
			case !regexp.MustCompile(test.err).MatchString(errStr(errs)):
				t.Fatalf("expected error matching %s, got\n%s", test.err, errStr(errs))
			}
		})
	}
}

func TestCheckFuncReturnCall(t *testing.T) {
	tests := []struct {
		name string
		src  string
		err  string
	}{
		{
			name: "ok no return",
			src:  "func foo() {}",
		},
		{
			name: "ok return",
			src:  "func foo() int { return: 1 }",
		},
		{
			name: "ok return, not first expr",
			src:  "func foo() int { 1, 2, 3, return: 1 }",
		},
		{
			name: "ok return in parens",
			src:  "func foo() int { (((return: 1))) }",
		},
		{
			name: "ok no return on declaration",
			src:  "func foo() int",
		},
		{
			name: "ok 0-ary return",
			src:  "func foo() { return() }",
		},
		{
			name: "ok 1-ary return empty struct",
			src:  "func foo() { return: [.] }",
		},
		{
			name: "missing return empty body",
			src:  "func foo() int {}",
			err:  "must end in a return",
		},
		{
			name: "missing return non-empty body",
			src:  "func foo() int { 1 }",
			err:  "must end in a return",
		},
		{
			name: "return type mismatch",
			src:  "func foo() int { return: \"hello\" }",
			err:  `cannot convert "hello" \(string\) to type int`,
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

func TestCheckFuncScope(t *testing.T) {
	tests := []struct {
		name string
		src  string
		err  string
	}{
		{
			name: "interface function lookup in call resolution",
			src: `
				func foo(t T) bool : =(T, T) bool {
					return: t = t
				}
			`,
		},
		{
			name: "interface function lookup in ID resolution",
			src: `
				func foo(t T) (T,T){bool} : =(T, T) bool {
					return: (=)
				}
			`,
		},
		{
			name: "interface function lookup causes ambiguity",
			src: `
				func foo() bool : =(int, int) bool {
					return: 1 = 1
				}
			`,
			err: "ambiguous",
		},
		{
			name: "param shadows module-level function",
			src: `
				func x(_ string)
				func foo(x int) {
					x("hello")
				}
			`,
			err: "x is not callable",
		},
		{
			name: "param shadows module-level var",
			src: `
				var x string
				func foo(x int) string {
					return: x
				}
			`,
			err: "cannot convert x \\(int\\) to type string",
		},
		{
			name: "param shadows interface func",
			src: `
				var x string
				func foo(x int) (){} : x() {
					return: x
				}
			`,
			err: "cannot convert x \\(int\\) to type \\(\\){}",
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

func TestArgumentConversions(t *testing.T) {
	tests := []struct {
		src  string
		parm string
		expr string
		err  string
	}{
		{
			parm: "int",
			expr: "1",
		},
		{
			parm: "&int",
			expr: "1",
		},
		{
			parm: "&&int",
			expr: "1",
			err:  `cannot convert 1 \(int\) to type &&int`,
		},
		{
			src:  "var x int",
			parm: "int",
			expr: "x",
		},
		{
			src:  "var x int",
			parm: "&int",
			expr: "x",
		},
		{
			src:  "var x int",
			parm: "&&int",
			expr: "x",
			err:  `cannot convert x \(int\) to type &&int`,
		},
		{
			src:  "func x()int",
			parm: "int",
			expr: "x()",
		},
		{
			src:  "func x()int",
			parm: "&int",
			expr: "x()",
		},
		{
			src:  "func x()int",
			parm: "&&int",
			expr: "x()",
			err:  `cannot convert returned int to &&int`,
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			parm: "int",
			expr: "make_point().x",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			parm: "&int",
			expr: "make_point().x",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			parm: "&&int",
			expr: "make_point().x",
		},
		{
			src: `
				type point [.x int, .y int]
				func make_point() point
			`,
			parm: "&&&int",
			expr: "make_point().x",
			err:  "cannot convert returned &int to &&&int",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.parm+" : "+test.expr, func(t *testing.T) {
			src := fmt.Sprintf("%s\nfunc fff(_ %s)\nvar _ (){} := {fff(%s)}",
				test.src, test.parm, test.expr)
			t.Log(src)
			switch _, errs := check("test", []string{src}, nil); {
			case test.err == "" && len(errs) == 0:
				break
			case test.err == "" && len(errs) > 0:
				t.Fatalf("unexpected error: %s", errs[0])
			case test.err != "" && len(errs) == 0:
				t.Fatalf("expected error matching %s, got nil", test.err)
			case !regexp.MustCompile(test.err).MatchString(errStr(errs)):
				t.Fatalf("expected error matching %s, got\n%s", test.err, errStr(errs))
			}
		})
	}
}

func TestConversions(t *testing.T) {
	tests := []struct {
		name string
		src  string
		err  string
	}{
		{src: "func f(x int) { int : x }"},
		{src: "func f(x int) { int8 : x }"},
		{src: "func f(x int) { int16 : x }"},
		{src: "func f(x int) { int32 : x }"},
		{src: "func f(x int) { int64 : x }"},
		{src: "func f(x int) { uint : x }"},
		{src: "func f(x int) { uint8 : x }"},
		{src: "func f(x int) { uint16 : x }"},
		{src: "func f(x int) { uint32 : x }"},
		{src: "func f(x int) { uint64 : x }"},
		{src: "func f(x int) { float32 : x }"},
		{src: "func f(x int) { float64 : x }"},
		{src: "type num int	func f(x int) { num : x }"},
		{src: "func f(x int8) { int : x }"},
		{src: "func f(x int8) { int8 : x }"},
		{src: "func f(x int8) { int16 : x }"},
		{src: "func f(x int8) { int32 : x }"},
		{src: "func f(x int8) { int64 : x }"},
		{src: "func f(x int8) { uint : x }"},
		{src: "func f(x int8) { uint8 : x }"},
		{src: "func f(x int8) { uint16 : x }"},
		{src: "func f(x int8) { uint32 : x }"},
		{src: "func f(x int8) { uint64 : x }"},
		{src: "func f(x int8) { float32 : x }"},
		{src: "func f(x int8) { float64 : x }"},
		{src: "type num int	func f(x int8) { num : x }"},
		{src: "func f(x int16) { int : x }"},
		{src: "func f(x int16) { int8 : x }"},
		{src: "func f(x int16) { int16 : x }"},
		{src: "func f(x int16) { int32 : x }"},
		{src: "func f(x int16) { int64 : x }"},
		{src: "func f(x int16) { uint : x }"},
		{src: "func f(x int16) { uint8 : x }"},
		{src: "func f(x int16) { uint16 : x }"},
		{src: "func f(x int16) { uint32 : x }"},
		{src: "func f(x int16) { uint64 : x }"},
		{src: "func f(x int16) { float32 : x }"},
		{src: "func f(x int16) { float64 : x }"},
		{src: "type num int	func f(x int16) { num : x }"},
		{src: "func f(x int32) { int : x }"},
		{src: "func f(x int32) { int8 : x }"},
		{src: "func f(x int32) { int16 : x }"},
		{src: "func f(x int32) { int32 : x }"},
		{src: "func f(x int32) { int64 : x }"},
		{src: "func f(x int32) { uint : x }"},
		{src: "func f(x int32) { uint8 : x }"},
		{src: "func f(x int32) { uint16 : x }"},
		{src: "func f(x int32) { uint32 : x }"},
		{src: "func f(x int32) { uint64 : x }"},
		{src: "func f(x int32) { float32 : x }"},
		{src: "func f(x int32) { float64 : x }"},
		{src: "type num int	func f(x int32) { num : x }"},
		{src: "func f(x int64) { int : x }"},
		{src: "func f(x int64) { int8 : x }"},
		{src: "func f(x int64) { int16 : x }"},
		{src: "func f(x int64) { int32 : x }"},
		{src: "func f(x int64) { int64 : x }"},
		{src: "func f(x int64) { uint : x }"},
		{src: "func f(x int64) { uint8 : x }"},
		{src: "func f(x int64) { uint16 : x }"},
		{src: "func f(x int64) { uint32 : x }"},
		{src: "func f(x int64) { uint64 : x }"},
		{src: "func f(x int64) { float32 : x }"},
		{src: "func f(x int64) { float64 : x }"},
		{src: "type num int	func f(x int64) { num : x }"},
		{src: "func f(x uint) { int : x }"},
		{src: "func f(x uint) { int8 : x }"},
		{src: "func f(x uint) { int16 : x }"},
		{src: "func f(x uint) { int32 : x }"},
		{src: "func f(x uint) { int64 : x }"},
		{src: "func f(x uint) { uint : x }"},
		{src: "func f(x uint) { uint8 : x }"},
		{src: "func f(x uint) { uint16 : x }"},
		{src: "func f(x uint) { uint32 : x }"},
		{src: "func f(x uint) { uint64 : x }"},
		{src: "func f(x uint) { float32 : x }"},
		{src: "func f(x uint) { float64 : x }"},
		{src: "type num int	func f(x uint) { num : x }"},
		{src: "func f(x uint8) { int : x }"},
		{src: "func f(x uint8) { int8 : x }"},
		{src: "func f(x uint8) { int16 : x }"},
		{src: "func f(x uint8) { int32 : x }"},
		{src: "func f(x uint8) { int64 : x }"},
		{src: "func f(x uint8) { uint : x }"},
		{src: "func f(x uint8) { uint8 : x }"},
		{src: "func f(x uint8) { uint16 : x }"},
		{src: "func f(x uint8) { uint32 : x }"},
		{src: "func f(x uint8) { uint64 : x }"},
		{src: "func f(x uint8) { float32 : x }"},
		{src: "func f(x uint8) { float64 : x }"},
		{src: "type num int	func f(x uint8) { num : x }"},
		{src: "func f(x uint16) { int : x }"},
		{src: "func f(x uint16) { int8 : x }"},
		{src: "func f(x uint16) { int16 : x }"},
		{src: "func f(x uint16) { int32 : x }"},
		{src: "func f(x uint16) { int64 : x }"},
		{src: "func f(x uint16) { uint : x }"},
		{src: "func f(x uint16) { uint8 : x }"},
		{src: "func f(x uint16) { uint16 : x }"},
		{src: "func f(x uint16) { uint32 : x }"},
		{src: "func f(x uint16) { uint64 : x }"},
		{src: "func f(x uint16) { float32 : x }"},
		{src: "func f(x uint16) { float64 : x }"},
		{src: "type num int	func f(x uint16) { num : x }"},
		{src: "func f(x uint32) { int : x }"},
		{src: "func f(x uint32) { int8 : x }"},
		{src: "func f(x uint32) { int16 : x }"},
		{src: "func f(x uint32) { int32 : x }"},
		{src: "func f(x uint32) { int64 : x }"},
		{src: "func f(x uint32) { uint : x }"},
		{src: "func f(x uint32) { uint8 : x }"},
		{src: "func f(x uint32) { uint16 : x }"},
		{src: "func f(x uint32) { uint32 : x }"},
		{src: "func f(x uint32) { uint64 : x }"},
		{src: "func f(x uint32) { float32 : x }"},
		{src: "func f(x uint32) { float64 : x }"},
		{src: "type num int	func f(x uint32) { num : x }"},
		{src: "func f(x uint64) { int : x }"},
		{src: "func f(x uint64) { int8 : x }"},
		{src: "func f(x uint64) { int16 : x }"},
		{src: "func f(x uint64) { int32 : x }"},
		{src: "func f(x uint64) { int64 : x }"},
		{src: "func f(x uint64) { uint : x }"},
		{src: "func f(x uint64) { uint8 : x }"},
		{src: "func f(x uint64) { uint16 : x }"},
		{src: "func f(x uint64) { uint32 : x }"},
		{src: "func f(x uint64) { uint64 : x }"},
		{src: "func f(x uint64) { float32 : x }"},
		{src: "func f(x uint64) { float64 : x }"},
		{src: "type num int	func f(x uint64) { num : x }"},
		{src: "func f(x float32) { int : x }"},
		{src: "func f(x float32) { int8 : x }"},
		{src: "func f(x float32) { int16 : x }"},
		{src: "func f(x float32) { int32 : x }"},
		{src: "func f(x float32) { int64 : x }"},
		{src: "func f(x float32) { uint : x }"},
		{src: "func f(x float32) { uint8 : x }"},
		{src: "func f(x float32) { uint16 : x }"},
		{src: "func f(x float32) { uint32 : x }"},
		{src: "func f(x float32) { uint64 : x }"},
		{src: "func f(x float32) { float32 : x }"},
		{src: "func f(x float32) { float64 : x }"},
		{src: "type num int	func f(x float32) { num : x }"},
		{src: "func f(x float64) { int : x }"},
		{src: "func f(x float64) { int8 : x }"},
		{src: "func f(x float64) { int16 : x }"},
		{src: "func f(x float64) { int32 : x }"},
		{src: "func f(x float64) { int64 : x }"},
		{src: "func f(x float64) { uint : x }"},
		{src: "func f(x float64) { uint8 : x }"},
		{src: "func f(x float64) { uint16 : x }"},
		{src: "func f(x float64) { uint32 : x }"},
		{src: "func f(x float64) { uint64 : x }"},
		{src: "func f(x float64) { float32 : x }"},
		{src: "func f(x float64) { float64 : x }"},
		{src: "type num int	func f(x float64) { num : x }"},
		{src: "type num int	func f(x num) { int : x }"},
		{src: "type num int	func f(x num) { int8 : x }"},
		{src: "type num int	func f(x num) { int16 : x }"},
		{src: "type num int	func f(x num) { int32 : x }"},
		{src: "type num int	func f(x num) { int64 : x }"},
		{src: "type num int	func f(x num) { uint : x }"},
		{src: "type num int	func f(x num) { uint8 : x }"},
		{src: "type num int	func f(x num) { uint16 : x }"},
		{src: "type num int	func f(x num) { uint32 : x }"},
		{src: "type num int	func f(x num) { uint64 : x }"},
		{src: "type num int	func f(x num) { float32 : x }"},
		{src: "type num int	func f(x num) { float64 : x }"},
		{src: "type num1 int		type num2 int		func f(x num1) { num2 : x }"},
		{
			src: "func f(x string) { int : x }",
			err: "cannot convert",
		},

		{src: "func f(x [uint8]) { string : x }"},
		{src: "type str [uint8]	func f(x str) { string : x }"},

		{src: "func f(x int) { &int : x }"},
		{
			src: "func f(x int) { &&int : x }",
			err: "cannot convert",
		},

		{src: "func f(x &int) { int : x }"},
		{src: "func f(x &&int) { &int : x }"},
		{src: "func f(x &&int) { int : x }"},

		{src: "func f(x [.x int]) { [.x int] : x }"},
		{src: "type t [.x int]	func f(x t) { [.x int] : x }"},
		{src: "type t [.x int]	func f(x [.x int]) { t : x }"},
		{
			src: "type t [.x int]	type u [.x int]	func f(x u) { t : x }",
			err: "cannot convert",
		},

		{
			name: "explicit conversion of an explicit conversion is ok",
			src:  "func f(x int) { int : (&int : x) }",
		},
		{
			name: "no implicit conversion of an explicit conversion",
			src:  "func f(x int) { x := &int : x }",
			err:  "cannot convert",
		},
		{
			name: "disambiguate id by conversion",
			src: `
				var x int
				func x()
				func f() { int : x }
			`,
		},
		{
			name: "disambiguate id by conversion with implicit reference",
			src: `
				var x int
				func x()
				func f() { &int : x }
			`,
		},
		{
			name: "disambiguate id by conversion and convert again",
			src: `
				var x int
				func x()
				func f() { uint8 : (int : x) }
			`,
		},
	}
	for _, test := range tests {
		test := test
		name := test.name
		if name == "" {
			name = test.src
		}
		t.Run(name, func(t *testing.T) {
			switch _, errs := check("test", []string{test.src}, nil); {
			case test.err == "" && len(errs) == 0:
				break
			case test.err == "" && len(errs) > 0:
				t.Fatalf("unexpected error: %s", errs[0])
			case test.err != "" && len(errs) == 0:
				t.Fatalf("expected error matching %s, got nil", test.err)
			case !regexp.MustCompile(test.err).MatchString(errStr(errs)):
				t.Fatalf("expected error matching %s, got\n%s", test.err, errStr(errs))
			}
		})
	}
}

func TestOverloadResolution(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		call     string
		ret      string // or ""
		want     string
		err      string
		otherMod testMod
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
			err:  `cannot convert "hello" \(string\) to type int`,
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
			name: "call def-function-type variable",
			src: `
				type my_fun (int){}
				var x my_fun
			`,
			call: "x(1)",
			want: "x",
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
			src: `
				func x(u int, s string)
				func x(i &int, j int)
				var int_ref &int
			`,
			call: "x(int_ref, \"hello\")",
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
			name: "built-in selector literal type",
			call: "[.x 4].x",
			want: "built-in .x(&[.x int])&int",
		},
		{
			name: "built-in selector ref literal type",
			call: "(&[.x int] : [.x 4]).x",
			want: "built-in .x(&[.x int])&int",
		},
		{
			name: "built-in selector def type",
			src: `
				type t [.x int]
				var t_var t
			`,
			call: "t_var.x",
			want: "built-in .x(&t)&int",
		},
		{
			name: "built-in selector ref def type",
			src: `
				type t [.x int]
				var t_ref_var &t
			`,
			call: "t_ref_var.x",
			want: "built-in .x(&t)&int",
		},
		{
			name: "built-in selector def ref type",
			src: `
				type t &[.x int]
				var t_var t
			`,
			call: "t_var.x",
			want: "built-in .x(t)&int",
		},
		{
			name: "built-in selector def ref ref type fails",
			src: `
				type t &&[.x int]
				var x t
			`,
			call: "x.x",
			err:  "not a struct type",
		},
		{
			name: "built-in selector, other field",
			src: `
				type point [.x float64, .y float64]
				var point_var point
			`,
			call: "point_var.y",
			want: "built-in .y(&point)&float64",
		},
		{
			name: "built-in selector, not a struct",
			src: `
				type point int
				var point_var point
			`,
			call: "point_var.z",
			err:  "is not a struct type",
		},
		{
			name: "built-in selector, no field",
			src: `
				type point [.x float64, .y float64]
				var point_var point
			`,
			call: "point_var.z",
			err:  "point has no field .z",
		},
		{
			name: "built-in selector, wrong return type",
			src: `
				type point [.x float64, .y float64]
				var point_var point
			`,
			call: "point_var.y",
			ret:  "string",
			err:  "cannot convert returned &float64 to string",
		},
		{
			name: "built-in selector mismatches, but func def matches",
			src: `
				type point [.x float64, .y float64]
				func .z(_ point)float64
				var point_var point
			`,
			call: "point_var.z",
			want: ".z(point)float64",
		},
		{
			name: "built-in selector matches, and func def mismatches",
			src: `
				type point [.x float64, .y float64]
				func .x(_ point)string
				var point_var point
			`,
			call: "point_var.x",
			ret:  "float64",
			want: "built-in .x(&point)&float64",
		},
		{
			name: "built-in selector and func def ambiguity",
			src: `
				type point [.x float64, .y float64]
				func .x(_ point)float64
				var point_var point
			`,
			call: "point_var.x",
			ret:  "float64",
			err:  "ambiguous",
		},
		{
			name: "built-in selector, other mod struct",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo [.x int, .y int]
				`,
			},
			call: "make_foo().x",
			want: "built-in .x(&other#foo)&int",
		},
		{
			name: "built-in selector, other mod opaque struct fails",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo [.x int, .y int]
				`,
			},
			call: "make_foo().x",
			err:  `is not a struct type`,
		},
		{
			name: "built-in switch literal type, not-typed case",
			call: "[?a] ?a {}",
			want: "built-in ?a(&[?a], (){})",
		},
		{
			name: "built-in switch literal type, typed case",
			call: "[?a 1] ?a (_ int) {}",
			want: "built-in ?a(&[?a int], (int){})",
		},
		{
			name: "built-in switch ref literal type, typed case",
			call: "(&[?a int] : [?a 1]) ?a (_ int) {}",
			want: "built-in ?a(&[?a int], (int){})",
		},
		{
			name: "built-in switch not-typed cases",
			src: `
				type a_or_b [?a, ?b]
				func make() a_or_b
			`,
			call: "make() ?a {} ?b {}",
			want: "built-in ?a?b(&a_or_b, (){}, (){})",
		},
		{
			name: "built-in switch def union",
			src: `
				type a_or_b [?a, ?b]
				var a_or_b_var a_or_b
			`,
			call: "a_or_b_var ?a {} ?b {}",
			want: "built-in ?a?b(&a_or_b, (){}, (){})",
		},
		{
			name: "built-in switch def union ref",
			src: `
				type a_or_b &[?a, ?b]
				var a_or_b_var a_or_b
			`,
			call: "a_or_b_var ?a {} ?b {}",
			want: "built-in ?a?b(a_or_b, (){}, (){})",
		},
		{
			name: "built-in switch ref def union",
			src:  "type a_or_b [?a, ?b]",
			call: "(&a_or_b : [?a]) ?a {} ?b {}",
			want: "built-in ?a?b(&a_or_b, (){}, (){})",
		},
		{
			name: "built-in switch def union ref ref fails",
			src: `
				type a_or_b &&[?a, ?b]
				var x a_or_b
			`,
			call: "(a_or_b : x) ?a {} ?b {}",
			err:  "not a union type",
		},
		{
			name: "built-in switch typed cases",
			src: `
				type a_or_b [?a string, ?b int]
				func make() a_or_b
			`,
			call: "make() ?a (_ string) {1} ?b (_ int) {1}",
			want: "built-in ?a?b(&a_or_b, (string){int}, (int){int})int",
		},
		{
			name: "built-in switch mixed typed and non-typed cases",
			src: `
				type a_or_b [?a, ?b int]
				func make() a_or_b
			`,
			call: "make() ?a () {1} ?b (_ int) {1}",
			want: "built-in ?a?b(&a_or_b, (){int}, (int){int})int",
		},
		{
			name: "built-in switch cases re-ordered",
			src: `
				type a_or_b [?a, ?b int]
				func make() a_or_b
			`,
			call: "make() ?b (_ int) {1} ?a () {1} ",
			want: "built-in ?b?a(&a_or_b, (int){int}, (){int})int",
		},
		{
			name: "built-in switch not all cases, case not typed",
			src: `
				type a_or_b [?a, ?b int]
				func make() a_or_b
			`,
			call: "make() ?a () {1} ",
			want: "built-in ?a(&a_or_b, (){})",
		},
		{
			name: "built-in switch not all cases, case typed",
			src: `
				type a_or_b [?a, ?b int]
				func make() a_or_b
			`,
			call: "make() ?b (_ int) {1} ",
			want: "built-in ?b(&a_or_b, (int){})",
		},
		{
			name: "built-in switch not all cases, does not convert return",
			src: `
				type a_or_b [?a, ?b int]
				func make() a_or_b
			`,
			call: "make() ?a () {1} ",
			ret:  "int",
			err:  "cannot convert returned \\[\\.\\] to int",
		},
		{
			name: "built-in switch not a union type",
			call: "1 ?b (_ int) {1} ",
			err:  "not a union",
		},
		{
			name: "built-in switch case name mismatch",
			src: `
				type a_or_b [?a, ?b int]
				func make() a_or_b
			`,
			call: "make() ?c (_ int) {1} ",
			err:  "no case \\?c",
		},
		{
			name: "built-in switch return type inferred",
			src: `
				type a_or_b [?a, ?b]
				func make() a_or_b
			`,
			call: "make() ?a () {uint8 : 1} ?b () {2}",
			ret:  "uint8",
			want: "built-in ?a?b(&a_or_b, (){uint8}, (){uint8})uint8",
		},
		{
			name: "built-in switch, other mod union",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo [?none, ?some int]
				`,
			},
			call: "make_foo() ?none {} ?some (i int) {}",
			want: "built-in ?none?some(&other#foo, (){}, (int){})",
		},
		{
			name: "built-in switch, other mod opaque union fails",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo [?none, ?some int]
				`,
			},
			call: "make_foo() ?none {} ?some (i int) {}",
			err:  `not a union type`,
		},
		{
			name: "no convert between non-literal types",
			src: `
				type point_a [.x float64, .y float64]
				type point_b [.x float64, .y float64]
				func f(_ point_b)
				var point_a_var point_a
			`,
			call: "f(point_a_var)",
			err:  `cannot convert point_a_var \(point_a\) to type point_b`,
		},
		{
			name: "convert defined to literal type",
			src: `
				type point [.x float64, .y float64]
				func f(_ [.x float64, .y float64])
				var point_var point
			`,
			call: "f(point_var)",
			want: "f([.x float64, .y float64])",
		},
		{
			name: "convert literal to defined type",
			src: `
				type point [.x float64, .y float64]
				func f(_ point)
				var lit_point_var [.x float64, .y float64]
			`,
			call: "f(lit_point_var)",
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
		{
			name: "built-in assign",
			src:  "var a int := 1",
			call: "a := 6",
			want: "built-in :=(&int, int)int",
		},
		{
			name: "built-in assign, ref lhs",
			src:  "var a int := 1",
			call: "(&int : a) := 6",
			want: "built-in :=(&int, int)int",
		},
		{
			name: "built-in assign, expected, lhs mismatch",
			src:  "var a string := \"\"",
			call: "a := 6",
			ret:  "int",
			err:  `cannot convert 6 \(int\) to type string`,
		},
		{
			name: "built-in assign, expected, rhs mismatch",
			src:  "var a int := 1",
			call: "a := \"\"",
			ret:  "int",
			err:  `cannot convert "" \(string\) to type int`,
		},
		{
			name: "built-in assign, lhs/rhs mismatch",
			src:  "var a int := 1",
			call: "a := \"\"",
			err:  `cannot convert "" \(string\) to type int`,
		},
		{
			name: "built-in new array, no expected type",
			call: "new(5, 0)",
			want: "built-in new(int, int)[int]",
		},
		{
			name: "built-in new array, expected type",
			call: "new(5, int8 : 0)",
			ret:  "[int8]",
			want: "built-in new(int, int8)[int8]",
		},
		{
			name: "built-in new array, expected defed type",
			src:  "type byte_array_ref &[int8]",
			call: "new(5, 0)",
			ret:  "byte_array_ref",
			err:  "cannot convert returned \\[int\\] to byte_array_ref",
		},
		{
			name: "built-in new array, expected non-array type",
			call: "new(5, 0)",
			ret:  "string",
			err:  "cannot convert returned \\[int\\] to string",
		},
		{
			name: "built-in new array, expected non-array def type",
			src:  "type test_type [.x int]",
			call: "new(5, 0)",
			ret:  "test_type",
			err:  "cannot convert returned \\[int\\] to test_type",
		},
		{
			name: "built-in bit-wise not, expected type",
			call: "^(int8 : 1)",
			ret:  "int8",
			want: "built-in ^(int8)int8",
		},
		{
			name: "built-in bit-wise not, no expected type",
			call: "^(int8 : 1)",
			want: "built-in ^(int8)int8",
		},
		{
			name: "built-in bit-wise xor, expected type",
			call: "(int8 : 1) ^ 2",
			ret:  "int8",
			want: "built-in ^(int8, int8)int8",
		},
		{
			name: "built-in bit-wise xor, no expected type",
			call: "(int8 : 1) ^ 3",
			want: "built-in ^(int8, int8)int8",
		},
		{
			name: "built-in bit-wise and, expected type",
			call: "(int8 : 1) & 2",
			ret:  "int8",
			want: "built-in &(int8, int8)int8",
		},
		{
			name: "built-in bit-wise and, no expected type",
			call: "(int8 : 1) & 3",
			want: "built-in &(int8, int8)int8",
		},
		{
			name: "built-in bit-wise or, expected type",
			call: "(int8 : 1) | 2",
			ret:  "int8",
			want: "built-in |(int8, int8)int8",
		},
		{
			name: "built-in bit-wise or, no expected type",
			call: "(int8 : 1) | 3",
			want: "built-in |(int8, int8)int8",
		},
		{
			name: "built-in bit-wise or, not for floats",
			call: "(float64 : 1.0) | 2.0",
			ret:  "float64",
			err:  "not found",
		},
		{
			name: "built-in left shift, expected type",
			call: "(int8 : 1) << 3",
			ret:  "int8",
			want: "built-in <<(int8, int)int8",
		},
		{
			name: "built-in left shift, no expected type",
			call: "(int8 : 1) << 3",
			want: "built-in <<(int8, int)int8",
		},
		{
			name: "built-in negate, expected type",
			call: "- (int8 : 1)",
			ret:  "int8",
			want: "built-in -(int8)int8",
		},
		{
			name: "built-in negate, no expected type",
			call: "- (int8 : 2)",
			want: "built-in -(int8)int8",
		},
		{
			name: "built-in minus, expected type",
			call: "(int8 : 2) - 1",
			ret:  "int8",
			want: "built-in -(int8, int8)int8",
		},
		{
			name: "built-in minus, no expected type",
			call: "(int8 : 2) - 1",
			want: "built-in -(int8, int8)int8",
		},
		{
			name: "built-in plus, expected type",
			call: "(int8 : 2) + 1",
			ret:  "int8",
			want: "built-in +(int8, int8)int8",
		},
		{
			name: "built-in plus, no expected type",
			call: "(int8 : 2) + 1",
			want: "built-in +(int8, int8)int8",
		},
		{
			name: "built-in times, expected type",
			call: "(int8 : 2) * 1",
			ret:  "int8",
			want: "built-in *(int8, int8)int8",
		},
		{
			name: "built-in times, no expected type",
			call: "(int8 : 2) * 1",
			want: "built-in *(int8, int8)int8",
		},
		{
			name: "built-in divide, expected type",
			call: "(int8 : 2) / 1",
			ret:  "int8",
			want: "built-in /(int8, int8)int8",
		},
		{
			name: "built-in divide, no expected type",
			call: "(int8 : 2) / 1",
			want: "built-in /(int8, int8)int8",
		},
		{
			name: "built-in mod, expected type",
			call: "(int8 : 2) % 1",
			ret:  "int8",
			want: "built-in %(int8, int8)int8",
		},
		{
			name: "built-in mod, no expected type",
			call: "(int8 : 2) % 1",
			want: "built-in %(int8, int8)int8",
		},
		{
			name: "built-in -, float is OK",
			call: "2.1 - 1",
			want: "built-in -(float64, float64)float64",
		},
		{
			name: "built-in eq",
			call: "2.0 = 2",
			want: "built-in =(float64, float64)bool",
		},
		{
			name: "built-in neq",
			call: "2.0 != 2",
			want: "built-in !=(float64, float64)bool",
		},
		{
			name: "built-in less",
			call: "2.0 < 2",
			want: "built-in <(float64, float64)bool",
		},
		{
			name: "built-in less eq",
			call: "2.0 <= 2",
			want: "built-in <=(float64, float64)bool",
		},
		{
			name: "built-in greater ",
			call: "2.0 > 2",
			want: "built-in >(float64, float64)bool",
		},
		{
			name: "built-in greater eq",
			call: "2.0 >= 2",
			want: "built-in >=(float64, float64)bool",
		},
		{
			name: "built-in op ambiguity",
			src:  "func +(_ int, _ int)int",
			call: "1 + 2",
			err:  "ambiguous call",
		},
		{
			name: "built-in op, other mod int",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo int64
				`,
			},
			call: "make_foo() + make_foo()",
			want: "built-in +(other#foo, other#foo)other#foo",
		},
		{
			name: "built-in op, other mod opaque int fails",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo int32
				`,
			},
			call: "make_foo() + make_foo()",
			err:  `built-in \+\(_, _\): does not support type other#_foo`,
		},
		{
			name: "built-in array index, no expected type",
			call: "[5, 6, 7][1]",
			want: "built-in []([int], int)&int",
		},
		{
			name: "built-in array index, expected type",
			call: "[(uint8 : 5), 6, 7][1]",
			ret:  "uint8",
			want: "built-in []([uint8], int)&uint8",
		},
		{
			name: "built-in def array index",
			src:  "type my_array [int]",
			call: "(my_array : [5, 6, 7])[1]",
			want: "built-in [](my_array, int)&int",
		},
		{
			name: "built-in string index",
			call: `"hello"[1]`,
			want: "built-in [](string, int)uint8",
		},
		{
			name: "built-in def string slice",
			src:  "type my_string string",
			call: `(my_string : "hello")[1]`,
			want: "built-in [](my_string, int)uint8",
		},
		{
			name: "built-in array slice, no expected type",
			call: "[5, 6, 7][1, 2]",
			want: "built-in []([int], int, int)[int]",
		},
		{
			name: "built-in array slice, expected type",
			call: "[(float32 : 5), 6, 7][1, 2]",
			ret:  "[float32]",
			want: "built-in []([float32], int, int)[float32]",
		},
		{
			name: "built-in def array slice",
			src:  "type my_array [int]",
			call: "(my_array : [5, 6, 7])[1, 2]",
			want: "built-in [](my_array, int, int)my_array",
		},
		{
			name: "built-in array slice, expected type not an array",
			call: "[5, 6, 7][1, 2]",
			ret:  "int",
			err:  "cannot convert returned \\[int\\] to int",
		},
		{
			name: "built-in array slice, arg not an array",
			call: "1[1, 2]",
			err:  "not an array or string",
		},
		{
			name: "built-in string slice",
			call: `"hello"[1, 5]`,
			want: "built-in [](string, int, int)string",
		},
		{
			name: "built-in def string slice",
			src:  "type my_string string",
			call: `(my_string : "hello")[1, 5]`,
			want: "built-in [](my_string, int, int)my_string",
		},
		{
			name: "built-in .length string",
			call: `"hello".length`,
			want: "built-in .length(string)int",
		},
		{
			name: "built-in .length def string",
			src:  "type my_string string",
			call: `(my_string : "hello").length`,
			want: "built-in .length(my_string)int",
		},
		{
			name: "built-in .length def string ref fails",
			src:  "type my_string &string",
			call: `(my_string : "hello").length`,
			err:  "not an array or string",
		},
		{
			name: "built-in .length array",
			call: `[1].length`,
			want: "built-in .length([int])int",
		},
		{
			name: "built-in .length def array",
			src:  "type my_array [int]",
			call: `(my_array : [1, 2, 3]).length`,
			want: "built-in .length(my_array)int",
		},
		{
			name: "built-in .length def array ref fails",
			src:  "type my_string &[int]",
			call: `(my_string : [1, 2, 3]).length`,
			err:  "not an array or string",
		},
		{
			name: "built-in .length, not an array or string",
			call: "5.length",
			err:  "not an array or string",
		},
		{
			name: "built-in .length, other mod array",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo [int]
				`,
			},
			call: "make_foo().length",
			want: "built-in .length(other#foo)int",
		},
		{
			name: "built-in .length, other mod opaque array fails",
			src: `
				import "other"
				func make_foo() other#foo
			`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo [int]
				`,
			},
			call: "make_foo().length",
			err:  "not an array or string",
		},
		{
			name: "built-in panic",
			call: "panic(\"hello\")",
			want: "built-in panic(string)",
		},
		{
			name: "built-in print",
			call: "print(\"hello\")",
			want: "built-in print(string)",
		},
		{
			name: "unify parm simple",
			src:  "func f(_ T)",
			call: "f(1)",
			want: "f(int)",
		},
		{
			name: "unify parm matches a defined type",
			src: `
				func f(_ T)
				type point [.x int, .y int]
			`,
			call: "f(point : [.x 0, .y 0])",
			want: "f(point)",
		},
		{
			name: "unify parm matches infers conversion to literal type",
			src: `
				func f(_ [.x X, .y Y])
				type (X, Y) pair [.x X, .y Y]
				var x (int, string) pair
			`,
			call: `f(x)`,
			want: "f([.x int, .y string])",
		},
		{
			name: "unify parm matches infers conversion from literal type",
			src: `
				func f(_ (X, Y) pair)
				type (X, Y) pair [.x X, .y Y]
			`,
			call: `f([.x 0, .y "hello"])`,
			want: "f((int, string) pair)",
		},
		{
			name: "unify parm matches a reference type",
			src:  "func f(_ T)",
			call: "f(&int : 1)",
			want: "f(&int)",
		},
		{
			name: "unify parm matches a same number of references",
			src: `
				func f(_ &&T)
				var x &int
			`,
			call: "f(&&int : x)",
			want: "f(&&int)",
		},
		{
			name: "unify parm infers dereference conversion",
			src: `
				func f(_ [.x T])
				var x &[.x int]
			`,
			call: "f(x)",
			want: "f([.x int])",
		},
		{
			name: "unify parm infers dereference conversion to type var",
			src: `
				func f(_ &T)
				var x &&int
			`,
			call: "f(x)",
			want: "f(&&int)",
		},
		{
			name: "unify parm infers reference conversion",
			src: `
				func f(_ &T)
				var x int
			`,
			call: "f(x)",
			want: "f(&int)",
		},
		{
			name: "unify parm infers reference conversion of def type",
			src: `
				func f(_ &T)
				type point [.x int, .y int]
				var x point
			`,
			call: "f(x)",
			want: "f(&point)",
		},
		{
			name: "unify parm infers reference conversion of def ref type",
			src: `
				func f(_ &T)
				type point &[.x int, .y int]
				var x point
			`,
			call: "f(x)",
			want: "f(&point)",
		},
		{
			name: "unify parm infers reference conversion, but conversion fails",
			src: `
				func f(_ &&T)
				var x int
			`,
			call: "f(x)",
			err:  `cannot convert argument x \(int\) to &&int`,
		},
		{
			name: "unify parm infers reference conversion to multiple references",
			src: `
				func f(_ &&T)
				func x() &int
			`,
			call: "f(x())",
			want: "f(&&int)",
		},
		{
			name: "unify parm ref conversion, bind happens inside literal",
			src: `
				func f(_ &&[.foo X])
				var x &[.foo int]
			`,
			call: "f(x)",
			want: "f(&&[.foo int])",
		},
	}
	for _, test := range tests {
		test := test
		t.Run(test.name, func(t *testing.T) {
			if strings.HasPrefix(test.name, "SKIP") {
				t.Skip()
			}
			var src string
			if test.ret != "" {
				src = fmt.Sprintf("%s\nvar zz %s := %s\n", test.src, test.ret, test.call)
			} else {
				src = fmt.Sprintf("%s\nvar zz (){} := { _ := (%s) }\n", test.src, test.call)
			}
			t.Log(src)
			mod, errs := check("test", []string{src}, []testMod{test.otherMod})
			switch {
			case test.err == "" && len(errs) == 0:
				expr := findVarDef(t, "zz", mod).Expr
				if test.ret == "" {
					expr = expr.(*Convert).Expr.(*BlockLit).Exprs[0].(*Call).Args[1]
				}
				call := findCall(expr)
				if call == nil {
					t.Fatalf("no call: %s", expr)
				}
				got := call.Func.String()
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
	case *Convert:
		return findCall(e.Expr)
	default:
		fmt.Printf("%T unimplemented", e)
		return nil
	}
}

// This tests some simple Church numerals implementation.
// It's not testing anything inparticular, really,
// but when creating it, it uncovered several bugs
// in calling def-function types and type instantiation,
// so it seems useful to just have it here to catch any regressions.
func TestChurch(t *testing.T) {
	const src = `
		Type num (num){num}
		Func zero() num { return: (f){ (x){x} } }
		Func next(n num) num { return: (f){ (x){ n(f(x)) } } }
		Func +(n num, m num) num { return: (f){ (x){ m(f)(n(f)(x)) } } }
		Func *(n num, m num) num { return: (f){ (x){ m(n(f))(x) } } }
		Func ^(n num, m num) num { return: m(n) }
	`
	if _, errs := check("church", []string{src}, nil); len(errs) > 0 {
		t.Errorf("failed to check: %s", errs[0])
	}
}

// Tests various switch return type inferences that should compile without error.
func TestSwitchReturnTypes(t *testing.T) {
	const src = `
		type a_or_b [?a, ?b]
		var _ (){} := {
			m() ?a {1} ?b {1},
			//m() ?a {"hello"} ?b {1},
			m() ?a {1},
			m() ?b {1},
		}
		var _ int := m() ?a {1} ?b {1}
		//var _ [.] := m() ?a {1} ?b {1}
		var _ [.] := m() ?a {1}
		var _ [.] := m() ?a {} ?b {}
		func m() a_or_b
	`
	if _, errs := check("test", []string{src}, nil); len(errs) > 0 {
		t.Errorf("%v\n", errs)
	}
}

func TestCaptureParm(t *testing.T) {
	const src = `
		var x (int){} := (i int){
			{i + i + i}
		}
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("%v\n", errs)
	}
	t.Log(mod)
	x := findVarDef(t, "x", mod)
	bOuter := x.Expr.(*Convert).Expr.(*BlockLit)
	bInner := bOuter.Exprs[0].(*Convert).Expr.(*BlockLit)
	if len(bInner.Caps) != 1 {
		t.Fatalf("got %d caps, expected 1", len(bInner.Caps))
	}
	if bInner.Caps[0].Parm != &bOuter.Parms[0] {
		t.Errorf("expected parameter capture, got %v", bInner.Caps[0])
	}
}

func TestCaptureCapture(t *testing.T) {
	const src = `
		var x (int){} := (i int){
			{{i + i + i}}
		}
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("%v\n", errs)
	}
	t.Log(mod)
	x := findVarDef(t, "x", mod)
	bOuter := x.Expr.(*Convert).Expr.(*BlockLit)
	bMid := bOuter.Exprs[0].(*Convert).Expr.(*BlockLit)
	if len(bMid.Caps) != 1 {
		t.Fatalf("got %d mid caps, expected 1", len(bMid.Caps))
	}
	if bMid.Caps[0].Parm != &bOuter.Parms[0] {
		t.Errorf("expected parameter capture, got %v", bMid.Caps[0])
	}

	bInner := bMid.Exprs[0].(*Convert).Expr.(*BlockLit)
	if len(bInner.Caps) != 1 {
		t.Fatalf("got %d inner caps, expected 1", len(bInner.Caps))
	}
	if bInner.Caps[0].Cap != bMid.Caps[0] {
		t.Errorf("expected capture capture, got %v", bMid.Caps[0])
	}
}

func TestCaptureOnCall(t *testing.T) {
	const src = `
		var x ((){}){} := (f (){}){
			{f()}
		}
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("%v\n", errs)
	}
	t.Log(mod)
	x := findVarDef(t, "x", mod)
	bOuter := x.Expr.(*Convert).Expr.(*BlockLit)
	bInner := bOuter.Exprs[0].(*Convert).Expr.(*BlockLit)
	if len(bInner.Caps) != 1 {
		t.Fatalf("got %d caps, expected 1", len(bInner.Caps))
	}
	if bInner.Caps[0].Parm != &bOuter.Parms[0] {
		t.Errorf("expected parameter capture, got %v", bInner.Caps[0])
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
		expr     string
		infer    string
		want     string
		src      string
		otherMod testMod
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
		{expr: "1.0", infer: "&&int8", want: "float64"},
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
		{
			expr:  "1.0",
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo float64
				`,
			},
		},
		{
			expr:  "1.0",
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "float64",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo float64
				`,
			},
		},

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
		{
			expr:  "1",
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo int64
				`,
			},
		},
		{
			expr:  "1",
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "int",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo int64
				`,
			},
		},

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
		{
			expr:  `"abc"`,
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo string
				`,
			},
		},
		{
			expr:  `"abc"`,
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "string",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo string
				`,
			},
		},

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
		{
			expr:  `(i int){"foo"}`,
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo (int){string}
				`,
			},
		},
		{
			expr:  `(i int){"foo"}`,
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "(int){string}",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo (int){string}
				`,
			},
		},

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
		{
			expr:  `[?none]`,
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo [?none, ?some int]
				`,
			},
		},
		{
			expr:  `[?none]`,
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "[?none]",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo [?none, ?some int]
				`,
			},
		},

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
		{
			expr:  `[.x 5, .y 6]`,
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo [.x int, .y int]
				`,
			},
		},
		{
			expr:  `[.x 5, .y 6]`,
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "[.x int, .y int]",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo [.x int, .y int]
				`,
			},
		},

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
		{
			expr:  `[5, 6]`,
			infer: "other#foo",
			want:  "other#foo",
			src:   `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo [int]
				`,
			},
		},
		{
			expr:  `[5, 6]`,
			infer: "other#foo",
			// Fails to use type other#foo, as it's opaque, not exported.
			want: "[int]",
			src:  `import "other"`,
			otherMod: testMod{
				path: "other",
				src: `
					Type foo := _foo
					type _foo [int]
				`,
			},
		},
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
			mod, errs := check("test", []string{src}, []testMod{test.otherMod})
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
			expr, _ := checkAndConvertExpr(mod.Files[0], parserExpr, infer)
			want := findTypeDef(t, "want", mod).Type
			if !eqType(expr.Type(), want) {
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
			got := literalType(findTypeDef(t, "got", mod).Type)
			lit := findTypeDef(t, "lit", mod).Type
			if !eqType(got, lit) {
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
				if !eqType(typ, s) {
					t.Errorf("%s != %s", typ, s)
				}
			}
			for i := range test.Diff {
				d := findVarDef(t, fmt.Sprintf("d%d", i), mod).T
				if eqType(typ, d) {
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
