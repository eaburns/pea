package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/llvm"
	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/mod"
	"github.com/eaburns/pea/parser"
)

var (
	libpea       = flag.String("libpea", "", "path to libpea source directory")
	root         = flag.String("root", "", "module root directory (required)")
	test         = flag.Bool("test", false, "whether to compile a test binary")
	verbose      = flag.Bool("v", false, "use verbose error messages")
	printCmds    = flag.Bool("print-commands", false, "print commands executed")
	lprofiler    = flag.Bool("lprofiler", false, "whether to link with -lprofiler for CPU profiling")
	dumpFG       = flag.Bool("dump-fg", false, "whether to dump the flowgraph")
	dumpCheck    = flag.Bool("dump-check", false, "whether to dump the checked graph")
	dumpLL       = flag.Bool("dump-ll", false, "whether to dump the .ll files")
	traceEsc     = flag.Bool("trace-esc", false, "whether to trace escape analysis")
	traceInline  = flag.Bool("trace-inl", false, "whether to trace inlining")
	printNAllocs = flag.Bool("print-nallocs", false, "whether to print the number of heap allocs")
	optFG        = flag.Bool("opt-fg", true, "whether to optimize the flowgraph")
)

func main() {
	flag.Parse()
	args := flag.Args()
	switch {
	case *root == "":
		usage("-root is required")
	case len(args) == 0:
		usage("a module path is required")
	}

	r := mod.NewRoot(*root)
	var mods []*mod.Mod
	var src []string
	for _, a := range args {
		if isSourceFile(a) {
			src = append(src, a)
		} else {
			m, err := r.Get(a)
			if err != nil {
				fail(fmt.Errorf("%s", err))
			}
			mods = append(mods, m)
		}
	}
	if len(src) > 0 && len(mods) > 0 {
		fail(fmt.Errorf("mixture of source files and modules is not supported"))
	}
	if len(src) > 0 {
		m, err := r.GetMainBySource(src)
		if err != nil {
			fail(fmt.Errorf("%s", err))
		}
		mods = append(mods, m)
	}
	for _, m := range mods {
		compile(m)
		if len(src) > 0 {
			// This is a temporary main module, built from a list of source.
			// Remove its .a file, since it will not be linked by other modules.
			os.Remove(binFile(m) + ".a")
		}
	}
}

func usage(msg string) {
	fmt.Printf("%s\n", msg)
	fmt.Printf("peac [flags] -root <root_path> <module_path>\n")
	flag.PrintDefaults()
	os.Exit(1)
}

func isSourceFile(path string) bool {
	ext := filepath.Ext(path)
	return ext == ".pea" || ext == ".ll" || ext == ".c"
}

func binFile(m *mod.Mod) string {
	return filepath.Join(m.FullPath, filepath.Base(m.Path))
}

func compile(m *mod.Mod) {
	aFiles := compileDeps(m)

	var fg *flowgraph.Mod
	var locs loc.Files
	if *dumpFG || *traceEsc || *traceInline || *printNAllocs {
		var opts []flowgraph.Option
		if *traceEsc {
			opts = append(opts, flowgraph.TraceEscape)
		}
		if *traceInline {
			opts = append(opts, flowgraph.TraceInlining)
		}
		if !*optFG {
			opts = append(opts, flowgraph.NoOptimize)
		}
		fg, locs = compileFG(m, opts...)
		if *printNAllocs {
			n := 0
			for _, f := range fg.Funcs {
				for _, b := range f.Blocks {
					for _, r := range b.Instrs {
						if a, ok := r.(*flowgraph.Alloc); ok && !a.Stack {
							n++
						}
					}
				}
			}
			fmt.Printf("%d heap allocations\n", n)
		}
		if *dumpFG {
			fmt.Println(fg)
		}
	}
	aFile := binFile(m) + ".a"
	// The change time depends on not only the source files,
	// but the latest time that any dependency has changed also,
	// since type-parameterized function changes in a dependency
	// can change the .a file for the dependent.
	if modTime(aFile).After(lastChange(append(aFiles, m.SrcFiles...))) {
		if *printCmds {
			fmt.Printf("---- %s: archive up-to-date\n", m.Path)
		}
	} else {
		if *printCmds {
			fmt.Printf("---- %s: building archive\n", m.Path)
		}
		if fg == nil {
			fg, locs = compileFG(m)
		}
		compileA(m, fg, locs)
	}
	aFiles = append(aFiles, aFile)

	if !*test && m.Path != "main" {
		return
	}

	binFilePath := binFile(m)
	if *test {
		binFilePath += ".test"
	}
	if modTime(binFilePath).After(lastChange(aFiles)) {
		if *printCmds {
			if *test {
				fmt.Printf("---- %s: test binary up-to-date\n", m.Path)
			} else {
				fmt.Printf("---- %s: binary up-to-date\n", m.Path)
			}
		}
		return
	}
	if *printCmds {
		if *test {
			fmt.Printf("---- %s: building test binary\n", m.Path)
		} else {
			fmt.Printf("---- %s: building binary\n", m.Path)
		}
	}
	if fg == nil {
		fg, locs = compileFG(m)
	}

	llFile := binFile(m) + ".main.ll"
	f, err := os.Create(llFile)
	if err != nil {
		fail(fmt.Errorf("failed to create output file: %s", err))
	}
	w := bufio.NewWriter(f)
	if *test {
		llvm.GenerateTestMain(w, fg, locs)
	} else {
		var main *flowgraph.FuncDef
		for _, fun := range fg.Funcs {
			if fun.Mod == "main" && fun.Name == "main" {
				main = fun
			}
		}
		if main == nil {
			fail(fmt.Errorf("no main function"))
		}
		llvm.GenerateMain(w, fg, main, locs)
	}
	w.Flush()
	if err := f.Close(); err != nil {
		fail(fmt.Errorf("failed to close output file: %s", err))
	}
	if *dumpLL {
		f, err := os.Open(llFile)
		if err != nil {
			fail(fmt.Errorf("%s", err))
		}
		if _, err := io.Copy(os.Stdout, f); err != nil {
			fail(fmt.Errorf("%s", err))
		}
	}
	defer os.Remove(llFile)
	oFile := binFile(m) + ".main.o"
	if err := compileLL(llFile, oFile); err != nil {
		fail(fmt.Errorf("%s", err))
	}
	defer os.Remove(oFile)
	link(binFilePath, append([]string{oFile}, aFiles...))
}

func link(binFile string, objs []string) {
	args := []string{
		"-g",
		"-o", binFile,
		"-pthread",
		"-ldl", // needed for libunwind
	}
	if *lprofiler {
		args = append(args, "-lprofiler")
	}
	args = append(append(objs, args...), filepath.Join(*libpea, "libpea.a"))
	if err := run("clang", args...); err != nil {
		fail(fmt.Errorf("%s", err))
	}
}

func compileDeps(m *mod.Mod) []string {
	var aFiles []string
	seen := make(map[*mod.Mod]bool)
	for _, d := range m.Deps {
		aFiles = append(aFiles, _compileDeps(d, seen)...)
	}
	return aFiles
}

func _compileDeps(m *mod.Mod, seen map[*mod.Mod]bool) []string {
	if seen[m] {
		return nil
	}
	seen[m] = true
	var aFiles []string
	for _, d := range m.Deps {
		aFiles = append(aFiles, _compileDeps(d, seen)...)
	}
	aFile := binFile(m) + ".a"
	// The change time depends on not only the source files,
	// but the latest time that any dependency has changed also,
	// since type-parameterized function changes in a dependency
	// can change the .a file for the dependent.
	if modTime(aFile).After(lastChange(append(aFiles, m.SrcFiles...))) {
		if *printCmds {
			fmt.Printf("---- %s: up-to-date\n", m.Path)
		}
	} else {
		if *printCmds {
			fmt.Printf("---- %s: building\n", m.Path)
		}
		fg, locs := compileFG(m)
		compileA(m, fg, locs)
	}
	return append(aFiles, aFile)
}

func compileA(m *mod.Mod, fg *flowgraph.Mod, locs loc.Files) {
	var oFiles []string
	for _, file := range m.SrcFiles {
		switch filepath.Ext(file) {
		case ".c":
			oFile := file + ".o"
			if err := run("clang", "-I", *libpea, "-I", filepath.Join(*libpea, "vendor/gc-8.2.0/include"), "-g", "-o", oFile, "-c", file); err != nil {
				fail(err)
			}
			defer os.Remove(oFile)
			oFiles = append(oFiles, oFile)
		case ".ll":
			oFile := file + ".ll.o"
			if err := compileLL(file, oFile); err != nil {
				fail(err)
			}
			defer os.Remove(oFile)
			oFiles = append(oFiles, oFile)
		}
	}

	llFile := binFile(m) + ".ll"
	f, err := os.Create(llFile)
	if err != nil {
		fail(fmt.Errorf("failed to create output file: %s", err))
	}
	w := bufio.NewWriter(f)
	llvm.GenerateDefs(w, fg, locs)
	w.Flush()
	if err := f.Close(); err != nil {
		fail(fmt.Errorf("failed to close output file: %s", err))
	}
	if *dumpLL {
		f, err := os.Open(llFile)
		if err != nil {
			fail(fmt.Errorf("%s", err))
		}
		if _, err := io.Copy(os.Stdout, f); err != nil {
			fail(fmt.Errorf("%s", err))
		}
	}
	defer os.Remove(llFile)
	oFile := binFile(m) + ".o"
	if err := compileLL(llFile, oFile); err != nil {
		fail(fmt.Errorf("%s", err))
	}
	defer os.Remove(oFile)
	oFiles = append(oFiles, oFile)

	aFile := binFile(m) + ".a"
	if err := run("ar", append([]string{"cr", aFile}, oFiles...)...); err != nil {
		fail(fmt.Errorf("%s", err))
	}
}

func compileFG(m *mod.Mod, fgOpts ...flowgraph.Option) (fg *flowgraph.Mod, locs loc.Files) {
	var peaFiles []string
	for _, file := range m.SrcFiles {
		if filepath.Ext(file) == ".pea" {
			peaFiles = append(peaFiles, file)
		}
	}
	if len(peaFiles) == 0 {
		fail(fmt.Errorf("module %s has no pea source files", m.Path))
	}
	p := parser.New()
	if wd, err := os.Getwd(); err == nil {
		p.TrimErrorPathPrefix = wd + "/"
	}
	for _, file := range peaFiles {
		if err := p.ParseFile(file); err != nil {
			fail(err)
		}
	}
	imp := checker.NewImporter(m.Root, p.Files, p.TrimErrorPathPrefix)
	checkOpts := []checker.Option{
		checker.UseImporter(imp),
		checker.Verbose(*verbose),
		checker.TrimErrorPathPrefix(p.TrimErrorPathPrefix),
	}
	checkMod, locs, errs := checker.Check(m.Path, p.Files, checkOpts...)
	if len(errs) > 0 {
		fail(errs...)
	}
	if *dumpCheck {
		checkMod.Print(os.Stdout, checker.PrintLocs(locs))
	}
	return flowgraph.Build(checkMod, fgOpts...), locs
}

func compileLL(file, oFile string) error {
	// We use clang here instead of opt+llc, because
	// it sets the target datalayout and triple for us.
	if err := run("clang", "-O2", "-o", oFile, "-c", file); err != nil {
		fail(fmt.Errorf("%s", err))
	}
	return nil
}

func run(cmd string, args ...string) error {
	c := exec.Command(cmd, args...)
	if *printCmds {
		fmt.Println(c)
	}
	var out strings.Builder
	c.Stdout = &out
	c.Stderr = &out
	if err := c.Run(); err != nil {
		return fmt.Errorf("%s failed: %s\n%s", cmd, err, out.String())
	}
	return nil
}

func lastChange(files []string) time.Time {
	if len(files) == 0 {
		return time.Time{}
	}
	latest := modTime(files[0])
	for _, file := range files[1:] {
		t := modTime(file)
		if t.After(latest) {
			latest = t
		}
	}
	return latest
}

func modTime(file string) time.Time {
	switch fileInfo, err := os.Stat(file); {
	case os.IsNotExist(err):
		return time.Time{}
	case err != nil:
		fail(fmt.Errorf("failed to get modtime for %s: %s", file, err))
		panic("unreachable")
	default:
		return fileInfo.ModTime()
	}
}

func fail(errs ...error) {
	for i, err := range errs {
		if i > 0 {
			fmt.Println("")
		}
		fmt.Println(err)
	}
	os.Exit(1)
}
