package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/llvm"
	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

var (
	libpea       = flag.String("libpea", "", "path to libpea source directory")
	root         = flag.String("root", "", "module root directory (required)")
	test         = flag.Bool("test", false, "whether to compile a test binary")
	v            = flag.Bool("v", false, "print commands executed")
	lprofiler    = flag.Bool("lprofiler", false, "whether to link with -lprofiler for CPU profiling")
	dumpFG       = flag.Bool("dump-fg", false, "whether to dump the flowgraph")
	traceEsc     = flag.Bool("trace-esc", false, "whether to trace escape analysis")
	traceInline  = flag.Bool("trace-inl", false, "whether to trace inlining")
	printNAllocs = flag.Bool("print-nallocs", false, "whether to print the number of heap allocs")
)

func main() {
	flag.Parse()
	args := flag.Args()
	switch {
	case *root == "":
		usage("-root is required")
	case len(args) == 0:
		usage("a module path is required")
	case len(args) > 1:
		usage("only one module path is supported")
	}
	m, err := load(*root, args[0])
	if err != nil {
		fail(fmt.Errorf("%s", err))
	}
	m.compile()
}

func usage(msg string) {
	fmt.Printf("%s\n", msg)
	fmt.Printf("peac [flags] -root <root_path> <module_path>\n")
	flag.PrintDefaults()
	os.Exit(1)
}

type Mod struct {
	Path     string
	FullPath string
	SrcFiles []string
	Deps     []*Mod
}

func (m *Mod) binFile() string {
	return filepath.Join(m.FullPath, filepath.Base(m.Path))
}

func load(rootDir, modPath string) (*Mod, error) {
	var path []string
	onPath := make(map[string]bool)
	seen := make(map[string]*Mod)
	var ld func(string) (*Mod, error)
	ld = func(modPath string) (*Mod, error) {
		path = append(path, modPath)
		defer func() { path = path[:len(path)-1] }()
		if onPath[modPath] {
			return nil, fmt.Errorf("dependency cycle: %v", path)
		}
		onPath[modPath] = true
		defer func() { delete(onPath, modPath) }()

		if mod, ok := seen[modPath]; ok {
			return mod, nil
		}
		mod := &Mod{
			Path:     modPath,
			FullPath: filepath.Join(rootDir, modPath),
		}
		seen[modPath] = mod

		dirInfos, err := ioutil.ReadDir(mod.FullPath)
		if err != nil {
			return nil, err
		}
		var imports []string
		seenImports := make(map[string]bool)
		for _, dirInfo := range dirInfos {
			// Ignore .pea.ll files, which are likely build artifacts.
			if strings.HasPrefix(dirInfo.Name(), ".pea.ll") {
				continue
			}
			filePath := filepath.Join(mod.FullPath, dirInfo.Name())
			if filepath.Ext(filePath) == ".pea" {
				imps, err := parser.ImportsOnly(filePath)
				if err != nil {
					return nil, err
				}
				for _, imp := range imps {
					if seenImports[imp] {
						continue
					}
					seenImports[imp] = true
					imports = append(imports, imp)
				}
			}
			if ext := filepath.Ext(filePath); ext == ".pea" || ext == ".c" || ext == ".ll" {
				mod.SrcFiles = append(mod.SrcFiles, filePath)
			}
		}
		for _, imp := range imports {
			m, err := ld(imp)
			if err != nil {
				return nil, err
			}
			mod.Deps = append(mod.Deps, m)
		}
		return mod, nil
	}
	return ld(modPath)
}

func (m *Mod) compile() {
	aFiles := m.compileDeps()

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
		fg, locs = m.flowgraph(opts...)
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
	aFile := m.binFile() + ".a"
	// The change time depends on not only the source files,
	// but the latest time that any dependency has changed also,
	// since type-parameterized function changes in a dependency
	// can change the .a file for the dependent.
	if modTime(aFile).After(lastChange(append(aFiles, m.SrcFiles...))) {
		if *v {
			fmt.Printf("---- %s: archive up-to-date\n", m.Path)
		}
	} else {
		if *v {
			fmt.Printf("---- %s: building archive\n", m.Path)
		}
		if fg == nil {
			fg, locs = m.flowgraph()
		}
		m.compileA(fg, locs)
	}
	aFiles = append(aFiles, aFile)

	if !*test && m.Path != "main" {
		return
	}

	binFile := m.binFile()
	if *test {
		binFile += ".test"
	}
	if modTime(binFile).After(lastChange(aFiles)) {
		if *v {
			if *test {
				fmt.Printf("---- %s: test binary up-to-date\n", m.Path)
			} else {
				fmt.Printf("---- %s: binary up-to-date\n", m.Path)
			}
		}
		return
	}
	if *v {
		if *test {
			fmt.Printf("---- %s: building test binary\n", m.Path)
		} else {
			fmt.Printf("---- %s: building binary\n", m.Path)
		}
	}
	if fg == nil {
		fg, locs = m.flowgraph()
	}

	llFile := m.binFile() + ".main.ll"
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
	defer os.Remove(llFile)
	oFile := m.binFile() + ".main.o"
	if err := compileLL(llFile, oFile); err != nil {
		fail(fmt.Errorf("%s", err))
	}
	defer os.Remove(oFile)
	link(binFile, append([]string{oFile}, aFiles...))
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

func (m *Mod) compileDeps() []string {
	var aFiles []string
	seen := make(map[*Mod]bool)
	for _, d := range m.Deps {
		aFiles = append(aFiles, d._compileDeps(seen)...)
	}
	return aFiles
}

func (m *Mod) _compileDeps(seen map[*Mod]bool) []string {
	if seen[m] {
		return nil
	}
	seen[m] = true
	var aFiles []string
	for _, d := range m.Deps {
		aFiles = append(aFiles, d._compileDeps(seen)...)
	}
	aFile := m.binFile() + ".a"
	// The change time depends on not only the source files,
	// but the latest time that any dependency has changed also,
	// since type-parameterized function changes in a dependency
	// can change the .a file for the dependent.
	if modTime(aFile).After(lastChange(append(aFiles, m.SrcFiles...))) {
		if *v {
			fmt.Printf("---- %s: up-to-date\n", m.Path)
		}
	} else {
		if *v {
			fmt.Printf("---- %s: building\n", m.Path)
		}
		m.compileA(m.flowgraph())
	}
	return append(aFiles, aFile)
}

func (m *Mod) compileA(fg *flowgraph.Mod, locs loc.Files) {
	var oFiles []string
	for _, file := range m.SrcFiles {
		switch filepath.Ext(file) {
		case ".c":
			oFile := strings.TrimSuffix(file, ".c") + ".o"
			if err := run("clang", "-g", "-o", oFile, "-c", file); err != nil {
				fail(err)
			}
			defer os.Remove(oFile)
			oFiles = append(oFiles, oFile)
		case ".ll":
			oFile := strings.TrimSuffix(file, ".ll") + ".ll.o"
			if err := compileLL(file, oFile); err != nil {
				fail(err)
			}
			defer os.Remove(oFile)
			oFiles = append(oFiles, oFile)
		}
	}

	llFile := m.binFile() + ".ll"
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
	defer os.Remove(llFile)
	oFile := m.binFile() + ".o"
	if err := compileLL(llFile, oFile); err != nil {
		fail(fmt.Errorf("%s", err))
	}
	defer os.Remove(oFile)
	oFiles = append(oFiles, oFile)

	aFile := m.binFile() + ".a"
	if err := run("llvm-ar", append([]string{"cr", aFile}, oFiles...)...); err != nil {
		fail(fmt.Errorf("%s", err))
	}
}

func (m *Mod) flowgraph(fgOpts ...flowgraph.Option) (fg *flowgraph.Mod, locs loc.Files) {
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
		p.TrimPathPrefix = wd + "/"
	}
	for _, file := range peaFiles {
		if err := p.ParseFile(file); err != nil {
			fail(err)
		}
	}
	imp := checker.NewImporterTemplateParser(*root, p)
	checkOpts := []checker.Option{checker.UseImporter(imp)}
	checkMod, locs, errs := checker.Check(m.Path, p.Files, checkOpts...)
	if len(errs) > 0 {
		fail(errs...)
	}
	return flowgraph.Build(checkMod, fgOpts...), locs
}

func compileLL(file, oFile string) error {
	if err := run("llc", file, "-filetype", "obj", "-o", oFile); err != nil {
		fail(fmt.Errorf("%s", err))
	}
	return nil
}

func run(cmd string, args ...string) error {
	c := exec.Command(cmd, args...)
	if *v {
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
	for _, err := range errs {
		fmt.Println(err)
	}
	os.Exit(1)
}
