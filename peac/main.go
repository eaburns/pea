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
	libpea = flag.String("libpea", "", "path to libpea source directory")
	root   = flag.String("root", "", "module root directory (required)")
	test   = flag.Bool("test", false, "whether to compile a test binary")
	v      = flag.Bool("v", false, "print the dependency graph")
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
	mod, err := load(*root, args[0])
	if err != nil {
		die("%s", err)
	}
	if *v {
		printDeps(mod)
	}
	if errs := compile(mod, *test); len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
		}
		os.Exit(1)
	}
}

func usage(msg string) {
	fmt.Printf("%s\n", msg)
	fmt.Printf("peac [flags] -root <root_path> <module_path>\n")
	flag.PrintDefaults()
	os.Exit(1)
}

func die(f string, vs ...interface{}) {
	fmt.Printf(f+"\n", vs...)
	os.Exit(1)
}

type Mod struct {
	Path     string
	FullPath string
	SrcFiles []string
	Deps     []*Mod
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

func printDeps(mod *Mod) {
	seen := make(map[*Mod]bool)
	var print func(*Mod, string)
	print = func(mod *Mod, indent string) {
		fmt.Printf("%s%s", indent, mod.Path)
		if needsUpdate(mod) {
			fmt.Printf("*\n")
		} else {
			fmt.Printf("\n")
		}
		if seen[mod] {
			if len(mod.Deps) > 0 {
				fmt.Printf("%s...\n", indent)
			}
			return
		}
		seen[mod] = true
		for _, dep := range mod.Deps {
			print(dep, indent+"\t")
		}
	}
	print(mod, "")
}

func compile(mod *Mod, testMain bool) []error {
	seen := make(map[*Mod]bool)
	var oFiles []string
	var buildObjFiles func(*Mod, bool) (bool, []error)
	buildObjFiles = func(mod *Mod, testMain bool) (bool, []error) {
		if seen[mod] {
			return false, nil
		}
		oFiles = append(oFiles, objFile(mod))
		seen[mod] = true
		// TODO: don't always recompile on testMain, but check if it needs updating.
		upToDate := !testMain && !needsUpdate(mod)
		for _, dep := range mod.Deps {
			changed, errs := buildObjFiles(dep, false)
			if len(errs) > 0 {
				return false, errs
			}
			if changed {
				upToDate = false
			}
		}
		if upToDate {
			if *v {
				fmt.Println("---- up-to-date mod", mod.Path)
			}
			return false, nil
		}
		return true, compile1(mod, testMain)
	}
	changed, errs := buildObjFiles(mod, testMain)
	if len(errs) > 0 {
		return errs
	}
	if filepath.Base(mod.Path) != "main" && !testMain {
		return nil
	}

	binFile := filepath.Join(mod.FullPath, filepath.Base(mod.Path))
	if testMain {
		binFile += ".test"
	}
	if !changed && modTime(binFile).After(time.Time{}) {
		if *v {
			if testMain {
				fmt.Println("---- up-to-date test binary", mod.Path)
			} else {
				fmt.Println("---- up-to-date binary", mod.Path)
			}
		}
		return nil
	}
	if *v {
		if testMain {
			fmt.Println("---- linking test binary", mod.Path)
		} else {
			fmt.Println("---- linking binary", mod.Path)
		}
	}
	if testMain {
		testOFile := filepath.Join(mod.FullPath, filepath.Base(mod.Path)+".test.pea.o")
		oFiles = append([]string{testOFile}, oFiles...)
		defer os.Remove(testOFile)
	} else if filepath.Base(mod.Path) == "main" {
		mainOFile := filepath.Join(mod.FullPath, filepath.Base(mod.Path)+".main.pea.o")
		oFiles = append([]string{mainOFile}, oFiles...)
		defer os.Remove(mainOFile)
	}
	args := append(oFiles, []string{
		"-g",
		"-o", binFile,
		"-pthread",
		"-ldl", // needed for libunwind
		filepath.Join(*libpea, "libpea.a"),
	}...)
	if err := run("clang", args...); err != nil {
		die("%s", err)
	}
	return nil
}

func compile1(mod *Mod, testMain bool) []error {
	if *v {
		fmt.Println("---- compiling mod", mod.Path)
	}
	var oFiles []string

	fg, locFiles, errs := buildFlowGraph(mod)
	if len(errs) > 0 {
		return errs
	}

	oFile := filepath.Join(mod.FullPath, filepath.Base(mod.Path)+".pea.o")
	compilePeaDefs(mod, fg, locFiles, oFile)
	defer os.Remove(oFile)
	oFiles = append(oFiles, oFile)

	if testMain {
		oFile = filepath.Join(mod.FullPath, filepath.Base(mod.Path)+".test.pea.o")
		compilePeaTestMain(mod, fg, locFiles, oFile)
	} else if filepath.Base(mod.Path) == "main" {
		oFile = filepath.Join(mod.FullPath, filepath.Base(mod.Path)+".main.pea.o")
		compilePeaMain(mod, fg, locFiles, oFile)
	}

	for _, file := range mod.SrcFiles {
		switch filepath.Ext(file) {
		case ".c":
			oFile := strings.TrimSuffix(file, ".c") + ".o"
			if err := run("clang", "-g", "-o", oFile, "-c", file); err != nil {
				return []error{err}
			}
			defer os.Remove(oFile)
			oFiles = append(oFiles, oFile)
		case ".ll":
			oFile := strings.TrimSuffix(file, ".ll") + ".ll.o"
			if err := compileLL(file, oFile); err != nil {
				return []error{err}
			}
			defer os.Remove(oFile)
			oFiles = append(oFiles, oFile)
		default:
			continue
		}
	}
	if err := run("llvm-ar", append([]string{"cr", objFile(mod)}, oFiles...)...); err != nil {
		die("%s", err)
	}
	return nil
}

func compilePeaDefs(mod *Mod, fg *flowgraph.Mod, locFiles loc.Files, oFile string) {
	llFile := strings.TrimSuffix(oFile, "o") + "ll"
	if *v {
		fmt.Println("defs file: ", llFile)
	}
	f, err := os.Create(llFile)
	if err != nil {
		die("failed to create output file: %s", err)
	}
	w := bufio.NewWriter(f)
	llvm.GenerateDefs(w, fg, locFiles)
	w.Flush()
	if err := f.Close(); err != nil {
		die("failed to close output file: %s", err)
	}
	defer os.Remove(llFile)

	if err := compileLL(llFile, oFile); err != nil {
		die("%s", err)
	}
}

func compilePeaTestMain(mod *Mod, fg *flowgraph.Mod, locFiles loc.Files, oFile string) {
	llFile := strings.TrimSuffix(oFile, "o") + "ll"
	if *v {
		fmt.Println("test main file: ", llFile)
	}
	f, err := os.Create(llFile)
	if err != nil {
		die("failed to create output file: %s", err)
	}
	w := bufio.NewWriter(f)
	llvm.GenerateTestMain(w, fg, locFiles)
	w.Flush()
	if err := f.Close(); err != nil {
		die("failed to close output file: %s", err)
	}
	defer os.Remove(llFile)

	if err := compileLL(llFile, oFile); err != nil {
		die("%s", err)
	}
}

func compilePeaMain(mod *Mod, fg *flowgraph.Mod, locFiles loc.Files, oFile string) {
	llFile := strings.TrimSuffix(oFile, "o") + "ll"
	if *v {
		fmt.Println("main file: ", llFile)
	}
	f, err := os.Create(llFile)
	if err != nil {
		die("failed to create output file: %s", err)
	}
	w := bufio.NewWriter(f)
	var main *flowgraph.FuncDef
	for _, fun := range fg.Funcs {
		if fun.Mod == "main" && fun.Name == "main" {
			main = fun
		}
	}
	if main == nil {
		die("no main function")
	}
	llvm.GenerateMain(w, fg, main, locFiles)
	w.Flush()
	if err := f.Close(); err != nil {
		die("failed to close output file: %s", err)
	}
	defer os.Remove(llFile)

	if err := compileLL(llFile, oFile); err != nil {
		die("%s", err)
	}
}

func buildFlowGraph(mod *Mod) (*flowgraph.Mod, loc.Files, []error) {
	var files []string
	for _, file := range mod.SrcFiles {
		if filepath.Ext(file) == ".pea" {
			files = append(files, file)
		}
	}
	if len(files) == 0 {
		return nil, nil, []error{fmt.Errorf("module %s has no pea source files", mod.Path)}
	}
	if *v {
		fmt.Print("pea files: ")
		for i, file := range files {
			if i > 0 {
				fmt.Print(", ")
			}
			fmt.Print(file)
		}
		fmt.Println("")
	}
	p := parser.New()
	if wd, err := os.Getwd(); err == nil {
		p.TrimPathPrefix = wd + "/"
	}
	for _, file := range files {
		if err := p.ParseFile(file); err != nil {
			return nil, nil, []error{err}
		}
	}
	imp := checker.NewImporter(*root, p.Files)
	m, locFiles, errs := checker.Check(mod.Path, p.Files, imp)
	if len(errs) > 0 {
		return nil, nil, errs
	}
	return flowgraph.Build(m), locFiles, nil
}

func compileLL(file, oFile string) error {
	sFile := strings.TrimSuffix(file, ".ll") + ".ll.s"
	if err := run("llc", file, "-o", sFile); err != nil {
		die("%s", err)
	}
	defer os.Remove(sFile)

	if err := run("clang", "-g", "-o", oFile, "-c", sFile); err != nil {
		die("%s", err)
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

func needsUpdate(mod *Mod) bool {
	return modTime(objFile(mod)).Before(latestChange(mod))
}

func objFile(mod *Mod) string {
	return filepath.Join(mod.FullPath, filepath.Base(mod.Path)+".a")
}

func latestChange(mod *Mod) time.Time {
	if len(mod.SrcFiles) == 0 {
		return time.Time{}
	}
	latest := modTime(mod.SrcFiles[0])
	for _, file := range mod.SrcFiles[1:] {
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
		die("failed to get modtime for %s: %s", file, err)
		panic("unreachable")
	default:
		return fileInfo.ModTime()
	}
}
