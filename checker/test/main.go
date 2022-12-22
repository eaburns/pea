package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/mod"
	"github.com/eaburns/pea/parser"
)

var (
	dump    = flag.Bool("d", false, "dump the check tree")
	root    = flag.String("root", ".", "module root directory")
	verbose = flag.Bool("v", true, "use verbose error messages")
)

func main() {
	flag.Parse()

	in := os.Stdin
	path := "<stdin>"
	if flag.NArg() == 1 {
		path = flag.Arg(0)
		f, err := os.Open(path)
		if err != nil {
			fmt.Printf("failed to open %s: %s", path, err)
			os.Exit(1)
		}
		defer f.Close()
		in = f
	}
	p := parser.New()
	if wd, err := os.Getwd(); err == nil {
		p.TrimErrorPathPrefix = wd + "/"
	}
	if err := p.Parse(path, in); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	r := mod.NewRoot(*root)
	imp := checker.NewImporter(r, p.Files, "")
	m, fs, errs := checker.Check("main", p.Files,
		checker.UseImporter(imp),
		checker.Verbose(*verbose),
		checker.TrimErrorPathPrefix(p.TrimErrorPathPrefix))
	if len(errs) > 0 {
		for i, err := range errs {
			if i > 0 {
				fmt.Println("")
			}
			fmt.Println(err)
		}
		os.Exit(1)
	}
	if *dump {
		m.Print(os.Stdout, checker.PrintLocs(fs),
			checker.TrimPathPrefix(p.TrimErrorPathPrefix))
	}
}
