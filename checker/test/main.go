package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/parser"
)

var (
	dump = flag.Bool("d", false, "dump the check tree")
	root = flag.String("root", ".", "module root directory")
)

func main() {
	flag.Parse()

	in := os.Stdin
	path := "<stdin>"
	if len(flag.Args()) == 1 {
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
	if err := p.Parse(path, in); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	imp := checker.NewImporter(*root, p.Files, "")
	m, fs, errs := checker.Check("main", p.Files, checker.UseImporter(imp))
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
		}
		os.Exit(1)
	}
	if *dump {
		m.Print(os.Stdout, checker.PrintLocs(fs))
	}
}
