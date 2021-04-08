package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/parser"
)

var (
	opt      = flag.Bool("opt", true, "whether to optimize")
	traceEsc = flag.Bool("esc", false, "whether to trace escape analysis")
	root     = flag.String("root", ".", "module root directory")
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
	imp := checker.NewImporter(*root, p.Files)
	m, _, errs := checker.Check("main", p.Files, imp)
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
		}
		os.Exit(1)
	}
	var options []flowgraph.Option
	if !*opt {
		options = append(options, flowgraph.NoOptimize)
	}
	if *traceEsc {
		options = append(options, flowgraph.TraceEscape)
	}
	g := flowgraph.Build(m, options...)
	fmt.Println(g.String())
}
