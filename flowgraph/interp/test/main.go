package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/flowgraph/interp"
	"github.com/eaburns/pea/mod"
	"github.com/eaburns/pea/parser"
)

var (
	opt   = flag.Bool("opt", true, "Enable optimizations")
	trace = flag.Bool("trace", false, "Enable instruction tracing")
	root  = flag.String("root", ".", "module root directory")
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
	r := mod.NewRoot(*root)
	imp := checker.NewImporter(r, p.Files, "")
	m, _, errs := checker.Check("main", p.Files, checker.UseImporter(imp))
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
	g := flowgraph.Build(m, options...)
	var main *flowgraph.FuncDef
	for _, f := range g.Funcs {
		if f.Name == "main" && len(f.Parms) == 0 {
			main = f
			break
		}
	}
	if main == nil {
		fmt.Println("no main function")
		os.Exit(1)
	}
	interp := interp.New()
	interp.Trace = *trace
	if g.Init != nil {
		interp.Eval(g.Init)
	}
	interp.Eval(main)
}
