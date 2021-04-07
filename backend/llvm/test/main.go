package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/eaburns/pea/backend/llvm"
	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/parser"
)

var (
	opt  = flag.Bool("opt", true, "whether to optimize")
	test = flag.Bool("test", false, "whether to generate a test binary")
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
	m, locFiles, errs := checker.Check("main", p.Files, nil)
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
		}
		os.Exit(1)
	}
	var flowgraphOpts []flowgraph.Option
	if !*opt {
		flowgraphOpts = append(flowgraphOpts, flowgraph.NoOptimize)
	}
	g := flowgraph.Build(m, flowgraphOpts...)

	llvmOpts := []llvm.Option{llvm.LocFiles(locFiles)}
	if *test {
		llvmOpts = append(llvmOpts, llvm.TestMain)
	}
	if err := llvm.Generate(os.Stdout, g, llvmOpts...); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
