package main

import (
	"fmt"
	"os"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/parser"
)

func main() {
	in := os.Stdin
	path := "<stdin>"
	if len(os.Args) > 1 {
		path = os.Args[1]
		f, err := os.Open(path)
		if err != nil {
			fmt.Printf("failed to open %s: %s", path, err)
			os.Exit(1)
		}
		defer f.Close()
		in = f
	}
	p := parser.NewParser()
	if err := p.Parse(path, in); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	m, fs, errs := checker.Check("main", p.Files, nil)
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
		}
		os.Exit(1)
	}
	m.Print(os.Stdout, checker.PrintLocs(fs))

	for _, def := range m.Defs {
		td, ok := def.(*checker.TypeDef)
		if !ok {
			continue
		}
		fmt.Printf("%s: %s\n", td.Name, td.Type.String())
	}
}
