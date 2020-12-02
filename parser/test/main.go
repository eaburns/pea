package main

import (
	"fmt"
	"os"

	"github.com/eaburns/pea/parser"
	"github.com/eaburns/pretty"
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
	pretty.Print(p.Files[0])
}
