package main

import (
	"fmt"
	"os"
	"strings"

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
	p := parser.New()
	if err := p.Parse(path, in); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	file := p.Files[0]
	if dir, err := os.Getwd(); err == nil {
		file.P = strings.TrimPrefix(file.P, dir+"/")
	}
	p.Files[0].Print(os.Stdout, parser.PrintLocs(file))
}
