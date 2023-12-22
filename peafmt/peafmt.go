package main

import (
	"fmt"
	"os"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
	"github.com/eaburns/pea/printer"
)

func main() {
	print(parse())
}

func parse() (*parser.File, loc.Files) {
	in := os.Stdin
	path := "<stdin>"
	if len(os.Args) > 1 {
		path = os.Args[1]
		f, err := os.Open(path)
		if err != nil {
			fail("failed to open %s: %s", path, err)
		}
		defer f.Close()
		in = f
	} else if samfile := os.Getenv("samfile"); samfile != "" {
		path = samfile
	}

	p := parser.New()
	wd, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to get the current directory: %s", err)
	} else {
		p.TrimErrorPathPrefix = wd + "/"
	}
	if err := p.Parse(path, in); err != nil {
		fail("%s", err)
	}
	var locs loc.Files
	for _, f := range p.Files {
		locs = append(locs, f)
	}
	return p.Files[0], locs
}

func print(file *parser.File, locs loc.Files) {
	out := os.Stdout
	path := "<stdout>"
	if len(os.Args) > 1 {
		path = os.Args[1]
		f, err := os.Create(path)
		if err != nil {
			fail("failed to open %s: %s", path, err)
		}
		defer f.Close()
		out = f
	} else if samfile := os.Getenv("samfile"); samfile != "" {
		path = samfile
	}
	if err := printer.Print(out, file, locs); err != nil {
		fail("failed to write to %s: %s", path, err)
	}
}

func fail(f string, xs ...interface{}) {
	fmt.Fprintf(os.Stderr, f, xs...)
	os.Exit(1)
}
