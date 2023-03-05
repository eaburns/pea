// make_tables downloads the UnicodeData.txt file from unicode.org
// and emits a tables.pea file with range tables for several categories.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"net/http"
	"os"
	"strconv"
	"strings"
	"unicode"
)

var url = "https://www.unicode.org/Public/15.0.0/ucd/UnicodeData.txt"
var catNames = [...]string{"L", "Nd", "P", "Z"}

type Range struct {
	min, max rune
}

func main() {
	flag.Parse()

	resp, err := http.Get(url)
	if err != nil {
		fail("failed to fetch UnicodeData.txt: %s", err)
	}
	in := bufio.NewReader(resp.Body)
	defer resp.Body.Close()

	line := -1
	var prevCat string
	cats := make(map[string][]Range)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		line++
		fields := strings.Split(scanner.Text(), ";")
		if len(fields) != 15 {
			fail("line %d: bad input: %d fields, expected 14", line, len(fields))
		}
		if len(fields[2]) == 0 {
			fail("line %d: bad category [%s]", line, fields[2])
		}

		var cat string
		for _, catName := range catNames {
			if strings.HasPrefix(fields[2], catName) {
				cat = catName
				break
			}
		}
		if cat == "" {
			continue
		}

		name := fields[1]
		c, err := strconv.ParseUint(fields[0], 16, 32)
		if err != nil {
			fail("line %d: bad rune: %s", line, err)
		}
		if c > unicode.MaxRune {
			fail("line %d: bad rune: too big", line)
		}
		r := rune(c)

		// The "Last>" condition handles ranges
		// which start with names <.* First>
		// and end with names <.* Last>.
		cs := cats[cat]
		if prevCat == cat && len(cs) > 0 &&
			(cs[len(cs)-1].max == r-1 || strings.HasSuffix(name, "Last>")) {
			cs[len(cs)-1].max = r
		} else {
			prevCat = cat
			cats[cat] = append(cats[cat], Range{min: r, max: r})
		}
	}
	if err := scanner.Err(); err != nil {
		fail("Failed to read the input: %s", err)
	}

	fmt.Printf("// This file was generated with go run make_tables.go.\n")
	fmt.Printf("// Tables from %s.\n", url)
	for _, catName := range catNames {
		fmt.Printf("var category_%s := category :: [\n", catName)
		for _, r := range cats[catName] {
			fmt.Printf("	[.min %d, .max %d],\n", r.min, r.max)
		}
		fmt.Println("]")
	}
}

func fail(f string, xs ...interface{}) {
	fmt.Printf(f, xs...)
	os.Exit(1)
}
