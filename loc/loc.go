// Package loc has routines for tracking file locations.
package loc

import "fmt"

// Loc compactly identifies a string in a set of files.
// The zero value indicates no location.
type Loc [2]int

// A Location identifies a string in a file.
// The zero value indicates no location.
type Location struct {
	Path string
	Line [2]int
	Col  [2]int
}

func (l Location) String() string {
	if (l == Location{}) {
		return ""
	}
	if l.Line[0] == l.Line[1] && l.Col[0] == l.Col[1] {
		return fmt.Sprintf("%s:%d.%d", l.Path, l.Line[0], l.Col[0])
	}
	return fmt.Sprintf("%s:%d.%d-%d.%d", l.Path, l.Line[0], l.Col[0], l.Line[1], l.Col[1])
}

// File is an interface describing a file
// by its path, size, and newline byte offsets.
type File interface {
	Path() string
	Len() int
	NewLines() []int
}

// Files tracks locations within a set of files.
type Files []File

// Len returns the total length of all files.
func (fs Files) Len() int {
	var n int
	for _, f := range fs {
		n += f.Len()
	}
	return n
}

// Loc returns the Loc for a node in the module AST.
func (fs Files) Location(l Loc) Location {
	switch {
	case len(fs) == 0:
		panic("no files")
	case l[0]-1 < 0 || l[1]-1 > fs.Len():
		panic("out of range")
	case l[0] > l[1]:
		panic("bad Loc")
	}
	p0, l0, c0 := fs.loc(l[0])
	p1, l1, c1 := fs.loc(l[1])
	if p0 != p1 {
		panic("multi-file Loc")
	}
	return Location{Path: p0, Line: [2]int{l0, l1}, Col: [2]int{c0, c1}}
}

func (fs Files) loc(q int) (string, int, int) {
	q-- // 0 value is no-location; locs start at 1
	var i int
	var f File
	var offs int
	for i, f = range fs {
		l := f.Len()
		if q < offs+l || q == offs+l && i == len(fs)-1 {
			break
		}
		offs += f.Len()
	}
	line, colStart := 1, offs-1
	for _, nl := range f.NewLines() {
		if offs+nl >= q {
			break
		}
		colStart = offs + nl
		line++
	}
	return f.Path(), line, q - colStart
}
