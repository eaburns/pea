package checker

import (
	"errors"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type Importer interface {
	Files() loc.Files
	Load(path string) ([]Def, error)
}

type defaultImporter struct {
	mods  map[string]*Mod
	files loc.Files
}

func newDefaultImporter(files []*parser.File) Importer {
	imp := &defaultImporter{
		mods: make(map[string]*Mod),
	}
	for _, f := range files {
		imp.files = append(imp.files, f)
	}
	return imp
}

func (imp *defaultImporter) Files() loc.Files {
	return imp.files
}

func (imp *defaultImporter) Load(path string) ([]Def, error) {
	return nil, errors.New("importing is not implemented")
}
