package checker

import (
	"path/filepath"
	"strings"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/mod"
	"github.com/eaburns/pea/parser"
)

type Importer interface {
	Files() loc.Files
	Load(path string) (*Mod, error)
	// Deps returns the transitive dependencies
	// in topological order with dependencies
	// appearing before their dependents.
	Deps() []*Mod
}

type defaultImporter struct {
	root                *mod.Root
	files               loc.Files
	loaded              map[string]*Mod
	deps []*Mod
	trimErrorPathPrefix string
}

func NewImporter(r *mod.Root, files []*parser.File, trimErrorPathPrefix string) Importer {
	imp := &defaultImporter{
		root:                r,
		loaded:              make(map[string]*Mod),
		trimErrorPathPrefix: trimErrorPathPrefix,
	}
	for _, f := range files {
		imp.files = append(imp.files, f)
	}
	return imp
}

func (imp *defaultImporter) Files() loc.Files {
	return imp.files
}

func (imp *defaultImporter) Load(path string) (*Mod, error) {
	path = cleanImportPath(path)
	if mod, ok := imp.loaded[path]; ok {
		return mod, nil
	}
	m, err := imp.root.Get(path)
	if err != nil {
		return nil, err
	}
	p := parser.NewWithOffset(imp.files.Len() + 1)
	p.TrimErrorPathPrefix = imp.trimErrorPathPrefix
	for _, srcFile := range m.SrcFiles {
		if filepath.Ext(srcFile) != ".pea" {
			continue
		}
		if err := p.ParseFile(srcFile); err != nil {
			return nil, err
		}
	}
	for _, f := range p.Files {
		imp.files = append(imp.files, f)
	}
	opts := []Option{
		UseImporter(imp),
		TrimErrorPathPrefix(imp.trimErrorPathPrefix),
	}
	mod, _, errs := Check(path, p.Files, opts...)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	mod.Imported = true
	imp.loaded[path] = mod
	imp.deps = append(imp.deps, mod)
	return mod, nil
}

func (imp *defaultImporter) Deps() []*Mod { return imp.deps }

func cleanImportPath(path string) string {
	return strings.TrimPrefix(filepath.Clean(path), "/")
}
