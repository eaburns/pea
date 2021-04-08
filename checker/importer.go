package checker

import (
	"io/ioutil"
	"path/filepath"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type Importer interface {
	Files() loc.Files
	Load(path string) (*Mod, error)
	// Deps returns the transitive dependencies
	// in topological order with dependencies
	// appearing before their dependents.
	Deps() []string
}

type defaultImporter struct {
	modRoot string
	files   loc.Files
	loaded  map[string]*Mod
	deps    []string
}

func NewImporter(modRoot string, files []*parser.File) Importer {
	imp := &defaultImporter{
		modRoot: modRoot,
		loaded:  make(map[string]*Mod),
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
	if mod, ok := imp.loaded[path]; ok {
		return mod, nil
	}

	fullPath := filepath.Join(imp.modRoot, path)
	fileInfos, err := ioutil.ReadDir(fullPath)
	if err != nil {
		return nil, err
	}
	p := parser.NewWithOffset(imp.files.Len() + 1)
	for _, fileInfo := range fileInfos {
		if filepath.Ext(fileInfo.Name()) != ".pea" {
			continue
		}
		filePath := filepath.Join(fullPath, fileInfo.Name())
		if err := p.ParseFile(filePath); err != nil {
			return nil, err
		}
	}
	mod, files, errs := Check(path, p.Files, imp)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	mod.Imported = true
	imp.loaded[path] = mod
	imp.deps = append(imp.deps, path)
	for _, f := range files {
		imp.files = append(imp.files, f)
	}
	return mod, nil
}

func (imp *defaultImporter) Deps() []string { return imp.deps }
