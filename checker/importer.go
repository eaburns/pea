package checker

import (
	"io/ioutil"
	"path/filepath"
	"strings"

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
	modRoot             string
	files               loc.Files
	loaded              map[string]*Mod
	deps                []string
	trimErrorPathPrefix string
}

func NewImporter(modRoot string, files []*parser.File, trimErrorPathPrefix string) Importer {
	imp := &defaultImporter{
		modRoot:             modRoot,
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

	fullPath := filepath.Join(imp.modRoot, path)
	fileInfos, err := ioutil.ReadDir(fullPath)
	if err != nil {
		return nil, err
	}
	p := parser.NewWithOffset(imp.files.Len() + 1)
	p.TrimErrorPathPrefix = imp.trimErrorPathPrefix
	for _, fileInfo := range fileInfos {
		if filepath.Ext(fileInfo.Name()) != ".pea" {
			continue
		}
		filePath := filepath.Join(fullPath, fileInfo.Name())
		if err := p.ParseFile(filePath); err != nil {
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
	imp.deps = append(imp.deps, path)
	return mod, nil
}

func (imp *defaultImporter) Deps() []string { return imp.deps }

func cleanImportPath(path string) string {
	return strings.TrimPrefix(filepath.Clean(path), "/")
}
