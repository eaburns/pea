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
	modRoot        string
	files          loc.Files
	loaded         map[string]*Mod
	deps           []string
	trimPathPrefix string
}

// NewImporterTemplateParser returns a new importer that importes modules from the given root.
// It is created starting with the loc.Files as templateParser,
// and any internal parses it creates copy the TrimPathPrefix
// from templateParser too.
func NewImporterTemplateParser(modRoot string, templateParser *parser.Parser) Importer {
	imp := NewImporter(modRoot, templateParser.Files).(*defaultImporter)
	imp.trimPathPrefix = templateParser.TrimPathPrefix
	return imp
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
	p.TrimPathPrefix = imp.trimPathPrefix
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
	mod, _, errs := Check(path, p.Files, UseImporter(imp))
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
