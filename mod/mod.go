package mod

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"sort"
	"strings"

	"github.com/eaburns/pea/parser"
)

// A Mod is information about a single module.
type Mod struct {
	Path     string
	FullPath string
	SrcFiles []string
	Deps     []*Mod
}

// A Loader loads information about modules and their dependencies from files.
type Loader struct {
	rootDir string
	mods    map[string]*Mod
}

// New returns a new Loader that loads modules from a root directory.
func NewLoader(rootDir string) *Loader {
	return &Loader{
		rootDir: rootDir,
		mods:    make(map[string]*Mod),
	}
}

// Load returns the module at a given module path.
func (ld *Loader) Load(modPath string) (*Mod, error) {
	return ld.load([]string{}, make(map[string]bool), modPath)
}

func (ld *Loader) load(path []string, onPath map[string]bool, modPath string) (*Mod, error) {
	path = append(path, modPath)
	defer func() { path = path[:len(path)-1] }()
	if onPath[modPath] {
		return nil, fmt.Errorf("dependency cycle: %v", path)
	}
	onPath[modPath] = true
	defer func() { delete(onPath, modPath) }()

	if mod, ok := ld.mods[modPath]; ok {
		return mod, nil
	}

	fullPath, err := ld.fullPath(modPath)
	if err != nil {
		return nil, err
	}
	srcFiles, err := sourceFiles(fullPath)
	if err != nil {
		return nil, err
	}
	sort.Strings(srcFiles)
	imports, err := importPaths(srcFiles)
	if err != nil {
		return nil, err
	}
	sort.Strings(imports)

	mod := &Mod{
		Path:     modPath,
		FullPath: fullPath,
		SrcFiles: srcFiles,
	}
	ld.mods[modPath] = mod
	for _, imp := range imports {
		m, err := ld.load(path, onPath, imp)
		if err != nil {
			return nil, err
		}
		mod.Deps = append(mod.Deps, m)
	}
	return mod, nil
}

// fullPath returns the full path rooted at a root path entry for a module path.
// It is an error if there are multiple possible full paths.
func (ld *Loader) fullPath(modPath string) (string, error) {
	return filepath.Join(ld.rootDir, modPath), nil
}

func sourceFiles(fullPath string) ([]string, error) {
	dirInfos, err := ioutil.ReadDir(fullPath)
	if err != nil {
		return nil, err
	}
	var srcFiles []string
	for _, dirInfo := range dirInfos {
		// Ignore .pea.ll files, which are likely build artifacts.
		if strings.HasPrefix(dirInfo.Name(), ".pea.ll") {
			continue
		}
		filePath := filepath.Join(fullPath, dirInfo.Name())
		if ext := filepath.Ext(filePath); ext == ".pea" || ext == ".c" || ext == ".ll" {
			srcFiles = append(srcFiles, filePath)
		}
	}
	return srcFiles, nil
}

func importPaths(srcFiles []string) ([]string, error) {
	var imports []string
	seenImports := make(map[string]bool)
	for _, srcFile := range srcFiles {
		if filepath.Ext(srcFile) != ".pea" {
			continue
		}
		imps, err := parser.ImportsOnly(srcFile)
		if err != nil {
			return nil, err
		}
		for _, imp := range imps {
			if seenImports[imp] {
				continue
			}
			seenImports[imp] = true
			imports = append(imports, imp)
		}
	}
	return imports, nil
}
