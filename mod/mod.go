package mod

import (
	"errors"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"sort"
	"strings"

	"github.com/eaburns/pea/parser"
)

// A Mod is information about a single module.
type Mod struct {
	Root     *Root
	Path     string
	FullPath string
	SrcFiles []string
	Deps     []*Mod
}

// A Root represents the root of a module directory hierarchy.
type Root struct {
	rootDir string
	mods    map[string]*Mod
}

// NewRoot returns a new Root for a module directory hierarchy.
func NewRoot(rootDir string) *Root {
	return &Root{
		rootDir: rootDir,
		mods:    make(map[string]*Mod),
	}
}

// GetMainBySource returns a new module with the path "main".
// Rather than being specified by its module root directory,
// the module is specified by listing its source files instead.
// It is an error if srcFiles is empty.
// The Path of the module is "main",
// the FullPath for the module is the directory of srcFiles[0],
// and the SrcFiles is srcFiles.
//
// The *Mod is unique; calls to GetMainBySource always return a new *Mod.
// This differs from Get, which will return the same *Mod
// when called with the same Root and modPath.
func (r *Root) GetMainBySource(srcFiles []string) (*Mod, error) {
	if len(srcFiles) == 0 {
		return nil, errors.New("no source files")
	}
	// Make a copy of the original slice.
	srcFiles = append([]string{}, srcFiles...)
	sort.Strings(srcFiles)
	imports, err := importPaths(srcFiles)
	if err != nil {
		return nil, err
	}
	sort.Strings(imports)
	mod := &Mod{
		Root:     r,
		Path:     "main",
		FullPath: filepath.Dir(srcFiles[0]),
		SrcFiles: srcFiles,
	}
	path := []string{"main"}
	onPath := map[string]bool{"main": true}
	for _, imp := range imports {
		m, err := r.get(path, onPath, imp)
		if err != nil {
			return nil, err
		}
		mod.Deps = append(mod.Deps, m)
	}
	return mod, nil
}

// Get returns the module at a given module path.
// All calls to Get with the same *Root and modPath return the same *Mod.
func (r *Root) Get(modPath string) (*Mod, error) {
	return r.get([]string{}, make(map[string]bool), modPath)
}

func (r *Root) get(path []string, onPath map[string]bool, modPath string) (*Mod, error) {
	path = append(path, modPath)
	defer func() { path = path[:len(path)-1] }()
	if onPath[modPath] {
		return nil, fmt.Errorf("dependency cycle: %v", path)
	}
	onPath[modPath] = true
	defer func() { delete(onPath, modPath) }()

	if mod, ok := r.mods[modPath]; ok {
		return mod, nil
	}

	fullPath, err := r.fullPath(modPath)
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
		Root:     r,
		Path:     modPath,
		FullPath: fullPath,
		SrcFiles: srcFiles,
	}
	r.mods[modPath] = mod
	for _, imp := range imports {
		m, err := r.get(path, onPath, imp)
		if err != nil {
			return nil, err
		}
		mod.Deps = append(mod.Deps, m)
	}
	return mod, nil
}

// fullPath returns the full path rooted at a root path entry for a module path.
// It is an error if there are multiple possible full paths.
func (r *Root) fullPath(modPath string) (string, error) {
	return filepath.Join(r.rootDir, modPath), nil
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
