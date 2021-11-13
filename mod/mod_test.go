package mod

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestGetNoDeps(t *testing.T) {
	rootPath := makeFS([]file{
		{"main/foo.pea", ""},
		{"main/bar.pea", ""},
		{"main/baz.pea", ""},
	})
	defer os.RemoveAll(rootPath)
	r := NewRoot(rootPath)
	m, err := r.Get("main")
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	if diff := cmp.Diff(m, &Mod{
		Root:     r,
		Path:     "main",
		FullPath: filepath.Join(rootPath, "main"),
		SrcFiles: []string{
			filepath.Join(rootPath, "main", "bar.pea"),
			filepath.Join(rootPath, "main", "baz.pea"),
			filepath.Join(rootPath, "main", "foo.pea"),
		},
		Deps: nil,
	}, diffOpts...); diff != "" {
		t.Errorf("%s", diff)
	}
}

func TestGetDeps(t *testing.T) {
	rootPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", ``},
		{"dep1/source.pea", ``},
	})
	defer os.RemoveAll(rootPath)
	r := NewRoot(rootPath)
	m, err := r.Get("main")
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	if diff := cmp.Diff(m, &Mod{
		Root:     r,
		Path:     "main",
		FullPath: filepath.Join(rootPath, "main"),
		SrcFiles: []string{
			filepath.Join(rootPath, "main", "bar.pea"),
			filepath.Join(rootPath, "main", "baz.pea"),
			filepath.Join(rootPath, "main", "foo.pea"),
		},
		Deps: []*Mod{
			{
				Root:     r,
				Path:     "dep0",
				FullPath: filepath.Join(rootPath, "dep0"),
				SrcFiles: []string{
					filepath.Join(rootPath, "dep0", "source.pea"),
				},
			},
			{
				Root:     r,
				Path:     "dep1",
				FullPath: filepath.Join(rootPath, "dep1"),
				SrcFiles: []string{
					filepath.Join(rootPath, "dep1", "source.pea"),
				},
			},
		},
	}, diffOpts...); diff != "" {
		t.Errorf("%s", diff)
	}
}

func TestGetDepsDeduped(t *testing.T) {
	rootPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", `import "dep1"`},
		{"dep1/source.pea", ``},
	})
	defer os.RemoveAll(rootPath)
	r := NewRoot(rootPath)
	m, err := r.Get("main")
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	dep1 := &Mod{
		Root:     r,
		Path:     "dep1",
		FullPath: filepath.Join(rootPath, "dep1"),
		SrcFiles: []string{
			filepath.Join(rootPath, "dep1", "source.pea"),
		},
	}
	if diff := cmp.Diff(m, &Mod{
		Root:     r,
		Path:     "main",
		FullPath: filepath.Join(rootPath, "main"),
		SrcFiles: []string{
			filepath.Join(rootPath, "main", "bar.pea"),
			filepath.Join(rootPath, "main", "baz.pea"),
			filepath.Join(rootPath, "main", "foo.pea"),
		},
		Deps: []*Mod{
			{
				Root:     r,
				Path:     "dep0",
				FullPath: filepath.Join(rootPath, "dep0"),
				SrcFiles: []string{
					filepath.Join(rootPath, "dep0", "source.pea"),
				},
				Deps: []*Mod{dep1},
			},
			dep1,
		},
	}, diffOpts...); diff != "" {
		t.Errorf("%s", diff)
	}
}

func TestGetNotFound(t *testing.T) {
	rootPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", `import "dep1"`},
		{"dep1/source.pea", `import "dep0"`},
	})
	defer os.RemoveAll(rootPath)
	r := NewRoot(rootPath)
	if _, err := r.Get("NOT_FOUND"); err == nil {
		t.Fatalf("expected not found error")
	}
}

func TestGetDepCycle(t *testing.T) {
	rootPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", `import "dep1"`},
		{"dep1/source.pea", `import "dep0"`},
	})
	defer os.RemoveAll(rootPath)
	r := NewRoot(rootPath)
	if _, err := r.Get("main"); err == nil {
		t.Fatalf("expected dependency cycle error")
	}
}

var diffOpts = []cmp.Option{
	// Ignore the Mod.Root.
	cmp.FilterPath(func(path cmp.Path) bool {
		for _, s := range path {
			if s.String() == ".Root" {
				return true
			}
		}
		return false
	}, cmp.Ignore()),
}

type file struct {
	path string
	data string
}

func makeFS(files []file) string {
	root, err := os.MkdirTemp("", "pea_mod_test.*")
	if err != nil {
		panic("os.MkdirTemp failed: " + err.Error())
	}
	for _, file := range files {
		path := filepath.Join(root, file.path)
		dir := filepath.Dir(path)
		if err := os.MkdirAll(dir, 0755); err != nil {
			os.RemoveAll(root)
			panic("os.MkdirAll failed: " + err.Error())
		}
		if err := os.WriteFile(path, []byte(file.data), 0666); err != nil {
			os.RemoveAll(root)
			panic("os.WriteFile failed: " + err.Error())
		}
	}
	return root
}
