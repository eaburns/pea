package mod

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestGetMainBySourceNoDeps(t *testing.T) {
	fsPath := makeFS([]file{
		{"x/y/z/foo.pea", ""},
		{"x/y/z/bar.pea", ""},
		{"x/y/z/baz.pea", ""},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(fsPath)
	m, err := r.GetMainBySource([]string{
		filepath.Join(fsPath, "x/y/z", "foo.pea"),
		filepath.Join(fsPath, "x/y/z", "bar.pea"),
		filepath.Join(fsPath, "x/y/z", "baz.pea"),
	})
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	if diff := cmp.Diff(m, &Mod{
		Root:     r,
		Path:     "main",
		FullPath: filepath.Join(fsPath, "x/y/z"),
		SrcFiles: []string{
			filepath.Join(fsPath, "x/y/z", "bar.pea"),
			filepath.Join(fsPath, "x/y/z", "baz.pea"),
			filepath.Join(fsPath, "x/y/z", "foo.pea"),
		},
		Deps: nil,
	}, diffOpts...); diff != "" {
		t.Errorf("%s", diff)
	}
}

func TestGetMainBySourceDeps(t *testing.T) {
	fsPath := makeFS([]file{
		{"x/y/z/foo.pea", `import "dep0"`},
		{"x/y/z/bar.pea", ``},
		{"x/y/z/baz.pea", `import "dep1"`},
		{"root/dep0/source.pea", ``},
		{"root/dep1/source.pea", ``},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(filepath.Join(fsPath, "root"))
	m, err := r.GetMainBySource([]string{
		filepath.Join(fsPath, "x/y/z", "foo.pea"),
		filepath.Join(fsPath, "x/y/z", "bar.pea"),
		filepath.Join(fsPath, "x/y/z", "baz.pea"),
	})
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	if diff := cmp.Diff(m, &Mod{
		Path:     "main",
		FullPath: filepath.Join(fsPath, "x/y/z"),
		SrcFiles: []string{
			filepath.Join(fsPath, "x/y/z", "bar.pea"),
			filepath.Join(fsPath, "x/y/z", "baz.pea"),
			filepath.Join(fsPath, "x/y/z", "foo.pea"),
		},
		Deps: []*Mod{
			{
				Path:     "dep0",
				FullPath: filepath.Join(fsPath, "root/dep0"),
				SrcFiles: []string{
					filepath.Join(fsPath, "root/dep0", "source.pea"),
				},
			},
			{
				Path:     "dep1",
				FullPath: filepath.Join(fsPath, "root/dep1"),
				SrcFiles: []string{
					filepath.Join(fsPath, "root/dep1", "source.pea"),
				},
			},
		},
	}, diffOpts...); diff != "" {
		for _, dep := range m.Deps {
			t.Logf("%#v\n", dep)
		}
		t.Errorf("%s", diff)
	}
}

func TestGetNoDeps(t *testing.T) {
	fsPath := makeFS([]file{
		{"main/foo.pea", ""},
		{"main/bar.pea", ""},
		{"main/baz.pea", ""},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(fsPath)
	m, err := r.Get("main")
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	if diff := cmp.Diff(m, &Mod{
		Root:     r,
		Path:     "main",
		FullPath: filepath.Join(fsPath, "main"),
		SrcFiles: []string{
			filepath.Join(fsPath, "main", "bar.pea"),
			filepath.Join(fsPath, "main", "baz.pea"),
			filepath.Join(fsPath, "main", "foo.pea"),
		},
		Deps: nil,
	}, diffOpts...); diff != "" {
		t.Errorf("%s", diff)
	}
}

func TestGetDeps(t *testing.T) {
	fsPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", ``},
		{"dep1/source.pea", ``},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(fsPath)
	m, err := r.Get("main")
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	if diff := cmp.Diff(m, &Mod{
		Path:     "main",
		FullPath: filepath.Join(fsPath, "main"),
		SrcFiles: []string{
			filepath.Join(fsPath, "main", "bar.pea"),
			filepath.Join(fsPath, "main", "baz.pea"),
			filepath.Join(fsPath, "main", "foo.pea"),
		},
		Deps: []*Mod{
			{
				Path:     "dep0",
				FullPath: filepath.Join(fsPath, "dep0"),
				SrcFiles: []string{
					filepath.Join(fsPath, "dep0", "source.pea"),
				},
			},
			{
				Path:     "dep1",
				FullPath: filepath.Join(fsPath, "dep1"),
				SrcFiles: []string{
					filepath.Join(fsPath, "dep1", "source.pea"),
				},
			},
		},
	}, diffOpts...); diff != "" {
		t.Errorf("%s", diff)
	}
}

func TestGetDepsDeduped(t *testing.T) {
	fsPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", `import "dep1"`},
		{"dep1/source.pea", ``},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(fsPath)
	m, err := r.Get("main")
	if err != nil {
		t.Fatalf("failed to load main: %s", err)
	}
	dep1 := &Mod{
		Root:     r,
		Path:     "dep1",
		FullPath: filepath.Join(fsPath, "dep1"),
		SrcFiles: []string{
			filepath.Join(fsPath, "dep1", "source.pea"),
		},
	}
	if diff := cmp.Diff(m, &Mod{
		Path:     "main",
		FullPath: filepath.Join(fsPath, "main"),
		SrcFiles: []string{
			filepath.Join(fsPath, "main", "bar.pea"),
			filepath.Join(fsPath, "main", "baz.pea"),
			filepath.Join(fsPath, "main", "foo.pea"),
		},
		Deps: []*Mod{
			{
				Path:     "dep0",
				FullPath: filepath.Join(fsPath, "dep0"),
				SrcFiles: []string{
					filepath.Join(fsPath, "dep0", "source.pea"),
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
	fsPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", `import "dep1"`},
		{"dep1/source.pea", `import "dep0"`},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(fsPath)
	if _, err := r.Get("NOT_FOUND"); err == nil {
		t.Fatalf("expected not found error")
	}
}

func TestGetDepCycle(t *testing.T) {
	fsPath := makeFS([]file{
		{"main/foo.pea", `import "dep0"`},
		{"main/bar.pea", `import "dep1"`},
		{"main/baz.pea", `import "dep0"`},
		{"dep0/source.pea", `import "dep1"`},
		{"dep1/source.pea", `import "dep0"`},
	})
	defer os.RemoveAll(fsPath)
	r := NewRoot(fsPath)
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
