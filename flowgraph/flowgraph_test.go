package flowgraph_test

import (
	"bufio"
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"unicode"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/flowgraph/interp"
	"github.com/eaburns/pea/parser"
)

func TestBuild(t *testing.T) {
	const subDir = "testdata"
	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err.Error())
	}
	dir := filepath.Join(cwd, subDir)
	fileInfos, err := ioutil.ReadDir(dir)
	if err != nil {
		t.Fatal(err.Error())
	}
	sort.Slice(fileInfos, func(i, j int) bool {
		return fileInfos[i].Name() < fileInfos[j].Name()
	})
	for _, fileInfo := range fileInfos {
		fileInfo := fileInfo
		t.Run(fileInfo.Name(), func(t *testing.T) {
			path := filepath.Join(dir, fileInfo.Name())
			c, err := loadMod(path)
			if err != nil {
				t.Fatalf(err.Error())
			}
			got := runTest(flowgraph.Build(c))
			want, err := expectedOutput(path)
			if err != nil {
				t.Fatalf(err.Error())
			}
			if got != want {
				t.Errorf("%s\ngot:\n%q\nwant:\n%q", path, got, want)
			}
		})
	}
}

func runTest(f *flowgraph.Mod) string {
	var stdout strings.Builder
	r := interp.New()
	r.Out = &stdout
	if f.Init != nil {
		r.Eval(f.Init)
	}
	var main *flowgraph.FuncDef
	for _, fun := range f.Funcs {
		if fun.Name == "main" {
			main = fun
			break
		}
	}
	if main == nil {
		panic("no main func")
	}
	r.Eval(main)
	return stdout.String()
}

func loadMod(path string) (*checker.Mod, error) {
	p := parser.New()
	if err := p.ParseFile(path); err != nil {
		return nil, err
	}
	c, _, errs := checker.Check("main", p.Files, nil)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	return c, nil
}

func expectedOutput(path string) (string, error) {
	// TODO: implement comment tracking on parse nodes
	// and use that to implement test expected output.
	bs, err := ioutil.ReadFile(path)
	if err != nil {
		return "", err
	}
	var comment strings.Builder
	scanner := bufio.NewScanner(bytes.NewReader(bs))
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "func main") {
			break
		}
		if !strings.HasPrefix(line, "//") {
			comment.Reset()
			continue
		}
		line = strings.TrimPrefix(line, "//")
		line = strings.TrimLeftFunc(line, unicode.IsSpace)
		if comment.Len() > 0 {
			comment.WriteRune('\n')
		}
		comment.WriteString(line)
	}
	return comment.String(), scanner.Err()
}
