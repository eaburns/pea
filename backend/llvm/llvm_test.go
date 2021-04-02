package llvm_test

import (
	"bufio"
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"unicode"

	"github.com/eaburns/pea/backend/llvm"
	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/parser"
)

func TestLLVM(t *testing.T) {
	const subDir = "../../flowgraph/testdata"
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
			t.Parallel()
			path := filepath.Join(dir, fileInfo.Name())
			f, err := loadMod(path)
			if err != nil {
				t.Fatalf(err.Error())
			}
			want, err := expectedOutput(path)
			if err != nil {
				t.Fatalf(err.Error())
			}
			got := runTest(t, f)
			if got != want {
				t.Errorf("%s\ngot:\n%q\nwant:\n%q", path, got, want)
			}
		})
	}
}

func runTest(t *testing.T, f *flowgraph.Mod) string {
	dir, err := ioutil.TempDir("/tmp", "pea_llvm_test-*")
	if err != nil {
		t.Fatalf("TempDir failed: %s", err)
	}
	defer func() { os.RemoveAll(dir) }()

	ll, err := os.Create(filepath.Join(dir, "t.ll"))
	if err != nil {
		t.Fatalf("failed to open t.ll: %s", err)
	}
	buf := bufio.NewWriter(ll)
	llvm.Generate(buf, f)
	buf.Flush()
	if err := ll.Close(); err != nil {
		t.Fatalf("error closing t.ll: %s", err)
	}

	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("error getting wd: %s", err)
	}
	runtimeLibs :=
		"-pthread " +
			"-L/usr/local/lib -lunwind " +
			filepath.Join(wd, "/../../libpea/libpea.o") + " " +
			filepath.Join(wd, "/../../libpea/vendor/gc-8.0.4/gc.a")
	cmd := exec.Command(
		"/bin/sh", "-c",
		"cd "+dir+"; "+
			"llvm-as t.ll -o t.bc && "+
			"opt -O2 t.bc -o t.opt.bc &&"+
			"llc t.opt.bc -o t.opt.s && "+
			"clang -g t.opt.s "+runtimeLibs+" && "+
			"./a.out")
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Log(stdout.String())
		t.Log(stderr.String())
		bs, err1 := ioutil.ReadFile(filepath.Join(dir, "t.opt.s"))
		if err1 != nil {
			panic(fmt.Sprintf("failed to log t.ll: %s", err1))
		}
		t.Log(string(bs) + "\n")
		t.Fatalf("error running the command: %s", err)
	}
	return stdout.String()
}

func loadMod(path string) (*flowgraph.Mod, error) {
	p := parser.New()
	if err := p.ParseFile(path); err != nil {
		return nil, err
	}
	c, _, errs := checker.Check("main", p.Files, nil)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	f := flowgraph.Build(c)
	return f, nil
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
