package checker

import (
	"strconv"
	"strings"
	"testing"
	"unicode"
	"unicode/utf8"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

func TestCommonPattern(t *testing.T) {
	tests := []struct {
		src  string
		pats []string
		want string
	}{
		{
			pats: []string{},
			want: "_",
		},
		{
			pats: []string{"int"},
			want: "int",
		},
		{
			src:  "type point [.x int, .y int]",
			pats: []string{"point"},
			want: "point",
		},
		{
			pats: []string{"T"},
			want: "T",
		},
		{
			pats: []string{"&float32"},
			want: "&float32",
		},
		{
			pats: []string{"[string]"},
			want: "[string]",
		},
		{
			pats: []string{"[.x int]"},
			want: "[.x int]",
		},
		{
			pats: []string{"[x? int]"},
			want: "[x? int]",
		},
		{
			pats: []string{"(int){}"},
			want: "(int){}",
		},
		{
			pats: []string{
				"int",
				"[x? int, y? string]",
			},
			want: "_",
		},
		{
			src: "type point [.x int, .y int]",
			pats: []string{
				"point",
				"point",
			},
			want: "point",
		},
		{
			src: `
				type point [.x int, .y int]
				type rect [.w int, .h int]
			`,
			pats: []string{
				"point",
				"rect",
			},
			want: "_",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(int, int) pair",
			},
			want: "(int, int) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(string, int) pair",
			},
			want: "(_, int) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(int, string) pair",
			},
			want: "(int, _) pair",
		},
		{
			src: "type (X, Y) pair [.x X, .y Y]",
			pats: []string{
				"(int, int) pair",
				"(string, string) pair",
			},
			want: "(_, _) pair",
		},
		{
			pats: []string{
				"int",
				"int",
			},
			want: "int",
		},
		{
			pats: []string{
				"int",
				"bool",
			},
			want: "_",
		},
		{
			pats: []string{
				"&int",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"&int",
				"&int",
			},
			want: "&int",
		},
		{
			pats: []string{
				"&int",
				"&string",
			},
			want: "&_",
		},
		{
			pats: []string{
				"[int]",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"[int]",
				"[int]",
			},
			want: "[int]",
		},
		{
			pats: []string{
				"[int]",
				"[string]",
			},
			want: "[_]",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.a int, .b string]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.y string, .x int]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.x int]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[.]",
				"[.]",
			},
			want: "[.]",
		},
		{
			pats: []string{
				"[.x int, .y string]",
				"[.x int, .y string]",
			},
			want: "[.x int, .y string]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int32, .y int]",
			},
			want: "[.x _, .y int]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int, .y int32]",
			},
			want: "[.x int, .y _]",
		},
		{
			pats: []string{
				"[.x int, .y int]",
				"[.x int32, .y int32]",
			},
			want: "[.x _, .y _]",
		},
		{
			pats: []string{
				"[.x int, .y float32, .z string]",
				"[.x int, .y bool, .z string]",
				"[.x bool, .y float32, .z string]",
			},
			want: "[.x _, .y _, .z string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?, z? string]",
			},
			want: "[x? int, y?, z? string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[a? int, b?, c? string]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?]",
			},
			want: "_",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? bool, y?, z? string]",
			},
			want: "[x? _, y?, z? string]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? int, y?, z? bool]",
			},
			want: "[x? int, y?, z? _]",
		},
		{
			pats: []string{
				"[x? int, y?, z? string]",
				"[x? bool, y?, z? bool]",
			},
			want: "[x? _, y?, z? _]",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int){float64}",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string, float32){float64}",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){}",
			},
			want: "(int, string){_}",
		},
		{
			pats: []string{
				"(){}",
				"(){}",
			},
			want: "(){}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){float64}",
			},
			want: "(int, string){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(bool, string){float64}",
			},
			want: "(_, string){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, bool){float64}",
			},
			want: "(int, _){float64}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int, string){bool}",
			},
			want: "(int, string){_}",
		},
		{
			pats: []string{
				"(int, string){float64}",
				"(int8, float32){bool}",
			},
			want: "(_, _){_}",
		},
		{
			pats: []string{
				"T",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"T",
				"T",
			},
			want: "T",
		},
		{
			pats: []string{
				"_",
				"int",
			},
			want: "_",
		},
		{
			pats: []string{
				"_",
				"_",
			},
			want: "_",
		},
		{
			pats: []string{
				"_1",
				"_2",
			},
			want: "_",
		},
		{
			pats: []string{
				"(int, string){_}",
			},
			want: "(int, string){_}",
		},
	}
	for _, test := range tests {
		t.Run(strings.Join(test.pats, ","), func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			var err error
			pats := make([]*typePattern, len(test.pats))
			for i, p := range test.pats {
				if pats[i], err = parseTestPattern(t, mod, p); err != nil {
					t.Fatalf("failed to parse type pattern %s: %s", p, err)
				}
			}
			if u := common(pats...); u.String() != test.want {
				t.Errorf("union(%s)=%s, want %s", pats, u.String(), test.want)
			}
		})
	}
}

func TestTypePatternString(t *testing.T) {
	tests := []struct {
		src  string
		pat  string
		want string
	}{
		{pat: "int", want: "int"},
		{pat: "&int", want: "&int"},
		{pat: "[int]", want: "[int]"},
		{pat: "[.x int, .y int]", want: "[.x int, .y int]"},
		{pat: "[x?, y?, z?]", want: "[x?, y?, z?]"},
		{pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{pat: "(){}", want: "(){}"},
		{pat: "(int){}", want: "(int){}"},
		{pat: "(){int}", want: "(){int}"},
		{pat: "_", want: "_"},
		{pat: "_0", want: "_"},
		{pat: "&_", want: "&_"},
		{pat: "[_]", want: "[_]"},
		{pat: "[.x _, .y int]", want: "[.x _, .y int]"},
		{pat: "[.x int, .y _]", want: "[.x int, .y _]"},
		{pat: "[x? _, y?, z? string]", want: "[x? _, y?, z? string]"},
		{pat: "[x? int, y?, z? _]", want: "[x? int, y?, z? _]"},
		{pat: "(_){}", want: "(_){}"},
		{pat: "(){_}", want: "(){_}"},
		{pat: "(int, _){}", want: "(int, _){}"},
		{pat: "[.x _, .y _0, .z _0]", want: "[.x _, .y _0, .z _0]"},
		{pat: "[.x _, .y _1, .z _1]", want: "[.x _, .y _0, .z _0]"},
		{pat: "[.x _, .y U, .z _]", want: "[.x _, .y U, .z _]"},
		{pat: "[.high [.med [.low _]]]", want: "[.high [.med [.low _]]]"},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(string, int) pair",
			want: "(string, int) pair",
		},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(_, _) pair",
			want: "(_, _) pair",
		},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(_0, _0) pair",
			want: "(_0, _0) pair",
		},
		{
			src:  "type (X, Y) pair [.x X, .y Y]",
			pat:  "(_, int) pair",
			want: "(_, int) pair",
		},
	}
	for _, test := range tests {
		t.Run(test.pat, func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			pat, err := parseTestPattern(t, mod, test.pat)
			if err != nil {
				t.Fatalf("failed to parse type pattern: %s", err)
			}
			if got := pat.String(); got != test.want {
				t.Errorf("(%s).String()=%s, want %s", test.pat, got, test.want)
			}
		})
	}
}

type typeParmScope struct {
	mod   *Mod
	parms []*TypeParm
}

func (s *typeParmScope) up() scope { return s.mod }

func (s *typeParmScope) findType(args []Type, name string, l loc.Loc) []Type {
	if len(args) != 0 {
		return nil
	}
	for _, parm := range s.parms {
		if parm.Name == name {
			return []Type{&TypeVar{Name: name, Def: parm, L: l}}
		}
	}
	return nil
}

func parseTestPattern(t *testing.T, m *Mod, src string) (*typePattern, error) {
	var parms []*TypeParm
	parmSet := make(map[string]*TypeParm)
	nextName := 'A'
	src2 := ""
	var prev rune
	for len(src) > 0 {
		r, w := utf8.DecodeRuneInString(src)
		src = src[w:]
		if r != '_' || unicode.IsLetter(prev) {
			src2 += string([]rune{r})
			prev = r
			continue
		}
		var name string
		for {
			r, w := utf8.DecodeRuneInString(src)
			if r < '0' || '9' < r {
				break
			}
			name += src[:w]
			src = src[w:]
		}
		if name == "" {
			name += "uniq" + strconv.Itoa(len(parmSet))
		}
		if _, ok := parmSet[name]; !ok {
			if nextName > 'Z' {
				panic("too many names")
			}
			p := &TypeParm{Name: string([]rune{nextName})}
			nextName++
			parmSet[name] = p
			parms = append(parms, p)
		}
		src2 += parmSet[name].Name
		prev = r
	}
	p, err := parser.ParseType(src2)
	if err != nil {
		return nil, err
	}
	typ, errs := _makeType(&typeParmScope{mod: m, parms: parms}, p, true, true)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	return &typePattern{parms: parms, typ: typ}, nil
}
