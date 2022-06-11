package checker

import (
	"fmt"
	"reflect"
	"sort"
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
			pats := make([]typePattern, len(test.pats))
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

func TestConvert(t *testing.T) {
	const src = `
		type T t T	// identity named type
		type int_val int
		type uint16_val uint16
		type int_ref &int
		type int_ref_ref &int_ref
		type byte_array [uint8]
		type string_val string
		type array_int [int]
		type struct_x_int_y_int [.x int, .y int]
		type struct_x_int [.x int]
		type struct_x_int_ref &[.x int]
		type struct_x_int2 [.x int]
		type struct_a_int [.a int]
		type union_x [x?]
		type union_x_int [x? int]
		type union_x_int_y_int [x? int, y? int]
		type func_int_to_int (int){int}
		type (X, Y) pair [.x X, .y Y]
		type (X, Y) pair2 [.x X, .y Y]

		type T pointer &T
	`
	c := func(xs ...interface{}) []interface{} { return xs }
	tests := []struct {
		src      string
		dst      string
		explicit bool
		// want is the source type string, ConvertKinds, and final type string.
		want []interface{}
	}{
		{src: "[int]", dst: "[int] pointer", want: c("[int]", Ref, "[int] pointer")},
		{src: "[int]", dst: "_ pointer", want: c("[int]", Ref, "[int] pointer")},

		// Anything can implicitly convert to [.].
		{src: "[.]", dst: "[.]", want: c("[.]", Noop, "[.]")},
		{src: "int", dst: "[.]", want: c("int", Drop, "[.]")},
		{src: "T", dst: "[.]", want: c("T", Drop, "[.]")},
		{src: "int t", dst: "[.]", want: c("int t", Drop, "[.]")},
		{src: "&int", dst: "[.]", want: c("&int", Drop, "[.]")},
		{src: "[int]", dst: "[.]", want: c("[int]", Drop, "[.]")},
		{src: "[.x int]", dst: "[.]", want: c("[.x int]", Drop, "[.]")},
		{src: "[x? int]", dst: "[.]", want: c("[x? int]", Drop, "[.]")},
		{src: "(int){}", dst: "[.]", want: c("(int){}", Drop, "[.]")},
		{src: "(){int}", dst: "[.]", want: c("(){int}", Drop, "[.]")},
		{src: "int_ref", dst: "[.]", want: c("int_ref", Drop, "[.]")},

		// Any type can implicitly convert to itself.
		{src: "[.]", dst: "[.]", want: c("[.]", Noop, "[.]")},
		{src: "int", dst: "int", want: c("int", Noop, "int")},
		{src: "T", dst: "T", want: c("T", Noop, "T")},
		{src: "int t", dst: "int t", want: c("int t", Noop, "int t")},
		{src: "int t t", dst: "int t t", want: c("int t t", Noop, "int t t")},
		{src: "&int", dst: "&int", want: c("&int", Noop, "&int")},
		{src: "[int]", dst: "[int]", want: c("[int]", Noop, "[int]")},
		{src: "[.x int]", dst: "[.x int]", want: c("[.x int]", Noop, "[.x int]")},
		{src: "[x? int]", dst: "[x? int]", want: c("[x? int]", Noop, "[x? int]")},
		{src: "(){}", dst: "(){}", want: c("(){}", Noop, "(){}")},
		{src: "(int){}", dst: "(int){}", want: c("(int){}", Noop, "(int){}")},
		{src: "(){int}", dst: "(){int}", want: c("(){int}", Noop, "(){int}")},
		{src: "(int){string}", dst: "(int){string}", want: c("(int){string}", Noop, "(int){string}")},

		// All reference types will explicitly numeric convert to uintref; normal numbers will not.
		{src: "&int", dst: "uintref", explicit: true, want: c("&int", NumConvert, "uintref")},
		{src: "int_ref", dst: "uintref", explicit: true, want: c("int_ref", NumConvert, "uintref")},
		// A double-reference type is just a NumConvert, not [Deref, NumConvert].
		{src: "&int_ref", dst: "uintref", explicit: true, want: c("&int_ref", NumConvert, "uintref")},
		{src: "(&int) t", dst: "uintref", explicit: true, want: c("(&int) t", NumConvert, "uintref")},
		{src: "(&int) t t", dst: "uintref", explicit: true, want: c("(&int) t t", NumConvert, "uintref")},
		{src: "int", dst: "uintref", explicit: true, want: nil},
		{src: "int_val", dst: "uintref", explicit: true, want: nil},
		{src: "int t", dst: "uintref", explicit: true, want: nil},
		{src: "uint64", dst: "uintref", explicit: true, want: nil},
		// The conversion must be explicit.
		{src: "&int", dst: "uintref", explicit: false, want: nil},

		// All numeric types will explicitly convert to other numeric types (but not to uintref).
		// This ∃st is not exhaustive, but covers several source types exhaustively
		{src: "int", dst: "int", explicit: true, want: c("int", Noop, "int")},
		{src: "int", dst: "int_val", explicit: true, want: c("int", Noop, "int_val")},
		{src: "int", dst: "int8", explicit: true, want: c("int", NumConvert, "int8")},
		{src: "int", dst: "int16", explicit: true, want: c("int", NumConvert, "int16")},
		{src: "int", dst: "int32", explicit: true, want: c("int", NumConvert, "int32")},
		{src: "int", dst: "int64", explicit: true, want: c("int", NumConvert, "int64")},
		{src: "int", dst: "uint", explicit: true, want: c("int", NumConvert, "uint")},
		{src: "int", dst: "uint8", explicit: true, want: c("int", NumConvert, "uint8")},
		{src: "int", dst: "uint16", explicit: true, want: c("int", NumConvert, "uint16")},
		{src: "int", dst: "uint32", explicit: true, want: c("int", NumConvert, "uint32")},
		{src: "int", dst: "uint64", explicit: true, want: c("int", NumConvert, "uint64")},
		{src: "int", dst: "float32", explicit: true, want: c("int", NumConvert, "float32")},
		{src: "int", dst: "float64", explicit: true, want: c("int", NumConvert, "float64")},
		{src: "uint16", dst: "int", explicit: true, want: c("uint16", NumConvert, "int")},
		{src: "uint16", dst: "int_val", explicit: true, want: c("uint16", NumConvert, "int_val")},
		{src: "uint16", dst: "int8", explicit: true, want: c("uint16", NumConvert, "int8")},
		{src: "uint16", dst: "int16", explicit: true, want: c("uint16", NumConvert, "int16")},
		{src: "uint16", dst: "int32", explicit: true, want: c("uint16", NumConvert, "int32")},
		{src: "uint16", dst: "int64", explicit: true, want: c("uint16", NumConvert, "int64")},
		{src: "uint16", dst: "uint", explicit: true, want: c("uint16", NumConvert, "uint")},
		{src: "uint16", dst: "uint8", explicit: true, want: c("uint16", NumConvert, "uint8")},
		{src: "uint16", dst: "uint16", explicit: true, want: c("uint16", Noop, "uint16")},
		{src: "uint16", dst: "uint32", explicit: true, want: c("uint16", NumConvert, "uint32")},
		{src: "uint16", dst: "uint64", explicit: true, want: c("uint16", NumConvert, "uint64")},
		{src: "uint16", dst: "float32", explicit: true, want: c("uint16", NumConvert, "float32")},
		{src: "uint16", dst: "float64", explicit: true, want: c("uint16", NumConvert, "float64")},
		{src: "uintref", dst: "int", explicit: true, want: c("uintref", NumConvert, "int")},
		{src: "uintref", dst: "int_val", explicit: true, want: c("uintref", NumConvert, "int_val")},
		{src: "uintref", dst: "int8", explicit: true, want: c("uintref", NumConvert, "int8")},
		{src: "uintref", dst: "int16", explicit: true, want: c("uintref", NumConvert, "int16")},
		{src: "uintref", dst: "int32", explicit: true, want: c("uintref", NumConvert, "int32")},
		{src: "uintref", dst: "int64", explicit: true, want: c("uintref", NumConvert, "int64")},
		{src: "uintref", dst: "uint", explicit: true, want: c("uintref", NumConvert, "uint")},
		{src: "uintref", dst: "uint8", explicit: true, want: c("uintref", NumConvert, "uint8")},
		{src: "uintref", dst: "uint16", explicit: true, want: c("uintref", NumConvert, "uint16")},
		{src: "uintref", dst: "uint32", explicit: true, want: c("uintref", NumConvert, "uint32")},
		{src: "uintref", dst: "uint64", explicit: true, want: c("uintref", NumConvert, "uint64")},
		{src: "uintref", dst: "float32", explicit: true, want: c("uintref", NumConvert, "float32")},
		{src: "uintref", dst: "float64", explicit: true, want: c("uintref", NumConvert, "float64")},
		{src: "float64", dst: "int", explicit: true, want: c("float64", NumConvert, "int")},
		{src: "float64", dst: "int_val", explicit: true, want: c("float64", NumConvert, "int_val")},
		{src: "float64", dst: "int8", explicit: true, want: c("float64", NumConvert, "int8")},
		{src: "float64", dst: "int16", explicit: true, want: c("float64", NumConvert, "int16")},
		{src: "float64", dst: "int32", explicit: true, want: c("float64", NumConvert, "int32")},
		{src: "float64", dst: "int64", explicit: true, want: c("float64", NumConvert, "int64")},
		{src: "float64", dst: "uint", explicit: true, want: c("float64", NumConvert, "uint")},
		{src: "float64", dst: "uint8", explicit: true, want: c("float64", NumConvert, "uint8")},
		{src: "float64", dst: "uint16", explicit: true, want: c("float64", NumConvert, "uint16")},
		{src: "float64", dst: "uint32", explicit: true, want: c("float64", NumConvert, "uint32")},
		{src: "float64", dst: "uint64", explicit: true, want: c("float64", NumConvert, "uint64")},
		{src: "float64", dst: "float32", explicit: true, want: c("float64", NumConvert, "float32")},
		{src: "float64", dst: "float64", explicit: true, want: c("float64", Noop, "float64")},
		{src: "int_val", dst: "int", explicit: true, want: c("int_val", Noop, "int")},
		{src: "int_val", dst: "int_val", explicit: true, want: c("int_val", Noop, "int_val")},
		{src: "int_val", dst: "int8", explicit: true, want: c("int_val", NumConvert, "int8")},
		{src: "int_val", dst: "int16", explicit: true, want: c("int_val", NumConvert, "int16")},
		{src: "int_val", dst: "int32", explicit: true, want: c("int_val", NumConvert, "int32")},
		{src: "int_val", dst: "int64", explicit: true, want: c("int_val", NumConvert, "int64")},
		{src: "int_val", dst: "uint", explicit: true, want: c("int_val", NumConvert, "uint")},
		{src: "int_val", dst: "uint8", explicit: true, want: c("int_val", NumConvert, "uint8")},
		{src: "int_val", dst: "uint16", explicit: true, want: c("int_val", NumConvert, "uint16")},
		{src: "int_val", dst: "uint32", explicit: true, want: c("int_val", NumConvert, "uint32")},
		{src: "int_val", dst: "uint64", explicit: true, want: c("int_val", NumConvert, "uint64")},
		{src: "int_val", dst: "float32", explicit: true, want: c("int_val", NumConvert, "float32")},
		{src: "int_val", dst: "float64", explicit: true, want: c("int_val", NumConvert, "float64")},
		{src: "int t", dst: "float64", explicit: true, want: c("int t", NumConvert, "float64")},
		{src: "int_val t", dst: "float64", explicit: true, want: c("int_val t", NumConvert, "float64")},
		{src: "int t t", dst: "float64", explicit: true, want: c("int t t", NumConvert, "float64")},
		// Two different defined numeric types can convert.
		{src: "int_val", dst: "uint16_val", explicit: true, want: c("int_val", NumConvert, "uint16_val")},
		// Cannot convert to uintref.
		{src: "int", dst: "uintref", explicit: true, want: nil},
		// Cannot implicitly convert to different types.
		{src: "int", dst: "int", explicit: false, want: c("int", Noop, "int")},
		{src: "int", dst: "int8", explicit: false, want: nil},
		{src: "int", dst: "int16", explicit: false, want: nil},
		{src: "int", dst: "int32", explicit: false, want: nil},
		{src: "int", dst: "int64", explicit: false, want: nil},
		{src: "int", dst: "uint", explicit: false, want: nil},

		// Explicit string conversion.
		{src: "[uint8]", dst: "string", explicit: true, want: c("[uint8]", StrConvert, "string")},
		{src: "byte_array", dst: "string", explicit: true, want: c("byte_array", StrConvert, "string")},
		{src: "[uint8]", dst: "string_val", explicit: true, want: c("[uint8]", StrConvert, "string_val")},
		{src: "byte_array", dst: "string_val", explicit: true, want: c("byte_array", StrConvert, "string_val")},
		// Must be explicit.
		{src: "[uint8]", dst: "string", explicit: false, want: nil},

		// Defined types can explicitly convert to and from equivalently structured types.
		{src: "struct_x_int", dst: "struct_x_int", explicit: true, want: c("struct_x_int", Noop, "struct_x_int")},
		{src: "struct_x_int", dst: "[.x int]", explicit: true, want: c("struct_x_int", Noop, "[.x int]")},
		{src: "struct_x_int", dst: "[.x int] t", explicit: true, want: c("struct_x_int", Noop, "[.x int] t")},
		{src: "struct_x_int", dst: "[.x int] t t", explicit: true, want: c("struct_x_int", Noop, "[.x int] t t")},
		{src: "struct_x_int", dst: "struct_x_int2", explicit: true, want: c("struct_x_int", Noop, "struct_x_int2")},
		{src: "struct_x_int", dst: "struct_x_int2 t", explicit: true, want: c("struct_x_int", Noop, "struct_x_int2 t")},
		{src: "struct_x_int", dst: "struct_x_int", explicit: true, want: c("struct_x_int", Noop, "struct_x_int")},
		{src: "[.x int]", dst: "struct_x_int", explicit: true, want: c("[.x int]", Noop, "struct_x_int")},
		{src: "[.x int] t", dst: "struct_x_int", explicit: true, want: c("[.x int] t", Noop, "struct_x_int")},
		{src: "[.x int] t t", dst: "struct_x_int", explicit: true, want: c("[.x int] t t", Noop, "struct_x_int")},
		{src: "struct_x_int2", dst: "struct_x_int", explicit: true, want: c("struct_x_int2", Noop, "struct_x_int")},
		{src: "struct_x_int2 t", dst: "struct_x_int", explicit: true, want: c("struct_x_int2 t", Noop, "struct_x_int")},
		{src: "[.x int] t", dst: "[.x int]", explicit: true, want: c("[.x int] t", Noop, "[.x int]")},
		{src: "[.x int] t", dst: "[.x int] t t", explicit: true, want: c("[.x int] t", Noop, "[.x int] t t")},
		{src: "[.x int]", dst: "[.x int] t t", explicit: true, want: c("[.x int]", Noop, "[.x int] t t")},
		{src: "[.x int] t", dst: "[.x int] t t", explicit: true, want: c("[.x int] t", Noop, "[.x int] t t")},
		{src: "[.x int] t", dst: "[.x int] t t", explicit: true, want: c("[.x int] t", Noop, "[.x int] t t")},
		{src: "[int] t", dst: "[int]", explicit: true, want: c("[int] t", Noop, "[int]")},
		{src: "[int]", dst: "[int] t", explicit: true, want: c("[int]", Noop, "[int] t")},
		{src: "[x?] t", dst: "[x?]", explicit: true, want: c("[x?] t", Noop, "[x?]")},
		{src: "[x?]", dst: "[x?] t", explicit: true, want: c("[x?]", Noop, "[x?] t")},
		{src: "[x? int] t", dst: "[x? int]", explicit: true, want: c("[x? int] t", Noop, "[x? int]")},
		{src: "[x? int]", dst: "[x? int] t", explicit: true, want: c("[x? int]", Noop, "[x? int] t")},
		{src: "(){} t", dst: "(){}", explicit: true, want: c("(){} t", Noop, "(){}")},
		{src: "(){}", dst: "(){} t", explicit: true, want: c("(){}", Noop, "(){} t")},
		{src: "(int){} t", dst: "(int){}", explicit: true, want: c("(int){} t", Noop, "(int){}")},
		{src: "(int){}", dst: "(int){} t", explicit: true, want: c("(int){}", Noop, "(int){} t")},
		{src: "(){int} t", dst: "(){int}", explicit: true, want: c("(){int} t", Noop, "(){int}")},
		{src: "(){int}", dst: "(){int} t", explicit: true, want: c("(){int}", Noop, "(){int} t")},
		{src: "(int){bool} t", dst: "(int){bool}", explicit: true, want: c("(int){bool} t", Noop, "(int){bool}")},
		{src: "(int){bool}", dst: "(int){bool} t", explicit: true, want: c("(int){bool}", Noop, "(int){bool} t")},
		// They cannot implicitly convert to/from non-literal types
		{src: "struct_x_int", dst: "struct_x_int2", explicit: false, want: nil},
		{src: "struct_x_int", dst: "struct_x_int t", explicit: false, want: nil},
		{src: "struct_x_int", dst: "[.x int] t", explicit: false, want: nil},
		{src: "(){} t t", dst: "(){} t", explicit: false, want: nil},

		// Defined types can implicitly convert to and from array, struct, union, and func literal types.
		{src: "array_int", dst: "[int]", want: c("array_int", Noop, "[int]")},
		{src: "[int]", dst: "array_int", want: c("[int]", Noop, "array_int")},
		{src: "struct_x_int", dst: "[.x int]", want: c("struct_x_int", Noop, "[.x int]")},
		{src: "[.x int]", dst: "struct_x_int", want: c("[.x int]", Noop, "struct_x_int")},
		{src: "union_x", dst: "[x?]", want: c("union_x", Noop, "[x?]")},
		{src: "[x?]", dst: "union_x", want: c("[x?]", Noop, "union_x")},
		{src: "union_x_int", dst: "[x? int]", want: c("union_x_int", Noop, "[x? int]")},
		{src: "[x? int]", dst: "union_x_int", want: c("[x? int]", Noop, "union_x_int")},
		{src: "func_int_to_int", dst: "(int){int}", want: c("func_int_to_int", Noop, "(int){int}")},
		{src: "(int){int}", dst: "func_int_to_int", want: c("(int){int}", Noop, "func_int_to_int")},
		// &int is not array, struct, union, or func literal type, it is a literal reference to a named type.
		{src: "int_ref", dst: "&int", want: nil},
		{src: "&int", dst: "int_ref", want: nil},
		// bool is defined as [false?, true?].
		{src: "bool", dst: "[false?, true?]", want: c("bool", Noop, "[false?, true?]")},
		{src: "[false?, true?]", dst: "bool", want: c("[false?, true?]", Noop, "bool")},

		// A union can explicitly convert to a union with a superset of the cases.
		{src: "[x?]", dst: "[x?]", explicit: true, want: c("[x?]", Noop, "[x?]")},
		{src: "[x?]", dst: "[x?, y?, z?]", explicit: true, want: c("[x?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[y?]", dst: "[x?, y?, z?]", explicit: true, want: c("[y?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[z?]", dst: "[x?, y?, z?]", explicit: true, want: c("[z?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[x?, y?]", dst: "[x?, y?, z?]", explicit: true, want: c("[x?, y?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[x?, z?]", dst: "[x?, y?, z?]", explicit: true, want: c("[x?, z?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[y?, z?]", dst: "[x?, y?, z?]", explicit: true, want: c("[y?, z?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[x? int]", dst: "[x? int, y? int]", explicit: true, want: c("[x? int]", UnionConvert, "[x? int, y? int]")},
		{src: "[y? int]", dst: "[x? int, y? int]", explicit: true, want: c("[y? int]", UnionConvert, "[x? int, y? int]")},
		{src: "[z?, x?, y?]", dst: "[x?, y?, z?]", explicit: true, want: c("[z?, x?, y?]", UnionConvert, "[x?, y?, z?]")},
		{src: "[x? int, y? int]", dst: "[x? int, y? int, z?]", explicit: true, want: c("[x? int, y? int]", UnionConvert, "[x? int, y? int, z?]")},
		{src: "[x? int, z?]", dst: "[x? int, y? int, z?]", explicit: true, want: c("[x? int, z?]", UnionConvert, "[x? int, y? int, z?]")},
		{src: "[y? int, z?]", dst: "[x? int, y? int, z?]", explicit: true, want: c("[y? int, z?]", UnionConvert, "[x? int, y? int, z?]")},
		{src: "[z?, y? int]", dst: "[x? int, y? int, z?]", explicit: true, want: c("[z?, y? int]", UnionConvert, "[x? int, y? int, z?]")},
		{src: "union_x_int", dst: "[x? int, y? int]", explicit: true, want: c("union_x_int", UnionConvert, "[x? int, y? int]")},
		{src: "union_x_int", dst: "union_x_int_y_int", explicit: true, want: c("union_x_int", UnionConvert, "union_x_int_y_int")},
		{src: "[x? int]", dst: "union_x_int_y_int", explicit: true, want: c("[x? int]", UnionConvert, "union_x_int_y_int")},
		// Case type mismatch.
		{src: "[x? uint]", dst: "[x? int, y? int]", explicit: true, want: nil},
		// Non-superset
		{src: "[x?, y?, z?]", dst: "[x?, a?, b?, y?]", explicit: true, want: nil},
		// Cannot implicitly convert.
		// TODO: disallow implicit union conversion.
		// {src: "[x?]", dst: "[x?, y?, z?]", explicit: false, want: nil},

		// Implicit dereference
		{src: "&int", dst: "int", want: c("&int", Deref, "int")},
		{src: "&struct_x_int_ref", dst: "[.x int]", want: c("&struct_x_int_ref", Deref, Deref, "[.x int]")},
		{src: "&struct_x_int_y_int", dst: "[.x int, .y int]", want: c("&struct_x_int_y_int", Deref, "[.x int, .y int]")},

		// Explicit dereferences
		{src: "&int_ref_ref", dst: "int_ref_ref", explicit: true, want: c("&int_ref_ref", Deref, "int_ref_ref")},
		{src: "&int_ref_ref", dst: "&int_ref", explicit: true, want: c("&int_ref_ref", Deref, "&int_ref")},
		{src: "&int_ref_ref", dst: "int_ref", explicit: true, want: c("&int_ref_ref", Deref, Deref, "int_ref")},
		{src: "&int_ref_ref", dst: "&int_val", explicit: true, want: c("&int_ref_ref", Deref, Deref, "&int_val")},
		{src: "&int_ref_ref", dst: "int_val", explicit: true, want: c("&int_ref_ref", Deref, Deref, Deref, "int_val")},
		{src: "&int_ref_ref", dst: "int", explicit: true, want: c("&int_ref_ref", Deref, Deref, Deref, "int")},
		{src: "&int_ref", dst: "&int_val", explicit: true, want: c("&int_ref", Deref, "&int_val")},
		{src: "&int_ref_ref", dst: "&int_val", explicit: true, want: c("&int_ref_ref", Deref, Deref, "&int_val")},
		// Must be explicit.
		{src: "&int_ref_ref", dst: "&int_val", explicit: false, want: nil},

		// Implicit references
		{src: "int", dst: "&int", want: c("int", Ref, "&int")},
		{src: "[.x int]", dst: "&struct_x_int_ref", want: c("[.x int]", Ref, Ref, "&struct_x_int_ref")},

		// Explicit references
		{src: "int_val", dst: "&int_val", explicit: true, want: c("int_val", Ref, "&int_val")},
		{src: "int", dst: "&int_val", explicit: true, want: c("int", Ref, "&int_val")},
		{src: "int", dst: "int_ref", explicit: true, want: c("int", Ref, "int_ref")},
		{src: "int", dst: "&int_ref", explicit: true, want: c("int", Ref, Ref, "&int_ref")},
		{src: "int", dst: "int_ref_ref", explicit: true, want: c("int", Ref, Ref, "int_ref_ref")},
		{src: "int", dst: "&int_ref_ref", explicit: true, want: c("int", Ref, Ref, Ref, "&int_ref_ref")},
		{src: "&int_val", dst: "&int_ref", explicit: true, want: c("&int_val", Ref, "&int_ref")},

		// Explicit no-op change of reference element value.
		{src: "&int", dst: "&int_val", explicit: true, want: c("&int", Noop, "&int_val")},
		{src: "&int", dst: "int_ref", explicit: true, want: c("&int", Noop, "int_ref")},
		{src: "&struct_x_int", dst: "&[.x int]", explicit: true, want: c("&struct_x_int", Noop, "&[.x int]")},
		{src: "&int_val", dst: "int_ref", explicit: true, want: c("&int_val", Noop, "int_ref")},

		// Explicit value and reference change.
		{src: "int", dst: "&float64", explicit: true, want: c("int", NumConvert, Ref, "&float64")},
		{src: "&int", dst: "float64", explicit: true, want: c("&int", Deref, NumConvert, "float64")},
		{src: "&int", dst: "&float64", explicit: true, want: c("&int", Deref, NumConvert, Ref, "&float64")},
		{src: "&byte_array", dst: "&string_val", explicit: true, want: c("&byte_array", Deref, StrConvert, Ref, "&string_val")},

		// Intersection.
		{src: "int", dst: "_", want: c("int", Noop, "int")},
		{src: "int_val", dst: "_", want: c("int_val", Noop, "int_val")},
		{src: "&int", dst: "_", want: c("&int", Noop, "&int")},
		{src: "T", dst: "_", want: c("T", Noop, "T")},
		{src: "[int]", dst: "_", want: c("[int]", Noop, "[int]")},
		{src: "[.x int]", dst: "_", want: c("[.x int]", Noop, "[.x int]")},
		{src: "[x? int]", dst: "_", want: c("[x? int]", Noop, "[x? int]")},
		{src: "(int){float64}", dst: "_", want: c("(int){float64}", Noop, "(int){float64}")},
		{src: "&int", dst: "&_", want: c("&int", Noop, "&int")},
		{src: "int", dst: "&_", want: c("int", Ref, "&int")},
		{src: "[.x int]", dst: "[.x _]", want: c("[.x int]", Noop, "[.x int]")},
		{src: "(int, string) pair", dst: "[.x _, .y _]", want: c("(int, string) pair", Noop, "[.x int, .y string]")},
		{src: "[.x int, .y string]", dst: "(_, _) pair", want: c("[.x int, .y string]", Noop, "(int, string) pair")},
		{src: "(int, string) pair", dst: "(_, _) pair2", explicit: true, want: c("(int, string) pair", Noop, "(int, string) pair2")},
		// TODO: _ gets renamed to A during parseTestPattern.
		{src: "[.x int, .y _]", dst: "[.x _, .y string]", want: c("[.x int, .y A]", Noop, "[.x int, .y string]")},

		// Some obvious failing conversions.
		{src: "[.x int]", dst: "int_val", explicit: true, want: nil},
		{src: "int_val", dst: "[.x int]", explicit: true, want: nil},
		{src: "[.x int]", dst: "[x? int]", explicit: true, want: nil},
		{src: "(){}", dst: "[int]", explicit: true, want: nil},

		// It is an error if the conversion does not fully ground the resulting type.
		{src: "_", dst: "_", explicit: false, want: nil},
		{src: "_", dst: "_", explicit: true, want: nil},
		{src: "[.x _, .y _]", dst: "[.x int, .y _]", want: nil},
	}
	for _, test := range tests {
		test := test
		name := test.src + " to " + test.dst
		if test.explicit {
			name += " explicit"
		}
		t.Run(name, func(t *testing.T) {
			mod, errs := check("test", []string{src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			src, err := parseTestPattern(t, mod, test.src)
			if err != nil {
				t.Fatalf("failed to parse type pattern %s: %s", test.src, err)
			}
			dst, err := parseTestPattern(t, mod, test.dst)
			if err != nil {
				t.Fatalf("failed to parse type pattern %s: %s", test.dst, err)
			}
			var bind map[*TypeParm]Type
			cvt, _ := convert(nil, src, dst, test.explicit, &bind)
			if cvt == nil {
				if len(test.want) != 0 {
					t.Fatalf("convert(%s, %s, %v)=nil, want %v",
						test.src, test.dst, test.explicit, test.want)
				}
				return // ok
			}
			got := []interface{}{src.typ.String()}
			var get func(cvt *Convert)
			get = func(cvt *Convert) {
				if cvt.Expr != nil {
					get(cvt.Expr.(*Convert))
				}
				got = append(got, cvt.Kind)
			}
			get(cvt)
			got = append(got, cvt.Type().String())

			if !reflect.DeepEqual(got, test.want) {
				t.Fatalf("convert(%s, %s, %v)=%v, want %v",
					test.src, test.dst, test.explicit, got, test.want)
			}
		})
	}
}

func TestPatternIntersection(t *testing.T) {
	tests := []struct {
		src  string
		a    string
		b    string
		want string
	}{
		// Sub _ with all the different kinds of types.
		{a: `_`, b: `_`, want: `_`},
		{a: `_`, b: `int`, want: `int`},
		{src: `type t int`, a: `_`, b: `t`, want: `t`},
		{src: `type (T, U) t int`, a: `_`, b: `(int, string) t`, want: `(int, string) t`},
		{a: `_`, b: `T`, want: `T`},
		{a: `_`, b: `&int`, want: `&int`},
		{a: `_`, b: `[int]`, want: `[int]`},
		{a: `_`, b: `[.x int]`, want: `[.x int]`},
		{a: `_`, b: `[x?]`, want: `[x?]`},
		{a: `_`, b: `[x? int]`, want: `[x? int]`},
		{a: `_`, b: `(int, string){float32}`, want: `(int, string){float32}`},

		// Sub _ with all the different kinds of types, leading to recursive sub error.
		{src: `type T t int`, a: `[.x _0, .y _0]`, b: `[.x _1 t, .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x &_1, .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x [_1], .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x [.y _1], .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x [y? _1], .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x (_1){}, .y _1]`, want: ``},
		{a: `[.x _0, .y _0]`, b: `[.x (){_1}, .y _1]`, want: ``},

		// Sub into all different spots.
		{a: `[.x int, .y _]`, b: `[.x _0, .y _0]`, want: `[.x int, .y int]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y &_0]`, want: `[.x int, .y &int]`},
		{src: `type T t int`, a: `[.x int, .y _]`, b: `[.x _0, .y _0 t]`, want: `[.x int, .y int t]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y [_0]]`, want: `[.x int, .y [int]]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y [.a _0]]`, want: `[.x int, .y [.a int]]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y [a? _0]]`, want: `[.x int, .y [a? int]]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y (_0){}]`, want: `[.x int, .y (int){}]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y (){_0}]`, want: `[.x int, .y (){int}]`},
		{a: `[.x int, .y _]`, b: `[.x _0, .y (_0, _0){_0}]`, want: `[.x int, .y (int, int){int}]`},

		{a: `int`, b: `int`, want: `int`},
		{a: `int`, b: `float32`, want: ``},
		{a: `int`, b: `&int`, want: ``},
		{a: `int`, b: `_`, want: `int`},
		{a: `_`, b: `int`, want: `int`},

		{src: `type t int 	type u string`, a: `t`, b: `t`, want: `t`},
		{src: `type t int 	type u string`, a: `t`, b: `[int]`, want: ``},
		{src: `type t int 	type u string`, a: `t`, b: `u`, want: ``},
		{src: `type t int 	type u string`, a: `t`, b: `_`, want: `t`},
		{src: `type t int 	type u string`, a: `_`, b: `u`, want: `u`},

		{src: `type T t T`, a: `int t`, b: `int t`, want: `int t`},
		{src: `type T t T`, a: `string t`, b: `int t`, want: ``},
		{src: `type T t T`, a: `int t`, b: `_ t`, want: `int t`},
		{src: `type T t T`, a: `_ t`, b: `int t`, want: `int t`},
		{src: `type T t T`, a: `_ t`, b: `_ t`, want: `_ t`},
		{src: `type T t T 	type U u U`, a: `int t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `_ t`, b: `int u`, want: ``},
		{src: `type T t T 	type U u U`, a: `int t`, b: `_ u`, want: ``},
		{src: `type T t T 	type U u U`, a: `_ t`, b: `_ u`, want: ``},

		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(_, int) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(_, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(int, _) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(_, _) t`, b: `(int, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, int) t`, b: `(_, _) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(_, int) t`, b: `(int, _) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(_, int) t`, want: `(int, int) t`},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(int, _) t`, want: `(int, _) t`},
		{src: `type (T, U) t int`, a: `(_, int) t`, b: `(_, int) t`, want: `(_, int) t`},
		{src: `type (T, U) t int`, a: `(_, _) t`, b: `(_, _) t`, want: `(_, _) t`},
		{src: `type (T, U) t int`, a: `(_, string) t`, b: `(int, _) t`, want: `(int, string) t`},
		{src: `type (T, U) t int`, a: `(int, string) t`, b: `(_, int) t`, want: ``},
		{src: `type (T, U) t int`, a: `(int, _) t`, b: `(string, _) t`, want: ``},
		{src: `type (T, U) t int`, a: `(_0, _0) t`, b: `(string, _) t`, want: `(string, string) t`},
		{src: `type (T, U) t int`, a: `(_0, _0) t`, b: `(string, int) t`, want: ``},

		{a: `T`, b: `T`, want: `T`},
		{a: `T`, b: `U`, want: `T`},
		{a: `T`, b: `int`, want: ``},
		{a: `T`, b: `_`, want: `T`},
		{a: `_`, b: `T`, want: `T`},

		{a: `&int`, b: `&int`, want: `&int`},
		{a: `&int`, b: `int`, want: ``},
		{a: `&int`, b: `&float32`, want: ``},
		{a: `&int`, b: `&_`, want: `&int`},
		{a: `&_`, b: `&int`, want: `&int`},
		{a: `&_`, b: `&_`, want: `&_`},

		{a: `[int]`, b: `[int]`, want: `[int]`},
		{a: `[int]`, b: `int`, want: ``},
		{a: `[int]`, b: `string`, want: ``},
		{a: `[int]`, b: `[_]`, want: `[int]`},
		{a: `[_]`, b: `[int]`, want: `[int]`},
		{a: `[_]`, b: `[_]`, want: `[_]`},

		{a: `[.]`, b: `[.]`, want: `[.]`},
		{a: `[.x int]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int]`, b: `int`, want: ``},
		{a: `[.x int]`, b: `[.x int, .y string]`, want: ``},
		{a: `[.x int]`, b: `[.y int]`, want: ``},
		{a: `[.x int]`, b: `[.x _]`, want: `[.x int]`},
		{a: `[.x _]`, b: `[.x int]`, want: `[.x int]`},
		{a: `[.x _]`, b: `[.x _]`, want: `[.x _]`},
		{a: `[.x _, .y string]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y _]`, b: `[.x int, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x _, .y string]`, want: `[.x int, .y string]`},
		{a: `[.x int, .y string]`, b: `[.x int, .y _]`, want: `[.x int, .y string]`},
		{a: `[.x _, .y string]`, b: `[.x _, .y string]`, want: `[.x _, .y string]`},
		{a: `[.x int, .y _]`, b: `[.x int, .y _]`, want: `[.x int, .y _]`},
		{a: `[.x _, .y _]`, b: `[.x _, .y _]`, want: `[.x _, .y _]`},
		{a: `[.x _, .y _]`, b: `[.x _]`, want: ``},
		{a: `[.x _, .y int]`, b: `[.x _, .y string]`, want: ``},
		{a: `[.x _, .y int]`, b: `[.x _, .y T]`, want: ``},
		{a: `[.x _, .y T]`, b: `[.x _, .y string]`, want: ``},
		{a: `[.x _, .y T]`, b: `[.x U, .y T]`, want: `[.x U, .y T]`},

		{a: `[x?]`, b: `[x?]`, want: `[x?]`},
		{a: `[x? float64]`, b: `[x? float64]`, want: `[x? float64]`},
		{a: `[x? int, y?, z? string]`, b: `[x? int, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? float64, y?]`, b: `int`, want: ``},
		{a: `[x? float64, y?]`, b: `[x? float64]`, want: ``},
		{a: `[x? float64, y?]`, b: `[a? float64, y?]`, want: ``},
		{a: `[x? float64, y?]`, b: `[x? float64, y? int]`, want: ``},
		{a: `[x? float64, y? string]`, b: `[x? float64, y? int]`, want: ``},
		{a: `[x? _, y?, z? string]`, b: `[x? int, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? int, y?, z? string]`, b: `[x? _, y?, z? string]`, want: `[x? int, y?, z? string]`},
		{a: `[x? _, y?, z? string]`, b: `[x? int]`, want: ``},
		{a: `[x? _, y?, z? string]`, b: `[x? int, y?, z? int]`, want: ``},
		{a: `[x? _, y?, z? string]`, b: `[x? int, y? string, z? string]`, want: ``},
		{a: `[x? _0, y? _0]`, b: `[x? int, y? _]`, want: `[x? int, y? int]`},
		{a: `[x? _0, y? _0]`, b: `[x? _, y? int]`, want: `[x? int, y? int]`},
		{a: `[x? _0, y? _0]`, b: `[x? string, y? int]`, want: ``},

		{a: `(){}`, b: `(){}`, want: `(){}`},
		{a: `(int){}`, b: `(int){}`, want: `(int){}`},
		{a: `(){int}`, b: `(){int}`, want: `(){int}`},
		{a: `(int){int}`, b: `(int){int}`, want: `(int){int}`},
		{a: `(int, string){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `int`, want: ``},
		{a: `(int, string){int}`, b: `(int, string, float32){int}`, want: ``},
		{a: `(int, string){int}`, b: `(string, int){int}`, want: ``},

		{a: `(_, string){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, _){int}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){_}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(_, string){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(int, _){int}`, want: `(int, string){int}`},
		{a: `(int, string){int}`, b: `(int, string){_}`, want: `(int, string){int}`},
		{a: `(_, _){_}`, b: `(int, string){int}`, want: `(int, string){int}`},
		{a: `(_, string){int}`, b: `(int, _){int}`, want: `(int, string){int}`},
		{a: `(int, _){int}`, b: `(_, string){int}`, want: `(int, string){int}`},
		{a: `(int, _){int}`, b: `(_){int}`, want: ``},
		{a: `(int, _){int}`, b: `(string, int){int}`, want: ``},
		{a: `(int, _){int}`, b: `(_, int){string}`, want: ``},
		{a: `(_0, _0){int}`, b: `(int, _){int}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(int, _){_}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(_, int){_}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(_, _){int}`, want: `(int, int){int}`},
		{a: `(_0, _0){_0}`, b: `(string, int){int}`, want: ``},
		{a: `(_0, _0){_0}`, b: `(int, string){int}`, want: ``},
		{a: `(_0, _0){_0}`, b: `(int, int){string}`, want: ``},
		{a: `(_0, _0){_1}`, b: `(int, int){string}`, want: `(int, int){string}`},

		{a: `[.x _0, .y _0]`, b: `[.x _, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x _0, .y _0]`, b: `[.x _0, .y _0]`, want: `[.x _0, .y _0]`},
		{a: `[.x _0, .y _0]`, b: `[.x _0, .y _1]`, want: `[.x _0, .y _0]`},
		{a: `[.x _0, .y _1]`, b: `[.x _0, .y _0]`, want: `[.x _0, .y _0]`},
		{a: `[.x _0, .y _1]`, b: `[.x _0, .y _1]`, want: `[.x _, .y _]`},
		{a: `[.x _0, .y _0]`, b: `[.x int, .y int]`, want: `[.x int, .y int]`},
		{a: `[.x _0, .y _0]`, b: `[.x string, .y int]`, want: ``},
		{a: `[.x string, .y int]`, b: `[.x _0, .y _0]`, want: ``},
		{a: `[.x int, .y int]`, b: `[.x _0, .y _0]`, want: `[.x int, .y int]`},
		{a: `[.x _0, .y _0, .z int]`, b: `[.x string, .y _0, .z _0]`, want: ``},
		{a: `[.x _0, .y _1, .z int]`, b: `[.x string, .y _0, .z _0]`, want: `[.x string, .y int, .z int]`},
		{a: `[.x _0, .y _0, .z int]`, b: `[.x string, .y _1, .z _0]`, want: `[.x string, .y string, .z int]`},
		{
			a:    `[.a _0,     .b _0, .c _1, .d _1, .e _2, .f _2, .g int]`,
			b:    `[.a string, .b _0, .c _0, .d _1, .e _1, .f _2, .g _2]`,
			want: ``,
		},
		{
			a:    `[.a _0, .b _0, .c _1, .d _1, .e int, .f _1]`,
			b:    `[.a _0, .b _0, .c _1, .d _1, .e _0, .f string]`,
			want: `[.a int, .b int, .c string, .d string, .e int, .f string]`,
		},
		{a: `[.x [.y [.z _]]]`, b: `[.x [.y [.z int]]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y [.z _]]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z _]]]`, b: `[.x [.y [.z _]]]`, want: `[.x [.y [.z _]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x [.y _]]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z int]]]`, b: `[.x _]`, want: `[.x [.y [.z int]]]`},
		{a: `[.x [.y [.z _]]]`, b: `[.x _]`, want: `[.x [.y [.z _]]]`},
		{a: `[.x [.y [.z _]]]`, b: `_`, want: `[.x [.y [.z _]]]`},
		{
			a: `[.a _0, .x [.y [.z _0]]]`,
			b: `[.a _1, .x _1]`,
			// This would be recursive.
			want: ``,
		},
		{
			a: `[.a [.y [.z _0]], .x _0]`,
			b: `[.a _1, .x _1]`,
			// This would be recursive.
			want: ``,
		},
		{
			a: `[.a _0, .b [.y [.z _0]], .c _]`,
			b: `[.a int, .b _2,              .c _2]`,
			// Sub int into _0, and then into [.x [.z _0]] which was bound to _2.
			want: `[.a int, .b [.y [.z int]], .c [.y [.z int]]]`,
		},
		{
			a: `[.a _0, .b [.y [.z _1]], .c _1]`,
			b: `[.a _2, .b _2,              .c int]`,
			// Merge _0 and _2, then sub [.y. [.z _1]] for _2 (and _0),
			// and then later sub int for _1.
			want: `[.a [.y [.z int]], .b [.y [.z int]], .c int]`,
		},
		{
			a:    `[.a _0,    .b [.x _1], .c _1, .d _2]`,
			b:    `[.a _3,    .b _3,       .c int, .d _3]`,
			want: `[.a [.x int], .b [.x int], .c int, .d [.x int]]`,
		},

		// Multi-step recursive substitution.
		{
			// _0 = [.x _10]
			// _10 = [.x _1]
			// _1 = [.x _11]
			// _11 = [.x _0]
			a: `[.a _0,		.b [.x _1],	.c _1,		.d [.x _0]]`,
			b: `[.a [.x _10],	.b _10,	.c [.x _11],	.d _11]`,
			want: ``,
		},
	}
	for _, test := range tests {
		t.Run("intersect("+test.a+", "+test.b+")", func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			a, err := parseTestPattern(t, mod, test.a)
			if err != nil {
				t.Fatalf("failed to parse type pattern %s: %s", test.a, err)
			}
			b, err := parseTestPattern(t, mod, test.b)
			if err != nil {
				t.Fatalf("failed to parse type pattern %s: %s", test.b, err)
			}

			// Given unique names.
			for i := range a.parms {
				a.parms[i].Name = fmt.Sprintf("A%d", i)
			}
			copyTypeParmNamesToVars(a.typ)
			for i := range b.parms {
				b.parms[i].Name = fmt.Sprintf("B%d", i)
			}
			copyTypeParmNamesToVars(b.typ)

			var bind map[*TypeParm]Type
			switch isect, note := intersection(a, b, &bind); {
			case test.want == "" && isect != nil:
				t.Fatalf("intersect(%s, %s)=%s, want nil", test.a, test.b, isect)
			case test.want == "":
				return // OK
			case test.want != "" && isect == nil:
				var n string
				if note != nil {
					n = " (" + note.(*_error).msg + ")"
				}
				t.Fatalf("intersect(%s, %s)=nil%s, want %s", test.a, test.b, n, test.want)
			case test.want != "" && isect.String() != test.want:
				t.Fatalf("intersect(%s, %s)=%s, want %s", test.a, test.b, isect, test.want)
			default:
				if aPrime := subType(bind, a.typ); !eqType(aPrime, isect.typ) {
					t.Errorf("sub(bind, %s)=%s, want %s", a.typ, aPrime, isect.typ)
				}
				if bPrime := subType(bind, b.typ); !eqType(bPrime, isect.typ) {
					t.Errorf("sub(bind, %s)=%s, want %s", b.typ, bPrime, isect.typ)
				}
			}
		})
	}
}

func TestPatternIntersectionOfTwoVariables(t *testing.T) {
	mod, errs := check("test", []string{""}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	a, err := parseTestPattern(t, mod, "_")
	if err != nil {
		t.Fatalf("failed to parse type pattern _: %s", err)
	}
	b, err := parseTestPattern(t, mod, "_")
	if err != nil {
		t.Fatalf("failed to parse type pattern _: %s", err)
	}
	var bind map[*TypeParm]Type
	isect, _ := intersection(a, b, &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s)=nil, wanted _", a, b)
	}
	if isect.isGroundType() {
		t.Fatalf("intersection(%s, %s).isGroundType()=true, want false", a, b)
	}
}

func TestPatternIntersectionIgnoreBoundTypeParameters(t *testing.T) {
	const src = `
		type T list [node? [.data T, .next T list], nil?]
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	pat0, err := parseTestPattern(t, mod, "_ list")
	if err != nil {
		t.Fatalf("failed to parse pat0: %s", errs)
	}
	pat0.parms = append(pat0.parms,
		&TypeParm{Name: "U0"},
		&TypeParm{Name: "U1"},
		&TypeParm{Name: "U2"})

	pat1, err := parseTestPattern(t, mod, "int list")
	if err != nil {
		t.Fatalf("failed to parse pat1: %s", errs)
	}

	var bind map[*TypeParm]Type
	isect, note := intersection(pat0, pat1, &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s) failed %s", pat0, pat1, note)
	}
	if bind == nil {
		t.Fatalf("intersection(%s, %s) got bindings nil, expected a binding to %s",
			pat0, pat1, pat0.parms[0].Name)
	}
	if _, ok := bind[pat0.parms[0]]; !ok || len(bind) != 1 {
		t.Fatalf("intersection(%s, %s) got bindings %v, expected a binding to %s",
			pat0, pat1, bindAsSlice(bind), pat0.parms[0].Name)
	}
}

func TestPatternSelfIntersection(t *testing.T) {
	const src = `
		type T list [node? [.data T, .next T list], nil?]
	`
	mod, errs := check("test", []string{src}, nil)
	if len(errs) > 0 {
		t.Fatalf("failed to parse and check: %s", errs[0])
	}
	pat, err := parseTestPattern(t, mod, "_ list")
	if err != nil {
		t.Fatalf("failed to parse pattern: %s", errs)
	}

	var bind map[*TypeParm]Type
	isect, note := intersection(pat, pattern(pat.typ), &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s) failed %s", pat, pattern(pat.typ), note)
	}
	if !eqType(isect.typ, pat.typ) || len(isect.parms) != 0 {
		t.Fatalf("intersection(%s, %s)=%s, wanted %s\n", pat, pattern(pat.typ), isect, pat.typ)
	}

	bind = nil
	isect, note = intersection(pattern(pat.typ), pat, &bind)
	if isect == nil {
		t.Fatalf("intersection(%s, %s) failed %s", pattern(pat.typ), pat, note)
	}
	if !eqType(isect.typ, pat.typ) || len(isect.parms) != 0 {
		t.Fatalf("intersection(%s, %s)=%s, wanted %s\n", pattern(pat.typ), pat, isect, pat.typ)
	}
}

func copyTypeParmNamesToVars(t Type) {
	switch t := t.(type) {
	case *DefType:
		for i := range t.Args {
			copyTypeParmNamesToVars(t.Args[i])
		}
	case *RefType:
		copyTypeParmNamesToVars(t.Type)
	case *ArrayType:
		copyTypeParmNamesToVars(t.ElemType)
	case *StructType:
		for i := range t.Fields {
			copyTypeParmNamesToVars(t.Fields[i].Type)
		}
	case *UnionType:
		for i := range t.Cases {
			copyTypeParmNamesToVars(t.Cases[i].Type)
		}
	case *FuncType:
		for i := range t.Parms {
			copyTypeParmNamesToVars(t.Parms[i])
		}
		copyTypeParmNamesToVars(t.Ret)
	case *TypeVar:
		if t.Def != nil {
			t.Name = t.Def.Name
		}
	case *BasicType:
	case nil:
	default:
		panic(fmt.Sprintf("bad type type: %T", t))
	}
}

// These are the old unify() tests.
// unify() is gone, but the same tests should work with convertType(),
// so we have repurposed them.
func TestPatternUnify(t *testing.T) {
	tests := []struct {
		src  string
		typ  string
		pat  string
		want string
	}{
		{typ: "int", pat: "string", want: ""},
		{typ: "int", pat: "int", want: "int"},
		{typ: "int", pat: "T", want: ""},
		{typ: "int", pat: "_", want: "int"},
		{typ: "int", pat: "&string", want: ""},
		{typ: "int", pat: "&int", want: "&int"},
		{typ: "int", pat: "[int]", want: ""},
		{typ: "int", pat: "[.x int, .y int]", want: ""},
		{typ: "int", pat: "[x? int, y?]", want: ""},
		{typ: "int", pat: "(int){float32}", want: ""},

		{src: "type t [.x int]", typ: "t", pat: "string", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "t", want: "t"},
		{src: "type t [.x int]", typ: "t", pat: "[.x int]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[.x _]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "T", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "_", want: "t"},
		{src: "type t [.x int]", typ: "t", pat: "&string", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "&t", want: "&t"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x int]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x _]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[int]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[.x int, .y int]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[x? int, y?]", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "(int){float32}", want: ""},

		{typ: "T", pat: "string", want: ""},
		{typ: "T", pat: "int", want: ""},
		{typ: "T", pat: "T", want: "T"},
		{typ: "T", pat: "_", want: "T"},
		{typ: "T", pat: "&string", want: ""},
		{typ: "T", pat: "&T", want: "&T"},
		{typ: "T", pat: "[int]", want: ""},
		{typ: "T", pat: "[.x int, .y int]", want: ""},
		{typ: "T", pat: "[x? int, y?]", want: ""},
		{typ: "T", pat: "(int){float32}", want: ""},

		{typ: "&int", pat: "string", want: ""},
		{typ: "&int", pat: "int", want: "int"},
		{typ: "&int", pat: "T", want: ""},
		{typ: "&int", pat: "_", want: "&int"},
		{typ: "&int", pat: "&int", want: "&int"},
		{typ: "&int", pat: "[int]", want: ""},
		{typ: "&int", pat: "[.x int, .y int]", want: ""},
		{typ: "&int", pat: "[x? int, y?]", want: ""},
		{typ: "&int", pat: "(int){float32}", want: ""},

		{typ: "[int]", pat: "string", want: ""},
		{typ: "[int]", pat: "int", want: ""},
		{typ: "[int]", pat: "T", want: ""},
		{typ: "[int]", pat: "_", want: "[int]"},
		{typ: "[int]", pat: "&int", want: ""},
		{typ: "[int]", pat: "&[int]", want: "&[int]"},
		{typ: "[int]", pat: "[int]", want: "[int]"},
		{typ: "[int]", pat: "[.x int, .y int]", want: ""},
		{typ: "[int]", pat: "[x? int, y?]", want: ""},
		{typ: "[int]", pat: "(int){float32}", want: ""},
		{src: "type t [int]", typ: "t", pat: "[int]", want: "[int]"},
		{src: "type t [int]", typ: "t", pat: "&[int]", want: "&[int]"},
		{src: "type t [int]", typ: "t", pat: "[_]", want: "[int]"},
		{src: "type t [int]", typ: "t", pat: "&[_]", want: "&[int]"},

		{typ: "[.x int, .y int]", pat: "string", want: ""},
		{typ: "[.x int, .y int]", pat: "int", want: ""},
		{typ: "[.x int, .y int]", pat: "T", want: ""},
		{typ: "[.x int, .y int]", pat: "_", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "&int", want: ""},
		{typ: "[.x int, .y int]", pat: "&[.x int, .y int]", want: "&[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int, .z int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.y int, .x int]", want: ""},
		{typ: "[.x int, .y int]", pat: "[.x int, .y int]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x _, .y int]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x int, .y _]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y int]", pat: "[.x _, .y _]", want: "[.x int, .y int]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x _, .y string]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x int, .y _]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y string]", pat: "[.x _, .y _]", want: "[.x int, .y string]"},
		{typ: "[.x int, .y int]", pat: "[x? int, y?]", want: ""},
		{typ: "[.x int, .y int]", pat: "(int){float32}", want: ""},
		{src: "type t [.x int]", typ: "t", pat: "[.x int]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x int]", want: "&[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "[.x _]", want: "[.x int]"},
		{src: "type t [.x int]", typ: "t", pat: "&[.x _]", want: "&[.x int]"},

		{typ: "[x? int, y?, z? string]", pat: "string", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "int", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "T", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "_", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "&int", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "&[x? int, y?, z? string]", want: "&[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[int]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[.x int, .y int]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? string, y?, z? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? int]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y? float32, z? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z?]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x?, y?, z? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[z? int, y?, x? string]", want: ""},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? _, y?, z? string]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? int, y?, z? _]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "[x? _, y?, z? _]", want: "[x? int, y?, z? string]"},
		{typ: "[x? int, y?, z? string]", pat: "(int){float32}", want: ""},
		{src: "type t [x? int]", typ: "t", pat: "[x? int]", want: "[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "&[x? int]", want: "&[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "[x? _]", want: "[x? int]"},
		{src: "type t [x? int]", typ: "t", pat: "&[x? _]", want: "&[x? int]"},

		{typ: "(int){float32}", pat: "string", want: ""},
		{typ: "(int){float32}", pat: "int", want: ""},
		{typ: "(int){float32}", pat: "T", want: ""},
		{typ: "(int){float32}", pat: "_", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "&int", want: ""},
		{typ: "(int){float32}", pat: "&(int){float32}", want: "&(int){float32}"},
		{typ: "(int){float32}", pat: "[int]", want: ""},
		{typ: "(int){float32}", pat: "[.x int, .y int]", want: ""},
		{typ: "(int){float32}", pat: "[x? int, y?]", want: ""},
		{typ: "(int){float32}", pat: "(int){float32}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(int){}", want: ""},
		{typ: "(int){float32}", pat: "(){float32}", want: ""},
		{typ: "(int){float32}", pat: "(int){float64}", want: ""},
		{typ: "(int){float32}", pat: "(int32){float32}", want: ""},
		{typ: "(int){float32}", pat: "(int, string){float32}", want: ""},
		{typ: "(int){float32}", pat: "(_){float32}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(int){_}", want: "(int){float32}"},
		{typ: "(int){float32}", pat: "(_){_}", want: "(int){float32}"},
		{typ: "(int, string){float32}", pat: "(_, _){_}", want: "(int, string){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(_){float32}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(int){_}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "(_){_}", want: "(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(_){float32}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(int){_}", want: "&(int){float32}"},
		{src: "type t (int){float32}", typ: "t", pat: "&(_){_}", want: "&(int){float32}"},

		{
			src: `
				type a b
				type b c
				type c d
				type d [.x int]
			`,
			typ:  "a",
			pat:  "[.x _]",
			want: "[.x int]",
		},
		{
			src: `
				type a b
				type b c
				type c d
				type d [.x int]
			`,
			typ:  "[.x int]",
			pat:  "a",
			want: "a",
		},

		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t string, .u int]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t string, .u _]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t _, .u int]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t _, .u _]",
			want: "[.t string, .u int]",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a", // args are reversed, so binding fails
			pat:  "[.t int, .u string]",
			want: "",
		},

		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "(string, int) a",
			pat:  "[.t string, .u int]",
			want: "[.t string, .u int]",
		},

		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(int, string) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(_, string) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(int, _) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(_, _) a",
			want: "(int, string) a",
		},
		{
			src: `
				type (T, U) a (T, U) b
				type (T, U) b (T, U) c
				type (T, U) c (T, U) d
				type (T, U) d [.t T, .u U]
			`,
			typ:  "[.t int, .u string][",
			pat:  "(string, int) a", // reversed parameters, so binding fails
			want: "",
		},
	}
	for _, test := range tests {
		t.Run(fmt.Sprintf("convertType(%s, %s)", test.typ, test.pat), func(t *testing.T) {
			mod, errs := check("test", []string{test.src}, nil)
			if len(errs) > 0 {
				t.Fatalf("failed to parse and check: %s", errs[0])
			}
			pat, err := parseTestPattern(t, mod, test.pat)
			if err != nil {
				t.Fatalf("failed to parse type pattern: %s", err)
			}
			typ, err := parseTestType(t, mod, test.typ)
			if err != nil {
				t.Fatalf("failed to parse type: %s", err)
			}
			bind, note := convertTypeDisallowInnerRef(pattern(typ), pat, false)
			if test.want == "" {
				if note == nil {
					t.Errorf("got %s %v, want error", subType(bind, typ), bindAsSlice(bind))
				}
			} else {
				if note != nil {
					t.Errorf("got error %s, want %s", note, test.want)
				} else if sub := subType(bind, pat.typ); sub.String() != test.want {
					t.Errorf("got %s %v, want %s", sub, bindAsSlice(bind), test.want)
				}
			}
		})
	}
}

func bindAsSlice(bind map[*TypeParm]Type) [][2]string {
	a := make([][2]string, 0, len(bind))
	for k, v := range bind {
		a = append(a, [2]string{k.Name, v.String()})
	}
	sort.Slice(a, func(i, j int) bool { return a[i][0] < a[j][0] })
	return a
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

func parseTestPattern(t *testing.T, m *Mod, src string) (typePattern, error) {
	var parms []*TypeParm
	parmSet := make(map[string]*TypeParm)
	nextName := 'A'
	src2 := ""
	var prev rune
	for len(src) > 0 {
		r, w := utf8.DecodeRuneInString(src)
		src = src[w:]
		if r != '_' || unicode.IsLetter(prev) || unicode.IsNumber(prev) || prev == '_' {
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
		return typePattern{}, err
	}
	typ, errs := _makeType(&typeParmScope{mod: m, parms: parms}, p, true, true)
	if len(errs) > 0 {
		return typePattern{}, errs[0]
	}
	return typePattern{parms: parms, typ: typ}, nil
}

func parseTestType(t *testing.T, m *Mod, src string) (Type, error) {
	p, err := parser.ParseType(src)
	if err != nil {
		return nil, err
	}
	typ, errs := _makeType(m, p, true, true)
	if len(errs) > 0 {
		return nil, errs[0]
	}
	return typ, nil
}
