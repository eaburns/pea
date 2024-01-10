package checker

import (
	"reflect"
	"testing"
)

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
		type (X, Y) pair_ref &[.x X, .y Y]
		type T abc_union_ref &[a?, b? T, c?]

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
		{src: "[int]", dst: "? pointer", want: c("[int]", Ref, "[int] pointer")},

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
		// This test is not exhaustive, but covers several source types exhaustively
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
		{src: "(int){[false?, true?]} t", dst: "(int){[false?, true?]}", explicit: true, want: c("(int){[false?, true?]} t", Noop, "(int){[false?, true?]}")},
		{src: "(int){[false?, true?]}", dst: "(int){[false?, true?]} t", explicit: true, want: c("(int){[false?, true?]}", Noop, "(int){[false?, true?]} t")},
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

		// Func conversion
		{src: "(int){int}", dst: "(int){float32}", want: nil},
		{src: "(int){int}", dst: "(float32){int}", want: nil},
		// Convert to [.] return type.
		{src: "(int){int}", dst: "(int){}", want: c("(int){int}", funcConvert, "(int){}")},
		{src: "(int){(int){}}", dst: "(int){}", want: c("(int){(int){}}", funcConvert, "(int){}")},
		{src: "(int32){int}", dst: "(int){}", want: nil},
		// Convert from ! return type.
		{src: "(int){!}", dst: "(int){}", want: c("(int){!}", funcConvert, "(int){}")},
		{src: "(int){!}", dst: "(int){int}", want: c("(int){!}", funcConvert, "(int){int}")},
		{src: "(int){!}", dst: "(int){[int]}", want: c("(int){!}", funcConvert, "(int){[int]}")},
		{src: "(float32){!}", dst: "(int){int}", want: nil},
		{src: "(int){!}", dst: "(int){?}", want: c("(int){!}", Noop, "(int){!}")},
		// Convert 0-ary function.
		{src: "(){}", dst: "(int){}", want: c("(){}", funcConvert, "(int){}")},
		{src: "(){}", dst: "(int, string){}", want: c("(){}", funcConvert, "(int, string){}")},
		{src: "(){int}", dst: "(int){int}", want: c("(){int}", funcConvert, "(int){int}")},
		{src: "(){int}", dst: "(int){?}", want: c("(){int}", funcConvert, "(int){int}")},

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
		{src: "int", dst: "?", want: c("int", Noop, "int")},
		{src: "int_val", dst: "?", want: c("int_val", Noop, "int_val")},
		{src: "&int", dst: "?", want: c("&int", Noop, "&int")},
		{src: "T", dst: "?", want: c("T", Noop, "T")},
		{src: "[int]", dst: "?", want: c("[int]", Noop, "[int]")},
		{src: "[.x int]", dst: "?", want: c("[.x int]", Noop, "[.x int]")},
		{src: "[x? int]", dst: "?", want: c("[x? int]", Noop, "[x? int]")},
		{src: "(int){float64}", dst: "?", want: c("(int){float64}", Noop, "(int){float64}")},
		{src: "&int", dst: "&?", want: c("&int", Noop, "&int")},
		{src: "int", dst: "&?", want: c("int", Ref, "&int")},
		{src: "[.x int]", dst: "[.x ?]", want: c("[.x int]", Noop, "[.x int]")},
		{src: "(int, string) pair", dst: "[.x ?1, .y ?2]", want: c("(int, string) pair", Noop, "[.x int, .y string]")},
		{src: "[.x int, .y string]", dst: "(?1, ?2) pair", want: c("[.x int, .y string]", Noop, "(int, string) pair")},
		{src: "(int, string) pair", dst: "(?1, ?2) pair2", explicit: true, want: c("(int, string) pair", Noop, "(int, string) pair2")},
		{src: "[.x int, .y ?1]", dst: "[.x ?2, .y string]", want: c("[.x int, .y ?1]", Noop, "[.x int, .y string]")},

		// Some obvious failing conversions.
		{src: "[.x int]", dst: "int_val", explicit: true, want: nil},
		{src: "int_val", dst: "[.x int]", explicit: true, want: nil},
		{src: "[.x int]", dst: "[x? int]", explicit: true, want: nil},
		{src: "(){}", dst: "[int]", explicit: true, want: nil},

		// It is not an error if the conversion does not fully ground the resulting type.
		{src: "?1", dst: "?2", explicit: false, want: c("?1", Noop, "?")},
		{src: "?1", dst: "?2", explicit: true, want: c("?1", Noop, "?")},
		{src: "[.x ?1, .y ?2]", dst: "[.x int, .y ?3]", want: c("[.x ?1, .y ?2]", Noop, "[.x int, .y ?]")},

		{
			src:  "[.x int, .y int]",
			dst:  "(int, int) pair_ref",
			want: c("[.x int, .y int]", Ref, "(int, int) pair_ref"),
		},

		// This doesn't convert, because [b? int] is not a subset of [a?, b? T, c?].
		// This is because int != T.
		{
			src: "[b? int]",
			// type T abc_union_ref &[a?, b? T, c?]
			dst:      "? abc_union_ref",
			explicit: true,
			want:     nil,
		},
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
			src := parseTestPattern(t, mod, test.src)
			dst := parseTestPattern(t, mod, test.dst)
			var bind map[*TypeParm]Type
			mode := implicit
			if test.explicit {
				mode = explicit
			}
			pat, cvt, _ := convertPattern(nil, src, dst, mode, &bind)
			if cvt == nil {
				if len(test.want) != 0 {
					t.Fatalf("convert(%s, %s, %v)=nil, want %v",
						test.src, test.dst, test.explicit, test.want)
				}
				return // ok
			}
			if !eqType(pat.Type, cvt.Type()) {
				t.Errorf("convert(%s, %s, %v) pat.cvt=%s != cvt.Type()=%s",
					test.src, test.dst, test.explicit, pat.Type, cvt.Type())
			}
			got := []interface{}{src.Type.String()}
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
