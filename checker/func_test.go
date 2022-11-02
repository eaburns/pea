package checker

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestSelectParm0(t *testing.T) {
	sel := checkTestMod("").findIDs(".x")[0].(*Select)
	want := typePattern{
		parms: sel.typeParms,
		typ: &RefType{
			Type: &TypeVar{
				Name: sel.typeParms[0].Name,
				Def:  sel.typeParms[0],
			},
		},
	}
	got := sel.parm(0)
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%T, expected %s: %s", got, want, diff)
	}
}

func TestSelectSubMatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "[.x int]")

	note := sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	parm0Want := typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: typ},
	}
	parm0Got := sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%s, expected %s: %s", parm0Got, parm0Want, diff)
	}

	retWant := typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: &BasicType{Kind: Int}},
	}
	retGot := sel.ret()
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	if diff := cmp.Diff(sel.Struct, typ, diffOpts...); diff != "" {
		t.Fatalf("sel.Struct=%s, expected %s: %s", sel.Struct, typ, diff)
	}

	if len(sel.typeParms) != 0 {
		t.Fatalf("len(sel.typeParms)=%d, expected 0", len(sel.typeParms))
	}
}

func TestSelectSubNonStructParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "int")

	note := sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note == nil {
		t.Fatalf("sub=nil, want error")
	}
}

func TestSelectSubStructWithWrongFieldsParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "[.y int]")

	note := sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note == nil {
		t.Fatalf("sub=nil, want error")
	}
}

// TestSelectRetBeforeSub tests the behavior of calling ret() before parm() and sub().
func TestSelectRetBeforeParmAndSub(t *testing.T) {
	sel := checkTestMod("").findIDs(".x")[0].(*Select)
	want := typePattern{
		parms: sel.typeParms,
		typ: &RefType{
			Type: &TypeVar{
				Name: sel.typeParms[1].Name,
				Def:  sel.typeParms[1],
			},
		},
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%T, expected %s: %s", got, want, diff)
	}
}

func TestSelectSubRetBeforeParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "int")

	note := sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	//
	// Parm0 is still a reference to a type variable.
	//
	parm0Want := typePattern{
		parms: sel.typeParms,
		typ: &RefType{
			Type: &TypeVar{
				Name: sel.typeParms[0].Name,
				Def:  sel.typeParms[0],
			},
		},
	}
	parm0Got := sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%T, expected %s: %s", parm0Got, parm0Want, diff)
	}

	//
	// But the return is the bound int type.
	//
	retWant := typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: &BasicType{Kind: Int}},
	}
	retGot := sel.ret()
	if diff := cmp.Diff(retWant, retGot, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	//
	// Now we can successfully substitute parm 0, as above.
	//

	typ = parseTestType(t, mod, "[.x int]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	parm0Want = typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: typ},
	}
	parm0Got = sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%s, expected %s: %s", parm0Got, parm0Want, diff)
	}

	retWant = typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: &BasicType{Kind: Int}},
	}
	retGot = sel.ret()
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	if diff := cmp.Diff(sel.Struct, typ, diffOpts...); diff != "" {
		t.Fatalf("sel.Struct=%s, expected %s: %s", sel.Struct, typ, diff)
	}

	if len(sel.typeParms) != 0 {
		t.Fatalf("len(sel.typeParms)=%d, expected 0", len(sel.typeParms))
	}
}

// Tests that it is possible to first substitute the return type
// with a type pattern that adds a new type parameter to the Func.
// Then we can later substitute the 0th arg with a compatible struct.
func TestSelectSubTypeParmRetBeforeParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)

	anyPat := any()
	retPat := sel.ret()
	retTypeParm := sel.typeParms[1]
	var bind map[*TypeParm]Type
	isect, note := intersection(anyPat, retPat, &bind)
	if note != nil {
		t.Fatalf("intersect(%s, %s)=%s, want nil", anyPat, retPat, note)
	}
	if bind == nil {
		t.Fatalf("bind is nil")
	}
	if note = sel.sub(bind); note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}
	sel.typeParms = append(sel.typeParms, isect.parms...)

	//
	// Parm0 is still a reference to a type variable.
	//
	parm0Want := typePattern{
		parms: sel.typeParms,
		typ: &RefType{
			Type: &TypeVar{
				Name: sel.typeParms[0].Name,
				Def:  sel.typeParms[0],
			},
		},
	}
	parm0Got := sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%T, expected %s: %s", parm0Got, parm0Want, diff)
	}

	//
	// But the return is the bound int type.
	//
	retWant := typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: bind[retTypeParm]},
	}
	retGot := sel.ret()
	if diff := cmp.Diff(retWant, retGot, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	//
	// Now we can successfully substitute parm 0, as above.
	//

	typ := parseTestType(t, mod, "[.x int]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	parm0Want = typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: typ},
	}
	parm0Got = sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%s, expected %s: %s", parm0Got, parm0Want, diff)
	}

	retWant = typePattern{
		parms: sel.typeParms,
		typ:   &RefType{Type: &BasicType{Kind: Int}},
	}
	retGot = sel.ret()
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	if diff := cmp.Diff(sel.Struct, typ, diffOpts...); diff != "" {
		t.Fatalf("sel.Struct=%s, expected %s: %s", sel.Struct, typ, diff)
	}

	if len(sel.typeParms) != 0 {
		t.Fatalf("len(sel.typeParms)=%d, expected 0", len(sel.typeParms))
	}
}

// First substitute the return, then parm0, but the parm0 type
// would lead to a return that mis-matches the first subbed return type.
// This is an error.
func TestSelectSubRetBeforeMismatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "int")

	note := sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Substitute parm0 with a type that doesn't match the expected return, &int.

	typ = parseTestType(t, mod, "[.x string]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note == nil {
		t.Fatalf("sub=nil, want failed conversion error")
	}
}
