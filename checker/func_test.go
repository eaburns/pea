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

func TestSwitchRet(t *testing.T) {
	sel := checkTestMod("").findIDs("x?y?z?")[0].(*Switch)
	want := typePattern{
		parms: sel.typeParms,
		typ:   typeVar(sel.typeParms[0]),
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchParmsAndRetWithoutSub(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	wantParms := []Type{
		// typeParms[0] is the return type.
		// Parameters begin at typeParms[1:].
		typeVar(sel.typeParms[1]),
		typeVar(sel.typeParms[2]),
		typeVar(sel.typeParms[3]),
		typeVar(sel.typeParms[4]),
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := typePattern{
		parms: sel.typeParms,
		typ:   typeVar(sel.typeParms[0]),
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubMatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "[x?, y? int, z?]")

	// We are substituting typeParms[1], which is the union argument.
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{
			Ret: typeVar(sel.typeParms[0]),
		},
		&FuncType{
			Parms: []Type{_int},
			Ret:   typeVar(sel.typeParms[0]),
		},
		&FuncType{
			Ret: typeVar(sel.typeParms[0]),
		},
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := typePattern{
		parms: sel.typeParms,
		typ:   typeVar(sel.typeParms[0]),
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubMatchingParm0IncompleteSwitch(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "[x?, y? int, z?, a?, b?, c?]")

	// We are substituting typeParms[1], which is the union argument.
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{Ret: _empty},
		&FuncType{Parms: []Type{_int}, Ret: _empty},
		&FuncType{Ret: _empty},
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := typePattern{
		parms: sel.typeParms,
		typ:   _empty,
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubParm1First(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "(){int}")

	// Parm1 is typeParms[2]
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[2]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	wantParms := []Type{
		typeVar(sel.typeParms[1]),
		&FuncType{Ret: _int},
		typeVar(sel.typeParms[3]),
		typeVar(sel.typeParms[4]),
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	// We do not yet substitute the return type.
	// We don't yet know whether this is a complete switch.
	// We only know that when we sub parm0.
	want := typePattern{
		parms: sel.typeParms,
		typ:   typeVar(sel.typeParms[0]),
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubRetFirst(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "string")

	// Ret is typeParms[0]
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	wantParms := []Type{
		typeVar(sel.typeParms[1]),
		typeVar(sel.typeParms[2]),
		typeVar(sel.typeParms[3]),
		typeVar(sel.typeParms[4]),
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	// We do not yet substitute the return type.
	// We don't yet know whether this is a complete switch.
	// We only know that when we sub parm0.
	want := typePattern{
		parms: sel.typeParms,
		typ:   _string,
	}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubRetThenParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	// Ret is typeParms[0]
	typ := parseTestType(t, mod, "string")
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? int, z?]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{Ret: _string},
		&FuncType{Parms: []Type{_int}, Ret: _string},
		&FuncType{Ret: _string},
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := typePattern{parms: sel.typeParms, typ: _string}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubTypeParmRetThenParm0ThenRet(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	anyPat := any()
	retPat := sel.ret()
	retTypeParm := sel.typeParms[0]
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
	wantParms := []Type{
		typeVar(sel.typeParms[1]),
		typeVar(sel.typeParms[2]),
		typeVar(sel.typeParms[3]),
		typeVar(sel.typeParms[4]),
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}
	want := typePattern{parms: sel.typeParms, typ: bind[retTypeParm]}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}

	// Parm0 is typeParms[1]
	unionType := parseTestType(t, mod, "[x?, y? int, z?]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[1]: unionType})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}
	wantParms = []Type{
		&RefType{Type: unionType},
		&FuncType{Ret: bind[retTypeParm]},
		&FuncType{Parms: []Type{_int}, Ret: bind[retTypeParm]},
		&FuncType{Ret: bind[retTypeParm]},
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}
	want = typePattern{parms: sel.typeParms, typ: bind[retTypeParm]}
	got = sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}

	// Ret is now bind[retTypeParm].(*TypeVar).Def
	typ := parseTestType(t, mod, "string")
	note = sel.sub(map[*TypeParm]Type{bind[retTypeParm].(*TypeVar).Def: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}
	wantParms = []Type{
		&RefType{Type: unionType},
		&FuncType{Ret: _string},
		&FuncType{Parms: []Type{_int}, Ret: _string},
		&FuncType{Ret: _string},
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}
	want = typePattern{parms: sel.typeParms, typ: _string}
	got = sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubParm2ThenMatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	// Parm2 is typeParms[3]
	typ := parseTestType(t, mod, "(int){string}")
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[3]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? int, z?]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{Ret: _string},
		&FuncType{Parms: []Type{_int}, Ret: _string},
		&FuncType{Ret: _string},
	}
	for i, wantType := range wantParms {
		want := typePattern{parms: sel.typeParms, typ: wantType}
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := typePattern{parms: sel.typeParms, typ: _string}
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubParm2ThenParameterMismatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	// Parm2 is typeParms[3]
	typ := parseTestType(t, mod, "(int){string}")
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[3]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? float64, z?]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note == nil {
		t.Error("sub=nil, want error", note)
	}
}

func TestSwitchSubNonFunctionParm2ThenParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	// Parm2 is typeParms[3]
	typ := parseTestType(t, mod, "int")
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[3]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? float64, z?]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note == nil {
		t.Error("sub=nil, want error", note)
	}
}

func TestSwitchSubRetThenMismatchingParm2ThenParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	// Ret is typeParms[0]
	typ := parseTestType(t, mod, "float64")
	note := sel.sub(map[*TypeParm]Type{sel.typeParms[0]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Parm2 is typeParms[3]
	typ = parseTestType(t, mod, "(int){string}")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[3]: typ})
	if note != nil {
		t.Fatalf("sub=%s, want nil", note)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? int, z?]")
	note = sel.sub(map[*TypeParm]Type{sel.typeParms[1]: typ})
	if note == nil {
		t.Error("sub=nil, want error", note)
	}
}

func typeVar(parm *TypeParm) *TypeVar {
	return &TypeVar{Name: parm.Name, Def: parm}
}
