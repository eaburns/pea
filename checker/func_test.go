package checker

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestSelectParm0(t *testing.T) {
	sel := checkTestMod("").findIDs(".x")[0].(*Select)
	want := makeTypePattern(
		sel.typeParms,
		&RefType{
			Type: &TypeVar{Def: selectParm0TypeParm(sel)},
		})
	got := sel.parm(0)
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%T, expected %s: %s", got, want, diff)
	}
}

func TestSelectSubMatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "[.x int]")

	sel, subErr := subSelect(sel, nil, map[*TypeParm]Type{
		selectParm0TypeParm(sel): typ,
	})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	parm0Want := makeTypePattern(sel.typeParms, &RefType{Type: typ})
	parm0Got := sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%s, expected %s: %s", parm0Got, parm0Want, diff)
	}

	retWant := makeTypePattern(
		sel.typeParms,
		&RefType{Type: &BasicType{Kind: Int}})
	retGot := sel.ret()
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	if diff := cmp.Diff(sel.Struct, typ, diffOpts...); diff != "" {
		t.Fatalf("sel.Struct=%s, expected %s: %s", sel.Struct, typ, diff)
	}
}

func TestSelectSubNonStructParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "int")

	sel, subErr := subSelect(sel, nil, map[*TypeParm]Type{
		selectParm0TypeParm(sel): typ,
	})
	if subErr == nil {
		t.Fatalf("sub=nil, want error")
	}
}

func TestSelectSubStructWithWrongFieldsParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "[.y int]")

	sel, subErr := subSelect(sel, nil, map[*TypeParm]Type{
		selectParm0TypeParm(sel): typ,
	})
	if subErr == nil {
		t.Fatalf("sub=nil, want error")
	}
}

// TestSelectRetBeforeSub tests the behavior of calling ret() before parm() and sub().
func TestSelectRetBeforeParmAndSub(t *testing.T) {
	sel := checkTestMod("").findIDs(".x")[0].(*Select)
	want := makeTypePattern(
		sel.typeParms,
		&RefType{
			Type: &TypeVar{
				Def: selectRetTypeParm(sel),
			},
		})
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%T, expected %s: %s", got, want, diff)
	}
}

func TestSelectSubRetBeforeParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "int")

	sel, subErr := subSelect(sel, nil, map[*TypeParm]Type{
		selectRetTypeParm(sel): typ,
	})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	//
	// Parm0 is still a reference to a type variable.
	//
	parm0Want := makeTypePattern(
		sel.typeParms,
		&RefType{
			Type: &TypeVar{Def: selectParm0TypeParm(sel)},
		})
	parm0Got := sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%T, expected %s: %s", parm0Got, parm0Want, diff)
	}

	//
	// But the return is the bound int type.
	//
	retWant := makeTypePattern(
		sel.typeParms,
		&RefType{Type: &BasicType{Kind: Int}})
	retGot := sel.ret()
	if diff := cmp.Diff(retWant, retGot, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	//
	// Now we can successfully substitute parm 0, as above.
	//

	typ = parseTestType(t, mod, "[.x int]")
	sel, subErr = subSelect(sel, nil, map[*TypeParm]Type{
		selectParm0TypeParm(sel): typ,
	})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	parm0Want = makeTypePattern(
		sel.typeParms,
		&RefType{Type: typ})
	parm0Got = sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%s, expected %s: %s", parm0Got, parm0Want, diff)
	}

	retWant = makeTypePattern(
		sel.typeParms,
		&RefType{Type: &BasicType{Kind: Int}})
	retGot = sel.ret()
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	if diff := cmp.Diff(sel.Struct, typ, diffOpts...); diff != "" {
		t.Fatalf("sel.Struct=%s, expected %s: %s", sel.Struct, typ, diff)
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
	retTypeParm := selectRetTypeParm(sel)
	var bind map[*TypeParm]Type
	u, note := unify(anyPat, retPat, &bind)
	if note != nil {
		t.Fatalf("unify(%s, %s)=%s, want nil", anyPat, retPat, note)
	}
	if bind == nil {
		t.Fatalf("bind is nil")
	}
	sel, subErr := subSelect(sel, u.Parms, bind)
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	//
	// Parm0 is still a reference to a type variable.
	//
	parm0Want := makeTypePattern(
		sel.typeParms,
		&RefType{
			Type: &TypeVar{
				Def: selectParm0TypeParm(sel),
			},
		})
	parm0Got := sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%T, expected %s: %s", parm0Got, parm0Want, diff)
	}

	//
	// But the return is the bound int type.
	//
	retWant := makeTypePattern(
		sel.typeParms,
		&RefType{Type: bind[retTypeParm]})
	retGot := sel.ret()
	if diff := cmp.Diff(retWant, retGot, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	//
	// Now we can successfully substitute parm 0, as above.
	//

	typ := parseTestType(t, mod, "[.x int]")
	sel, subErr = subSelect(sel, nil, map[*TypeParm]Type{selectParm0TypeParm(sel): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	parm0Want = makeTypePattern(
		sel.typeParms,
		&RefType{Type: typ})
	parm0Got = sel.parm(0)
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("parm(0)=%s, expected %s: %s", parm0Got, parm0Want, diff)
	}

	retWant = makeTypePattern(
		sel.typeParms,
		&RefType{Type: &BasicType{Kind: Int}})
	retGot = sel.ret()
	if diff := cmp.Diff(parm0Want, parm0Got, diffOpts...); diff != "" {
		t.Fatalf("ret=%s, expected %s: %s", retGot, retWant, diff)
	}

	if diff := cmp.Diff(sel.Struct, typ, diffOpts...); diff != "" {
		t.Fatalf("sel.Struct=%s, expected %s: %s", sel.Struct, typ, diff)
	}
}

// First substitute the return, then parm0, but the parm0 type
// would lead to a return that mis-matches the first subbed return type.
// This is OK. Just set the return type.
// The convert error should be reported by the parent expression.
func TestSelectSubRetBeforeMismatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs(".x")[0].(*Select)
	typ := parseTestType(t, mod, "int")

	sel, subErr := subSelect(sel, nil, map[*TypeParm]Type{selectRetTypeParm(sel): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Substitute parm0 with a type that doesn't match the expected return, &int.

	typ = parseTestType(t, mod, "[.x string]")
	sel, subErr = subSelect(sel, nil, map[*TypeParm]Type{selectParm0TypeParm(sel): typ})
	if subErr != nil {
		t.Fatalf("%s=nil, want nil", subErr)
	}
}

func TestSwitchRet(t *testing.T) {
	sel := checkTestMod("").findIDs("x?y?z?")[0].(*Switch)
	want := makeTypePattern(
		sel.typeParms,
		typeVar(switchRetTypeParm(sel)))
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchParmsAndRetWithoutSub(t *testing.T) {
	mod := checkTestMod("")
	sel := mod.findIDs("x?y?z?")[0].(*Switch)

	wantParms := []Type{
		typeVar(switchParmTypeParm(sel, 0)),
		typeVar(switchParmTypeParm(sel, 1)),
		typeVar(switchParmTypeParm(sel, 2)),
		typeVar(switchParmTypeParm(sel, 3)),
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sel.typeParms, wantType)
		got := sel.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := makeTypePattern(sel.typeParms, typeVar(switchRetTypeParm(sel)))
	got := sel.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubMatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "[x?, y? int, z?]")

	// We are substituting typeParms[1], which is the union argument.
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{
			Parms: []Type{_empty},
			Ret:   typeVar(switchRetTypeParm(sw)),
		},
		&FuncType{
			Parms: []Type{_int},
			Ret:   typeVar(switchRetTypeParm(sw)),
		},
		&FuncType{
			Parms: []Type{_empty},
			Ret:   typeVar(switchRetTypeParm(sw)),
		},
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := makeTypePattern(sw.typeParms, typeVar(switchRetTypeParm(sw)))
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubMatchingParm0IncompleteSwitch(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("if:x:y:z:")[0].(*Switch)
	typ := parseTestType(t, mod, "[x?, y? int, z?, a?, b?, c?]")

	// We are substituting typeParms[1], which is the union argument.
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{Parms: []Type{_empty}, Ret: _empty},
		&FuncType{Parms: []Type{_int}, Ret: _empty},
		&FuncType{Parms: []Type{_empty}, Ret: _empty},
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := makeTypePattern(sw.typeParms, _empty)
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubParm1First(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "(){int}")

	// Parm1 is typeParms[2]
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 1): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	wantParms := []Type{
		typeVar(switchParmTypeParm(sw, 0)),
		// For now, Parms are nil, since we substituted (){int}.
		// But late when substituting arg 0 (the union),
		// this will be replaced by ([.]){int}.
		&FuncType{/* Parms: nil */Ret: _int},
		typeVar(switchParmTypeParm(sw, 2)),
		typeVar(switchParmTypeParm(sw, 3)),
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	// We do not yet substitute the return type.
	// We don't yet know whether this is a complete switch.
	// We only know that when we sub parm0.
	want := makeTypePattern(sw.typeParms, typeVar(switchRetTypeParm(sw)))
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubRetFirst(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)
	typ := parseTestType(t, mod, "string")

	// Ret is typeParms[0]
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchRetTypeParm(sw): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	wantParms := []Type{
		typeVar(switchParmTypeParm(sw, 0)),
		typeVar(switchParmTypeParm(sw, 1)),
		typeVar(switchParmTypeParm(sw, 2)),
		typeVar(switchParmTypeParm(sw, 3)),
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	// We do not yet substitute the return type.
	// We don't yet know whether this is a complete switch.
	// We only know that when we sub parm0.
	want := makeTypePattern(sw.typeParms, _string)
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubRetThenParm0(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)

	// Ret is typeParms[0]
	typ := parseTestType(t, mod, "string")
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchRetTypeParm(sw): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? int, z?]")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{Parms: []Type{_empty}, Ret: _string},
		&FuncType{Parms: []Type{_int}, Ret: _string},
		&FuncType{Parms: []Type{_empty}, Ret: _string},
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := makeTypePattern(sw.typeParms, _string)
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubTypeParmRetThenParm0ThenRet(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)

	anyPat := any()
	retPat := sw.ret()
	retTypeParm := switchRetTypeParm(sw)
	var bind map[*TypeParm]Type
	u, note := unify(anyPat, retPat, &bind)
	if note != nil {
		t.Fatalf("unify(%s, %s)=%s, want nil", anyPat, retPat, note)
	}
	if bind == nil {
		t.Fatalf("bind is nil")
	}
	sw, subErr := subSwitch(sw, u.Parms, bind)
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}
	wantParms := []Type{
		typeVar(switchParmTypeParm(sw, 0)),
		typeVar(switchParmTypeParm(sw, 1)),
		typeVar(switchParmTypeParm(sw, 2)),
		typeVar(switchParmTypeParm(sw, 3)),
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}
	want := makeTypePattern(sw.typeParms, bind[retTypeParm])
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}

	// Parm0 is typeParms[1]
	unionType := parseTestType(t, mod, "[x?, y? int, z?]")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): unionType})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}
	wantParms = []Type{
		&RefType{Type: unionType},
		&FuncType{Parms: []Type{_empty}, Ret: bind[retTypeParm]},
		&FuncType{Parms: []Type{_int}, Ret: bind[retTypeParm]},
		&FuncType{Parms: []Type{_empty}, Ret: bind[retTypeParm]},
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}
	want = makeTypePattern(sw.typeParms, bind[retTypeParm])
	got = sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}

	// Ret is now bind[retTypeParm].(*TypeVar).Def
	typ := parseTestType(t, mod, "string")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{bind[retTypeParm].(*TypeVar).Def: typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}
	wantParms = []Type{
		&RefType{Type: unionType},
		&FuncType{Parms: []Type{_empty}, Ret: _string},
		&FuncType{Parms: []Type{_int}, Ret: _string},
		&FuncType{Parms: []Type{_empty}, Ret: _string},
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}
	want = makeTypePattern(sw.typeParms, _string)
	got = sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubParm2ThenMatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)

	// Parm2 is typeParms[3]
	typ := parseTestType(t, mod, "(int){string}")
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 2): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? int, z?]")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	wantParms := []Type{
		&RefType{Type: typ},
		&FuncType{Parms: []Type{_empty}, Ret: _string},
		&FuncType{Parms: []Type{_int}, Ret: _string},
		&FuncType{Parms: []Type{_empty}, Ret: _string},
	}
	for i, wantType := range wantParms {
		want := makeTypePattern(sw.typeParms, wantType)
		got := sw.parm(i)
		if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
			t.Fatalf("parm(%d)=%s, expected %s: %s", i, got, want, diff)
		}
	}

	want := makeTypePattern(sw.typeParms, _string)
	got := sw.ret()
	if diff := cmp.Diff(want, got, diffOpts...); diff != "" {
		t.Fatalf("ret()=%s, expected %s: %s", got, want, diff)
	}
}

func TestSwitchSubParm2ThenParameterMismatchingParm0(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)

	// Parm2 is typeParms[3]
	typ := parseTestType(t, mod, "(int){string}")
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 2): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? float64, z?]")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr == nil {
		t.Error("sub=nil, want error", subErr)
	}
}

func TestSwitchSubNonFunctionParm2ThenParm0(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)

	// Parm2 is typeParms[3]
	typ := parseTestType(t, mod, "int")
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 2): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? float64, z?]")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr == nil {
		t.Error("sub=nil, want error", subErr)
	}
}

func TestSwitchSubRetThenMismatchingParm2ThenParm0(t *testing.T) {
	mod := checkTestMod("")
	sw := mod.findIDs("x?y?z?")[0].(*Switch)

	// Ret is typeParms[0]
	typ := parseTestType(t, mod, "float64")
	sw, subErr := subSwitch(sw, nil, map[*TypeParm]Type{switchRetTypeParm(sw): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Parm2 is typeParms[3]
	typ = parseTestType(t, mod, "(int){string}")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 2): typ})
	if subErr != nil {
		t.Fatalf("sub=%s, want nil", subErr)
	}

	// Parm0 is typeParms[1]
	typ = parseTestType(t, mod, "[x?, y? int, z?]")
	sw, subErr = subSwitch(sw, nil, map[*TypeParm]Type{switchParmTypeParm(sw, 0): typ})
	if subErr == nil {
		t.Error("sub=nil, want error", subErr)
	}
}

func subSelect(sel *Select, parms *TypeParmSet, bind map[*TypeParm]Type) (*Select, *CandidateError) {
	f, err := sel.sub(parms, bind)
	return f.(*Select), err
}

func selectParm0TypeParm(f *Select) *TypeParm {
	return f.T.Parms[0].(*RefType).Type.(*TypeVar).Def
}

func selectRetTypeParm(f *Select) *TypeParm {
	return f.T.Ret.(*RefType).Type.(*TypeVar).Def
}

func subSwitch(sw *Switch, parms *TypeParmSet, bind map[*TypeParm]Type) (*Switch, *CandidateError) {
	f, err := sw.sub(parms, bind)
	return f.(*Switch), err
}

func switchParmTypeParm(f *Switch, i int) *TypeParm {
	return f.T.Parms[i].(*TypeVar).Def
}

func switchRetTypeParm(f *Switch) *TypeParm {
	return f.T.Ret.(*TypeVar).Def
}

func typeVar(parm *TypeParm) *TypeVar {
	return &TypeVar{Def: parm}
}
