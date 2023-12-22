package checker

import (
	"fmt"
	"sort"
	"strconv"

	"github.com/eaburns/pea/loc"
)

// A TypePattern describes a set of types matching the pattern,
// it consists of a set of type parameters, and a type.
// The type may contain type variables bound to the parameters.
// The pattern represents all types that are the result of
// substituting types for the type parameters.
type TypePattern struct {
	Parms *TypeParmSet
	Type  Type
}

func makeTypePattern(parms *TypeParmSet, typ Type) TypePattern {
	return TypePattern{Parms: parms, Type: typ}
}

// any returns a new TypePattern with a single, bound type variable.
func any() TypePattern {
	n := "_"
	p := &TypeParm{Name: n}
	v := &TypeVar{Def: p}
	return TypePattern{Parms: NewTypeParmSet(p), Type: v}
}

// pattern returns the type pattern for a ground type.
// pattern panics if typ is nil.
func pattern(typ Type) TypePattern {
	if typ == nil {
		panic("impossible")
	}
	return TypePattern{Type: typ}
}

// patternOrAny returns the pattern for a ground type, or if typ is nil, any().
func patternOrAny(typ Type) TypePattern {
	if typ == nil {
		return any()
	}
	return pattern(typ)
}

func (pat TypePattern) withType(typ Type) TypePattern {
	if typ == nil {
		panic("impossible")
	}
	pat.Type = typ
	return pat
}

// instType returns the type in the definition of a defined type type pattern,
// substituted with the type arguments of the defined type.
// instType panics if pat is not a defined type type pattern.
func (pat TypePattern) instType() TypePattern {
	if pat.Type.(*DefType).Inst.Type == nil {
		// There was an error figuring out the DefType's definition type.
		// Just use the any() pattern, and the error will be reported elsewhere.
		return any()
	}
	return pat.withType(pat.Type.(*DefType).Inst.Type)
}

// typeArg returns the type argument type of a defined type type pattern;
// panics if pat is not a defined type type pattern.
func (pat TypePattern) typeArg(i int) TypePattern {
	return pat.withType(pat.Type.(*DefType).Args[i])
}

// refElem returns the element type of a literal reference type pattern;
// panics if pat is not a literal reference type pattern.
func (pat TypePattern) refElem() TypePattern {
	return pat.withType(pat.Type.(*RefType).Type)
}

// arrayElem returns the array element type of a literal array type pattern;
// panics if pat is not a literal array type pattern.
func (pat TypePattern) arrayElem() TypePattern {
	return pat.withType(pat.Type.(*ArrayType).ElemType)
}

// field returns the ith struct field type of a literal struct type pattern;
// panics if pat is not a literal struct type pattern.
func (pat TypePattern) field(i int) TypePattern {
	return pat.withType(pat.Type.(*StructType).Fields[i].Type)
}

// Case returns the ith union case type of a literal union type pattern;
// panics if pat is not a literal union type pattern.
// panics if the ith field is untyped.
func (pat TypePattern) Case(i int) TypePattern {
	return pat.withType(pat.Type.(*UnionType).Cases[i].Type)
}

// parm returns the ith function parm type of a literal function type pattern;
// panics if pat is not a literal function type pattern.
func (pat TypePattern) parm(i int) TypePattern {
	return pat.withType(pat.Type.(*FuncType).Parms[i])
}

// ret returns the return type of a literal function type pattern;
// panics if pat is not a literal function type pattern.
func (pat TypePattern) ret() TypePattern {
	if pat.Type.(*FuncType).Ret == nil {
		return any()
	}
	return pat.withType(pat.Type.(*FuncType).Ret)
}

// groundType returns the ground type of the type pattern;
// it panics if either pat.typ is nil or not grounded.
func (pat TypePattern) groundType() Type {
	if pat.Type == nil {
		panic("type is nil")
	}
	if !pat.isGroundType() {
		panic(fmt.Sprintf("not grounded: %s", pat))
	}
	return pat.Type
}

// isGroundType returns whether the type pattern is a ground type;
// whether its type has no referenced type parameters.
func (pat TypePattern) isGroundType() bool {
	return !walkType(pat.Type, func(t Type) bool {
		tv, ok := t.(*TypeVar)
		return ok && pat.bound(tv)
		return false
	})
}

// bound returns whether the type variable is bound to a type parameter of the TypePattern.
func (pat *TypePattern) bound(v *TypeVar) bool {
	return pat.Parms.Contains(v.Def)
}

// common returns a new TypePattern that is the most specific simple common pattern of pats.
func common(pats ...TypePattern) TypePattern {
	switch len(pats) {
	case 0:
		return any()
	case 1:
		return pats[0]
	}

	var nextParm int
	var pat TypePattern

	newVar := func() *TypeVar {
		n := "_" + strconv.Itoa(nextParm)
		nextParm++
		p := &TypeParm{Name: n}
		pat.Parms = pat.Parms.Union(NewTypeParmSet(p))
		return &TypeVar{Def: p}
	}

	var buildType func(types []Type) Type
	buildType = func(types []Type) Type {
		switch t0 := types[0].(type) {
		case *DefType:
			for _, t := range types {
				if t, ok := t.(*DefType); !ok || t0.Def != t.Def {
					return newVar()
				}
			}
			d := &DefType{
				Name: t0.Name,
				Args: make([]Type, len(t0.Args)),
				Def:  t0.Def,
			}
			ts := make([]Type, len(types))
			for i := range t0.Args {
				for j, t := range types {
					ts[j] = t.(*DefType).Args[i]
				}
				d.Args[i] = buildType(ts)
			}
			return instType(d)
		case *RefType:
			for _, t := range types {
				if _, ok := t.(*RefType); !ok {
					return newVar()
				}
			}
			var r RefType
			ts := make([]Type, len(types))
			for i, t := range types {
				ts[i] = t.(*RefType).Type
			}
			r.Type = buildType(ts)
			return &r
		case *ArrayType:
			for _, t := range types {
				if _, ok := t.(*ArrayType); !ok {
					return newVar()
				}
			}
			var a ArrayType
			ts := make([]Type, len(types))
			for i, t := range types {
				ts[i] = t.(*ArrayType).ElemType
			}
			a.ElemType = buildType(ts)
			return &a
		case *StructType:
			for _, t := range types {
				t, ok := t.(*StructType)
				if !ok || len(t.Fields) != len(t0.Fields) {
					return newVar()
				}
				for i := range t.Fields {
					if t.Fields[i].Name != t0.Fields[i].Name {
						return newVar()
					}
				}
			}

			s := &StructType{Fields: make([]FieldDef, len(t0.Fields))}
			for i := range t0.Fields {
				s.Fields[i].Name = t0.Fields[i].Name
				ts := make([]Type, len(types))
				for j, t := range types {
					ts[j] = t.(*StructType).Fields[i].Type
				}
				s.Fields[i].Type = buildType(ts)
			}
			return s
		case *UnionType:
			for _, t := range types {
				t, ok := t.(*UnionType)
				if !ok || len(t.Cases) != len(t0.Cases) {
					return newVar()
				}
				for i := range t.Cases {
					if t.Cases[i].Name != t0.Cases[i].Name ||
						(t.Cases[i].Type == nil) != (t0.Cases[i].Type == nil) {
						return newVar()
					}
				}
			}
			u := &UnionType{Cases: make([]CaseDef, len(t0.Cases))}
			for i := range t0.Cases {
				u.Cases[i].Name = t0.Cases[i].Name
				if t0.Cases[i].Type == nil {
					u.Cases[i].Type = nil
					continue
				}
				ts := make([]Type, len(types))
				for j, t := range types {
					ts[j] = t.(*UnionType).Cases[i].Type
				}
				u.Cases[i].Type = buildType(ts)
			}
			return u
		case *FuncType:
			for _, t := range types {
				if t, ok := t.(*FuncType); !ok || len(t.Parms) != len(t0.Parms) {
					return newVar()
				}
			}
			var fun FuncType
			for i := range t0.Parms {
				ts := make([]Type, len(types))
				for j, t := range types {
					ts[j] = t.(*FuncType).Parms[i]
				}
				fun.Parms = append(fun.Parms, buildType(ts))
			}
			ts := make([]Type, len(types))
			for i, t := range types {
				ts[i] = t.(*FuncType).Ret
			}
			fun.Ret = buildType(ts)
			return &fun
		case *TypeVar:
			for i, t := range types {
				if t, ok := t.(*TypeVar); !ok || pats[i].bound(t) || t.Def != t0.Def {
					return newVar()
				}
			}
			return copyTypeWithLoc(t0, loc.Loc{})
		case *BasicType:
			for _, t := range types {
				if t, ok := t.(*BasicType); !ok || t.Kind != t0.Kind {
					return newVar()
				}
			}
			return copyTypeWithLoc(t0, loc.Loc{})
		default:
			panic(fmt.Sprintf("impossible type type %T", t0))
		}
	}

	ts := make([]Type, len(pats))
	for i, p := range pats {
		ts[i] = p.Type
	}
	pat.Type = buildType(ts)
	return pat
}

// isection returns the intersection of type patterns a and b,
// that share the same type parameters.
// The type patterns are specified by a shared list of TypeParms, tparms,
// and the two pattern's types, a and b.
// The returned type pattern is given by its separate parameters and type.
// The return parameters always begin with the input, tparms,
// with possibly additional parameters appended.
//
// If there is no intersection, the input type parameters are returned with a nil type.
//
// The intersection of [.] and any type is [.].
//
// The intersection of two literal ref patterns, is the literal ref pattern
// of the intersection of the element patterns.
//
// The intersection of two literal array patterns is the array
// of the intersection of the element patterns.
//
// The intersection of two literal union patterns where one is open and one is closed
// and all cases of the open pattern have a corresponding case in the closed pattern
// with the same name and typedness, is a closed literal union pattern
// with cases corresponding to the closed input literal union pattern,
// but with the type of each typed case that matches a case in the open pattern
// being the intersection of the two types.
//
// The intersection of two open literal union patterns with disjoint cases
// or cases that match in name and typedness, where the types intersect,
// is an open literal union pattern with the union of the disjoint cases,
// and the intersected types of the matching cases, with all cases sorted by name.
//
// The intersection of two literal struct patterns with the same fields in the same order
// is the literal struct type with the same fields with the type of eacd field being
// the intersection of the corresponding fields in the intersected patterns.
//
// The intersection of two literal function patterns with the same arity
// is a literal function pattern with parameter types that are the intersection
// of the corresponding parameter patterns and the return pattern
// that is the intersection of the two return patterns.
//
// The intersection of a literal type with any named type pattern
// is the named type, with any type arguments substituted
// based on the intersection of the literal type with the named pattern's
// definition type pattern.
//
// If the two type patterns unify, the intersection is the unification.
//
// Otherwise there is no intersection.
func isection(tparms []*TypeParm, a, b Type) ([]*TypeParm, Type) {
	var bind map[*TypeParm]Type
	return _isection(tparms, a, b, &bind)
}

func _isection(tparms []*TypeParm, a, b Type, bind *map[*TypeParm]Type) ([]*TypeParm, Type) {
	if isEmptyStruct(a) || isEmptyStruct(b) {
		return tparms, _empty
	}
	if tps, typ, ok := isectLiteralRefs(tparms, a, b); ok {
		return tps, typ
	}
	if tps, typ, ok := isectLiteralArrays(tparms, a, b); ok {
		return tps, typ
	}
	if tps, typ, ok := isectOpenUnions(tparms, a, b); ok {
		return tps, typ
	}
	if tps, typ, ok := isectOpenAndClosedUnions(tparms, a, b); ok {
		return tps, typ
	}
	if tps, typ, ok := isectLiteralStructs(tparms, a, b); ok {
		return tps, typ
	}
	if tps, typ, ok := isectLiteralFuncs(tparms, a, b); ok {
		return tps, typ
	}
	if tps, typ, ok := isectLiteralAndNamed(tparms, a, b); ok {
		return tps, typ
	}
	tparmSet := NewTypeParmSet(tparms...)
	pat, _ := unify(makeTypePattern(tparmSet, a), makeTypePattern(tparmSet, b), bind)
	if pat == nil {
		return tparms, nil
	}
	// TODO: use TypeParmSet consistently to get rid of need for slice.
	if pat.Parms != nil {
		tparms = append(tparms, pat.Parms.parmSlice...)
	}
	return tparms, pat.Type
}

func isectLiteralRefs(tparms []*TypeParm, _a, _b Type) ([]*TypeParm, Type, bool) {
	a, ok := _a.(*RefType)
	if !ok {
		return nil, nil, false
	}
	b, ok := _b.(*RefType)
	if !ok {
		return nil, nil, false
	}
	tparms, typ := isection(tparms, a.Type, b.Type)
	if typ == nil {
		return nil, nil, false
	}
	return tparms, &RefType{Type: typ, L: a.L}, true
}

func isectLiteralArrays(tparms []*TypeParm, _a, _b Type) ([]*TypeParm, Type, bool) {
	a, ok := _a.(*ArrayType)
	if !ok {
		return nil, nil, false
	}
	b, ok := _b.(*ArrayType)
	if !ok {
		return nil, nil, false
	}
	tparms, typ := isection(tparms, a.ElemType, b.ElemType)
	if typ == nil {
		return nil, nil, false
	}
	return tparms, &ArrayType{ElemType: typ, L: a.L}, true
}

func isectOpenUnions(tparms []*TypeParm, _a, _b Type) ([]*TypeParm, Type, bool) {
	a, ok := _a.(*UnionType)
	if !ok || !a.Open {
		return nil, nil, false
	}
	b, ok := _b.(*UnionType)
	if !ok || !b.Open {
		return nil, nil, false
	}
	var cases []*CaseDef
	seen := make(map[string]*CaseDef)
	for _, c := range a.Cases {
		copy := c
		seen[c.Name] = &copy
		cases = append(cases, &copy)
	}
	for _, c := range b.Cases {
		prev, seen := seen[c.Name]
		if !seen {
			copy := c
			cases = append(cases, &copy)
			continue
		}
		if (prev.Type == nil) != (c.Type == nil) {
			// Different typedness.
			return nil, nil, false
		}
		if prev.Type == nil {
			continue
		}
		tparms, prev.Type = isection(tparms, prev.Type, c.Type)
		if prev.Type == nil {
			return nil, nil, false
		}
	}
	sort.Slice(cases, func(i, j int) bool {
		return cases[i].Name < cases[j].Name
	})
	u := &UnionType{Open: true, L: a.L}
	for _, c := range cases {
		u.Cases = append(u.Cases, *c)
	}
	return tparms, u, true
}

func isectOpenAndClosedUnions(tparms []*TypeParm, _a, _b Type) ([]*TypeParm, Type, bool) {
	a, ok := _a.(*UnionType)
	if !ok {
		return nil, nil, false
	}
	b, ok := _b.(*UnionType)
	if !ok {
		return nil, nil, false
	}
	if a.Open && b.Open {
		// Handled by another case.
		return nil, nil, false
	}
	if a.Open {
		a, b = b, a
	}

	var cases []*CaseDef
	seen := make(map[string]*CaseDef)
	for _, c := range a.Cases {
		copy := c
		seen[c.Name] = &copy
		cases = append(cases, &copy)
	}
	for _, c := range b.Cases {
		prev, seen := seen[c.Name]
		if !seen {
			return nil, nil, false
		}
		if (prev.Type == nil) != (c.Type == nil) {
			// Different typedness.
			return nil, nil, false
		}
		if prev.Type == nil {
			continue
		}
		tparms, prev.Type = isection(tparms, prev.Type, c.Type)
		if prev.Type == nil {
			return nil, nil, false
		}
	}
	u := &UnionType{Open: false, L: a.L}
	for _, c := range cases {
		u.Cases = append(u.Cases, *c)
	}
	return tparms, u, true
}

func isectLiteralStructs(tparms []*TypeParm, _a, _b Type) ([]*TypeParm, Type, bool) {
	a, ok := _a.(*StructType)
	if !ok {
		return nil, nil, false
	}
	b, ok := _b.(*StructType)
	if !ok {
		return nil, nil, false
	}
	if len(a.Fields) != len(b.Fields) {
		return nil, nil, false
	}
	s := &StructType{L: a.L}
	for i := range a.Fields {
		if a.Fields[i].Name != b.Fields[i].Name {
			return nil, nil, false
		}
		f := FieldDef{Name: a.Fields[i].Name, L: a.Fields[i].L}
		tparms, f.Type = isection(tparms, a.Fields[i].Type, b.Fields[i].Type)
		if f.Type == nil {
			return nil, nil, false
		}
		s.Fields = append(s.Fields, f)
	}
	return tparms, s, true
}

func isectLiteralFuncs(tparms []*TypeParm, _a, _b Type) ([]*TypeParm, Type, bool) {
	a, ok := _a.(*FuncType)
	if !ok {
		return nil, nil, false
	}
	b, ok := _b.(*FuncType)
	if !ok {
		return nil, nil, false
	}
	if len(a.Parms) != len(b.Parms) {
		return nil, nil, false
	}
	f := &FuncType{L: a.L}
	for i := range a.Parms {
		var parm Type
		tparms, parm = isection(tparms, a.Parms[i], b.Parms[i])
		if parm == nil {
			return nil, nil, false
		}
		f.Parms = append(f.Parms, parm)
	}
	tparms, f.Ret = isection(tparms, a.Ret, b.Ret)
	if f.Ret == nil {
		return nil, nil, false
	}
	return tparms, f, true
}

func isectLiteralAndNamed(tparms []*TypeParm, a, b Type) ([]*TypeParm, Type, bool) {
	// Make a the literal type.
	if isLiteralType(b) {
		a, b = b, a
	}
	if !isLiteralType(a) || !isVisibleDefinedType(b) {
		return nil, nil, false
	}

	// TODO: is it even possible for a type pattern here
	// to have bound type arguments?
	// If not, they are always concrete types,
	// and we don't need to worry about bind/substitute.

	var bind map[*TypeParm]Type
	tparms2 := append(tparms, b.(*DefType).Def.Parms...)
	tparms, typ := _isection(tparms2, a, b.(*DefType).Def.Type, &bind)
	if typ == nil {
		fmt.Printf("nil\n")
		return nil, nil, false
	}
	if bind != nil {
		b = subType(bind, b)
	}
	// Here, if there is an intersection with the defined type's underlying type,
	// we return the defined type itself.
	return tparms, b, true
}

// A TypeParmSet is an immutable set of type parameters.
// A nil *TypeParmSet is an empty set.
type TypeParmSet struct {
	// parmSlice is the slice of all parameters.
	parmSlice []*TypeParm
	// parmMap is allocated only if there are enough parameters,
	// otherwise it is nil.
	parmMap map[*TypeParm]bool
}

// NewTypeParmSet returns a new TypeParmSet of the given type parameters.
func NewTypeParmSet(parms ...*TypeParm) *TypeParmSet {
	if len(parms) == 0 {
		return nil
	}
	var tps TypeParmSet
	if len(parms) > 5 {
		tps.parmMap = make(map[*TypeParm]bool)
		for _, p := range parms {
			if !tps.parmMap[p] {
				tps.parmSlice = append(tps.parmSlice, p)
				tps.parmMap[p] = true
			}
		}
	} else {
		for _, p := range parms {
			var found bool
			for _, q := range tps.parmSlice {
				if q == p {
					found = true
					break
				}
			}
			if !found {
				tps.parmSlice = append(tps.parmSlice, p)
			}
		}
	}
	return &tps
}

// Len returns the number of *TypeParms in the set.
func (tps *TypeParmSet) Len() int {
	if tps == nil {
		return 0
	}
	return len(tps.parmSlice)
}

// Contains returns whether the TypeParmSet contains a TypeParm.
func (tps *TypeParmSet) Contains(parm *TypeParm) bool {
	if tps == nil {
		return false
	}
	if tps.parmMap != nil {
		return tps.parmMap[parm]
	}
	for _, p := range tps.parmSlice {
		if p == parm {
			return true
		}
	}
	return false
}

// ForEach calls f for each *TypeParm in the set,
// exiting when f returns false.
func (tps *TypeParmSet) ForEach(f func(*TypeParm)) {
	if tps == nil {
		return
	}
	for _, p := range tps.parmSlice {
		f(p)
	}
}

// Union returns the union of the tps and others.
func (tps *TypeParmSet) Union(others ...*TypeParmSet) *TypeParmSet {
	var parms []*TypeParm
	if tps != nil {
		parms = tps.parmSlice
	}
	for _, o := range others {
		if o != nil {
			parms = append(parms, o.parmSlice...)
		}
	}
	return NewTypeParmSet(parms...)
}

// Minus the set of type parameters of tps for which remove returns false.
func (tps *TypeParmSet) Minus(remove func(*TypeParm) bool) *TypeParmSet {
	var parms []*TypeParm
	if tps != nil {
		for _, p := range tps.parmSlice {
			if !remove(p) {
				parms = append(parms, p)
			}
		}
	}
	return NewTypeParmSet(parms...)
}
