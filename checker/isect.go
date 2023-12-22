package checker

import (
	"fmt"
	"sort"
)

// TypePattern returns possibly modified versions of a and b that align and true,
// or return a and b unmodified and false.
//
// The following rules are used to align:
//
// * If the patterns unify, a and b are returned with true.
//
// * If either is a bound type variable, a and b are returned with true.
//
// * If both are open literal union types where all cases that match in name
// also match in typedness and if typed, their types align, then
// the resulting type for each is a literal union where
// mismatching cases from the other type are copied over,
// matching, typed cases have the type replaced with the corresponding
// aligned type, and the cases are sorted by name.
//
// * If one type is an open, literal union and the other is a non-open literal union
// where all cases in the open literal union are covered by a corresponding case
// in the non-open union with the same typedness, if typed, the types unify,
// then the result is that the open, literal union becomes a closed literal union
// with cases from the closed union added if not already present,
// and the cases are ordered to match that of the closed literal union.
//
// *If one is a literal type and the other is a defined type such that the literal type
// aligns with the defined type's definition type, the literal type becomes
// an instance of the defined type, with type arguments substituted
// using binding needed to unify the two aligned type definition types.
//
// * If the two types are any other matching kinds of literal types,
// where all composed types (eg literal ref element, literal struct field types,
// literal array element types) align, they are the same kind of literal type,
// but with the aligned, composed types.
//
// * Otherwise, the types do not align.
//
// Parameters of the returned patterns, are a superset of the corresponding input patterns.
func alignPatterns2(a, b TypePattern) (TypePattern, TypePattern, bool) {
	var unused map[*TypeParm]Type
	if x, _ := unify(a, b, &unused); x != nil {
		return a, b, true
	}

	if isTypeVar(a.Type) && a.bound(a.Type.(*TypeVar)) ||
		isTypeVar(b.Type) && b.bound(b.Type.(*TypeVar)) {
		return a, b, true
	}

	if isOpenLiteralUnion(a.Type) && isOpenLiteralUnion(b.Type) {
		return alignOpenUnions(a, b)
	}

	if isOpenLiteralUnion(a.Type) && isLiteralUnion(b.Type) {
		return alignOpenAndClosedUnions(a, b)
	}
	if isLiteralUnion(a.Type) && isOpenLiteralUnion(b.Type) {
		b, a, ok := alignOpenAndClosedUnions(b, a)
		return a, b, ok
	}

	if isLiteralType(a.Type) && isDefType(b.Type) {
		return alignLiteralAndDef(a, b)
	}
	if isDefType(a.Type) && isLiteralType(b.Type) {
		b, a, ok := alignLiteralAndDef(b, a)
		return a, b, ok
	}

	return a, b, false
}

func alignOpenUnions(a, b TypePattern) (TypePattern, TypePattern, bool) {
	aCases := make(map[string]int)
	aa := &UnionType{Open: true, L: a.Type.Loc()}
	for i, c := range a.Type.(*UnionType).Cases {
		aCases[c.Name] = i
		aa.Cases = append(aa.Cases, c)
	}
	bCases := make(map[string]int)
	bb := &UnionType{Open: true, L: b.Type.Loc()}
	for i, c := range b.Type.(*UnionType).Cases {
		bCases[c.Name] = i
		bb.Cases = append(bb.Cases, c)
	}

	aParms := a.Parms
	bParms := b.Parms
	for i := range aa.Cases {
		aCase := &aa.Cases[i]
		j, ok := bCases[aCase.Name]
		if !ok {
			bCases[aCase.Name] = len(bb.Cases)
			bb.Cases = append(bb.Cases, *aCase)
			continue
		}
		bCase := &bb.Cases[j]
		if (bCase.Type == nil) != (aCase.Type == nil) {
			return a, b, false
		}
		if bCase.Type == nil {
			continue
		}
		alignedACase, alignedBCase, ok := alignPatterns2(
			makeTypePattern(aParms, aCase.Type),
			makeTypePattern(bParms, bCase.Type))
		if !ok {
			return a, b, false
		}
		aParms = alignedACase.Parms
		aCase.Type = alignedACase.Type
		bParms = alignedBCase.Parms
		bCase.Type = alignedBCase.Type
	}

	for _, bCase := range bb.Cases {
		if _, ok := aCases[bCase.Name]; !ok {
			aa.Cases = append(aa.Cases, bCase)
		}
	}
	sort.Slice(aa.Cases, func(i, j int) bool {
		return aa.Cases[i].Name < aa.Cases[j].Name
	})
	sort.Slice(bb.Cases, func(i, j int) bool {
		return bb.Cases[i].Name < bb.Cases[j].Name
	})
	return makeTypePattern(aParms, aa), makeTypePattern(bParms, bb), true
}

// a is the open literal union, b is closed.
func alignOpenAndClosedUnions(a, b TypePattern) (TypePattern, TypePattern, bool) {
	panic("unimplemented")
}

// a is the literal type, b is a defined type.
func alignLiteralAndDef(a, b TypePattern) (TypePattern, TypePattern, bool) {
	bDefType := b.Type.(*DefType)
	parms := NewTypeParmSet(bDefType.Def.Parms...)
	defPat := makeTypePattern(parms, bDefType.Def.Type)

	alignedA, alignedDefPat, ok := alignPatterns2(a, defPat)
	if !ok {
		fmt.Printf("%s and %s do not align\n", a, defPat)
		return a, b, false
	}
	var bind map[*TypeParm]Type
	u, _ := unify(alignedA, alignedDefPat, &bind)
	if u == nil {
		// Just because they aligned doesn't mean they also can be unified.
		// If they cannot unify, they fail alignment here.
		fmt.Printf("%s and %s do not unify\n", alignedA, alignedDefPat)
		return a, b, false
	}
	aDefType := *bDefType
	aDefType.L = a.Type.Loc()
	var args []Type
	parms.ForEach(func(p *TypeParm) {
		args = append(args, &TypeVar{
			SourceName: p.Name,
			Def:        p,
			L:          a.Type.Loc(),
		})
	})
	aDefType.Args = subTypes(bind, args)
	aParms := alignedA.Parms.Union(u.Parms)
	aa := makeTypePattern(aParms, instType(&aDefType))
	return aa, b, true
}

func isOpenLiteralUnion(p Type) bool {
	u, ok := p.(*UnionType)
	return ok && u.Open
}

func isLiteralUnion(p Type) bool {
	_, ok := p.(*UnionType)
	return ok
}
