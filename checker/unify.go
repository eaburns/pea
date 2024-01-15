package checker

import (
	"fmt"
	"sort"
	"strings"
)

type patternUnifyError interface{ Cause }

// unify returns the unification of two type patterns or nil if they cannot unify.
// The unification of a and b is a type pattern resulting from
// substituting bound type variables in a with corresponding types in b, and
// substituting bound type variables in b with corresponding types in a.
// If there is no such substitution, the type patterns cannot unify.
//
// The bind parameter is a pointer to a map from type parameters to their bound types.
// The pointed-to map may be a nil map (the pointer itself must not be nil).
// If any type parameters are bound, unify will first allocate a map if the map is nil,
// and it will add the new bindings to the map.
//
// If the patterns cannot unify, notes may be non-nil
// if there is a note to give more information as to why.
func unify(a, b TypePattern, bind *map[*TypeParm]Type) (*TypePattern, patternUnifyError) {
	a.Type, b.Type = expandOpenLits(a.Type, b.Type)

	if a.Parms.Len() == 0 && b.Parms.Len() == 0 {
		// Fast-path the common case of simply checking two types align,
		// but there are no type parameters to bind.
		if err := unionAndBindTypeParms(nil, a, b); err != nil {
			return nil, err
		}
		return &a, nil
	}

	// Make a set for each type parameter with >0 bound variables
	// and unions the sets of type parameters that would bind with each other.
	var sets disjointSets
	findBoundVars(&sets, a)
	findBoundVars(&sets, b)
	if err := unionAndBindTypeParms(&sets, a, b); err != nil {
		return nil, err
	}

	// Make a new type parameter for bound type variables in the input patterns
	// that did not bind to a type.
	var parms []*TypeParm
	seen := make(map[string]int)
	for _, parm := range sets.setSlice {
		set := sets.find(parm)
		if set.bind == nil {
			name := strings.TrimRight(parm.Name, "0123456789")
			seen[parm.Name] = seen[parm.Name] + 1
			if n := seen[name]; n > 1 {
				name = fmt.Sprintf("%s%d", name, n)
			}
			parm := &TypeParm{Name: name}
			parms = append(parms, parm)
			set.bind = &TypeVar{Def: parm}
		}
	}

	// Substitute type parameters in each binding recursively.
	// If the substitution would create infinite recursion, it's an error (note).
	// Also establish a mapping for each type parameter in the input types
	// to their ultimate substitution.
	if *bind == nil {
		*bind = make(map[*TypeParm]Type)
	}
	for _, parm := range sets.setSlice {
		s := sets.find(parm)
		if !s.substituted {
			sub, err := subSets(&sets, make(map[*set]bool), nil, s.bind)
			if err != nil {
				return nil, err
			}
			s.bind = sub
			s.substituted = true
		}
		// There may already be a binding if the map was used
		// on a previous call to unify().
		if prev, ok := (*bind)[parm]; ok && !eqType(prev, s.bind) {
			return nil, &PatternBindingError{Parm: parm, Prev: prev, Cur: s.bind}
		}
		(*bind)[parm] = s.bind
	}
	return &TypePattern{
		Parms: NewTypeParmSet(parms...),
		Type:  subType(*bind, a.Type),
	}, nil
}

func expandOpenLits(a, _b Type) (Type, Type) {
	open := walkType(a, isOpenLit) || walkType(_b, isOpenLit)
	if !open {
		// If there are no open literals, short-circuit and just return the types.
		return a, _b
	}
	switch a := a.(type) {
	case *RefType:
		b, ok := _b.(*RefType)
		if !ok {
			return a, _b
		}
		aa, bb := expandOpenLits(a.Type, b.Type)
		return &RefType{Type: aa, L: a.L},
			&RefType{Type: bb, L: b.L}

	case *ArrayType:
		b, ok := _b.(*ArrayType)
		if !ok {
			return a, _b
		}
		aa, bb := expandOpenLits(a.ElemType, b.ElemType)
		return &ArrayType{ElemType: aa, L: a.L},
			&ArrayType{ElemType: bb, L: b.L}

	case *StructType:
		b, ok := _b.(*StructType)
		if !ok {
			return a, _b
		}
		switch {
		case a.Open && b.Open:
			return expandStructOpenOpen(a, b)
		case a.Open:
			return expandStructOpenClosed(a, b)
		case b.Open:
			bbType, aaType := expandStructOpenClosed(b, a)
			return aaType, bbType
		case len(a.Fields) != len(b.Fields):
			return a, _b
		}
		aaFields := make([]FieldDef, len(a.Fields))
		bbFields := make([]FieldDef, len(b.Fields))
		for i, aField := range a.Fields {
			bField := b.Fields[i]
			name := aField.Name
			if bField.Name != name {
				return a, _b
			}
			aaFieldType, bbFieldType := expandOpenLits(aField.Type, bField.Type)
			aaFields[i] = FieldDef{Name: name, Type: aaFieldType, L: aField.L}
			bbFields[i] = FieldDef{Name: name, Type: bbFieldType, L: bField.L}
		}
		return &StructType{Fields: aaFields, L: a.L},
			&StructType{Fields: bbFields, L: b.L}

	case *UnionType:
		b, ok := _b.(*UnionType)
		if !ok {
			return a, _b
		}
		switch {
		case a.Open && b.Open:
			return expandUnionOpenOpen(a, b)
		case a.Open:
			return expandUnionOpenClosed(a, b)
		case b.Open:
			bbType, aaType := expandUnionOpenClosed(b, a)
			return aaType, bbType
		case len(a.Cases) != len(b.Cases):
			return a, _b
		}
		aaCases := make([]CaseDef, len(a.Cases))
		bbCases := make([]CaseDef, len(b.Cases))
		for i, aCase := range a.Cases {
			bCase := b.Cases[i]
			name := aCase.Name
			if bCase.Name != name {
				return a, _b
			}
			aaCaseType, bbCaseType := expandOpenLits(aCase.Type, bCase.Type)
			aaCases[i] = CaseDef{Name: name, Type: aaCaseType, L: aCase.L}
			bbCases[i] = CaseDef{Name: name, Type: bbCaseType, L: bCase.L}
		}
		return &UnionType{Cases: aaCases, L: a.L},
			&UnionType{Cases: bbCases, L: b.L}

	case *FuncType:
		b, ok := _b.(*FuncType)
		if !ok || len(a.Parms) != len(b.Parms) {
			return a, _b
		}
		aaParms := make([]Type, len(a.Parms))
		bbParms := make([]Type, len(b.Parms))
		for i := range a.Parms {
			aaParms[i], bbParms[i] = expandOpenLits(a.Parms[i], b.Parms[i])
		}
		aaRet, bbRet := expandOpenLits(a.Ret, b.Ret)
		return &FuncType{Parms: aaParms, Ret: aaRet, L: a.L},
			&FuncType{Parms: bbParms, Ret: bbRet, L: b.L}

	case *DefType, *BasicType, *TypeVar:
		return a, _b

	default:
		panic(fmt.Sprintf("impossible Type type: %T", a))
	}
}

func isOpenLit(t Type) bool {
	switch t := t.(type) {
	case *StructType:
		return t.Open
	case *UnionType:
		return t.Open
	default:
		return false
	}
}

func expandStructOpenOpen(a, b *StructType) (Type, Type) {
	aFields := structFields(a)
	aa := &StructType{Fields: copyFields(a.Fields), Open: true, L: a.L}
	for _, f := range b.Fields {
		if _, ok := aFields[f.Name]; ok {
			continue
		}
		aa.Fields = append(aa.Fields, FieldDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		})
	}
	sort.Slice(aa.Fields, func(i, j int) bool {
		return aa.Fields[i].Name < aa.Fields[j].Name
	})

	bFields := structFields(b)
	bb := &StructType{Fields: copyFields(b.Fields), Open: true, L: b.L}
	for _, f := range a.Fields {
		if _, ok := bFields[f.Name]; ok {
			continue
		}
		bb.Fields = append(bb.Fields, FieldDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		})
	}
	sort.Slice(bb.Fields, func(i, j int) bool {
		return bb.Fields[i].Name < bb.Fields[j].Name
	})
	return aa, bb
}

func copyFields(fs []FieldDef) []FieldDef {
	copy := make([]FieldDef, len(fs))
	for i, f := range fs {
		copy[i] = FieldDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		}
	}
	return copy
}

func expandStructOpenClosed(open, closed *StructType) (Type, Type) {
	if closed.Open {
		panic("impossible")
	}
	if len(open.Fields) > len(closed.Fields) {
		return open, closed
	}
	// Make sure all fields in open and in closed.
	closedFields := structFields(closed)
	for i := range open.Fields {
		if _, ok := closedFields[open.Fields[i].Name]; !ok {
			return open, closed
		}
	}
	// The resulting struct has all fields from closed, in the order from closed,
	// but with the type copied from open if the field was in open.
	openFields := structFields(open)
	oopen := &StructType{L: open.L}
	for _, f := range closed.Fields {
		if openField, ok := openFields[f.Name]; ok {
			// If the open struct already has this field, copy from there.
			f = *openField
		}
		oopen.Fields = append(oopen.Fields, FieldDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		})
	}
	return oopen, closed
}

func structFields(s *StructType) map[string]*FieldDef {
	fields := make(map[string]*FieldDef, len(s.Fields))
	for i := range s.Fields {
		fields[s.Fields[i].Name] = &s.Fields[i]
	}
	return fields
}

func expandUnionOpenOpen(a, b *UnionType) (Type, Type) {
	aCases := unionCases(a)
	aa := &UnionType{Cases: copyCases(a.Cases), Open: true, L: a.L}
	for _, f := range b.Cases {
		if _, ok := aCases[f.Name]; ok {
			continue
		}
		aa.Cases = append(aa.Cases, CaseDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		})
	}
	sort.Slice(aa.Cases, func(i, j int) bool {
		return aa.Cases[i].Name < aa.Cases[j].Name
	})

	bCases := unionCases(b)
	bb := &UnionType{Cases: copyCases(b.Cases), Open: true, L: b.L}
	for _, f := range a.Cases {
		if _, ok := bCases[f.Name]; ok {
			continue
		}
		bb.Cases = append(bb.Cases, CaseDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		})
	}
	sort.Slice(bb.Cases, func(i, j int) bool {
		return bb.Cases[i].Name < bb.Cases[j].Name
	})
	return aa, bb
}

func copyCases(fs []CaseDef) []CaseDef {
	copy := make([]CaseDef, len(fs))
	for i, f := range fs {
		copy[i] = CaseDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		}
	}
	return copy
}

func expandUnionOpenClosed(open, closed *UnionType) (Type, Type) {
	if closed.Open {
		panic("impossible")
	}
	if len(open.Cases) > len(closed.Cases) {
		return open, closed
	}
	// Make sure all cases in open and in closed.
	closedCases := unionCases(closed)
	for i := range open.Cases {
		if _, ok := closedCases[open.Cases[i].Name]; !ok {
			return open, closed
		}
	}
	// The resulting union has all cases from closed, in the order from closed,
	// but with the type copied from open if the case was in open.
	openCases := unionCases(open)
	oopen := &UnionType{L: open.L}
	for _, f := range closed.Cases {
		if openCase, ok := openCases[f.Name]; ok {
			// If the open union already has this case, copy from there.
			f = *openCase
		}
		oopen.Cases = append(oopen.Cases, CaseDef{
			Name: f.Name,
			Type: copyTypeWithLoc(f.Type, f.L),
			L:    f.L,
		})
	}
	return oopen, closed
}

func unionCases(s *UnionType) map[string]*CaseDef {
	fields := make(map[string]*CaseDef, len(s.Cases))
	for i := range s.Cases {
		fields[s.Cases[i].Name] = &s.Cases[i]
	}
	return fields
}

func findBoundVars(sets *disjointSets, pat TypePattern) {
	walkType(pat.Type, func(t Type) bool {
		if tv, ok := t.(*TypeVar); ok && pat.bound(tv) {
			sets.find(tv.Def)
		}
		return false
	})
}

type patternAlignError interface{ Cause }

// unionAndBindTypeParms assigns types to type parameters,
// and unions type parameters that would be assigned to eachother.
//
// If sets is nil, unionAndBindTypeParms panics if there are any bound type variables.
// If the caller guarantees there are no bound type variables, sets may be passed as nil,
// in which case unionAndBindTypeParms will return an error if the types are not aligned.
func unionAndBindTypeParms(sets *disjointSets, a, b TypePattern) patternAlignError {
	aVar, aOk := a.Type.(*TypeVar)
	bVar, bOk := b.Type.(*TypeVar)
	switch {
	case aOk && bOk && a.bound(aVar) && b.bound(bVar):
		if sets == nil {
			panic("unexpected bound type variable")
		}
		aSet := sets.find(aVar.Def)
		bSet := sets.find(bVar.Def)
		switch set := sets.union(aSet, bSet); {
		case aSet.bind != nil && bSet.bind != nil:
			if !eqType(aSet.bind, bSet.bind) {
				return &PatternBindingError{Parm: aVar.Def, Prev: bSet.bind, Cur: bSet.bind}
			}
			set.bind = aSet.bind
		case aSet.bind != nil:
			set.bind = aSet.bind
		case bSet.bind != nil:
			set.bind = bSet.bind
		}
		return nil
	case aOk && a.bound(aVar):
		if sets == nil {
			panic("unexpected bound type variable")
		}
		set := sets.find(aVar.Def)
		if set.bind == nil {
			set.bind = b.Type
			return nil
		}
		if !eqType(set.bind, b.Type) {
			return &PatternBindingError{Parm: aVar.Def, Prev: set.bind, Cur: b.Type}
		}
		return nil
	case bOk && b.bound(bVar):
		if sets == nil {
			panic("unexpected bound type variable")
		}
		set := sets.find(bVar.Def)
		if set.bind == nil {
			set.bind = a.Type
			return nil
		}
		if !eqType(set.bind, a.Type) {
			return &PatternBindingError{Parm: bVar.Def, Prev: set.bind, Cur: a.Type}
		}
		return nil
	}
	switch bType := b.Type.(type) {
	case *DefType:
		switch aDefType, ok := a.Type.(*DefType); {
		case ok && aDefType.Def != bType.Def:
			return &DiffNamedTypeError{A: a.Type, B: b.Type}
		case !ok && isBasicType(a.Type):
			return &DiffNamedTypeError{A: a.Type, B: b.Type}
		case !ok:
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		}
		for i := range bType.Args {
			if err := unionAndBindTypeParms(sets, a.typeArg(i), b.typeArg(i)); err != nil {
				return err
			}
		}
		return nil

	case *RefType:
		if _, ok := a.Type.(*RefType); !ok {
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		}
		return unionAndBindTypeParms(sets, a.refElem(), b.refElem())

	case *ArrayType:
		if _, ok := a.Type.(*ArrayType); !ok {
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		}
		return unionAndBindTypeParms(sets, a.arrayElem(), b.arrayElem())

	case *StructType:
		aStructType, ok := a.Type.(*StructType)
		switch {
		case !ok:
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		case len(aStructType.Fields) != len(bType.Fields):
			return &DiffFieldsError{A: aStructType, B: bType}
		}
		for i := range aStructType.Fields {
			aField := &aStructType.Fields[i]
			bField := &bType.Fields[i]
			if aField.Name != bField.Name {
				return &DiffFieldsError{A: aStructType, B: bType}
			}
			if aField.Type == nil || bField.Type == nil {
				continue
			}
			if err := unionAndBindTypeParms(sets, a.field(i), b.field(i)); err != nil {
				return &DiffFieldsError{
					A:     aStructType,
					B:     bType,
					Cause: err,
					Field: aField.Name,
				}
			}
		}
		return nil

	case *UnionType:
		aUnionType, ok := a.Type.(*UnionType)
		switch {
		case !ok:
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		case len(aUnionType.Cases) != len(bType.Cases):
			return &DiffCasesError{A: aUnionType, B: bType}
		}
		for i := range aUnionType.Cases {
			aCase := &aUnionType.Cases[i]
			bCase := &bType.Cases[i]
			if aCase.Name != bCase.Name {
				return &DiffCasesError{A: aUnionType, B: bType}
			}
			if err := unionAndBindTypeParms(sets, a.Case(i), b.Case(i)); err != nil {
				return &DiffCasesError{
					A:     aUnionType,
					B:     bType,
					Cause: err,
					Case:  aCase.Name,
				}
			}
		}
		return nil

	case *FuncType:
		aFuncType, ok := a.Type.(*FuncType)
		switch {
		case !ok:
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		case len(aFuncType.Parms) != len(bType.Parms):
			return &DiffFuncError{A: aFuncType, B: bType}
		}
		for i := range bType.Parms {
			if err := unionAndBindTypeParms(sets, a.parm(i), b.parm(i)); err != nil {
				return &DiffFuncError{A: aFuncType, B: bType, Cause: err, Parm: i}
			}
		}
		if err := unionAndBindTypeParms(sets, a.ret(), b.ret()); err != nil {
			return &DiffFuncError{A: aFuncType, B: bType, Cause: err, Parm: -1}
		}
		return nil

	case *BasicType:
		if !eqType(a.Type, bType) {
			switch {
			case isDefType(a.Type) || isBasicType(a.Type):
				return &DiffNamedTypeError{A: a.Type, B: b.Type}
			default:
				return &DiffTypeKindError{A: a.Type, B: b.Type}
			}
		}
		return nil

	case *TypeVar:
		if !eqType(a.Type, bType) {
			if aType, ok := a.Type.(*TypeVar); ok {
				return &DiffTypeVarError{A: aType, B: bType}
			}
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		}
		return nil

	default:
		panic(fmt.Sprintf("impossible Type type: %T", b))
	}
}

func subSets(sets *disjointSets, onPath map[*set]bool, path []*TypeParm, typ Type) (Type, *PatternSubError) {
	switch typ := typ.(type) {
	case nil:
		// The type was an error, just ignore it.
		return nil, nil
	case *RefType:
		copy := *typ
		elem, err := subSets(sets, onPath, path, typ.Type)
		if err != nil {
			return nil, err
		}
		copy.Type = elem
		return &copy, nil
	case *DefType:
		copy := *typ
		copy.Args = nil
		for _, arg := range typ.Args {
			argCopy, err := subSets(sets, onPath, path, arg)
			if err != nil {
				return nil, err
			}
			copy.Args = append(copy.Args, argCopy)
		}
		return instType(&copy), nil
	case *ArrayType:
		copy := *typ
		elem, err := subSets(sets, onPath, path, typ.ElemType)
		if err != nil {
			return nil, err
		}
		copy.ElemType = elem
		return &copy, nil
	case *StructType:
		copy := *typ
		copy.Fields = nil
		for i := range typ.Fields {
			f := typ.Fields[i]
			fieldCopy, err := subSets(sets, onPath, path, f.Type)
			if err != nil {
				return nil, err
			}
			f.Type = fieldCopy
			copy.Fields = append(copy.Fields, f)
		}
		return &copy, nil
	case *UnionType:
		copy := *typ
		copy.Cases = nil
		for i := range typ.Cases {
			c := typ.Cases[i]
			typeCopy, err := subSets(sets, onPath, path, c.Type)
			if err != nil {
				return nil, err
			}
			c.Type = typeCopy
			copy.Cases = append(copy.Cases, c)
		}
		return &copy, nil
	case *FuncType:
		copy := *typ
		copy.Parms = nil
		for _, p := range typ.Parms {
			parmCopy, err := subSets(sets, onPath, path, p)
			if err != nil {
				return nil, err
			}
			copy.Parms = append(copy.Parms, parmCopy)
		}
		retCopy, err := subSets(sets, onPath, path, typ.Ret)
		if err != nil {
			return nil, err
		}
		copy.Ret = retCopy
		return &copy, nil
	case *TypeVar:
		if _, ok := sets.setMap[typ.Def]; !ok {
			// Not a bound type variable in any pattern. Just copy it.
			copy := *typ
			return &copy, nil
		}
		set := sets.find(typ.Def)
		if eqType(set.bind, typ) {
			// Self bindings are not an error.
			copy := *typ
			return &copy, nil
		}
		if onPath[set] {
			return nil, &PatternSubError{Loop: append(path, typ.Def)}
		}
		if set.substituted {
			// set.bind was already successfully substituted.
			// Don't re-substitute it. Just return it.
			return set.bind, nil
		}
		onPath[set] = true
		defer func() { onPath[set] = false }()
		path = append(path, typ.Def)
		copy, err := subSets(sets, onPath, path, set.bind)
		if err != nil {
			return nil, err
		}
		set.bind = copy
		set.substituted = true
		return copy, nil
	case *BasicType:
		copy := *typ
		return &copy, nil
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}

type disjointSets struct {
	setMap   map[*TypeParm]*set
	setSlice []*TypeParm
}

type set struct {
	name   string
	rank   int
	parent *set

	// bind is only used on root sets after computing unions.
	bind        Type
	substituted bool
}

func (sets *disjointSets) find(t *TypeParm) *set {
	if sets.setMap == nil {
		sets.setMap = make(map[*TypeParm]*set)
	}
	s := sets.setMap[t]
	if s == nil {
		s = &set{name: t.Name, rank: 0}
		s.parent = s
		sets.setMap[t] = s
		sets.setSlice = append(sets.setSlice, t)
	}
	root := s
	for root.parent != root {
		root = root.parent
	}
	for s.parent != root {
		p := s.parent
		s.parent = root
		s = p
	}
	return root
}

func (sets *disjointSets) union(x, y *set) *set {
	if x == y {
		return x
	}
	if x.rank < y.rank {
		x, y = y, x
	}
	y.parent = x
	if x.rank != y.rank {
		x.rank++
	}
	return x
}
