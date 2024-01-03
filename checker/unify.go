package checker

import (
	"fmt"
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
	if a.Parms.Len() == 0 && b.Parms.Len() == 0 {
		// Fast-path the common case of simply checking two types align.
		if err := alignTypes(nil, a, b); err != nil {
			return nil, err
		}
		return &a, nil
	}

	// Make a set for each type parameter with >0 bound variables
	// and unions the sets of type parameters that would bind with each other.
	var sets disjointSets
	findBoundVars(&sets, a)
	findBoundVars(&sets, b)
	if err := alignTypes(&sets, a, b); err != nil {
		return nil, err
	}

	// Find the unique binding for each type parameter set.
	// If there are multiple bindings, it's an error (note).
	if err := bindSets(&sets, a, b); err != nil {
		return nil, err
	}
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

func findBoundVars(sets *disjointSets, pat TypePattern) {
	walkType(pat.Type, func(t Type) bool {
		if tv, ok := t.(*TypeVar); ok && pat.bound(tv) {
			sets.find(tv.Def)
		}
		return false
	})
}

type patternAlignError interface{ Cause }

// alignTypes returns an error if two types do not "align".
// If sets is non-nil, sets corresponding to
// aligned, bound type variables are unioned.
func alignTypes(sets *disjointSets, a, b TypePattern) patternAlignError {
	aVar, aOk := a.Type.(*TypeVar)
	bVar, bOk := b.Type.(*TypeVar)
	switch {
	case aOk && bOk && a.bound(aVar) && b.bound(bVar):
		if sets != nil {
			sets.union(aVar.Def, bVar.Def)
		}
		return nil
	case aOk && a.bound(aVar):
		return nil
	case bOk && b.bound(bVar):
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
			if err := alignTypes(sets, a.typeArg(i), b.typeArg(i)); err != nil {
				return err
			}
		}
		return nil

	case *RefType:
		if _, ok := a.Type.(*RefType); !ok {
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		}
		return alignTypes(sets, a.refElem(), b.refElem())

	case *ArrayType:
		if _, ok := a.Type.(*ArrayType); !ok {
			return &DiffTypeKindError{A: a.Type, B: b.Type}
		}
		return alignTypes(sets, a.arrayElem(), b.arrayElem())

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
			if err := alignTypes(sets, a.field(i), b.field(i)); err != nil {
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
			if (aCase.Type == nil) != (bCase.Type == nil) {
				return &DiffCasesError{
					A:    aUnionType,
					B:    bType,
					Case: aCase.Name,
				}
			}
			if aCase.Type != nil {
				if err := alignTypes(sets, a.Case(i), b.Case(i)); err != nil {
					return &DiffCasesError{
						A:     aUnionType,
						B:     bType,
						Cause: err,
						Case:  aCase.Name,
					}
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
			if err := alignTypes(sets, a.parm(i), b.parm(i)); err != nil {
				return &DiffFuncError{A: aFuncType, B: bType, Cause: err, Parm: i}
			}
		}
		if err := alignTypes(sets, a.ret(), b.ret()); err != nil {
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

// bindSets adds bindings for the type of each set.
// It panics if the types don't align (since it's assumed to be called after alignTypes).
func bindSets(sets *disjointSets, a, b TypePattern) *PatternBindingError {
	aVar, aOk := a.Type.(*TypeVar)
	bVar, bOk := b.Type.(*TypeVar)
	switch {
	case aOk && bOk && a.bound(aVar) && b.bound(bVar):
		return nil
	case aOk && a.bound(aVar):
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
		for i := range bType.Args {
			if note := bindSets(sets, a.typeArg(i), b.typeArg(i)); note != nil {
				return note
			}
		}
		return nil

	case *RefType:
		return bindSets(sets, a.refElem(), b.refElem())

	case *ArrayType:
		return bindSets(sets, a.arrayElem(), b.arrayElem())

	case *StructType:
		for i := range bType.Fields {
			if a.Type.(*StructType).Fields[i].Type == nil ||
				b.Type.(*StructType).Fields[i].Type == nil {
				continue
			}
			if note := bindSets(sets, a.field(i), b.field(i)); note != nil {
				return note
			}
		}
		return nil

	case *UnionType:
		for i := range bType.Cases {
			if bType.Cases[i].Type == nil {
				continue
			}
			if note := bindSets(sets, a.Case(i), b.Case(i)); note != nil {
				return note
			}
		}
		return nil

	case *FuncType:
		for i := range bType.Parms {
			if note := bindSets(sets, a.parm(i), b.parm(i)); note != nil {
				return note
			}
		}
		return bindSets(sets, a.ret(), b.ret())

	case *BasicType:
		return nil

	case *TypeVar:
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
			if c.Type != nil {
				typeCopy, err := subSets(sets, onPath, path, c.Type)
				if err != nil {
					return nil, err
				}
				c.Type = typeCopy
			}
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

func (sets *disjointSets) union(a, b *TypeParm) {
	x := sets.find(a)
	y := sets.find(b)
	if x == y {
		return
	}
	if x.rank < y.rank {
		x, y = y, x
	}
	y.parent = x
	if x.rank != y.rank {
		x.rank++
	}
}
