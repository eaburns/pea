package checker

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/eaburns/pea/loc"
)

// A TypePattern describes a set of types matching the pattern,
// it consists of a set of type parameters, and a type.
// The type may contain type variables bound to the parameters.
// The pattern represents all types that are the result of
// substituting types for the type parameters.
type TypePattern struct {
	Parms []*TypeParm
	Type  Type
}

func (pat TypePattern) Loc() loc.Loc { return pat.Type.Loc() }

func makeTypePattern(parms []*TypeParm, typ Type) TypePattern {
	return TypePattern{
		Parms: filterTypeParms(parms, typ),
		Type:  typ,
	}
}

// any returns a new TypePattern with a single, bound type variable.
func any() TypePattern {
	n := "_"
	p := &TypeParm{Name: n}
	v := &TypeVar{Name: n, Def: p}
	return TypePattern{Parms: []*TypeParm{p}, Type: v}
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
	for _, p := range pat.Parms {
		if v.Def == p {
			return true
		}
	}
	return false
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
		pat.Parms = append(pat.Parms, p)
		return &TypeVar{Name: n, Def: p}
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

type patternIsectError interface{ Cause }

// intersection returns the intersection of two type patterns or nil if there intersection is empty.
//
// The bind parameter is a pointer to a map from type parameters to their bound types.
// The pointed-to map may be a nil map (the pointer itself must not be nil).
// If any type parameters are bound, intersection will first allocate a map if the map is nil,
// and it will add the new bindings to the map.
//
// If the intersection is empty returned notes may be non-nil
// if there is a note to give more information as to why.
func intersection(a, b TypePattern, bind *map[*TypeParm]Type) (*TypePattern, patternIsectError) {
	if len(a.Parms) == 0 && len(b.Parms) == 0 {
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
			set.bind = &TypeVar{Name: parm.Name, Def: parm}
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
		// on a previous call to intersection().
		if prev, ok := (*bind)[parm]; ok && !eqType(prev, s.bind) {
			return nil, &PatternBindingError{Parm: parm, Prev: prev, Cur: s.bind}
		}
		(*bind)[parm] = s.bind
	}
	isect := &TypePattern{Parms: parms, Type: subType(*bind, a.Type)}
	return isect, nil
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

// filterTypeParms filters parms to only contain *TypeParms
// that are referenced by at least one TypeVar in types.
// The order of the parameters is preserved;
// and the underlying array is only copied
// if the slice is actually modified.
func filterTypeParms(parms []*TypeParm, types ...Type) []*TypeParm {
	if len(parms) == 0 {
		return parms
	}
	nSeen := 0
	seen := make([]bool, len(parms))
	for _, t := range types {
		walkType(t, func(t Type) bool {
			if tv, ok := t.(*TypeVar); ok {
				for i, p := range parms {
					if !seen[i] && p == tv.Def {
						seen[i] = true
						nSeen++
					}
				}
			}
			return false
		})
	}
	switch {
	case nSeen == 0:
		return nil
	case nSeen == len(parms):
		return parms
	default:
		copy := make([]*TypeParm, 0, nSeen)
		for i, p := range parms {
			if seen[i] {
				copy = append(copy, p)
			}
		}
		return copy
	}
}
