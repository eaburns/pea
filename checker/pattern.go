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
	Type   Type
}

func (pat TypePattern) Loc() loc.Loc { return pat.Type.Loc() }

func makeTypePattern(parms []*TypeParm, typ Type) TypePattern {
	return TypePattern{
		Parms: filterTypeParms(parms, typ),
		Type:   typ,
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
	var isGround func(Type) bool
	isGround = func(t Type) bool {
		switch t := t.(type) {
		case *DefType:
			for i := range t.Args {
				if !isGround(t.Args[i]) {
					return false
				}
			}
		case *RefType:
			return isGround(t.Type)
		case *ArrayType:
			return isGround(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				if !isGround(t.Fields[i].Type) {
					return false
				}
			}
		case *UnionType:
			for i := range t.Cases {
				if !isGround(t.Cases[i].Type) {
					return false
				}
			}
		case *FuncType:
			for i := range t.Parms {
				if !isGround(t.Parms[i]) {
					return false
				}
			}
			return isGround(t.Ret)
		case *TypeVar:
			if pat.bound(t) {
				return false
			}
		case *BasicType:
		case nil:
		default:
			panic(fmt.Sprintf("bad type type: %T", t))
		}
		return true
	}
	return isGround(pat.Type)
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

// intersection returns the intersection of two type patterns or nil if there intersection is empty.
//
// The bind parameter is a pointer to a map from type parameters to their bound types.
// The pointed-to map may be a nil map (the pointer itself must not be nil).
// If any type parameters are bound, intersection will first allocate a map if the map is nil,
// and it will add the new bindings to the map.
//
// If the intersection is empty returned notes may be non-nil
// if there is a note to give more information as to why.
func intersection(a, b TypePattern, bind *map[*TypeParm]Type) (*TypePattern, note) {
	if len(a.Parms) == 0 && len(b.Parms) == 0 {
		// Fast-path the common case of checking two types.
		if !eqType(a.Type, b.Type) {
			return nil, nil
		}
		return &a, nil
	}

	// Make a set for each type parameter with >0 bound variables
	// and unions the sets of type parameters that would bind with each other.
	var sets disjointSets
	findBoundVars(&sets, a)
	findBoundVars(&sets, b)
	if !unionSets(&sets, a, b) {
		return nil, nil
	}

	// Find the unique binding for each type parameter set.
	// If there are multiple bindings, it's an error (note).
	if note := bindSets(&sets, a, b); note != nil {
		return nil, note
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
			sub, note := subSets(&sets, make(map[*set]bool), nil, s.bind)
			if sub == nil {
				return nil, note
			}
			s.bind = sub
			s.substituted = true
		}
		// There may already be a binding if the map was used
		// on a previous call to intersection().
		if prev, ok := (*bind)[parm]; ok && !eqType(prev, s.bind) {
			return nil, newNote("%s binds %s and %s", parm.Name, prev, s.bind)
		}
		(*bind)[parm] = s.bind
	}
	isect := &TypePattern{Parms: parms, Type: subType(*bind, a.Type)}
	return isect, nil
}

func findBoundVars(sets *disjointSets, pat TypePattern) {
	switch typ := pat.Type.(type) {
	case *DefType:
		for i := range typ.Args {
			findBoundVars(sets, pat.typeArg(i))
		}
	case *RefType:
		findBoundVars(sets, pat.refElem())
	case *ArrayType:
		findBoundVars(sets, pat.arrayElem())
	case *StructType:
		for i := range typ.Fields {
			if pat.Type.(*StructType).Fields[i].Type == nil {
				continue
			}
			findBoundVars(sets, pat.field(i))
		}
	case *UnionType:
		for i := range typ.Cases {
			if typ.Cases[i].Type == nil {
				continue
			}
			findBoundVars(sets, pat.Case(i))
		}
	case *FuncType:
		for i := range typ.Parms {
			findBoundVars(sets, pat.parm(i))
		}
		if typ.Ret != nil {
			findBoundVars(sets, pat.ret())
		}
	case *BasicType:
	case *TypeVar:
		if pat.bound(typ) {
			sets.find(typ.Def)
		}
	default:
		panic(fmt.Sprintf("impossible Type type: %T", typ))
	}
}

func unionSets(sets *disjointSets, a, b TypePattern) bool {
	aVar, aOk := a.Type.(*TypeVar)
	bVar, bOk := b.Type.(*TypeVar)
	switch {
	case aOk && bOk && a.bound(aVar) && b.bound(bVar):
		sets.union(aVar.Def, bVar.Def)
		return true
	case aOk && a.bound(aVar):
		return true
	case bOk && b.bound(bVar):
		return true
	}
	switch bType := b.Type.(type) {
	case *DefType:
		aType, ok := a.Type.(*DefType)
		if !ok || aType.Def != bType.Def {
			return false
		}
		for i := range bType.Args {
			if !unionSets(sets, a.typeArg(i), b.typeArg(i)) {
				return false
			}
		}
		return true

	case *RefType:
		_, ok := a.Type.(*RefType)
		return ok && unionSets(sets, a.refElem(), b.refElem())

	case *ArrayType:
		_, ok := a.Type.(*ArrayType)
		return ok && unionSets(sets, a.arrayElem(), b.arrayElem())

	case *StructType:
		aType, ok := a.Type.(*StructType)
		if !ok || len(aType.Fields) != len(bType.Fields) {
			return false
		}
		for i := range aType.Fields {
			if a.Type.(*StructType).Fields[i].Type == nil ||
				b.Type.(*StructType).Fields[i].Type == nil {
				continue
			}
			if aType.Fields[i].Name != bType.Fields[i].Name ||
				!unionSets(sets, a.field(i), b.field(i)) {
				return false
			}
		}
		return true

	case *UnionType:
		aType, ok := a.Type.(*UnionType)
		if !ok || len(aType.Cases) != len(bType.Cases) {
			return false
		}
		for i := range aType.Cases {
			aCase := &aType.Cases[i]
			bCase := &bType.Cases[i]
			if aCase.Name != bCase.Name ||
				(aCase.Type == nil) != (bCase.Type == nil) ||
				(aCase.Type != nil && !unionSets(sets, a.Case(i), b.Case(i))) {
				return false
			}
		}
		return true

	case *FuncType:
		aType, ok := a.Type.(*FuncType)
		if !ok || len(aType.Parms) != len(bType.Parms) {
			return false
		}
		for i := range bType.Parms {
			if !unionSets(sets, a.parm(i), b.parm(i)) {
				return false
			}
		}
		return unionSets(sets, a.ret(), b.ret())

	case *BasicType:
		return eqType(a.Type, bType)

	case *TypeVar:
		return eqType(a.Type, bType)

	default:
		panic(fmt.Sprintf("impossible Type type: %T", b))
	}
}

// bindSets adds bindings for the type of each set.
// It panics if the types don't align (since it's assumed to be called after findSets).
func bindSets(sets *disjointSets, a, b TypePattern) note {
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
			return newNote("%s binds %s and %s", aVar, set.bind, b.Type)
		}
		return nil
	case bOk && b.bound(bVar):
		set := sets.find(bVar.Def)
		if set.bind == nil {
			set.bind = a.Type
			return nil
		}
		if !eqType(set.bind, a.Type) {
			return newNote("%s binds %s and %s", bVar, set.bind, a.Type)
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

func subSets(sets *disjointSets, onPath map[*set]bool, path []*TypeParm, typ Type) (Type, note) {
	switch typ := typ.(type) {
	case nil:
		// The type was an error, just ignore it.
		return nil, nil
	case *RefType:
		copy := *typ
		elem, note := subSets(sets, onPath, path, typ.Type)
		if elem == nil {
			return nil, note
		}
		copy.Type = elem
		return &copy, nil
	case *DefType:
		copy := *typ
		copy.Args = nil
		for _, arg := range typ.Args {
			argCopy, note := subSets(sets, onPath, path, arg)
			if argCopy == nil {
				return nil, note
			}
			copy.Args = append(copy.Args, argCopy)
		}
		return instType(&copy), nil
	case *ArrayType:
		copy := *typ
		elem, note := subSets(sets, onPath, path, typ.ElemType)
		if elem == nil {
			return nil, note
		}
		copy.ElemType = elem
		return &copy, nil
	case *StructType:
		copy := *typ
		copy.Fields = nil
		for i := range typ.Fields {
			f := typ.Fields[i]
			fieldCopy, note := subSets(sets, onPath, path, f.Type)
			if fieldCopy == nil {
				return nil, note
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
				typeCopy, note := subSets(sets, onPath, path, c.Type)
				if typeCopy == nil {
					return nil, note
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
			parmCopy, note := subSets(sets, onPath, path, p)
			if parmCopy == nil {
				return nil, note
			}
			copy.Parms = append(copy.Parms, parmCopy)
		}
		retCopy, note := subSets(sets, onPath, path, typ.Ret)
		if retCopy == nil {
			return nil, note
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
			var pathStr string
			for i, elem := range path {
				if i > 0 {
					pathStr += " -> "
				}
				pathStr += elem.Name
			}
			pathStr += " = " + path[0].Name
			return nil, newNote("recursive intersection: %s", pathStr)
		}
		if set.substituted {
			// set.bind was already successfully substituted.
			// Don't re-substitute it. Just return it.
			return set.bind, nil
		}
		onPath[set] = true
		defer func() { onPath[set] = false }()
		path = append(path, typ.Def)
		copy, note := subSets(sets, onPath, path, set.bind)
		if copy == nil {
			return nil, note
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
	var visit func(Type)
	visit = func(t Type) {
		switch t := t.(type) {
		case nil:
		case *RefType:
			visit(t.Type)
		case *DefType:
			for _, arg := range t.Args {
				visit(arg)
			}
		case *ArrayType:
			visit(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				visit(t.Fields[i].Type)
			}
		case *UnionType:
			for i := range t.Cases {
				visit(t.Cases[i].Type)
			}
		case *FuncType:
			for _, p := range t.Parms {
				visit(p)
			}
			visit(t.Ret)
		case *TypeVar:
			for i, p := range parms {
				if !seen[i] && p == t.Def {
					seen[i] = true
					nSeen++
				}
			}
		case *BasicType:
		default:
			panic(fmt.Sprintf("unsupported Type type: %T", t))
		}
	}
	for _, t := range types {
		visit(t)
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
