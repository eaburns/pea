package checker

import (
	"fmt"
	"strconv"

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

func makeTypePattern(parms []*TypeParm, typ Type) TypePattern {
	return TypePattern{Parms: parms, Type: typ}
}

// any returns a new TypePattern with a single, bound type variable.
func any() TypePattern {
	n := "_"
	p := &TypeParm{Name: n}
	v := &TypeVar{Def: p}
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
