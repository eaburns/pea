package checker

import (
	"fmt"
	"strconv"

	"github.com/eaburns/pea/loc"
)

// typePattern is a type pattern.
type typePattern struct {
	parms []*TypeParm
	typ   Type
}

// any returns a new typePattern with a single, bound type variable.
func any() *typePattern {
	n := "_"
	p := &TypeParm{Name: n}
	v := &TypeVar{Name: n, Def: p}
	return &typePattern{parms: []*TypeParm{p}, typ: v}
}

// pattern returns the typePattern for the type.
func pattern(t Type) *typePattern {
	return &typePattern{parms: nil, typ: t}
}

// bound returns whether the type variable is bound to a type parameter of the typePattern.
func (pat *typePattern) bound(v *TypeVar) bool {
	for _, parm := range pat.parms {
		if v.Def == parm {
			return true
		}
	}
	return false
}

func (pat *typePattern) String() string {
	c := copyTypeWithLoc(pat.typ, loc.Loc{})

	refs := make(map[*TypeParm]int)
	var countBoundVarRefs func(Type)
	countBoundVarRefs = func(t Type) {
		switch t := t.(type) {
		case *DefType:
			for i := range t.Args {
				countBoundVarRefs(t.Args[i])
			}
		case *RefType:
			countBoundVarRefs(t.Type)
		case *ArrayType:
			countBoundVarRefs(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				countBoundVarRefs(t.Fields[i].Type)
			}
		case *UnionType:
			for i := range t.Cases {
				countBoundVarRefs(t.Cases[i].Type)
			}
		case *FuncType:
			for i := range t.Parms {
				countBoundVarRefs(t.Parms[i])
			}
			countBoundVarRefs(t.Ret)
		case *TypeVar:
			if pat.bound(t) {
				refs[t.Def] = refs[t.Def] + 1
			}
		case *BasicType:
		case nil:
		default:
			panic(fmt.Sprintf("bad type type: %T", t))
		}
	}
	countBoundVarRefs(c)

	i := 0
	name := make(map[*TypeParm]string)
	var renameBoundVars func(Type)
	renameBoundVars = func(t Type) {
		switch t := t.(type) {
		case *DefType:
			for i := range t.Args {
				renameBoundVars(t.Args[i])
			}
		case *RefType:
			renameBoundVars(t.Type)
		case *ArrayType:
			renameBoundVars(t.ElemType)
		case *StructType:
			for i := range t.Fields {
				renameBoundVars(t.Fields[i].Type)
			}
		case *UnionType:
			for i := range t.Cases {
				renameBoundVars(t.Cases[i].Type)
			}
		case *FuncType:
			for i := range t.Parms {
				renameBoundVars(t.Parms[i])
			}
			renameBoundVars(t.Ret)
		case *TypeVar:
			if !pat.bound(t) {
				return
			}
			if _, ok := name[t.Def]; !ok {
				n := "_"
				if refs[t.Def] > 1 {
					n += strconv.Itoa(i)
					i++
				}
				name[t.Def] = n
			}
			t.Name = name[t.Def]
		case *BasicType:
		case nil:
		default:
			panic(fmt.Sprintf("bad type type: %T", t))
		}
	}
	renameBoundVars(c)
	return c.String()
}

// common returns a new typePattern that is the most specific simple common pattern of pats.
func common(pats ...*typePattern) *typePattern {
	switch len(pats) {
	case 0:
		return any()
	case 1:
		return pats[0]
	}

	var nextParm int
	var pat typePattern

	var newVar func() *TypeVar
	newVar = func() *TypeVar {
		n := "_" + strconv.Itoa(nextParm)
		nextParm++
		p := &TypeParm{Name: n}
		pat.parms = append(pat.parms, p)
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
		ts[i] = p.typ
	}
	pat.typ = buildType(ts)
	return &pat
}
