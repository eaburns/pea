package checker

import (
	"fmt"

	"github.com/eaburns/pea/loc"
)

func isEmptyStruct(typ Type) bool {
	s, ok := typ.(*StructType)
	return ok && len(s.Fields) == 0
}

func refType(typ Type) Type {
	if typ == nil {
		return nil
	}
	return &RefType{Type: typ, L: typ.Loc()}
}

func isRefType(typ Type) bool {
	_, ok := typ.(*RefType)
	return ok
}

func trim1Ref(typ Type) Type {
	if typ == nil {
		return nil
	}
	if ref, ok := typ.(*RefType); ok {
		return ref.Type
	}
	return typ
}

func literalType(typ Type) Type {
	if typ == nil {
		return nil
	}
	switch typ := typ.(type) {
	case *RefType:
		if typ.Type == nil {
			return nil
		}
		return &RefType{Type: literalType(typ.Type), L: typ.L}
	case *DefType:
		if typ.Inst == nil || typ.Inst.Type == nil {
			return nil
		}
		return literalType(typ.Inst.Type)
	default:
		return typ
	}
}

// valueType is the type with all leading references removed.
func valueType(typ Type) Type {
	var n int
	for {
		ref, ok := typ.(*RefType)
		if !ok {
			return typ
		}
		n++
		typ = ref.Type
	}
}

// refDepth returns the number of refs removed by valueType.
func refDepth(typ Type) int {
	var n int
	for {
		ref, ok := typ.(*RefType)
		if !ok {
			return n
		}
		n++
		typ = ref.Type
	}
}

func eqType(a, b Type) bool {
	if a == nil || b == nil {
		// nil indicates an error; just be equal to anything.
		return true
	}
	switch a := a.(type) {
	case *DefType:
		if a.Def.Alias {
			panic("impossible")
		}
		b, ok := b.(*DefType)
		if !ok {
			return false
		}
		if b.Def.Alias {
			panic("impossible")
		}
		if len(a.Args) != len(b.Args) || a.Def != b.Def {
			return false
		}
		for i, aArg := range a.Args {
			bArg := b.Args[i]
			if !eqType(aArg, bArg) {
				return false
			}
		}
		return true
	case *RefType:
		b, ok := b.(*RefType)
		return ok && eqType(a.Type, b.Type)
	case *ArrayType:
		b, ok := b.(*ArrayType)
		return ok && eqType(a.ElemType, b.ElemType)
	case *StructType:
		b, ok := b.(*StructType)
		if !ok || len(a.Fields) != len(b.Fields) {
			return false
		}
		for i := range a.Fields {
			aField := &a.Fields[i]
			bField := &b.Fields[i]
			if aField.Name != bField.Name || !eqType(aField.Type, bField.Type) {
				return false
			}
		}
		return true
	case *UnionType:
		b, ok := b.(*UnionType)
		if !ok || len(a.Cases) != len(b.Cases) {
			return false
		}
		for i := range a.Cases {
			aCase := &a.Cases[i]
			bCase := &b.Cases[i]
			if aCase.Name != bCase.Name ||
				(aCase.Type == nil) != (bCase.Type == nil) ||
				(aCase.Type != nil && !eqType(aCase.Type, bCase.Type)) {
				return false
			}
		}
		return true
	case *FuncType:
		b, ok := b.(*FuncType)
		if !ok || len(a.Parms) != len(b.Parms) {
			return false
		}
		for i, aParm := range a.Parms {
			bParm := b.Parms[i]
			if !eqType(aParm, bParm) {
				return false
			}
		}
		return eqType(a.Ret, b.Ret)
	case *BasicType:
		b, ok := b.(*BasicType)
		return ok && a.Kind == b.Kind
	case *TypeVar:
		b, ok := b.(*TypeVar)
		return ok && a.Def == b.Def
	default:
		panic(fmt.Sprintf("impossible Type type: %T", a))
	}
}

func instType(typ Type) Type {
	typ = resolveAlias(typ)
	switch typ := typ.(type) {
	case nil:
		break
	case *RefType:
		typ.Type = instType(typ.Type)
	case *DefType:
		if typ.Def == nil {
			return typ
		}
		if typ.Inst = findInst(typ.Def, typ.Args); typ.Inst == nil {
			typ.Inst = newInst(typ.Def, typ.Args)
		}
	case *ArrayType:
		typ.ElemType = instType(typ.ElemType)
	case *StructType:
		for i := range typ.Fields {
			typ.Fields[i].Type = instType(typ.Fields[i].Type)
		}
	case *UnionType:
		for i := range typ.Cases {
			typ.Cases[i].Type = instType(typ.Cases[i].Type)
		}
	case *FuncType:
		for i := range typ.Parms {
			typ.Parms[i] = instType(typ.Parms[i])
		}
		typ.Ret = instType(typ.Ret)
	case *TypeVar:
		break
	case *BasicType:
		break
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
	return typ
}

func findInst(def *TypeDef, args []Type) *TypeInst {
next:
	for _, inst := range def.Insts {
		for i, a := range inst.Args {
			if !eqType(args[i], a) {
				continue next
			}
		}
		return inst
	}
	return nil
}

func newInst(def *TypeDef, args []Type) *TypeInst {
	if def.Alias {
		panic(fmt.Sprintf("should-be resolved alias: %s", def.Name))
	}
	inst := &TypeInst{Args: make([]Type, len(args))}
	sub := make(map[*TypeParm]Type, len(def.Parms))
	for i, arg := range args {
		// We are making one canonical instance.
		// For location-independence,
		// set the canonical instance's arguments
		// to the corresponding parameter locations.
		inst.Args[i] = copyTypeWithLoc(arg, def.Parms[i].L)
		sub[&def.Parms[i]] = inst.Args[i]
	}
	def.Insts = append(def.Insts, inst)
	inst.Type = subType(sub, def.Type)
	return inst
}

func subType(sub map[*TypeParm]Type, typ Type) Type {
	switch typ := typ.(type) {
	case nil:
		return nil
	case *RefType:
		copy := *typ
		copy.Type = subType(sub, typ.Type)
		return &copy
	case *DefType:
		copy := *typ
		copy.Args = nil
		for _, arg := range typ.Args {
			copy.Args = append(copy.Args, subType(sub, arg))
		}
		if typ.Inst != nil {
			return instType(&copy)
		}
		return &copy
	case *ArrayType:
		copy := *typ
		copy.ElemType = subType(sub, typ.ElemType)
		return &copy
	case *StructType:
		var copy StructType
		for i := range typ.Fields {
			f := typ.Fields[i]
			f.Type = subType(sub, f.Type)
			copy.Fields = append(copy.Fields, f)
		}
		return &copy
	case *UnionType:
		var copy UnionType
		for i := range typ.Cases {
			c := typ.Cases[i]
			c.Type = subType(sub, c.Type)
			copy.Cases = append(copy.Cases, c)
		}
		return &copy
	case *FuncType:
		var copy FuncType
		for _, p := range typ.Parms {
			copy.Parms = append(copy.Parms, subType(sub, p))
		}
		copy.Ret = subType(sub, typ.Ret)
		return &copy
	case *TypeVar:
		if s, ok := sub[typ.Def]; ok {
			return s
		}
		copy := *typ
		return &copy
	case *BasicType:
		copy := *typ
		return &copy
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}

func copyTypeWithLoc(typ Type, l loc.Loc) Type {
	switch typ := typ.(type) {
	case nil:
		return nil
	case *RefType:
		return &RefType{
			Type: copyTypeWithLoc(typ.Type, l),
			L:    l,
		}
	case *DefType:
		copy := *typ
		for i := range copy.Args {
			copy.Args[i] = copyTypeWithLoc(copy.Args[i], l)
		}
		copy.L = l
		return &copy
	case *ArrayType:
		return &ArrayType{
			ElemType: copyTypeWithLoc(typ.ElemType, l),
			L:        l,
		}
	case *StructType:
		var copy StructType
		for i := range typ.Fields {
			f := typ.Fields[i]
			f.L = l
			f.Type = copyTypeWithLoc(f.Type, l)
			copy.Fields = append(copy.Fields, f)
		}
		copy.L = l
		return &copy
	case *UnionType:
		var copy UnionType
		for i := range typ.Cases {
			c := typ.Cases[i]
			c.L = l
			c.Type = copyTypeWithLoc(c.Type, l)
			copy.Cases = append(copy.Cases, c)
		}
		copy.L = l
		return &copy
	case *FuncType:
		var copy FuncType
		for _, p := range typ.Parms {
			copy.Parms = append(copy.Parms, copyTypeWithLoc(p, l))
		}
		copy.Ret = copyTypeWithLoc(typ.Ret, l)
		copy.L = l
		return &copy
	case *TypeVar:
		copy := *typ
		copy.L = l
		return &copy
	case *BasicType:
		copy := *typ
		copy.L = l
		return &copy
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}
