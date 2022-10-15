package checker

import (
	"fmt"

	"github.com/eaburns/pea/loc"
)

func isEnd(typ Type) bool {
	basic, ok := typ.(*BasicType)
	return ok && basic.Kind == End
}

func isEmptyStruct(typ Type) bool {
	s, ok := typ.(*StructType)
	return ok && len(s.Fields) == 0
}

func refLiteral(typ Type) Type {
	if typ == nil {
		return nil
	}
	return &RefType{Type: typ, L: typ.Loc()}
}

func isRefLiteral(typ Type) bool {
	_, ok := typ.(*RefType)
	return ok
}

func arrayLiteral(typ Type) Type {
	if typ == nil {
		return nil
	}
	return &ArrayType{ElemType: typ, L: typ.Loc()}
}

var (
	boolUnion = &UnionType{
		Cases: []CaseDef{
			{Name: "false?"},
			{Name: "true?"},
		},
	}
	orderingUnion = &UnionType{
		Cases: []CaseDef{
			{Name: "less?"},
			{Name: "equal?"},
			{Name: "greater?"},
		},
	}
)

// literalType returns the literal type or nil if the type cannot be converted to a literal.
func literalType(typ Type) Type {
	switch typ := typ.(type) {
	case nil:
		return nil
	case *RefType:
		if typ.Type == nil {
			return nil
		}
		lit := literalType(typ.Type)
		if lit == nil {
			return nil
		}
		return &RefType{Type: lit, L: typ.L}
	case *DefType:
		if typ.Inst == nil || typ.Inst.Type == nil {
			return nil
		}
		if !isVisibleDefinedType(typ) {
			return nil
		}
		return literalType(typ.Inst.Type)
	case *ArrayType:
		return typ
	case *StructType:
		return typ
	case *UnionType:
		return typ
	case *FuncType:
		return typ
	case *BasicType:
		switch typ.Kind {
		case Bool:
			return copyTypeWithLoc(boolUnion, typ.Loc())
		case Ordering:
			return copyTypeWithLoc(orderingUnion, typ.Loc())
		default:
			return nil
		}
	default:
		return nil
	}
}

func isVisibleDefinedType(typ Type) bool {
	if isBool(typ) || isOrdering(typ) {
		return true
	}
	defType, ok := typ.(*DefType)
	if !ok {
		return false
	}
	def := defType.Def
	return !def.File.Mod.Imported || def.Exp && !def.Opaque
}

func isLiteralType(typ Type) bool {
	switch typ := typ.(type) {
	case nil:
		return true
	case *RefType:
		return isLiteralType(typ.Type)
	case *DefType:
		return false
	case *TypeVar:
		return false
	case *BasicType:
		return false
	default:
		return true
	}
}

// funcType returns a *FuncType if typ is a function type, otherwise nil.
func funcType(typ Type) *FuncType {
	switch typ := typ.(type) {
	case *FuncType:
		return typ
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return funcType(typ.Inst.Type)
		}
	}
	return nil
}

func isStructType(typ Type) bool {
	switch typ := typ.(type) {
	case *StructType:
		return true
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return isStructType(typ.Inst.Type)
		}
	}
	return false
}

func isStructRefType(typ Type) bool {
	switch typ := typ.(type) {
	case *RefType:
		return isStructType(typ.Type)
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return isStructRefType(typ.Inst.Type)
		}
	}
	return false
}

func isUnionType(typ Type) bool {
	switch typ := typ.(type) {
	case *UnionType:
		return true
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return isUnionType(typ.Inst.Type)
		}
	case *BasicType:
		return typ.Kind == Bool || typ.Kind == Ordering
	}
	return false
}

func isUnionRefType(typ Type) bool {
	switch typ := typ.(type) {
	case *RefType:
		return isUnionType(typ.Type)
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return isUnionRefType(typ.Inst.Type)
		}
	}
	return false
}

func isByteArray(typ Type) bool {
	return isArrayType(typ) &&
		basicKind(literalType(typ).(*ArrayType).ElemType) == Uint8
}

func isArrayType(typ Type) bool {
	switch typ := typ.(type) {
	case *ArrayType:
		return true
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return isArrayType(typ.Inst.Type)
		}
	}
	return false
}

func isStringType(typ Type) bool {
	switch typ := typ.(type) {
	case *DefType:
		if typ.Inst != nil && typ.Inst.Type != nil && isVisibleDefinedType(typ) {
			return isStringType(typ.Inst.Type)
		}
	case *BasicType:
		return typ.Kind == String
	}
	return false
}

func isUintRef(typ Type) bool {
	basic, ok := typ.(*BasicType)
	return ok && basic.Kind == UintRef
}

func isBool(typ Type) bool {
	basic, ok := typ.(*BasicType)
	return ok && basic.Kind == Bool
}

func isOrdering(typ Type) bool {
	basic, ok := typ.(*BasicType)
	return ok && basic.Kind == Ordering
}

func isBasicNum(typ Type) bool {
	basic, ok := typ.(*BasicType)
	if !ok {
		return false
	}
	switch basic.Kind {
	case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintRef, Float32, Float64:
		return true
	default:
		return false
	}
}

func basicType(typ Type) Type {
	switch typ := typ.(type) {
	case nil:
		return nil
	case *RefType:
		if typ.Type == nil {
			return nil
		}
		lit := basicType(typ.Type)
		if lit == nil {
			return nil
		}
		return &RefType{Type: lit, L: typ.L}
	case *DefType:
		if typ.Inst == nil || typ.Inst.Type == nil || !isVisibleDefinedType(typ) {
			return nil
		}
		return basicType(typ.Inst.Type)
	case *BasicType:
		return typ
	default:
		return nil
	}
}

func basicKind(typ Type) BasicTypeKind {
	switch typ := typ.(type) {
	case nil:
		return noBasicTypeKind
	case *RefType:
		if typ.Type == nil {
			return noBasicTypeKind
		}
		return basicKind(typ.Type)
	case *DefType:
		if typ.Inst == nil || typ.Inst.Type == nil || !isVisibleDefinedType(typ) {
			return noBasicTypeKind
		}
		return basicKind(typ.Inst.Type)
	case *BasicType:
		return typ.Kind
	default:
		return noBasicTypeKind
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

func eqType(a, b Type) bool {
	if a == nil || b == nil {
		// nil indicates an error; just be equal to anything.
		return true
	}
	switch a := a.(type) {
	case *DefType:
		if a.Def.Alias {
			panic(fmt.Sprintf("impossible alias: %s", a))
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
		if !ok {
			return false
		}
		return ok && (a.Def == nil && b.Def == nil && a.Name == b.Name || a.Def == b.Def)
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
		var args []Type
		for _, a := range typ.Args {
			args = append(args, instType(resolveAlias(a)))
		}
		if typ.Inst = findInst(typ.Def, args); typ.Inst == nil {
			typ.Inst = newInst(typ.Def, args)
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

func resolveAlias(typ Type) Type {
	defType, ok := typ.(*DefType)
	if !ok || !defType.Def.Alias {
		return typ
	}
	aliased := copyTypeWithLoc(defType.Def.Type, defType.L)
	sub := make(map[*TypeParm]Type)
	for i, arg := range defType.Args {
		sub[&defType.Def.Parms[i]] = arg
	}
	return resolveAlias(subType(sub, aliased))
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
		copy.Args = append([]Type{}, typ.Args...) // copy the slice
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

func hasTypeVariable(typ Type) bool {
	switch typ := typ.(type) {
	case nil:
		return false
	case *RefType:
		return hasTypeVariable(typ.Type)
	case *DefType:
		for _, arg := range typ.Args {
			if hasTypeVariable(arg) {
				return true
			}
		}
		return false
	case *ArrayType:
		return hasTypeVariable(typ.ElemType)
	case *StructType:
		for i := range typ.Fields {
			if hasTypeVariable(typ.Fields[i].Type) {
				return true
			}
		}
		return false
	case *UnionType:
		for i := range typ.Cases {
			if hasTypeVariable(typ.Cases[i].Type) {
				return true
			}
		}
		return false
	case *FuncType:
		for i := range typ.Parms {
			if hasTypeVariable(typ.Parms[i]) {
				return true
			}
		}
		return hasTypeVariable(typ.Ret)
	case *TypeVar:
		return true
	case *BasicType:
		return false
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}
