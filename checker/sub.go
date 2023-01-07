package checker

import (
	"fmt"
)

type bindings struct {
	Parms  map[*ParmDef]*ParmDef
	Locals map[*LocalDef]*LocalDef
	Caps   map[*BlockCap]*BlockCap
	Types  map[*TypeParm]Type
	Funcs  map[*FuncDecl]Func
}

func subFuncInstExprs(inst *FuncInst) {
	bindings := bindings{
		Parms:  make(map[*ParmDef]*ParmDef),
		Locals: make(map[*LocalDef]*LocalDef),
		Caps:   make(map[*BlockCap]*BlockCap),
		Types:  make(map[*TypeParm]Type),
		Funcs:  make(map[*FuncDecl]Func),
	}

	// Here we use the Def.TypeParms instead of the inst.typeParms
	// to populate the substitution map, because we are going to be substituting
	// from the fields of Def, not from the fields of the FuncInst itself.
	for i := range inst.TypeArgs {
		bindings.Types[&inst.Def.TypeParms[i]] = inst.TypeArgs[i]
	}
	inst.Parms = make([]*ParmDef, 0, len(inst.Def.Parms))
	for i := range inst.Def.Parms {
		parm := &inst.Def.Parms[i]
		instParm := &ParmDef{
			Name: parm.Name,
			T:    subType(bindings.Types, parm.T),
			L:    parm.L,
		}
		bindings.Parms[parm] = instParm
		inst.Parms = append(inst.Parms, instParm)
	}

	var argI int
	for i := range inst.Def.Parms {
		p := &inst.Def.Parms[i]
		for i := range p.Constraints {
			decl := &p.Constraints[i]
			arg := inst.ConstraintArgs[argI]
			if arg == nil {
				panic("bad nil constraint arg")
			}
			if _, ok := arg.(*FuncDecl); ok {
				panic(fmt.Sprintf("%s bad decl mapping %s => %s", inst, decl, arg))
			}
			argI++
			bindings.Funcs[decl] = arg
			// If the ConstraintArg is a parameter access,
			// then we need to add the parameter's def
			// to the Parms slice now.
			if exprFunc, ok := arg.(*ExprFunc); ok {
				if cvt, ok := exprFunc.Expr.(*Convert); ok {
					if p, ok := cvt.Expr.(*Parm); ok {
						inst.Parms = append(inst.Parms, p.Def)
					}
				}
			}
		}
	}
	for i := range inst.Def.Constraints {
		decl := &inst.Def.Constraints[i]
		arg := inst.ConstraintArgs[argI]
		if _, ok := arg.(*FuncDecl); ok {
			panic(fmt.Sprintf("%s bad decl mapping %s => %s", inst, decl, arg))
		}
		argI++
		bindings.Funcs[decl] = arg
		if exprFunc, ok := arg.(*ExprFunc); ok {
			if cvt, ok := exprFunc.Expr.(*Convert); ok {
				inst.Parms = append(inst.Parms, cvt.Expr.(*Parm).Def)
			}
		}
	}

	inst.Locals = make([]*LocalDef, 0, len(inst.Def.Locals))
	for _, local := range inst.Def.Locals {
		instLocal := &LocalDef{
			Name: local.Name,
			T:    subType(bindings.Types, local.T),
			L:    local.L,
		}
		bindings.Locals[local] = instLocal
		inst.Locals = append(inst.Locals, instLocal)
	}
	inst.Exprs = make([]Expr, 0, len(inst.Def.Exprs))
	for _, expr := range inst.Def.Exprs {
		inst.Exprs = append(inst.Exprs, expr.subExpr(bindings))
	}
}

func subFunc(bindings bindings, fun Func) Func {
	switch fun := fun.(type) {
	case *FuncDecl:
		b, ok := bindings.Funcs[fun]
		if !ok {
			panic(fmt.Sprintf("failed to find a binding for %s (%p)", fun, fun))
		}
		return b
	case *FuncInst:
		// Here we are creating a new, substituted FuncInst;
		// we are not instantiating it's Exprs.
		// If the instance turns out to be a new canonical
		// and all of its types are non-variables,
		// it will be added to the Mod.toSub list
		// and its Exprs will be substituted on the next round.
		copy := &FuncInst{
			TypeArgs:       subTypes(bindings.Types, fun.TypeArgs),
			ConstraintArgs: subFuncs(bindings, fun.ConstraintArgs),
			Def:            fun.Def,
			T:              subType(bindings.Types, fun.T).(*FuncType),
		}
		// Here we memoize the function instance,
		// to get the memoized version
		// or add this version to the Insts table.
		return memoizeFuncInst(copy)
	case *Select:
		structCopy := subType(bindings.Types, fun.Struct).(*StructType)
		var fieldCopy *FieldDef
		for i := range fun.Struct.Fields {
			if fun.Field == &fun.Struct.Fields[i] {
				fieldCopy = &structCopy.Fields[i]
			}
		}
		if fieldCopy == nil {
			panic("impossible field not found")
		}
		return &Select{Struct: structCopy, Field: fieldCopy}
	case *Switch:
		unionCopy := subType(bindings.Types, fun.Union).(*UnionType)
		casesCopy := make([]*CaseDef, len(fun.Cases))
		for i, cas := range fun.Cases {
			if cas == nil {
				continue
			}
			for j := range fun.Union.Cases {
				if cas == &fun.Union.Cases[j] {
					casesCopy[i] = &unionCopy.Cases[j]
				}
			}
			if casesCopy[i] == nil {
				panic("impossible case not found")
			}
		}
		return &Switch{
			N:     fun.N,
			Names: fun.Names,
			T:     subType(bindings.Types, fun.T).(*FuncType),
			Union: unionCopy,
			Cases: casesCopy,
		}
	case *Builtin:
		return &Builtin{
			Op:    fun.Op,
			Parms: subTypes(bindings.Types, fun.Parms),
			Ret:   subType(bindings.Types, fun.Ret),
		}
	case *ExprFunc:
		return &ExprFunc{
			Expr:     fun.Expr.subExpr(bindings),
			FuncType: subType(bindings.Types, fun.FuncType).(*FuncType),
		}
	default:
		panic(fmt.Sprintf("bad Func type: %T", fun))
	}
}

func subFuncs(bindings bindings, funcs []Func) []Func {
	copy := make([]Func, 0, len(funcs))
	for _, fun := range funcs {
		copy = append(copy, subFunc(bindings, fun))
	}
	return copy
}

func subExprs(bindings bindings, exprs []Expr) []Expr {
	var copy []Expr
	for _, expr := range exprs {
		copy = append(copy, expr.subExpr(bindings))
	}
	return copy
}

func (c *Call) subExpr(bindings bindings) Expr {
	if _, ok := c.Func.(*FuncDecl); !ok {
		// This is a short-cut for the common case: substituting a non-funcdecl.
		// The argument types and return type will not change.
		return &Call{
			Func: subFunc(bindings, c.Func),
			Args: subExprs(bindings, c.Args),
			T:    subType(bindings.Types, c.T),
			L:    c.L,
		}
	}

	// Here we are substituting a FuncDecl -- an iface call.
	// The function actually called may have
	// different argument types and/or a different return type.
	// The types are guaranteed to be implicitly convertable,
	// so we first substitute and then call convert().
	// Interface instantiation guarantees that the convert() cannot fail,
	// otherwise we would have returned an error
	// before getting to substitution.

	fun := subFunc(bindings, c.Func)
	args := subExprs(bindings, c.Args)
	for i := range args {
		// Here we explicit=true to allow re-converting
		// an explicit convert argument
		// into the type of the iface function parameter
		// in the case tha the iface argument has parameters
		// differing from the iface declaration in reference count.
		// For example:
		/*
			func foo(s S) : bar(&S) {
				bar(&S :: s)
			}
			func bar(_ &string)
			func main() {
				str_ref := &string :: "",
				foo(str_ref)
			}
		*/
		// when passing (&S :: s) to bar, it will be &&string
		// coming out of an explicit convert.
		// We need to be able to convert it back to &string
		// to pass to bar(&string).
		args[i] = mustConvertExpr(args[i], fun.parm(i), explicit)
	}
	call := &Call{
		Func: fun,
		Args: args,
		T:    refLiteral(fun.ret().groundType()),
		L:    c.L,
	}
	return mustConvertExpr(call, pattern(subType(bindings.Types, c.T)), implicit)
}

func (c *Convert) subExpr(bindings bindings) Expr {
	return &Convert{
		Kind:     c.Kind,
		Explicit: c.Explicit,
		Expr:     c.Expr.subExpr(bindings),
		T:        subType(bindings.Types, c.T),
		L:        c.L,
	}
}

func (v *Var) subExpr(bindings bindings) Expr {
	// We copy the variable, but do not substitute the type,
	// since module variables cannot have variadaic types.
	return &Var{Def: v.Def, T: v.T, L: v.L}
}

func (l *Local) subExpr(bindings bindings) Expr {
	def, ok := bindings.Locals[l.Def]
	if !ok {
		panic("no binding")
	}
	return &Local{
		Def: def,
		T:   subType(bindings.Types, l.T),
		L:   l.L,
	}
}

func (p *Parm) subExpr(bindings bindings) Expr {
	def, ok := bindings.Parms[p.Def]
	if !ok {
		return &Parm{
			Def: p.Def,
			T:   subType(bindings.Types, p.T),
			L:   p.L,
		}
	}
	return &Parm{
		Def: def,
		T:   subType(bindings.Types, p.T),
		L:   p.L,
	}
}

func (c *Cap) subExpr(bindings bindings) Expr {
	def, ok := bindings.Caps[c.Def]
	if !ok {
		panic("no binding")
	}
	return &Cap{
		Def: def,
		T:   subType(bindings.Types, c.T),
		L:   c.L,
	}
}

func (a *ArrayLit) subExpr(bindings bindings) Expr {
	return &ArrayLit{
		Array: subType(bindings.Types, a.Array).(*ArrayType),
		Elems: subExprs(bindings, a.Elems),
		T:     subType(bindings.Types, a.T),
		L:     a.L,
	}
}

func (s *StructLit) subExpr(bindings bindings) Expr {
	return &StructLit{
		Struct: subType(bindings.Types, s.Struct).(*StructType),
		Fields: subExprs(bindings, s.Fields),
		T:      subType(bindings.Types, s.T),
		L:      s.L,
	}
}

func (u *UnionLit) subExpr(bindings bindings) Expr {
	unionCopy := subType(bindings.Types, u.Union).(*UnionType)
	var caseCopy *CaseDef
	for i := 0; i < len(u.Union.Cases); i++ {
		if u.Case == &u.Union.Cases[i] {
			caseCopy = &unionCopy.Cases[i]
		}
	}
	if caseCopy == nil {
		panic("impossible case not found")
	}
	var valCopy Expr
	if u.Val != nil {
		valCopy = u.Val.subExpr(bindings)
	}
	return &UnionLit{
		Union: unionCopy,
		Case:  caseCopy,
		Val:   valCopy,
		T:     subType(bindings.Types, u.T),
		L:     u.L,
	}
}

func (b *BlockLit) subExpr(bindings bindings) Expr {
	capsCopy := make([]*BlockCap, 0, len(b.Caps))
	for _, cap := range b.Caps {
		capCopy := &BlockCap{
			Name:  cap.Name,
			T:     subType(bindings.Types, cap.T),
			L:     cap.L,
			Parm:  bindings.Parms[cap.Parm],
			Local: bindings.Locals[cap.Local],
			Cap:   bindings.Caps[cap.Cap],
		}
		if cap.Expr != nil {
			capCopy.Expr = cap.Expr.subExpr(bindings)
		}
		if capCopy.Parm == nil && capCopy.Local == nil && capCopy.Cap == nil && capCopy.Expr == nil {
			panic("no binding")
		}
		bindings.Caps[cap] = capCopy
		capsCopy = append(capsCopy, capCopy)
	}
	parmsCopy := make([]ParmDef, 0, len(b.Parms))
	for _, parm := range b.Parms {
		parmsCopy = append(parmsCopy, ParmDef{
			Name: parm.Name,
			T:    subType(bindings.Types, parm.T),
			L:    parm.L,
		})
	}
	for i := range b.Parms {
		bindings.Parms[&b.Parms[i]] = &parmsCopy[i]
	}
	localsCopy := make([]*LocalDef, 0, len(b.Locals))
	for _, local := range b.Locals {
		localCopy := &LocalDef{
			Name: local.Name,
			T:    subType(bindings.Types, local.T),
			L:    local.L,
		}
		bindings.Locals[local] = localCopy
		localsCopy = append(localsCopy, localCopy)
	}
	return &BlockLit{
		Caps:   capsCopy,
		Parms:  parmsCopy,
		Locals: localsCopy,
		Ret:    subType(bindings.Types, b.Ret),
		Exprs:  subExprs(bindings, b.Exprs),
		T:      subType(bindings.Types, b.T),
		L:      b.L,
	}
}

func (s *StrLit) subExpr(bindings bindings) Expr {
	// Just return a copy, since the type cannot be a type variable.
	return &StrLit{Text: s.Text, T: s.T, L: s.L}
}

func (n *IntLit) subExpr(bindings bindings) Expr {
	// Just return a copy, since the type cannot be a type variable.
	return &IntLit{Text: n.Text, Val: n.Val, T: n.T, L: n.L}
}

func (f *FloatLit) subExpr(bindings bindings) Expr {
	// Just return a copy, since the type cannot be a type variable.
	return &FloatLit{Text: f.Text, Val: f.Val, T: f.T, L: f.L}
}

func subType(sub map[*TypeParm]Type, typ Type) Type {
	if sub == nil {
		return typ
	}
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
		return instType(&copy)
	case *ArrayType:
		copy := *typ
		copy.ElemType = subType(sub, typ.ElemType)
		return &copy
	case *StructType:
		copy := *typ
		copy.Fields = nil
		for i := range typ.Fields {
			f := typ.Fields[i]
			f.Type = subType(sub, f.Type)
			copy.Fields = append(copy.Fields, f)
		}
		return &copy
	case *UnionType:
		copy := *typ
		copy.Cases = nil
		for i := range typ.Cases {
			c := typ.Cases[i]
			c.Type = subType(sub, c.Type)
			copy.Cases = append(copy.Cases, c)
		}
		return &copy
	case *FuncType:
		copy := *typ
		copy.Parms = nil
		for _, p := range typ.Parms {
			copy.Parms = append(copy.Parms, subType(sub, p))
		}
		copy.Ret = subType(sub, typ.Ret)
		return &copy
	case *TypeVar:
		if sub != nil {
			if s, ok := sub[typ.Def]; ok {
				return s
			}
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

func subTypes(sub map[*TypeParm]Type, types []Type) []Type {
	copy := make([]Type, 0, len(types))
	for _, t := range types {
		copy = append(copy, subType(sub, t))
	}
	return copy
}
