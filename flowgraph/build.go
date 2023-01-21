package flowgraph

import (
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"

	"github.com/eaburns/pea/checker"
	"github.com/eaburns/pea/loc"
)

type Option func(*modBuilder)

var (
	// NoOptimize disables all optimization passes.
	NoOptimize    Option = func(mb *modBuilder) { mb.noOptimize = true }
	TraceEscape          = func(mb *modBuilder) { mb.traceEsc = true }
	TraceInlining        = func(mb *modBuilder) { mb.traceInline = true }
)

func Build(mod *checker.Mod, opts ...Option) *Mod {
	mb := newModBuilder(mod.Path)
	for _, o := range opts {
		o(mb)
	}
	mb.Deps = mod.Deps
	// Set the Imported bit so .String() methods add the path.
	mod.Imported = true
	mb.buildDefs(mod)
	mb.topoSortFuncs()
	if !mb.noOptimize {
		mb.optimize()
	}
	for _, fb := range mb.funcBuilders {
		mb.Funcs = append(mb.Funcs, fb.FuncDef)
	}
	return mb.Mod
}

type modBuilder struct {
	*Mod
	typeDef      map[*checker.TypeInst]Type
	varDef       map[*checker.VarDef]*VarDef
	funcDef      map[*checker.FuncInst]*funcBuilder
	funcBuilders []*funcBuilder

	nextInstr int
	nextBlock int

	trace bool

	// options
	noOptimize  bool
	traceEsc    bool
	traceInline bool
}

type funcBuilder struct {
	mod *modBuilder
	*FuncDef
	b0 *blockBuilder

	// functions referenced by this function
	// and functions referencing this function.
	outRefs map[*funcBuilder]int
	inRefs  map[*funcBuilder]int
	// inlineNonBlocks is the number
	// of non-block-literal functions inlined.
	inlineNonBlocks int

	parmDef    map[*checker.ParmDef]*ParmDef
	parmAlloc  map[*ParmDef]*Alloc
	localAlloc map[*checker.LocalDef]*Alloc
	capField   map[*checker.BlockCap]*FieldDef

	// fields set for block literals.
	parent      *funcBuilder
	blockType   *StructType
	longReturns bool
	returnField *FieldDef
	frameField  *FieldDef
}

type blockBuilder struct {
	fun *funcBuilder
	*BasicBlock
	L    loc.Loc
	done bool
}

func (mb *modBuilder) buildDefs(mod *checker.Mod) {
	var varInits []checker.Expr
	for _, d := range mod.Defs {
		if d, ok := d.(*checker.VarDef); ok {
			v := &VarDef{
				Mod:  mb.Mod.Path,
				Name: d.Name,
				Type: mb.buildType(d.T),
			}
			mb.Vars = append(mb.Vars, v)
			varInits = append(varInits, d.Expr)
			mb.varDef[d] = v
		}
	}
	for _, d := range mod.Defs {
		switch d := d.(type) {
		case *checker.FuncDef:
			for _, n := range d.Insts {
				mb.buildFuncInst(n)
			}
		case *checker.TestDef:
			mb.buildTestDef(d)
		}
	}

	fb := mb.newFuncBuilder(mod.Path, "<init>", nil, &checker.StructType{}, loc.Loc{})
	fb.SourceName = mod.Path + " <init>"
	b0 := fb.buildBlock0(nil)
	var b1 *blockBuilder
	if len(varInits) > 0 {
		b1 = fb.buildBlocks(varInits)
	} else {
		b1 = fb.newBlock(loc.Loc{})
		b1.Return(nil)
	}
	b0.jump(b1)
	mb.Init = fb.FuncDef
}

func (mb *modBuilder) topoSortFuncs() {
	var sorted []*funcBuilder
	done := make(map[*funcBuilder]bool)
	var refs []*funcBuilder
	var sortFunc func(*funcBuilder)
	sortFunc = func(fb *funcBuilder) {
		if done[fb] {
			return
		}
		done[fb] = true
		refs := refs[:0]
		for f := range fb.outRefs {
			refs = append(refs, f)
		}
		sort.Slice(refs, func(i, j int) bool { return refs[i].SourceName < refs[j].SourceName })
		for _, f := range refs {
			sortFunc(f)
		}
		sorted = append(sorted, fb)
	}
	for _, f := range mb.funcBuilders {
		sortFunc(f)
	}
	mb.funcBuilders = sorted
}

func newModBuilder(path string) *modBuilder {
	return &modBuilder{
		Mod:     &Mod{Path: path},
		typeDef: make(map[*checker.TypeInst]Type),
		varDef:  make(map[*checker.VarDef]*VarDef),
		funcDef: make(map[*checker.FuncInst]*funcBuilder),
	}
}

func (mb *modBuilder) intType() *IntType {
	// TODO: set the size based on a config param.
	return &IntType{Size: 64}
}

func (mb *modBuilder) stringType() *AddrType {
	return &AddrType{
		Elem: &StructType{
			Name: "string",
			Fields: []*FieldDef{
				{Num: 0, Name: "length", Type: mb.intType()},
				{Num: 1, Name: "data", Type: &ArrayType{
					Elem: &IntType{Size: 8, Unsigned: true},
				}},
			},
		},
	}
}

func isEnd(typ checker.Type) bool {
	basic, ok := typ.(*checker.BasicType)
	return ok && basic.Kind == checker.End
}

func (mb *modBuilder) buildType(typ checker.Type) Type {
	switch typ := typ.(type) {
	case *checker.DefType:
		if t, ok := mb.typeDef[typ.Inst]; ok {
			return t
		}
		// Add a binding, in case this becomes recursive.
		//
		// Add an empty field so that t.isEmpty()==false;
		// if the struct is recursive, it cannot be empty,
		// because it must at least have a reference to itself.
		// This makes it non-empty.
		// Note that if it is not recursive, isEmpty won't be called
		// during the recursive buildType here,
		// so the empty field in that case is ignored.
		t := &StructType{Fields: []*FieldDef{{Name: "!!" + typ.String() + "!!"}}}
		mb.typeDef[typ.Inst] = t

		// There are three important cases to handle:
		// 1) The defined type is a struct. It may be recursive,
		// 	and the recursive definition uses t.
		// 	Copy the struct we built into t,
		//	and set the struct name so the backend
		// 	can build a recursive struct definition with it.
		// 2) The defined type is an address. It may be recursive,
		// 	but the recursive definition uses StructType t
		// 	instead of an AddrType.
		// 	Further, the LLVM backend cannot define
		// 	recursive pointer types, only structs.
		// 	We break the recursive in the type
		// 	by replacing all references to the t StructType
		// 	with *struct{}, a void pointer.
		// 	The flowgraph and llvm backends
		// 	both will convert this automatically
		// 	to any other pointer type, and our def is a pointer
		// 	so we've got it.
		// 3) The defined type is any non-struct, non-address type.
		// 	It cannot be recursive; it must be just a value.
		// 	Just return that type.
		switch inner := mb.buildType(typ.Inst.Type).(type) {
		case *StructType:
			*t = *inner
			t.Mod = typ.Def.Mod
			t.Name = typ.Name
			for _, a := range typ.Args {
				t.Args = append(t.Args, mb.buildType(a))
			}
			t.SourceName = typ.String()
			mb.Types = append(mb.Types, t)
			return t
		case *AddrType:
			var ret Type = inner
			fixDefAddrType(make(map[Type]bool), &ret, t)
			mb.typeDef[typ.Inst] = inner
			if s, ok := inner.Elem.(*StructType); ok && s.Mod == "" && s.Name == "" {
				s.Mod = typ.Def.Mod
				s.Name = typ.Def.Name
				for _, a := range typ.Args {
					s.Args = append(s.Args, mb.buildType(a))
				}
				mb.Types = append(mb.Types, s)
			}
			return ret
		default:
			mb.typeDef[typ.Inst] = inner
			return inner
		}

	case *checker.RefType:
		return &AddrType{Elem: mb.buildType(typ.Type)}

	case *checker.ArrayType:
		elem := mb.buildType(typ.ElemType)
		return &StructType{
			Name: "array",
			Args: []Type{elem},
			Fields: []*FieldDef{
				{Num: 0, Name: "length", Type: mb.intType()},
				{Num: 1, Name: "data", Type: &ArrayType{Elem: elem}},
			},
		}

	case *checker.StructType:
		fs := make([]*FieldDef, 0, len(typ.Fields))
		for i, f := range typ.Fields {
			typ := mb.buildType(f.Type)
			if typ.isEmpty() {
				continue
			}
			fs = append(fs, &FieldDef{
				Num:  i,
				Name: strings.TrimPrefix(f.Name, "."),
				Type: typ,
			})
		}
		return &StructType{Fields: fs}

	case *checker.UnionType:
		var cs []*CaseDef
		for _, c := range typ.Cases {
			if c.Type == nil {
				continue
			}
			typ := mb.buildType(c.Type)
			if typ.isEmpty() {
				continue
			}
			cs = append(cs, &CaseDef{
				Name: strings.TrimSuffix(c.Name, "?"),
				Type: typ,
			})
		}
		switch {
		case len(cs) == 0:
			switch {
			case len(typ.Cases) > math.MaxUint32:
				return &IntType{Size: 64, Unsigned: true}
			case len(typ.Cases) > math.MaxUint16:
				return &IntType{Size: 32, Unsigned: true}
			case len(typ.Cases) > math.MaxUint8:
				return &IntType{Size: 16, Unsigned: true}
			default:
				return &IntType{Size: 8, Unsigned: true}
			}
		case isPointer(typ):
			return cs[0].Type
		default:
			return &StructType{
				Fields: []*FieldDef{
					{Num: 0, Name: "tag", Type: mb.intType()},
					{Num: 1, Name: "data", Type: &UnionType{Cases: cs}},
				},
			}
		}

	case *checker.FuncType:
		funcType := &FuncType{Parms: make([]Type, len(typ.Parms)+1)}
		funcType.Parms[0] = &AddrType{Elem: &StructType{}} // closure
		for i, p := range typ.Parms {
			funcType.Parms[i+1] = mb.buildType(p)
		}
		funcType.Ret = mb.buildType(typ.Ret)
		funcType.Terminal = isEnd(typ.Ret)
		return &StructType{
			Name: "block",
			Args: []Type{funcType},
			Fields: []*FieldDef{
				{Num: 0, Name: "func", Type: funcType},
				{Num: 1, Name: "caps", Type: funcType.Parms[0]},
			},
		}

	case *checker.BasicType:
		switch typ.Kind {
		case checker.End:
			return &StructType{}
		case checker.Int:
			return mb.intType()
		case checker.Int8:
			return &IntType{Size: 8}
		case checker.Int16:
			return &IntType{Size: 16}
		case checker.Int32:
			return &IntType{Size: 32}
		case checker.Int64:
			return &IntType{Size: 64}
		case checker.UintRef:
			i := mb.intType()
			i.Unsigned = true
			return i
		case checker.Uint:
			i := mb.intType()
			i.Unsigned = true
			return i
		case checker.Uint8:
			return &IntType{Size: 8, Unsigned: true}
		case checker.Uint16:
			return &IntType{Size: 16, Unsigned: true}
		case checker.Uint32:
			return &IntType{Size: 32, Unsigned: true}
		case checker.Uint64:
			return &IntType{Size: 64, Unsigned: true}
		case checker.Float32:
			return &FloatType{Size: 32}
		case checker.Float64:
			return &FloatType{Size: 64}
		case checker.String:
			// stringType() returns the address of the string struct,
			// but here we need the struct itself.
			return mb.stringType().Elem
		default:
			panic(fmt.Sprintf("bad checker.BasicKind: %d", typ.Kind))
		}

	default:
		panic(fmt.Sprintf("bad checker.Type type: %T", typ))
	}
}

func fixDefAddrType(seen map[Type]bool, typ *Type, s *StructType) {
	if st, ok := (*typ).(*StructType); ok && st == s {
		*typ = &AddrType{Elem: &StructType{}}
		return
	}
	if seen[*typ] {
		return
	}
	seen[*typ] = true
	typePointer := typ
	switch typ := (*typ).(type) {
	case nil:
		return
	case *IntType:
	case *FloatType:
	case *AddrType:
		fixDefAddrType(seen, &typ.Elem, s)
	case *ArrayType:
		fixDefAddrType(seen, &typ.Elem, s)
	case *FrameType:
	case *StructType:
		for i := range typ.Args {
			fixDefAddrType(seen, &typ.Args[i], s)
		}
		for _, f := range typ.Fields {
			fixDefAddrType(seen, &f.Type, s)
		}
	case *UnionType:
		for _, c := range typ.Cases {
			fixDefAddrType(seen, &c.Type, s)
		}
		// A case may have changed from &StructType{}
		// to an &AddrType{}, so re-do pointer optimization.
		if len(typ.Cases) == 2 {
			if isAddr(typ.Cases[0].Type) && typ.Cases[1].Type == nil {
				*typePointer = typ.Cases[0].Type
			} else if typ.Cases[0].Type == nil && isAddr(typ.Cases[1].Type) {
				*typePointer = typ.Cases[1].Type
			}
		}
	case *FuncType:
		for i := range typ.Parms {
			fixDefAddrType(seen, &typ.Parms[i], s)
		}
		fixDefAddrType(seen, &typ.Ret, s)
	default:
		panic(fmt.Sprintf("bad Type type: %T", typ))
	}
}

func isAddr(typ Type) bool {
	_, ok := typ.(*AddrType)
	return ok
}

func isPointer(typ *checker.UnionType) bool {
	var isRef func(checker.Type) bool
	isRef = func(typ checker.Type) bool {
		switch typ := typ.(type) {
		case *checker.RefType:
			return true
		case *checker.DefType:
			return isRef(typ.Inst.Type)
		default:
			return false
		}
	}
	return len(typ.Cases) == 2 &&
		(typ.Cases[0].Type == nil || typ.Cases[1].Type == nil) &&
		(isRef(typ.Cases[0].Type) || isRef(typ.Cases[1].Type))
}

func (mb *modBuilder) buildFuncInst(inst *checker.FuncInst) *funcBuilder {
	if funcDef, ok := mb.funcDef[inst]; ok {
		return funcDef
	}
	fb := mb.newFuncBuilder(inst.Def.Mod, inst.Def.Name, inst.Parms, inst.T.Ret, inst.Loc())
	fb.Exp = inst.Def.Exp
	fb.Inst = len(inst.TypeArgs) > 0 || len(inst.ConstraintArgs) > 0

	var sourceName strings.Builder
	sourceName.WriteString(inst.String())
	if len(inst.ConstraintArgs) > 0 {
		sourceName.WriteString(" : ")
	}
	for i, d := range inst.ConstraintArgs {
		if i > 0 {
			sourceName.WriteString(", ")
		}
		sourceName.WriteRune('(')
		sourceName.WriteString(d.String())
		sourceName.WriteRune(')')
	}
	fb.SourceName = sourceName.String()

	mb.funcDef[inst] = fb
	if inst.Def.Exprs == nil {
		fb.Decl = true
		return fb
	}
	// Don't build the flowgraph for other-module functions
	// that aren't type-parameterized, and do not pass
	// a loose inlinability check. These will just end up
	// as declarations, defined in the other module's object file.
	if mb.Path != inst.Def.Mod && !fb.Inst && !canInlineLoose(mb, inst) {
		return fb
	}
	b0 := fb.buildBlock0(inst.Locals)
	b1 := fb.buildBlocks(inst.Exprs)
	b0.jump(b1)
	return fb
}

// canInlineLoose gives a loose estimation whether it's worth
// building the flowgraph for an other-module function
// for use in later inlining.
//
// TODO: remove loose inlining estimation.
// Other-module functions should already be compiled to flowgraph.
// We can estimate their inlinability using that info,
// but for now, there's nowhere to communicate that
// without just building the entire flowgraph for the function.
func canInlineLoose(mb *modBuilder, inst *checker.FuncInst) bool {
	oldTrace := mb.trace
	mb.trace = mb.traceInline
	defer func() { mb.trace = oldTrace }()

	// If any parameter is function type,
	// assume that a block literal may be passed
	// and then further inlined.
	for _, a := range inst.Parms {
		if _, ok := a.Type().(*checker.FuncType); ok {
			mb.tr("loose inlinable %s: function parameter", inst)
			return true
		}
	}

	// nStmts is an approximation of how big the function is.
	// However, some of the statements counted may actually
	// end up in other functions, for example if they are in blocks
	// that are never inlined. Further, other inlining may take place
	// that makes the function even bigger than merely
	// its number of statements.
	//
	nStmts := len(inst.Exprs)
	for _, e := range inst.Exprs {
		nStmts += numStmts(e)
	}
	const limit = 10
	ok := nStmts < limit
	if !ok {
		mb.tr("no loose inlinable %s: %d statements >= %d", inst, nStmts, limit)
	} else {
		mb.tr("loose inlinable %s: %d statements < %d", inst, nStmts, limit)
	}
	return ok
}

// numStmts returns  the rumber of statments under the expression.
// Statements under an expression can only appear in block literals.
func numStmts(expr checker.Expr) int {
	switch expr := expr.(type) {
	case *checker.Call:
		n := 0
		for _, a := range expr.Args {
			n += numStmts(a)
		}
		return n
	case *checker.Convert:
		return numStmts(expr.Expr)
	case *checker.Var:
		return 0
	case *checker.Local:
		return 0
	case *checker.Parm:
		return 0
	case *checker.Cap:
		return 0
	case *checker.ArrayLit:
		n := 0
		for _, e := range expr.Elems {
			n += numStmts(e)
		}
		return n
	case *checker.StructLit:
		n := 0
		for _, e := range expr.Fields {
			n += numStmts(e)
		}
		return n
	case *checker.UnionLit:
		if expr.Val != nil {
			return numStmts(expr.Val)
		}
		return 0
	case *checker.BlockLit:
		n := len(expr.Exprs)
		for _, e := range expr.Exprs {
			n += numStmts(e)
		}
		return n
	case *checker.StrLit:
		return 0
	case *checker.IntLit:
		return 0
	case *checker.FloatLit:
		return 0
	default:
		panic(fmt.Sprintf("unknown checker.Expr type: %T", expr))
	}
}

func (mb *modBuilder) buildTestDef(def *checker.TestDef) *funcBuilder {
	empty := &checker.StructType{}
	fb := mb.newFuncBuilder(def.Mod, def.Name, nil, empty, def.L)
	fb.Test = true
	fb.SourceName = def.Mod + " test " + def.Name
	b0 := fb.buildBlock0(def.Locals)
	b1 := fb.buildBlocks(def.Exprs)
	b0.jump(b1)
	return fb
}

func (mb *modBuilder) buildBlockLit(parent *funcBuilder, lit *checker.BlockLit) *funcBuilder {
	parms := make([]*checker.ParmDef, len(lit.Parms))
	for i := range lit.Parms {
		parms[i] = &lit.Parms[i]
	}
	n := len(mb.funcBuilders)
	funcName := fmt.Sprintf("<block%d>", n)
	capsName := fmt.Sprintf("<caps%d>", n)
	fb := mb.newFuncBuilder(mb.Path, funcName, parms, lit.Ret, lit.Loc())
	fb.SourceName = mb.Path + " " + funcName
	fb.parent = parent
	for fb.parent.parent != nil {
		fb.parent = fb.parent.parent
	}
	fb.blockType = &StructType{Name: capsName}

	blockData := &ParmDef{
		Name:      "<block>",
		Type:      &AddrType{Elem: fb.blockType},
		BlockData: true,
	}
	fb.Parms = append([]*ParmDef{blockData}, fb.Parms...)
	fb.Type.Parms = append([]Type{blockData.Type}, fb.Type.Parms...)
	for i, cap := range lit.Caps {
		typ := mb.buildType(cap.T)
		if typ.isEmpty() {
			continue
		}
		field := &FieldDef{
			Num:  i,
			Name: cap.Name,
			Type: &AddrType{Elem: typ},
		}
		fb.blockType.Fields = append(fb.blockType.Fields, field)
		fb.capField[cap] = field
	}
	if !fb.parent.Type.Ret.isEmpty() {
		fb.returnField = &FieldDef{
			Num:  len(fb.blockType.Fields),
			Name: "<return>",
			Type: &AddrType{Elem: fb.parent.Type.Ret},
		}
		fb.blockType.Fields = append(fb.blockType.Fields, fb.returnField)
	}
	fb.frameField = &FieldDef{
		Num:  len(fb.blockType.Fields),
		Name: "<frame>",
		Type: &FrameType{},
	}
	fb.blockType.Fields = append(fb.blockType.Fields, fb.frameField)

	b0 := fb.buildBlock0(lit.Locals)
	b1 := fb.buildBlocks(lit.Exprs)
	b0.jump(b1)

	if !fb.longReturns {
		// If the function turns out to not long return,
		// remove the frame and return fields from caps.
		fb.frameField = nil
		if fb.blockType.Fields[len(fb.blockType.Fields)-1].Name != "<frame>" {
			panic("impossible")
		}
		fb.blockType.Fields = fb.blockType.Fields[:len(fb.blockType.Fields)-1]
		if fb.returnField != nil {
			fb.returnField = nil
			if fb.blockType.Fields[len(fb.blockType.Fields)-1].Name != "<return>" {
				panic("impossible")
			}
			fb.blockType.Fields = fb.blockType.Fields[:len(fb.blockType.Fields)-1]
		}
	}
	return fb
}

func (fb *funcBuilder) newBlock(l0 loc.Loc) *blockBuilder {
	fb.mod.nextBlock++
	block := &BasicBlock{Num: fb.mod.nextBlock - 1, Func: fb.FuncDef}
	fb.Blocks = append(fb.Blocks, block)
	return &blockBuilder{fun: fb, BasicBlock: block, L: l0}
}

func (mb *modBuilder) newFuncBuilder(path, name string, parms []*checker.ParmDef, ret checker.Type, l loc.Loc) *funcBuilder {
	fun := &FuncDef{
		Mod:  path,
		Name: name,
		Type: &FuncType{},
		L:    l,
	}
	fb := &funcBuilder{
		mod:        mb,
		FuncDef:    fun,
		outRefs:    make(map[*funcBuilder]int),
		inRefs:     make(map[*funcBuilder]int),
		parmDef:    make(map[*checker.ParmDef]*ParmDef),
		parmAlloc:  make(map[*ParmDef]*Alloc),
		localAlloc: make(map[*checker.LocalDef]*Alloc),
		capField:   make(map[*checker.BlockCap]*FieldDef),
	}
	for _, p := range parms {
		t := mb.buildType(p.Type())
		if t.isEmpty() {
			continue
		}
		fun.Type.Parms = append(fun.Type.Parms, t)

		var byValue bool
		if !t.isSmall() {
			t = &AddrType{Elem: t}
			byValue = true
		}
		pd := &ParmDef{
			Name:    p.Name,
			Type:    t,
			ByValue: byValue,
			L:       p.Loc(),
		}
		fb.parmDef[p] = pd
		fun.Parms = append(fun.Parms, pd)
	}
	fun.Type.Ret = mb.buildType(ret)
	if !fun.Type.Ret.isEmpty() {
		t := &AddrType{Elem: fun.Type.Ret}
		fun.Parms = append(fun.Parms, &ParmDef{
			Name:     "<return>",
			Type:     t,
			RetValue: true,
			L:        ret.Loc(),
		})
	}
	mb.funcBuilders = append(mb.funcBuilders, fb)
	return fb
}

func (fb *funcBuilder) buildBlock0(locals []*checker.LocalDef) *blockBuilder {
	fb.b0 = fb.newBlock(loc.Loc{})
	for _, p := range fb.Parms {
		if p.RetValue || p.BlockData {
			continue
		}
		fb.b0.L = p.L
		if p.ByValue {
			fb.parmAlloc[p] = fb.b0.alloc(p.Type.(*AddrType).Elem)
		} else {
			fb.parmAlloc[p] = fb.b0.alloc(p.Type)
		}
	}
	for _, l := range locals {
		fb.b0.L = l.L
		t := fb.mod.buildType(l.T)
		if !t.isEmpty() {
			fb.localAlloc[l] = fb.b0.alloc(t)
		}
	}
	for _, p := range fb.Parms {
		if p.RetValue || p.BlockData {
			continue
		}
		fb.b0.L = p.L
		if p.ByValue {
			fb.b0.copy(fb.parmAlloc[p], fb.b0.parm(p))
		} else {
			fb.b0.store(fb.parmAlloc[p], fb.b0.parm(p))
		}
	}
	return fb.b0
}

func (fb *funcBuilder) buildBlocks(exprs []checker.Expr) *blockBuilder {
	b1 := fb.newBlock(loc.Loc{})
	bb := b1
	for i, expr := range exprs {
		var v Value
		if bb, v = bb.expr(expr); bb.done {
			break
		}
		if i == len(exprs)-1 && fb.blockType != nil {
			// v may be nil if the result expression type is !.
			// In this case, the return will never execute,
			// since the function called will have returned instead.
			if !bb.fun.Type.Ret.isEmpty() && v != nil {
				parm := bb.parm(bb.fun.Parms[len(bb.fun.Parms)-1])
				if bb.fun.Type.Ret.isSmall() {
					bb.store(parm, v)
				} else {
					bb.copy(parm, v)
				}
			}
			bb.Return(nil)
		}
	}
	if !bb.done {
		bb.Return(nil)
	}
	return b1
}

func (bb *blockBuilder) buildType(typ checker.Type) Type {
	return bb.fun.mod.buildType(typ)
}

func (bb *blockBuilder) expr(expr checker.Expr) (*blockBuilder, Value) {
	origLoc := bb.L
	bb.L = expr.Loc()
	defer func() { bb.L = origLoc }()

	switch expr := expr.(type) {
	case *checker.Call:
		return bb.Call(expr)
	case *checker.Convert:
		return bb.convert(expr)
	case *checker.Var:
		v := bb.fun.mod.varDef[expr.Def]
		if v == nil {
			v = &VarDef{
				Mod:  expr.Def.Mod,
				Name: expr.Def.Name,
				Type: bb.buildType(expr.Def.T),
			}
			bb.fun.mod.Vars = append(bb.fun.mod.Vars, v)
			bb.fun.mod.varDef[expr.Def] = v
		}
		return bb, bb.Var(v)
	case *checker.Local:
		a, ok := bb.fun.localAlloc[expr.Def]
		if !ok {
			return bb, nil
		}
		return bb, a
	case *checker.Parm:
		a, ok := bb.fun.parmAlloc[bb.fun.parmDef[expr.Def]]
		if !ok {
			if !bb.buildType(expr.Def.Type()).isEmpty() {
				fmt.Printf("%s has no parm %s\n", bb.fun.FuncDef, expr.Def.Name)
				panic("no parm")
			}
			return bb, nil
		}
		return bb, a
	case *checker.Cap:
		f, ok := bb.fun.capField[expr.Def]
		if !ok {
			return bb, nil
		}
		block := bb.parm(bb.fun.Parms[0])
		field := bb.field(block, f)
		return bb, bb.load(field)
	case *checker.ArrayLit:
		return bb.arrayLit(expr)
	case *checker.StructLit:
		return bb.structLit(expr)
	case *checker.UnionLit:
		return bb.unionLit(expr)
	case *checker.BlockLit:
		return bb.blockLit(expr)
	case *checker.StrLit:
		return bb, bb.strLit(expr)
	case *checker.IntLit:
		return bb, bb.checkerIntLit(expr, bb.buildType(expr.T))
	case *checker.FloatLit:
		return bb, bb.floatLit(expr, bb.buildType(expr.T))
	default:
		panic(fmt.Sprintf("bad checker.Expr type: %T", expr))
	}
}

func (bb *blockBuilder) Call(call *checker.Call) (*blockBuilder, Value) {
	if bb.L == (loc.Loc{}) {
		panic(fmt.Sprintf("00 call without a loc: %s", call))
	}
	switch fun := call.Func.(type) {
	case *checker.FuncInst:
		return bb.buildStaticCall(call)
	case *checker.ExprFunc:
		return bb.buildIndirectCall(call)
	case *checker.Select:
		return bb.buildSelect(call)
	case *checker.Switch:
		return bb.buildSwitch(fun, call)
	case *checker.Builtin:
		switch fun.Op {
		case checker.BitNot, checker.BitXor, checker.BitAnd, checker.BitOr,
			checker.LeftShift, checker.RightShift, checker.Negate,
			checker.Minus, checker.Plus, checker.Times, checker.Divide,
			checker.Modulus, checker.Eq, checker.Panic, checker.Print:
			return bb.buildOp(call)
		case checker.Cmp:
			return bb.buildCmp(call)
		case checker.Assign:
			return bb.buildAssign(call)
		case checker.NewArray:
			return bb.buildNewArray(call)
		case checker.Index:
			return bb.buildIndex(call)
		case checker.Slice:
			return bb.buildSlice(call)
		case checker.Length:
			return bb.buildLength(call)
		case checker.Return:
			return bb.buildReturn(call)
		default:
			panic(fmt.Sprintf("bad checker.Op: %d", fun.Op))
		}
	default:
		panic(fmt.Sprintf("bad checker.Func type: %T\ndef:\n%s", call.Func, bb.fun.FuncDef))
	}
}

func (bb *blockBuilder) buildStaticCall(call *checker.Call) (*blockBuilder, Value) {
	fun := call.Func.(*checker.FuncInst)
	var args []Value
	for _, expr := range call.Args {
		var arg Value
		if bb, arg = bb.expr(expr); arg != nil {
			args = append(args, arg)
		}
		if bb.L == (loc.Loc{}) {
			panic(fmt.Sprintf("arg without a loc: %s", expr))
		}
	}
	funcBuilder := bb.fun.mod.buildFuncInst(fun)
	funcVal := bb.Func(funcBuilder)
	var ret Value
	if !funcBuilder.Type.Ret.isEmpty() {
		ret = bb.alloc(funcBuilder.Type.Ret)
		args = append(args, ret)
	} else {
		ret = bb.null(&AddrType{Elem: funcBuilder.Type.Ret})
	}
	bb.call(funcVal, args)
	return bb, ret
}

func (bb *blockBuilder) buildIndirectCall(call *checker.Call) (*blockBuilder, Value) {
	fun := call.Func.(*checker.ExprFunc)
	bb, funcBlockVal := bb.expr(fun.Expr)
	funcBlockType := funcBlockVal.Type().(*AddrType).Elem.(*StructType)
	funcVal := bb.load(bb.field(funcBlockVal, funcBlockType.Fields[0]))
	funcClosure := bb.load(bb.field(funcBlockVal, funcBlockType.Fields[1]))
	args := []Value{funcClosure}
	for _, expr := range call.Args {
		var arg Value
		if bb, arg = bb.expr(expr); arg != nil {
			args = append(args, arg)
		}
	}
	var ret Value
	funcType := funcVal.Type().(*FuncType)
	if !funcType.Ret.isEmpty() {
		ret = bb.alloc(funcType.Ret)
		args = append(args, ret)
	} else {
		ret = bb.null(&AddrType{Elem: funcType.Ret})
	}
	bb.call(funcVal, args)
	return bb, ret
}

func (bb *blockBuilder) buildSelect(call *checker.Call) (*blockBuilder, Value) {
	fun := call.Func.(*checker.Select)
	bb, base := bb.expr(call.Args[0])
	// We are careful to get the struct type from the call.Func, not base Value.
	// The type of the base value may have recursive pointers type-erased.
	// We need the _actual_ struct type, converted from the checker.Call.
	structType := bb.buildType(fun.Struct).(*StructType)
	var fieldDef *FieldDef
	for i := range structType.Fields {
		if structType.Fields[i].Name == fun.Field.Name[1:] {
			fieldDef = structType.Fields[i]
			break
		}
	}
	if fieldDef == nil {
		panic("bad field")
	}
	// We are careful to get the element type from call.Type,
	// so that we ensure it isn't a type-erased pointer
	// representing a recursive reference type.
	t := bb.buildType(call.Type()).(*AddrType).Elem
	a := bb.alloc(t)
	f := bb.field(base, fieldDef)
	bb.store(a, f)
	return bb, a
}

type switchCase struct {
	num int
	fun loc.Locer

	// base is the base of the Union if this is a typed case.
	base Value
	// def is the case def of the case if this is a typed case.
	def *CaseDef

	// bDefault is the default case block if this is a default case.
	bDefault *blockBuilder
}

func (bb *blockBuilder) buildSwitch(sw *checker.Switch, call *checker.Call) (*blockBuilder, Value) {
	if bb, v, ok := bb.simplifyCmpSwitch(sw, call); ok {
		return bb, v
	}
	if isPointer(sw.Union) {
		return bb.buildPointerSwitch(sw, call)
	}

	var tag Value
	var unionBase Value
	typ := bb.buildType(sw.Union)
	bb, base := bb.expr(call.Args[0])
	if _, ok := typ.(*IntType); ok {
		tag = bb.load(base)
	} else {
		unionHeaderType := typ.(*StructType)
		tag = bb.load(bb.field(base, unionHeaderType.Fields[0]))
		unionBase = bb.field(base, unionHeaderType.Fields[1])
	}

	var defaultCase *switchCase
	handledNums := make([]bool, len(sw.Union.Cases))
	switchCases := make([]*switchCase, 0, len(sw.Cases))
	for i, expr := range call.Args[1:] {
		var sc switchCase
		cas := sw.Cases[i]
		if cas == nil {
			sc.num = -1
			defaultCase = &sc
		} else {
			sc.num = caseNum(sw.Union, cas.Name)
			handledNums[sc.num] = true
			if cas.Type != nil {
				union := unionBase.Type().(*AddrType).Elem.(*UnionType)
				sc.def = caseDef(union, strings.TrimSuffix(cas.Name, "?"))
				sc.base = unionBase
			}
			switchCases = append(switchCases, &sc)
		}
		if cvt, ok := expr.(*checker.Convert); ok && cvt.Kind == checker.Deref {
			if sc.fun, ok = cvt.Expr.(*checker.BlockLit); ok {
				continue
			}
		}
		bb, sc.fun = bb.expr(expr)
	}

	var res Value
	if resType := bb.buildType(sw.T.Ret); !resType.isEmpty() {
		res = bb.alloc(resType)
	}

	bDone := &blockBuilder{
		fun:        bb.fun,
		BasicBlock: &BasicBlock{Func: bb.fun.FuncDef},
		L:          call.L,
	}

	var bDefault *blockBuilder
	if defaultCase != nil {
		// Populate all un-handled cases with the default case.
		bDefault = &blockBuilder{
			fun:        bb.fun,
			BasicBlock: &BasicBlock{Func: bb.fun.FuncDef},
			L:          defaultCase.fun.Loc(),
		}
		for i, handled := range handledNums {
			if handled {
				continue
			}
			sc := *defaultCase // copies .fun for loc
			sc.num = i
			sc.bDefault = bDefault
			switchCases = append(switchCases, &sc)
		}
	}

	sort.Slice(switchCases, func(i, j int) bool {
		return switchCases[i].num < switchCases[j].num
	})
	bb.buildCases(0, len(sw.Union.Cases), tag, res, switchCases, bDone)

	if bDefault != nil {
		bb.fun.mod.nextBlock++
		bDefault.Num = bb.fun.mod.nextBlock - 1
		bb.fun.Blocks = append(bb.fun.Blocks, bDefault.BasicBlock)
		bDefault.buildBranchCall(defaultCase.fun, nil, res, bDone)
	}

	bb.fun.mod.nextBlock++
	bDone.Num = bb.fun.mod.nextBlock - 1
	bb.fun.Blocks = append(bb.fun.Blocks, bDone.BasicBlock)
	return bDone, res
}

func caseDef(union *UnionType, caseName string) *CaseDef {
	for _, def := range union.Cases {
		if def.Name == caseName {
			return def
		}
	}
	panic("impossible")
}

func (bb *blockBuilder) buildCases(min, max int, tag, res Value, cases []*switchCase, bDone *blockBuilder) {
	total := true
	next := min
	for _, c := range cases {
		if c.num != next {
			total = false
			break
		}
		next++
	}
	total = total && next == max

	switch {
	case len(cases) == 0:
		panic("impossible no cases")
	case len(cases) == 1:
		if !total {
			b0Body := bb.fun.newBlock(cases[0].fun.Loc())
			bb.ifEq(tag, cases[0].num, b0Body, bDone)
			bb = b0Body
		}
		bb.buildCaseCall(cases[0], res, bDone)
		return
	case len(cases) == 2:
		b0 := bb.fun.newBlock(cases[0].fun.Loc())
		b1 := bb.fun.newBlock(cases[1].fun.Loc())
		bb.ifEq(tag, cases[0].num, b0, b1)
		b0.buildCaseCall(cases[0], res, bDone)

		if !total {
			b1Body := bb.fun.newBlock(cases[1].fun.Loc())
			b1.ifEq(tag, cases[1].num, b1Body, bDone)
			b1 = b1Body
		}
		b1.buildCaseCall(cases[1], res, bDone)
		return
	case len(cases) == 3:
		b0 := bb.fun.newBlock(cases[0].fun.Loc())
		b1 := bb.fun.newBlock(cases[1].fun.Loc())
		b2 := bb.fun.newBlock(cases[2].fun.Loc())

		bb.ifEq(tag, cases[0].num, b0, b1)
		b0.buildCaseCall(cases[0], res, bDone)

		b1Body := bb.fun.newBlock(cases[1].fun.Loc())
		b1.ifEq(tag, cases[1].num, b1Body, b2)
		b1Body.buildCaseCall(cases[1], res, bDone)

		if !total {
			b2Body := bb.fun.newBlock(cases[2].fun.Loc())
			b2.ifEq(tag, cases[2].num, b2Body, bDone)
			b2 = b2Body
		}
		b2.buildCaseCall(cases[2], res, bDone)
		return
	}

	i := len(cases) / 2
	bLeft := bb.fun.newBlock(cases[0].fun.Loc())
	bRight := bb.fun.newBlock(cases[i].fun.Loc())
	bb.ifLess(tag, cases[i].num, bLeft, bRight)
	bLeft.buildCases(min, cases[i].num, tag, res, cases[:i], bDone)
	bRight.buildCases(cases[i].num, max, tag, res, cases[i:], bDone)
}

func (bb *blockBuilder) buildCaseCall(cas *switchCase, res Value, bDone *blockBuilder) {
	bb.L = cas.fun.Loc()
	if cas.bDefault != nil {
		// Jump to the default case.
		bb.jump(cas.bDefault)
		return
	}
	if cas.base == nil {
		bb.buildBranchCall(cas.fun, nil, res, bDone)
		return
	}
	var val Value = bb.Case(cas.base, cas.def)
	if cas.def.Type.isSmall() {
		val = bb.load(val)
	}
	bb.buildBranchCall(cas.fun, val, res, bDone)
}

func (bb *blockBuilder) buildPointerSwitch(sw *checker.Switch, call *checker.Call) (*blockBuilder, Value) {
	bb, base := bb.expr(call.Args[0])
	funcs := make([]loc.Locer, len(sw.Cases))
	for i, expr := range call.Args[1:] {
		if cvt, ok := expr.(*checker.Convert); ok && cvt.Kind == checker.Deref {
			if funcs[i], ok = cvt.Expr.(*checker.BlockLit); ok {
				continue
			}
		}
		bb, funcs[i] = bb.expr(expr)
	}

	var res Value
	if resType := bb.buildType(sw.T.Ret); !resType.isEmpty() {
		res = bb.alloc(resType)
	}

	bDone := &blockBuilder{
		fun:        bb.fun,
		BasicBlock: &BasicBlock{Func: bb.fun.FuncDef},
	}
	switch {
	case len(sw.Cases) == 1 && sw.Cases[0] == nil:
		// Only a default case.
		bb.buildBranchCall(funcs[0], nil, res, bDone)
	case len(sw.Cases) == 1 && sw.Cases[0].Type == nil:
		tag := bb.load(base)
		bNil := bb.fun.newBlock(loc.Loc{})
		bb.ifNull(tag, bNil, bDone)
		bNil.buildBranchCall(funcs[0], nil, res, bDone)
	case len(sw.Cases) == 1:
		tag := bb.load(base)
		bNonNil := bb.fun.newBlock(loc.Loc{})
		bb.ifNull(tag, bDone, bNonNil)
		bNonNil.buildBranchCall(funcs[0], tag, res, bDone)
	case len(sw.Cases) == 2:
		// Find which case indices are the nil and non-nil,
		// taking into account that one or the other (but not both)
		// may be a default case (sw.Cases[i]==nil).
		nilIndex := 0
		nonNilIndex := 1
		if sw.Cases[nilIndex] != nil {
			if sw.Cases[nilIndex].Type != nil {
				nilIndex, nonNilIndex = nonNilIndex, nilIndex
			}
		} else {
			if sw.Cases[nonNilIndex].Type == nil {
				nilIndex, nonNilIndex = nonNilIndex, nilIndex
			}
		}
		tag := bb.load(base)
		bNil := bb.fun.newBlock(loc.Loc{})
		bNonNil := bb.fun.newBlock(loc.Loc{})
		bb.ifNull(tag, bNil, bNonNil)
		bNil.buildBranchCall(funcs[nilIndex], nil, res, bDone)
		if sw.Cases[nonNilIndex] == nil {
			// Don't pass the tag if this is the default case.
			bNonNil.buildBranchCall(funcs[nonNilIndex], nil, res, bDone)
		} else {
			bNonNil.buildBranchCall(funcs[nonNilIndex], tag, res, bDone)
		}
	}
	bb.fun.mod.nextBlock++
	bDone.Num = bb.fun.mod.nextBlock - 1
	bb.fun.Blocks = append(bb.fun.Blocks, bDone.BasicBlock)
	return bDone, res
}

// fun is either a *checker.BlockLit or a Value address of a function header struct.
func (bb *blockBuilder) buildBranchCall(fun loc.Locer, arg, res Value, bDone *blockBuilder) {
	bb.L = fun.Loc()
	var capsVal, funVal Value
	if lit, ok := fun.(*checker.BlockLit); ok {
		fb := bb.fun.mod.buildBlockLit(bb.fun, lit)
		capsVal = bb.blockCaps(fb, lit)
		funVal = bb.Func(fb)
	} else {
		funcExpr := fun.(Value)
		hdrType := funcExpr.Type().(*AddrType).Elem.(*StructType)
		capsVal = bb.load(bb.field(funcExpr, hdrType.Fields[1]))
		f := bb.field(funcExpr, hdrType.Fields[0])
		funVal = bb.load(f)
	}
	funArgs := []Value{capsVal}
	if arg != nil {
		funArgs = append(funArgs, arg)
	}
	if res != nil {
		funArgs = append(funArgs, res)
	}
	bb.call(funVal, funArgs)
	bb.jump(bDone)
}

// Converts switch calls with arg[0] as built-in <=> call into simple comparison ops.
func (bb *blockBuilder) simplifyCmpSwitch(sw *checker.Switch, call *checker.Call) (*blockBuilder, Value, bool) {
	if len(call.Args) == 0 {
		return nil, nil, false
	}
	cmpCall := cmpCall(call.Args[0])
	if cmpCall == nil {
		return nil, nil, false
	}

	// Try to convert the <=> to a simple comparison, <, <=, =, !=, >, or >=.
	var op OpKind
	switch less, equal, greater := boolConstArgs(sw, call); {
	case less == "true?" && equal == "false?" && greater == "false?":
		op = Less
	case less == "true?" && equal == "true?" && greater == "false?":
		op = LessEq
	case less == "false?" && equal == "true?" && greater == "false?":
		op = Eq
	case less == "true?" && equal == "false?" && greater == "true?":
		op = Neq
	case less == "false?" && equal == "false?" && greater == "true?":
		op = Greater
	case less == "false?" && equal == "true?" && greater == "true?":
		op = GreaterEq
	default:
		fmt.Printf("less=%s, equal=%s, greater=%s\n", less, equal, greater)
	}
	if op != 0 {
		cmpFun := cmpCall.Func.(*checker.Builtin)
		boolType := bb.buildType(cmpFun.Ret)
		bb, arg0 := bb.expr(cmpCall.Args[0])
		bb, arg1 := bb.expr(cmpCall.Args[1])
		op := bb.op(op, boolType, arg0, arg1)
		ret := bb.alloc(boolType)
		bb.store(ret, op)
		return bb, ret, true
	}

	// TODO: we can further optimize switch on a built-in <=> call.
	// Today, the behavior of this fallthrough is that <=> will be
	// a double branch resulting in 0, 1, or 2 for less, equal, and greater;
	// then another 3-way switch to call the argument blocks.
	// We can merge these two into a single 3-way switching structure.

	return nil, nil, false
}

func cmpCall(expr checker.Expr) *checker.Call {
	if !isOrderingOrPartialOrderingRef(expr.Type()) {
		return nil
	}
	for {
		cvt, ok := expr.(*checker.Convert)
		if !ok {
			break
		}
		expr = cvt.Expr
	}
	call, ok := expr.(*checker.Call)
	if !ok {
		return nil
	}
	fun, ok := call.Func.(*checker.Builtin)
	if !ok || fun.Op != checker.Cmp {
		return nil
	}
	return call
}

func isOrderingOrPartialOrderingRef(typ checker.Type) bool {
	ref, ok := typ.(*checker.RefType)
	return ok && (isOrdering(ref.Type) || isPartialOrdering(ref.Type))
}

func isOrdering(typ checker.Type) bool {
	union, ok := typ.(*checker.UnionType)
	return ok && len(union.Cases) == 3 &&
		union.Cases[0].Name == "less?" && union.Cases[0].Type == nil &&
		union.Cases[1].Name == "equal?" && union.Cases[1].Type == nil &&
		union.Cases[2].Name == "greater?" && union.Cases[2].Type == nil
}

func isPartialOrdering(typ checker.Type) bool {
	union, ok := typ.(*checker.UnionType)
	return ok && len(union.Cases) == 4 &&
		union.Cases[0].Name == "less?" && union.Cases[0].Type == nil &&
		union.Cases[1].Name == "equal?" && union.Cases[1].Type == nil &&
		union.Cases[2].Name == "greater?" && union.Cases[2].Type == nil &&
		union.Cases[3].Name == "none?" && union.Cases[3].Type == nil
}

func isBool(typ checker.Type) bool {
	union, ok := typ.(*checker.UnionType)
	return ok && len(union.Cases) == 2 &&
		union.Cases[0].Name == "false?" && union.Cases[0].Type == nil &&
		union.Cases[1].Name == "true?" && union.Cases[1].Type == nil
}

// boolConstArgs returns "true?", "false?", or ""
// for each of the less, equal, and greater cases of the switch.
// If the switch is incomplete "" is returned for all three.
// If the switch is complete for each of return value
// if it can be proven to be a block literal that only returns
// a bool constant, then its value is either "true?" or "false?"
// corresponding to the boolean value,
// otherwise if it cannot be statically proven, then "".
func boolConstArgs(sw *checker.Switch, call *checker.Call) (less, equal, greater string) {
	switch len(sw.Cases) {
	case 1:
		break
	case 2:
		if sw.Cases[0] != nil && sw.Cases[1] != nil {
			break
		}
		var other string
		for i := range sw.Cases {
			if sw.Cases[i] == nil {
				other = boolConstBlock(call.Args[i+1])
			}
		}
		for i := range sw.Cases {
			if sw.Cases[i] == nil {
				continue
			}
			x := boolConstBlock(call.Args[i+1])
			switch sw.Cases[i].Name {
			case "less?":
				less = x
				equal = other
				greater = other
			case "equal?":
				less = other
				equal = x
				greater = other
			case "greater?":
				less = other
				greater = x
				equal = other
			default:
				panic("impossible")
			}
		}
	case 3:
		unseen := [...]*string{&less, &equal, &greater}
		for i := range sw.Cases {
			if sw.Cases[i] == nil {
				continue
			}
			b := boolConstBlock(call.Args[i+1])
			switch sw.Cases[i].Name {
			case "less?":
				less = b
				unseen[0] = nil
			case "equal?":
				equal = b
				unseen[1] = nil
			case "greater?":
				greater = b
				unseen[2] = nil
			default:
				panic("impossible")
			}
		}
		for i := range sw.Cases {
			if sw.Cases[i] != nil {
				continue
			}
			b := boolConstBlock(call.Args[i+1])
			for _, u := range unseen {
				if u != nil {
					*u = b
				}
			}
		}
	default:
		panic("impossible")
	}
	return less, equal, greater
}

// boolConstBlock returns "true?", "false?", or ""
// depending on whether expr is a block literal that can be statically proven
// to only return the true ("true?"), false ("false?"),
// or if it is no such block literal or cannot be proven either way ("").
func boolConstBlock(expr checker.Expr) string {
	for {
		cvt, ok := expr.(*checker.Convert)
		if !ok || cvt.Kind != checker.Noop {
			break
		}
		expr = cvt.Expr
	}
	blockLit, ok := expr.(*checker.BlockLit)
	if !ok || len(blockLit.Exprs) != 1 {
		return ""
	}
	expr = blockLit.Exprs[0]
	if !isBool(expr.Type()) {
		return ""
	}
	for {
		cvt, ok := expr.(*checker.Convert)
		if !ok {
			break
		}
		expr = cvt.Expr
	}

	// The block is returning a union literal.
	if unionLit, ok := expr.(*checker.UnionLit); ok {
		return unionLit.Case.Name
	}

	// Check whether the block is returning
	// a constant access to a bool literal.
	vr, ok := expr.(*checker.Var)
	if !ok || !vr.Def.Const {
		return ""
	}
	call, ok := vr.Def.Expr.(*checker.Call)
	if !ok {
		return ""
	}
	if fun, ok := call.Func.(*checker.Builtin); !ok || fun.Op != checker.Assign {
		return ""
	}
	rhs := call.Args[1]
	for {
		cvt, ok := rhs.(*checker.Convert)
		if !ok || cvt.Kind != checker.Noop {
			break
		}
		rhs = cvt.Expr
	}
	unionLit, _ := rhs.(*checker.UnionLit)
	return unionLit.Case.Name
}

func (bb *blockBuilder) buildOp(call *checker.Call) (*blockBuilder, Value) {
	fun := call.Func.(*checker.Builtin)
	var args []Value
	for _, expr := range call.Args {
		var v Value
		if bb, v = bb.expr(expr); v != nil {
			args = append(args, v)
		}
	}

	typ := bb.buildType(fun.Ret)
	if _, ok := typ.(*IntType); ok && fun.Op == checker.Divide {
		yes := bb.fun.newBlock(call.L)
		no := bb.fun.newBlock(call.L)
		bb.ifEq(args[1], 0, yes, no)
		yes.op(Panic, nil, yes.strLit(&checker.StrLit{
			Text: "divide by zero",
			T:    &checker.BasicType{Kind: checker.String, L: call.L},
			L:    call.L,
		}))
		yes.done = true
		bb = no
	}

	op := bb.op(opMap[fun.Op], typ, args...)
	if op.Op == Panic {
		// Panic breaks control flow, so end the block.
		bb.done = true
	}
	var ret Value
	if !typ.isEmpty() {
		ret = bb.alloc(typ)
		bb.store(ret, op)
	} else if !bb.done {
		// If the basic block isn't being marked complete
		// (i.e, not return or panic), and the op returns [.],
		// then return a null [.] reference.
		ret = bb.null(&AddrType{Elem: typ})
	}
	return bb, ret
}

func (bb *blockBuilder) buildCmp(call *checker.Call) (*blockBuilder, Value) {
	fun := call.Func.(*checker.Builtin)
	var args []Value
	for _, expr := range call.Args {
		var v Value
		if bb, v = bb.expr(expr); v != nil {
			args = append(args, v)
		}
	}
	orderingType := bb.buildType(fun.Ret)
	result := bb.alloc(orderingType)

	if isOrdering(fun.Ret) {
		// If it's not a partial ordering, check and a<=b, a<b.
		leBlock := bb.fun.newBlock(call.Loc())
		ltBlock := bb.fun.newBlock(call.Loc())
		eqBlock := bb.fun.newBlock(call.Loc())
		gtBlock := bb.fun.newBlock(call.Loc())
		doneBlock := bb.fun.newBlock(call.Loc())
		bb.ifLessEq(args[0], args[1], leBlock, gtBlock)
		leBlock.ifLessValue(args[0], args[1], ltBlock, eqBlock)
		ltBlock.store(result, ltBlock.intLit("0", orderingType))
		ltBlock.jump(doneBlock)
		eqBlock.store(result, eqBlock.intLit("1", orderingType))
		eqBlock.jump(doneBlock)
		gtBlock.store(result, gtBlock.intLit("2", orderingType))
		gtBlock.jump(doneBlock)
		return doneBlock, result
	}
	leBlock := bb.fun.newBlock(call.Loc())
	ltBlock := bb.fun.newBlock(call.Loc())
	eqBlock := bb.fun.newBlock(call.Loc())
	notLEBlock := bb.fun.newBlock(call.Loc())
	gtBlock := bb.fun.newBlock(call.Loc())
	noneBlock := bb.fun.newBlock(call.Loc())
	doneBlock := bb.fun.newBlock(call.Loc())
	// a <= b ?
	bb.ifLessEq(args[0], args[1], leBlock, notLEBlock)
	// a < b?
	leBlock.ifLessValue(args[0], args[1], ltBlock, eqBlock)
	ltBlock.store(result, ltBlock.intLit("0", orderingType))
	ltBlock.jump(doneBlock)
	eqBlock.store(result, eqBlock.intLit("1", orderingType))
	eqBlock.jump(doneBlock)
	// a > b?
	notLEBlock.ifGreaterValue(args[0], args[1], gtBlock, noneBlock)
	gtBlock.store(result, gtBlock.intLit("2", orderingType))
	gtBlock.jump(doneBlock)
	noneBlock.store(result, noneBlock.intLit("3", orderingType))
	noneBlock.jump(doneBlock)
	return doneBlock, result
}

func (bb *blockBuilder) buildAssign(call *checker.Call) (*blockBuilder, Value) {
	var dst, src Value
	bb, dst = bb.expr(call.Args[0])
	bb, src = bb.expr(call.Args[1])
	switch typ := bb.buildType(call.Args[1].Type()); {
	case typ.isEmpty():
		return bb, nil
	case typ.isSmall():
		bb.store(dst, src)
	default:
		bb.copy(dst, src)
	}
	return bb, nil
}

func (bb *blockBuilder) buildNewArray(c *checker.Call) (*blockBuilder, Value) {
	fun := c.Func.(*checker.Builtin)
	bb, n := bb.expr(c.Args[0])
	bb, fill := bb.expr(c.Args[1])

	arrayType := bb.buildType(fun.Ret).(*StructType)
	elemType := arrayType.Fields[1].Type.(*ArrayType).Elem
	if elemType.isEmpty() {
		return bb, nil
	}
	// TODO: check that new array size is non-negative

	a := bb.alloc(arrayType)
	bb.store(bb.field(a, arrayType.Fields[0]), n)
	data := bb.allocN(n, elemType)
	bb.store(bb.field(a, arrayType.Fields[1]), data)

	cond := bb.fun.newBlock(c.L)
	body := bb.fun.newBlock(c.L)
	done := bb.fun.newBlock(c.L)
	intType := bb.fun.mod.intType()
	i := bb.alloc(intType)
	bb.store(i, n)
	bb.jump(cond)
	cond.ifEq(cond.load(i), 0, done, body)
	one := body.intLit("1", intType)
	iMinusOne := body.op(Minus, intType, body.load(i), one)
	elem := body.index(data, iMinusOne)
	if elemType.isSmall() {
		body.store(elem, fill)
	} else {
		body.copy(elem, fill)
	}
	body.store(i, iMinusOne)
	body.jump(cond)

	return done, a
}

func (bb *blockBuilder) buildIndex(call *checker.Call) (*blockBuilder, Value) {
	bb, base := bb.expr(call.Args[0])
	bb, index := bb.expr(call.Args[1])
	arrayType := base.Type().(*AddrType).Elem.(*StructType)

	// Bounds check.
	positive := bb.fun.newBlock(call.L)
	oob := bb.fun.newBlock(call.L)
	inBounds := bb.fun.newBlock(call.L)
	bb.ifLess(index, 0, oob, positive)
	arrayLenField := positive.field(base, arrayType.Fields[0])
	arrayLen := positive.load(arrayLenField)
	positive.ifLessEq(arrayLen, index, oob, inBounds)
	// oob block.
	arrayLenField = oob.field(base, arrayType.Fields[0])
	arrayLen = oob.load(arrayLenField)
	str := oob.op(IndexOOBString, bb.fun.mod.stringType(), index, arrayLen)
	oob.op(Panic, nil, str)
	bb = inBounds

	data := bb.field(base, arrayType.Fields[1])
	elem := bb.index(bb.load(data), index)

	retType := bb.buildType(call.T).(*AddrType)

	// If the return type is a double pointer, this is an array index.
	if _, ok := retType.Elem.(*AddrType); ok {
		a := bb.alloc(elem.Type())
		bb.store(a, elem)
		return bb, a
	}
	// Otherwise, this is a string index.
	a := bb.alloc(retType.Elem)
	bb.store(a, bb.load(elem))
	return bb, a
}

func (bb *blockBuilder) buildSlice(call *checker.Call) (*blockBuilder, Value) {
	bb, base := bb.expr(call.Args[0])
	bb, start := bb.expr(call.Args[1])
	bb, end := bb.expr(call.Args[2])

	arrayType := base.Type().(*AddrType).Elem.(*StructType)

	// Bounds check.
	positive := bb.fun.newBlock(call.L)
	oob := bb.fun.newBlock(call.L)
	notReversed := bb.fun.newBlock(call.L)
	inBounds := bb.fun.newBlock(call.L)
	bb.ifLess(start, 0, oob, positive)
	positive.ifLessEq(start, end, notReversed, oob)
	srcLenField := notReversed.field(base, arrayType.Fields[0])
	srcLen := notReversed.load(srcLenField)
	notReversed.ifLessValue(srcLen, end, oob, inBounds)
	// oob block.
	srcLenField = oob.field(base, arrayType.Fields[0])
	srcLen = oob.load(srcLenField)
	str := oob.op(SliceOOBString, bb.fun.mod.stringType(), start, end, srcLen)
	oob.op(Panic, nil, str)
	bb = inBounds

	a := bb.alloc(arrayType)
	// Note we write the data field first, then the length field.
	// This is so that a Go translation of the output
	// can convert writing an array.length field into
	// slicing the array with an upper limit,
	// writing an array.data into an assignment,
	// and still get the correct result here.
	// This slice operation can be translated to Go as:
	// 	a := src[start:]			// store data field.
	// 	a := a[:end-start]		// store length field
	srcDataField := bb.field(base, arrayType.Fields[1])
	dstData := bb.slice(bb.load(srcDataField), start)
	dstDataField := bb.field(a, arrayType.Fields[1])
	bb.store(dstDataField, dstData)
	dstLen := bb.op(Minus, bb.fun.mod.intType(), end, start)
	dstLenField := bb.field(a, arrayType.Fields[0])
	bb.store(dstLenField, dstLen)
	return bb, a
}

func (bb *blockBuilder) buildLength(call *checker.Call) (*blockBuilder, Value) {
	bb, base := bb.expr(call.Args[0])
	typ := base.Type().(*AddrType).Elem.(*StructType)
	field := bb.field(base, typ.Fields[0])
	a := bb.alloc(typ.Fields[0].Type)
	bb.store(a, bb.load(field))
	return bb, a
}

func (bb *blockBuilder) buildReturn(call *checker.Call) (*blockBuilder, Value) {
	var v Value
	if len(call.Args) == 1 {
		bb, v = bb.expr(call.Args[0])
	}
	if v == nil {
		var frame Value
		if bb.fun.blockType != nil {
			frame = bb.frame()
		}
		bb.Return(frame)
		return bb, nil
	}

	var frame Value
	if bb.fun.blockType != nil {
		frame = bb.frame()
	}
	if frame != nil {
		// We check the frame _before_ storing the return value.
		// This ensures that we never write to the return value
		// if it is not below our current frame on the stack.
		// That way escape analysis can safely mark
		// return values as non-escaping.
		bb.op(CheckFrame, &StructType{}, frame)
	}
	if bb.fun.blockType != nil {
		block := bb.parm(bb.fun.Parms[0])
		field := bb.field(block, bb.fun.returnField)
		if bb.fun.parent.Type.Ret.isSmall() {
			bb.store(bb.load(field), v)
		} else {
			bb.copy(bb.load(field), v)
		}
	} else {
		parm := bb.parm(bb.fun.Parms[len(bb.fun.Parms)-1])
		if bb.fun.Type.Ret.isSmall() {
			bb.store(parm, v)
		} else {
			bb.copy(parm, v)
		}
	}
	bb.Return(frame)
	return bb, nil
}

func (bb *blockBuilder) convert(cvt *checker.Convert) (*blockBuilder, Value) {
	switch cvt.Kind {
	case checker.Noop:
		bb, expr := bb.expr(cvt.Expr)
		if expr == nil {
			return bb, expr
		}
		dst := bb.buildType(cvt.Type())
		if !dst.isSmall() {
			// If the no-op convert converts one non-small type to another,
			// the non-small types are being passed around by address,
			// so we need to make this an address-type too.
			dst = &AddrType{Elem: dst}
		}
		// We use string equality here, because .eq()
		// checks for structural equality, but llvm
		// requires types to be actually equal, else need a cast.
		if expr.Type().String() == dst.String() {
			return bb, expr
		}
		// This should only be possible if both types are addresses.
		return bb, bb.bitCast(expr, dst)
	case checker.Drop:
		bb, _ := bb.expr(cvt.Expr)
		return bb, nil
	case checker.Deref:
		bb, v := bb.expr(cvt.Expr)
		if v != nil && v.Type().(*AddrType).Elem.isEmpty() {
			// If we dereferenced an pointer to an empty type;
			// return nil, which is the dereferenced-to empty value.
			// The containing expression should check and drop it.
			v = nil
		}
		if v != nil && v.Type().(*AddrType).Elem.isSmall() {
			v = bb.load(v)
		}
		return bb, v
	case checker.Ref:
		var v Value
		bb, expr := bb.expr(cvt.Expr)
		if dst := bb.buildType(cvt.Expr.Type()); dst.isSmall() {
			v = bb.alloc(dst)
			bb.store(v, expr)
		} else {
			// There is already a hidden alloc here to hold the non-small value.
			// Just use that one, don't copy to a new one.
			v = expr
		}
		if refType := bb.buildType(cvt.Type()); refType.String() != v.Type().String() {
			// The type checker may change the type to a compatible reference,
			// essentially collapsing a Noop convert into the Ref convert.
			// If so, we need to bitcast.
			v = bb.bitCast(v, refType)
		}
		return bb, v
	case checker.StrConvert:
		return bb.buildStrConvert(cvt)
	case checker.NumConvert:
		bb, v := bb.expr(cvt.Expr)
		dstType := bb.buildType(cvt.Type())
		if v.Type().eq(dstType) {
			return bb, v
		}
		return bb, bb.op(NumConvert, dstType, v)
	case checker.UnionConvert:
		return bb.unionConvert(cvt)
	default:
		panic(fmt.Sprintf("impossible checker.Convert.Kind: %d", cvt.Kind))
	}
}

func (bb *blockBuilder) buildStrConvert(cvt *checker.Convert) (*blockBuilder, Value) {
	bb, v := bb.expr(cvt.Expr)
	arrayType := v.Type().(*AddrType).Elem.(*StructType)
	n := bb.load(bb.field(v, arrayType.Fields[0]))
	arrayData := bb.load(bb.field(v, arrayType.Fields[1]))
	strType := bb.buildType(cvt.Type()).(*StructType)
	a := bb.alloc(strType)
	strData := bb.allocN(n, &IntType{Size: 8, Unsigned: true})
	bb.store(bb.field(a, strType.Fields[1]), strData)
	bb.store(bb.field(a, strType.Fields[0]), n)

	cond := bb.fun.newBlock(cvt.L)
	body := bb.fun.newBlock(cvt.L)
	done := bb.fun.newBlock(cvt.L)
	intType := bb.fun.mod.intType()
	i := bb.alloc(intType)
	bb.store(i, n)
	bb.jump(cond)
	cond.ifEq(cond.load(i), 0, done, body)
	one := body.intLit("1", intType)
	iMinusOne := body.op(Minus, intType, body.load(i), one)
	arrayElem := body.load(body.index(arrayData, iMinusOne))
	body.store(body.index(strData, iMinusOne), arrayElem)
	body.store(i, iMinusOne)
	body.jump(cond)

	return done, a
}

func (bb *blockBuilder) unionConvert(cvt *checker.Convert) (*blockBuilder, Value) {
	dstType := bb.buildType(cvt.Type())
	bb, src := bb.expr(cvt.Expr)

	switch dstType := dstType.(type) {
	case *AddrType:
		return bb.unionConvertToAddr(dstType, src)
	case *IntType:
		return bb.unionConvertToInt(cvt, dstType, src)
	case *StructType:
		// The source could be an IntType, *AddrType pointer-style union,
		// or an AddrType of a StructType union.
		if _, ok := src.Type().(*IntType); ok {
			return bb.unionConvertIntToUnion(cvt, dstType, src)
		}
		if isPointer(cvt.Expr.Type().(*checker.UnionType)) {
			return bb.unionConvertAddrToUnion(cvt, dstType, src)
		}
		return bb.unionConvertUnionToUnion(cvt, dstType, src)
	default:
		panic(fmt.Sprintf("bad union type: %s", dstType))
	}
	panic("unimplemented")
}

func (bb *blockBuilder) unionConvertToAddr(dstType *AddrType, src Value) (*blockBuilder, Value) {
	// The source is either the untyped case or the typed case.
	// If it is the untyped case, it must be an IntType; this is the null case.
	// If it is the typed case, it will be a StructType union with one data element;
	// and we want to get a pointer to this data element.
	if _, ok := src.Type().(*IntType); ok {
		return bb, bb.null(dstType)
	}

	structAddrType, ok := src.Type().(*AddrType)
	if !ok {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}
	structType, ok := structAddrType.Elem.(*StructType)
	if !ok || len(structType.Fields) != 2 {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}
	unionField := bb.field(src, structType.Fields[1])
	unionAddrType, ok := unionField.Type().(*AddrType)
	if !ok {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}
	unionType, ok := unionAddrType.Elem.(*UnionType)
	if !ok || len(unionType.Cases) != 1 {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}
	caseAddr := bb.Case(unionField, unionType.Cases[0])
	if !caseAddr.Type().(*AddrType).Elem.eq(dstType) {
		panic(fmt.Sprintf("impossible union convert: %s <- %s",
			dstType, caseAddr.Type()))
	}
	return bb, bb.load(caseAddr)
}

func unionType(typ checker.Type) *checker.UnionType {
	switch typ := typ.(type) {
	case *checker.UnionType:
		return typ
	case *checker.DefType:
		return unionType(typ.Inst.Type)
	}
	panic(fmt.Sprintf("not a union type: %s", typ))
}

func (bb *blockBuilder) unionConvertToInt(cvt *checker.Convert, dstType *IntType, src Value) (*blockBuilder, Value) {
	// The source must be an IntType too.
	if _, ok := src.Type().(*IntType); !ok {
		panic(fmt.Sprintf("impossible union convert src type: %T", src.Type()))
	}
	dst := bb.alloc(dstType)
	done := &blockBuilder{
		fun:        bb.fun,
		BasicBlock: &BasicBlock{Func: bb.fun.FuncDef},
		L:          cvt.Loc(),
	}
	dstUnion := unionType(cvt.Type())
	srcUnion := unionType(cvt.Expr.Type())
	for srcNum := range srcUnion.Cases {
		srcCase := &srcUnion.Cases[srcNum]
		dstNum := caseNum(dstUnion, srcCase.Name)
		if srcNum == len(srcUnion.Cases)-1 {
			bb.store(dst, bb.intLit(strconv.Itoa(dstNum), dstType))
			bb.jump(done)
			break
		}
		yes := bb.fun.newBlock(cvt.Loc())
		no := bb.fun.newBlock(cvt.Loc())
		bb.ifEq(src, srcNum, yes, no)
		yes.store(dst, yes.intLit(strconv.Itoa(dstNum), dstType))
		yes.jump(done)
		bb = no
	}

	bb.fun.mod.nextBlock++
	done.Num = bb.fun.mod.nextBlock - 1
	bb.fun.Blocks = append(bb.fun.Blocks, done.BasicBlock)
	return done, done.load(dst)
}

func (bb *blockBuilder) unionConvertAddrToUnion(cvt *checker.Convert, dstType *StructType, src Value) (*blockBuilder, Value) {
	srcUnion := unionType(cvt.Expr.Type())
	dstUnion := unionType(cvt.Type())
	if len(srcUnion.Cases) != 2 {
		panic(fmt.Sprintf("impossible pointer union type: %s", srcUnion))
	}
	nullCaseNum := -1
	nonNullCaseNum := -1
	var nonNullCase *CaseDef
	for i := range srcUnion.Cases {
		srcCase := &srcUnion.Cases[i]
		name := srcCase.Name
		if srcCase.Type == nil {
			nullCaseNum = caseNum(dstUnion, name)
		} else {
			nonNullCaseNum = caseNum(dstUnion, srcCase.Name)
			nonNullCase = unionCase(dstType.Fields[1].Type.(*UnionType), name)
		}
	}
	if nullCaseNum < 0 || nonNullCaseNum < 0 {
		panic(fmt.Sprintf("impossible pointer union type: %s", srcUnion))
	}

	res := bb.alloc(dstType)
	tag := bb.field(res, dstType.Fields[0])
	tagIntType := tag.Type().(*AddrType).Elem
	yes := bb.fun.newBlock(cvt.Loc())
	no := bb.fun.newBlock(cvt.Loc())
	done := bb.fun.newBlock(cvt.Loc())
	bb.ifNull(src, yes, no)

	yes.store(tag, yes.intLit(strconv.Itoa(nullCaseNum), tagIntType))
	yes.jump(done)

	no.store(tag, no.intLit(strconv.Itoa(nonNullCaseNum), tagIntType))
	unionField := no.field(res, dstType.Fields[1])
	valField := no.Case(unionField, nonNullCase)
	no.store(valField, src)
	no.jump(done)
	return done, res
}

func (bb *blockBuilder) unionConvertIntToUnion(cvt *checker.Convert, dstType *StructType, src Value) (*blockBuilder, Value) {
	if _, ok := src.Type().(*IntType); !ok {
		panic(fmt.Sprintf("impossible union convert src type: %T", src.Type()))
	}

	res := bb.alloc(dstType)
	tag := bb.field(res, dstType.Fields[0])
	tagIntType := tag.Type().(*AddrType).Elem
	done := &blockBuilder{
		fun:        bb.fun,
		BasicBlock: &BasicBlock{Func: bb.fun.FuncDef},
		L:          cvt.Loc(),
	}
	dstUnion := unionType(cvt.Type())
	srcUnion := unionType(cvt.Expr.Type())
	for srcNum := range srcUnion.Cases {
		srcCase := &srcUnion.Cases[srcNum]
		dstNum := caseNum(dstUnion, srcCase.Name)
		if srcNum == len(srcUnion.Cases)-1 {
			bb.store(tag, bb.intLit(strconv.Itoa(dstNum), tagIntType))
			bb.jump(done)
			break
		}
		yes := bb.fun.newBlock(cvt.Loc())
		no := bb.fun.newBlock(cvt.Loc())
		bb.ifEq(src, srcNum, yes, no)
		yes.store(tag, yes.intLit(strconv.Itoa(dstNum), tagIntType))
		yes.jump(done)
		bb = no
	}
	bb.fun.mod.nextBlock++
	done.Num = bb.fun.mod.nextBlock - 1
	bb.fun.Blocks = append(bb.fun.Blocks, done.BasicBlock)
	return done, res
}

func (bb *blockBuilder) unionConvertUnionToUnion(cvt *checker.Convert, dstStructType *StructType, src Value) (*blockBuilder, Value) {
	srcStructAddrType, ok := src.Type().(*AddrType)
	if !ok {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}
	srcStructType, ok := srcStructAddrType.Elem.(*StructType)
	if !ok || len(srcStructType.Fields) != 2 {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}
	srcUnionType, ok := srcStructType.Fields[1].Type.(*UnionType)
	if !ok {
		panic(fmt.Sprintf("impossible union convert src type: %s", src.Type()))
	}

	dstUnionType, ok := dstStructType.Fields[1].Type.(*UnionType)
	if !ok {
		panic(fmt.Sprintf("impossible union convert dst type: %s", dstStructType))
	}

	res := bb.alloc(dstStructType)
	tag := bb.field(res, dstStructType.Fields[0])
	srcTagField := bb.load(bb.field(src, srcStructType.Fields[0]))
	tagIntType := tag.Type().(*AddrType).Elem
	done := &blockBuilder{
		fun:        bb.fun,
		BasicBlock: &BasicBlock{Func: bb.fun.FuncDef},
		L:          cvt.Loc(),
	}
	dstUnion := unionType(cvt.Type())
	srcUnion := unionType(cvt.Expr.Type())
	for srcNum := range srcUnion.Cases {
		srcCase := &srcUnion.Cases[srcNum]
		dstNum := caseNum(dstUnion, srcCase.Name)
		if srcNum == len(srcUnion.Cases)-1 {
			bb.store(tag, bb.intLit(strconv.Itoa(dstNum), tagIntType))
			if c := unionCase(srcUnionType, srcCase.Name); c != nil {
				srcUnionField := bb.field(src, srcStructType.Fields[1])
				srcCaseAddr := bb.Case(srcUnionField, c)
				dstCase := unionCase(dstUnionType, srcCase.Name)
				dstUnionField := bb.field(res, dstStructType.Fields[1])
				dstCaseAddr := bb.Case(dstUnionField, dstCase)
				if dstCaseAddr.Type().(*AddrType).Elem.isSmall() {
					bb.store(dstCaseAddr, bb.load(srcCaseAddr))
				} else {
					bb.copy(dstCaseAddr, srcCaseAddr)
				}
			}
			bb.jump(done)
			break
		}
		yes := bb.fun.newBlock(cvt.Loc())
		no := bb.fun.newBlock(cvt.Loc())
		bb.ifEq(srcTagField, srcNum, yes, no)
		yes.store(tag, yes.intLit(strconv.Itoa(dstNum), tagIntType))
		if c := unionCase(srcUnionType, srcCase.Name); c != nil {
			srcUnionField := yes.field(src, srcStructType.Fields[1])
			srcCaseAddr := yes.Case(srcUnionField, c)
			dstCase := unionCase(dstUnionType, c.Name)
			dstUnionField := yes.field(res, dstStructType.Fields[1])
			dstCaseAddr := yes.Case(dstUnionField, dstCase)
			if dstCaseAddr.Type().(*AddrType).Elem.isSmall() {
				yes.store(dstCaseAddr, yes.load(srcCaseAddr))
			} else {
				yes.copy(dstCaseAddr, srcCaseAddr)
			}
		}
		yes.jump(done)
		bb = no
	}

	bb.fun.mod.nextBlock++
	done.Num = bb.fun.mod.nextBlock - 1
	bb.fun.Blocks = append(bb.fun.Blocks, done.BasicBlock)

	return done, res
}

func (bb *blockBuilder) arrayLit(lit *checker.ArrayLit) (*blockBuilder, Value) {
	mod := bb.fun.mod
	arrayType := bb.buildType(lit.T).(*StructType)
	elemType := arrayType.Fields[1].Type.(*ArrayType).Elem

	a := bb.alloc(arrayType)
	lenVal := bb.intLit(strconv.Itoa(len(lit.Elems)), mod.intType())
	lenField := bb.field(a, arrayType.Fields[0])
	bb.store(lenField, lenVal)

	dataField := bb.field(a, arrayType.Fields[1])
	if len(lit.Elems) == 0 {
		// Empty array literals are a relatively common case.
		// We shouldn't bother calling alloc here.
		bb.store(dataField, bb.null(&ArrayType{Elem: elemType}))
		return bb, a
	}
	dataVal := bb.allocNImm(len(lit.Elems), elemType)
	bb.store(dataField, dataVal)
	for i, expr := range lit.Elems {
		var v Value
		bb, v = bb.expr(expr)
		idx := bb.index(dataVal, bb.intLit(strconv.Itoa(i), mod.intType()))
		if elemType.isSmall() {
			bb.store(idx, v)
		} else {
			bb.copy(idx, v)
		}
	}
	return bb, a
}

func (bb *blockBuilder) structLit(lit *checker.StructLit) (*blockBuilder, Value) {
	structType := bb.buildType(lit.T).(*StructType)
	if structType.isEmpty() {
		return bb, nil
	}
	a := bb.alloc(structType)
	var i int
	for _, expr := range lit.Fields {
		var v Value
		bb, v = bb.expr(expr)
		if v == nil {
			continue
		}
		field := bb.field(a, structType.Fields[i])
		if structType.Fields[i].Type.isSmall() {
			bb.store(field, v)
		} else {
			bb.copy(field, v)
		}
		i++
	}
	return bb, a
}

func (bb *blockBuilder) unionLit(lit *checker.UnionLit) (*blockBuilder, Value) {
	var v Value
	if lit.Val != nil {
		bb, v = bb.expr(lit.Val)
	}
	n := caseNum(lit.Union, lit.Case.Name)
	switch t := bb.buildType(lit.T).(type) {
	case *IntType:
		return bb, bb.intLit(strconv.Itoa(n), t)
	case *AddrType:
		if v == nil {
			return bb, bb.null(t)
		} else {
			return bb, v
		}
	case *StructType:
		a := bb.alloc(t)
		tag := bb.field(a, t.Fields[0])
		bb.store(tag, bb.intLit(strconv.Itoa(n), bb.fun.mod.intType()))
		if v == nil {
			return bb, a
		}
		data := bb.field(a, t.Fields[1])
		c := unionCase(t.Fields[1].Type.(*UnionType), lit.Case.Name)
		cas := bb.Case(data, c)
		if c.Type.isSmall() {
			bb.store(cas, v)
		} else {
			bb.copy(cas, v)
		}
		return bb, a
	default:
		panic(fmt.Sprintf("bad union type: %s", t))
	}
}

func caseNum(u *checker.UnionType, name string) int {
	for i := range u.Cases {
		if u.Cases[i].Name == name {
			return i
		}
	}
	panic("no such case")
}

// unionCase returns the case of the UnionType with the given name
// or nil if there is not typed case of that name.
func unionCase(u *UnionType, name string) *CaseDef {
	name = strings.TrimSuffix(name, "?")
	for _, c := range u.Cases {
		if c.Name == name {
			return c
		}
	}
	return nil
}

func (bb *blockBuilder) blockLit(lit *checker.BlockLit) (*blockBuilder, Value) {
	headerType := bb.buildType(lit.Type()).(*StructType)
	a := bb.alloc(headerType)
	fb := bb.fun.mod.buildBlockLit(bb.fun, lit)
	funcVal := bb.Func(fb)
	funcField := bb.field(a, headerType.Fields[0])
	bb.store(funcField, funcVal)

	dataVal := bb.blockCaps(fb, lit)
	dataField := bb.field(a, headerType.Fields[1])
	bb.store(dataField, dataVal)
	return bb, a
}

func (bb *blockBuilder) blockCaps(fb *funcBuilder, blockLit *checker.BlockLit) Value {
	capsType := fb.blockType
	if capsType.isEmpty() {
		return bb.null(&AddrType{Elem: capsType})
	}
	oldLoc := bb.L
	caps := bb.alloc(capsType)
	for i, cap := range blockLit.Caps {
		bb.L = cap.L
		var v Value
		switch {
		case cap.Parm != nil:
			v = bb.fun.parmAlloc[bb.fun.parmDef[cap.Parm]]
		case cap.Local != nil:
			v = bb.fun.localAlloc[cap.Local]
		case cap.Cap != nil:
			parentBlock := bb.parm(bb.fun.Parms[0])
			field := bb.field(parentBlock, bb.fun.capField[cap.Cap])
			v = bb.load(field)
		case cap.Expr != nil:
			if bb, v = bb.expr(cap.Expr); v == nil {
				break
			}
			a := bb.alloc(v.Type())
			if v.Type().isSmall() {
				bb.store(a, v)
			} else {
				bb.copy(a, v)
			}
			v = a
		default:
			panic(fmt.Sprintf("bad capture: %#v", cap))
		}
		if v != nil {
			bb.store(bb.field(caps, capsType.Fields[i]), v)
		}
	}
	bb.L = oldLoc

	if !fb.longReturns {
		return caps
	}

	var longRetType Type
	if bb.fun.parent == nil {
		longRetType = bb.fun.Type.Ret
	} else {
		longRetType = bb.fun.parent.Type.Ret
	}
	if !longRetType.isEmpty() {
		var returnLoc Value
		if bb.fun.blockType != nil {
			parentBlock := bb.parm(bb.fun.Parms[0])
			returnField := bb.field(parentBlock, bb.fun.returnField)
			returnLoc = bb.load(returnField)
		} else {
			returnLoc = bb.parm(bb.fun.Parms[len(bb.fun.Parms)-1])
		}
		bb.store(bb.field(caps, fb.returnField), returnLoc)
	}

	frame := bb.frame()
	bb.store(bb.field(caps, fb.frameField), frame)
	return caps
}

func (bb *blockBuilder) strLit(lit *checker.StrLit) Value {
	mod := bb.fun.mod
	strDef := &StrDef{Mod: mod.Mod, Num: len(mod.Strings), Text: lit.Text}
	mod.Strings = append(mod.Strings, strDef)

	strType := bb.buildType(lit.T).(*StructType)
	a := bb.alloc(strType)
	lenVal := bb.intLit(strconv.Itoa(len(lit.Text)), mod.intType())
	lenField := bb.field(a, strType.Fields[0])
	bb.store(lenField, lenVal)
	dataVal := bb.String(strDef)
	dataField := bb.field(a, strType.Fields[1])
	bb.store(dataField, dataVal)
	return a
}

func (bb *blockBuilder) store(dst, src Value) {
	addr, ok := dst.Type().(*AddrType)
	if !ok {
		panic(fmt.Sprintf("store to non-address type %s", dst.Type()))
	}
	if !addr.Elem.eq(src.Type()) {
		panic(fmt.Sprintf("store type mismatch: got %s, want %s",
			src.Type(), addr.Elem))
	}
	if !src.Type().isSmall() {
		panic(fmt.Sprintf("store from non-small type %s", src.Type()))
	}

	// If the types are not equal, they must be
	// structurally equal addresses; bitcast.
	if src.Type().String() != addr.Elem.String() {
		src = bb.bitCast(src, addr.Elem)
	}

	r := &Store{Dst: dst, Src: src, L: bb.L}
	bb.addInstr(r)
}

func (bb *blockBuilder) copy(dst, src Value) *Copy {
	if _, ok := dst.Type().(*AddrType); !ok {
		panic(fmt.Sprintf("copy to non-address type %s", dst.Type()))
	}
	if !dst.Type().eq(src.Type()) {
		panic(fmt.Sprintf("copy type mismatch: got %s, want %s", src.Type(), dst.Type()))
	}
	if _, ok := src.Type().(*AddrType); !ok {
		panic(fmt.Sprintf("copy from non-address type %s", src.Type()))
	}
	r := &Copy{Dst: dst, Src: src, L: bb.L}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) call(fun Value, args []Value) *Call {
	if bb.L == (loc.Loc{}) {
		panic("call without a loc")
	}
	funcType, ok := fun.Type().(*FuncType)
	if !ok {
		panic(fmt.Sprintf("calling non-function type %s", fun.Type()))
	}
	r := &Call{Func: fun, Args: args, L: bb.L}
	bb.addInstr(r)
	if funcType.Terminal {
		bb.addInstr(&Unreach{L: bb.L})
	}
	return r
}

func (bb *blockBuilder) ifEq(v Value, x int, yes, no *blockBuilder) *If {
	if x < 0 {
		panic("bad x — must be non-negative")
	}
	r := &If{
		Value: v,
		Op:    Eq,
		X:     x,
		Yes:   yes.BasicBlock,
		No:    no.BasicBlock,
		L:     bb.L,
	}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) ifLess(v Value, x int, yes, no *blockBuilder) *If {
	if x < 0 {
		panic("bad x — must be non-negative")
	}
	r := &If{
		Value: v,
		Op:    Less,
		X:     x,
		Yes:   yes.BasicBlock,
		No:    no.BasicBlock,
		L:     bb.L,
	}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) ifLessValue(v, x Value, yes, no *blockBuilder) *If {
	r := &If{
		Value:  v,
		Op:     Less,
		XValue: x,
		Yes:    yes.BasicBlock,
		No:     no.BasicBlock,
		L:      bb.L,
	}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) ifLessEq(v, x Value, yes, no *blockBuilder) *If {
	r := &If{
		Value:  v,
		Op:     LessEq,
		XValue: x,
		Yes:    yes.BasicBlock,
		No:     no.BasicBlock,
		L:      bb.L,
	}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) ifGreaterValue(v, x Value, yes, no *blockBuilder) *If {
	r := &If{
		Value:  v,
		Op:     Greater,
		XValue: x,
		Yes:    yes.BasicBlock,
		No:     no.BasicBlock,
		L:      bb.L,
	}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) ifNull(v Value, yes, no *blockBuilder) *If {
	if _, ok := v.Type().(*AddrType); !ok {
		panic(fmt.Sprintf("ifNull from non-address type %s", v.Type()))
	}
	r := &If{
		Value: v,
		Op:    Eq,
		Yes:   yes.BasicBlock,
		No:    no.BasicBlock,
		L:     bb.L,
	}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) jump(to *blockBuilder) *Jump {
	r := &Jump{Dst: to.BasicBlock, L: bb.L}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) Return(frame Value) *Return {
	r := &Return{Frame: frame, L: bb.L}
	bb.addInstr(r)
	return r
}

func (bb *blockBuilder) frame() Value {
	if bb.fun.blockType != nil {
		bb.fun.longReturns = true
		block := bb.parm(bb.fun.Parms[0])
		blockType := bb.fun.Parms[0].Type.(*AddrType).Elem.(*StructType)
		frameField := blockType.Fields[len(blockType.Fields)-1]
		return bb.load(bb.field(block, frameField))
	}
	if len(bb.fun.b0.Instrs) > 0 {
		if frame, ok := bb.fun.b0.Instrs[0].(*Frame); ok {
			return frame
		}
	}
	v := &Frame{}
	bb.fun.mod.nextInstr++
	v.setNum(bb.fun.mod.nextInstr - 1)
	for _, use := range v.Uses() {
		use.addUser(v)
	}
	bb.fun.b0.Instrs = append([]Instruction{v}, bb.fun.b0.Instrs...)
	return v
}

func (bb *blockBuilder) alloc(typ Type) *Alloc {
	if typ.isEmpty() {
		panic("bad alloc of an empty type")
	}
	v := &Alloc{CountImm: -1, T: &AddrType{Elem: typ}, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) allocNImm(n int, typ Type) *Alloc {
	if typ.isEmpty() {
		panic("bad alloc of an empty type")
	}
	v := &Alloc{CountImm: n, T: &ArrayType{Elem: typ}, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) allocN(n Value, typ Type) *Alloc {
	if typ.isEmpty() {
		panic("bad alloc of an empty type")
	}
	v := &Alloc{Count: n, T: &ArrayType{Elem: typ}, L: bb.L}
	// Unlike alloc and allocNImm, allocN depends on a dynamic size value.
	// It cannot go in block0, since the size is not computed in block 0.
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) load(addr Value) *Load {
	t := addr.Type().(*AddrType)
	v := &Load{Addr: addr, AddrType: *t, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) bitCast(src Value, dstType Type) *BitCast {
	if !src.Type().isSmall() {
		panic(fmt.Sprintf("bitcast from non-small type %s", src.Type()))
	}
	if !dstType.isSmall() {
		panic(fmt.Sprintf("bitcast to non-small type %s", dstType))
	}
	v := &BitCast{Src: src, T: dstType, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) Func(fb *funcBuilder) *Func {
	bb.fun.outRefs[fb]++
	fb.inRefs[bb.fun]++
	v := &Func{Def: fb.FuncDef, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) String(x *StrDef) *String {
	v := &String{Def: x, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) Var(x *VarDef) *Var {
	v := &Var{Def: x, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) parm(p *ParmDef) *Parm {
	if !p.Type.isSmall() {
		panic(fmt.Sprintf("bad, non-small parameter type: %s", p.Type))
	}
	v := &Parm{Def: p, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) field(base Value, field *FieldDef) *Field {
	t := base.Type().(*AddrType).Elem.(*StructType)
	v := &Field{Base: base, Def: field, BaseType: *t, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) Case(base Value, cas *CaseDef) *Case {
	t := base.Type().(*AddrType).Elem.(*UnionType)
	v := &Case{Base: base, Def: cas, BaseType: *t, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) index(base, index Value) *Index {
	t := base.Type().(*ArrayType)
	if intType := bb.fun.mod.intType(); !index.Type().eq(intType) {
		panic(fmt.Sprintf("index type mismatch: got %s, want %s", index.Type(), intType))
	}
	v := &Index{Base: base, Index: index, BaseType: *t, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) slice(base, index Value) *Slice {
	t := base.Type().(*ArrayType)
	if intType := bb.fun.mod.intType(); !index.Type().eq(intType) {
		panic(fmt.Sprintf("silce type mismatch: got %s, want %s", index.Type(), intType))
	}
	v := &Slice{Base: base, Index: index, BaseType: *t, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) intLit(text string, typ Type) *Int {
	t, ok := typ.(*IntType)
	if !ok {
		panic(fmt.Sprintf("got %s, want IntType", typ))
	}
	v := &Int{Text: text, T: *t, L: bb.L}
	if _, ok := v.Val.SetString(text, 0); !ok {
		panic("impossible")
	}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) checkerIntLit(lit *checker.IntLit, typ Type) *Int {
	t, ok := typ.(*IntType)
	if !ok {
		panic(fmt.Sprintf("got %s, want IntType", typ))
	}
	v := &Int{Text: lit.Text, Val: lit.Val, T: *t, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) floatLit(lit *checker.FloatLit, typ Type) *Float {
	t, ok := typ.(*FloatType)
	if !ok {
		panic(fmt.Sprintf("got %s, want FloatType", typ))
	}
	v := &Float{Text: lit.Text, T: *t, Val: lit.Val, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) null(typ Type) *Null {
	_, isAddr := typ.(*AddrType)
	_, isArray := typ.(*ArrayType)
	if !isAddr && !isArray {
		panic(fmt.Sprintf("got %s, want AddrType or ArrayType", typ))
	}
	v := &Null{T: typ, L: bb.L}
	bb.addValue(v)
	return v
}

var opMap = map[checker.Op]OpKind{
	checker.BitNot:     BitNot,
	checker.BitXor:     BitXor,
	checker.BitAnd:     BitAnd,
	checker.BitOr:      BitOr,
	checker.LeftShift:  LeftShift,
	checker.RightShift: RightShift,
	checker.Negate:     Negate,
	checker.Minus:      Minus,
	checker.Plus:       Plus,
	checker.Times:      Times,
	checker.Divide:     Divide,
	checker.Modulus:    Modulus,
	checker.Eq:         Eq,
	checker.Panic:      Panic,
	checker.Print:      Print,
}

func (bb *blockBuilder) op(op OpKind, typ Type, args ...Value) *Op {
	v := &Op{Op: op, Args: args, T: typ, L: bb.L}
	bb.addValue(v)
	return v
}

func (bb *blockBuilder) addInstr(r Instruction) {
	if bb.done {
		panic("adding to a complete block")
	}
	bb.Instrs = append(bb.Instrs, r)
	for _, use := range r.Uses() {
		use.addUser(r)
	}
	if t, ok := r.(Terminal); ok {
		bb.done = true
		for _, o := range t.Out() {
			o.addIn(bb.BasicBlock)
		}
	}
}

func (bb *blockBuilder) addValue(v Value) {
	bb.addInstr(v)
	bb.fun.mod.nextInstr++
	v.setNum(bb.fun.mod.nextInstr - 1)
}
