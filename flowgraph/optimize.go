package flowgraph

import (
	"fmt"
	"strings"
)

func (mb *modBuilder) optimize() {
	for _, fb := range mb.funcBuilders {
		moveAllocsToStack(fb.FuncDef)
		inlineCalls(fb)
		rmSelfTailCalls(fb)
		rmAllocs(fb.FuncDef)
		mergeBlocks(fb.FuncDef)
	}
	var i int
	for _, fb := range mb.funcBuilders {
		if disused(fb) {
			continue
		}
		moveStackAllocsToFront(fb.FuncDef)
		renumber(fb.FuncDef)
		mb.funcBuilders[i] = fb
		i++
	}
	mb.funcBuilders = mb.funcBuilders[:i]
}

func disused(fb *funcBuilder) bool {
	return !fb.Exp && len(fb.inRefs) == 0 &&
		(fb.Mod != "main" || fb.Name != "main") &&
		!strings.Contains(fb.Name, "<init>")
}

func moveStackAllocsToFront(f *FuncDef) {
	var front []Instruction
	for _, b := range f.Blocks {
		var i int
		for _, r := range b.Instrs {
			if a, ok := r.(*Alloc); ok && a.Stack {
				front = append(front, a)
			} else {
				b.Instrs[i] = r
				i++
			}
		}
		b.Instrs = b.Instrs[:i]
	}
	if len(front) > 0 {
		f.Blocks[0].Instrs = append(front, f.Blocks[0].Instrs...)
	}
}

func renumber(f *FuncDef) {
	var valueNum int
	for blockNum, b := range f.Blocks {
		b.Num = blockNum
		for _, r := range b.Instrs {
			if v, ok := r.(Value); ok {
				v.setNum(valueNum)
				valueNum++
			}
		}
	}
}

func rmUnreach(f *FuncDef) {
	if len(f.Blocks) == 0 {
		return
	}
	seen := make(map[*BasicBlock]bool)
	seen[f.Blocks[0]] = true
	todo := []*BasicBlock{f.Blocks[0]}
	for len(todo) > 0 {
		b := todo[len(todo)-1]
		todo = todo[:len(todo)-1]
		for _, o := range b.Out() {
			if !seen[o] {
				seen[o] = true
				todo = append(todo, o)
			}
		}
	}
	var i int
	for _, b := range f.Blocks {
		if seen[b] {
			f.Blocks[i] = b
			i++
			continue
		}
		for _, o := range b.Out() {
			o.rmIn(b)
		}
		for _, r := range b.Instrs {
			for _, u := range r.Uses() {
				u.rmUser(r)
			}
		}
	}
	f.Blocks = f.Blocks[:i]
}

func rmDeletes(f *FuncDef) {
	var todo []Instruction
	seen := make(map[Instruction]bool)
	for _, b := range f.Blocks {
		for _, r := range b.Instrs {
			if seen[r] {
				continue
			}
			if r.isDeleted() {
				todo = append(todo, r)
				seen[r] = true
				continue
			}
			if v, ok := r.(Value); ok && (len(v.UsedBy()) == 0 || isWriteOnlyAlloc(v)) {
				todo = append(todo, r)
				seen[r] = true
				continue
			}
		}
	}
	for len(todo) > 0 {
		r := todo[len(todo)-1]
		todo = todo[:len(todo)-1]
		if v, ok := r.(Value); ok {
			for _, u := range v.UsedBy() {
				if !seen[u] {
					todo = append(todo, u)
					seen[u] = true
				}
			}
		}
		r.delete()
		for _, u := range r.Uses() {
			u.rmUser(r)
			if !seen[u] && len(u.UsedBy()) == 0 || isWriteOnlyAlloc(u) {
				todo = append(todo, u)
				seen[u] = true
			}
		}
	}
	for _, b := range f.Blocks {
		var i int
		for _, r := range b.Instrs {
			if !r.isDeleted() {
				b.Instrs[i] = r
				i++
			}
		}
		b.Instrs = b.Instrs[:i]
	}
}

func isWriteOnlyAlloc(v Value) bool {
	_, ok := v.(*Alloc)
	return ok && isWriteOnly(v)
}

func isWriteOnly(v Value) bool {
	for _, u := range v.UsedBy() {
		switch u := u.(type) {
		case *Copy:
			if u.Dst != v {
				return false
			}
		case *Store:
			if u.Dst != v {
				return false
			}
		case *Field:
			if !isWriteOnly(u) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func rmAllocs(f *FuncDef) {
	subValues := make(map[Value]Value)
	for _, b := range f.Blocks {
	nextInstr:
		for _, r := range b.Instrs {
			alloc, ok := r.(*Alloc)
			if !ok || !alloc.Type().isSmall() {
				continue
			}
			var store *Store
			for _, u := range alloc.UsedBy() {
				switch u := u.(type) {
				case *Load:
					// OK
				case *Store:
					if store != nil || u.Dst != alloc {
						continue nextInstr
					}
					store = u
				default:
					continue nextInstr
				}
			}
			if store == nil {
				continue
			}
			alloc.delete()
			store.delete()
			for _, u := range alloc.UsedBy() {
				if ld, ok := u.(*Load); ok {
					ld.delete()
					subValues[ld] = store.Src
				}
			}
		}
	}
	for _, b := range f.Blocks {
		for _, r := range b.Instrs {
			r.subValues(subValues)
		}
	}
	rmDeletes(f)
}

func mergeBlocks(f *FuncDef) {
	var done []*BasicBlock
	for _, b := range f.Blocks {
		ins := b.In()
		if len(ins) != 1 || len(ins[0].Out()) != 1 {
			done = append(done, b)
			continue
		}
		in := ins[0]
		// We remove the last instruction of in here.
		// Since it only goes out to one block, it must have been a Jump,
		// and thus has no uses to update.
		in.Instrs = append(in.Instrs[:len(in.Instrs)-1], b.Instrs...)
		for _, o := range b.Out() {
			o.rmIn(b)
			o.addIn(in)
		}
	}
	f.Blocks = done
}

func moveAllocsToStack(f *FuncDef) {
	leaks := findLeaks(f)
	for _, b := range f.Blocks {
		for _, r := range b.Instrs {
			if a, ok := r.(*Alloc); ok && !a.Stack {
				a.Stack = !escapes(leaks, a)
			}
		}
	}
}

func escapes(leaks map[Value]bool, v Value) bool {
	if a, ok := v.(*Alloc); ok && a.Count != nil {
		return true
	}
	for _, u := range v.UsedBy() {
		switch u := u.(type) {
		case *Store:
			if u.Src == v && leaks[u.Dst] {
				return true
			}
		case *Field:
			if escapes(leaks, u) {
				return true
			}
		case *Case:
			if escapes(leaks, u) {
				return true
			}
		case *Index:
			if escapes(leaks, u) {
				return true
			}
		case *Slice:
			if escapes(leaks, u) {
				return true
			}
		case *Call:
			funcType := u.Func.Type().(*FuncType)
			var i int
			if s, ok := u.Func.(*Func); !ok ||
				len(s.Def.Parms) > 0 && s.Def.Parms[0].BlockData {
				// This is a function expression call.
				// The 0th argument is the captures data,
				// which never escapes, so skip it.
				i++
			}
			// funcType.Parms has fewer elements than u.Args
			// in the case that the function has a return argument
			// at the end. Since the return argument can never escape,
			// we skip checking it here.
			for ; i < len(funcType.Parms); i++ {
				if _, ok := funcType.Parms[i].(*AddrType); ok && u.Args[i] == v {
					return true
				}
			}
		}
	}
	return false
}

func findLeaks(f *FuncDef) map[Value]bool {
	var todo []Value
	leaks := make(map[Value]bool)
	for _, b := range f.Blocks {
		for _, r := range b.Instrs {
			switch r := r.(type) {
			case *Alloc:
				if r.Count != nil {
					leak(&todo, leaks, r, "array alloc")
				}
			case *Var:
				leak(&todo, leaks, r, "var")
			case *Parm:
				leak(&todo, leaks, r, "parm")
			case *Call:
				// Iterate over funcType.Parms, not r.Args.
				// The former  skips any return param.
				// The return param cannot leak,
				// since the call can only write into it.
				funcType := r.Func.Type().(*FuncType)
				for i := range funcType.Parms {
					arg := r.Args[i]
					_, isAddr := arg.Type().(*AddrType)
					_, isArray := arg.Type().(*ArrayType)
					if isAddr || isArray {
						leak(&todo, leaks, arg, "arg %d of %s", i, r)
					}
				}
			}
		}
	}
	for len(todo) > 0 {
		v := todo[len(todo)-1]
		todo = todo[:len(todo)-1]
		switch v := v.(type) {
		case *Load:
			_, isAddr := v.Type().(*AddrType)
			_, isArray := v.Type().(*ArrayType)
			if isAddr || isArray {
				// v is an address-typed load and it leaks
				// (probably stored into a leaky location).
				// So the address it is holding must also leak.
				leak(&todo, leaks, v.Addr, "loaded address leaks x%d", v.Num())
			}
		// If a Field, Case, Index, or Slice leak,
		// the entire base needs is marked as a leak,
		// becasue any Copy into the base
		// stores into the leaked component.
		//
		// TODO: we could instead leaked components at the Copy level:
		// If the Field.Base is the .Dst of a Copy, Copy.Src leaks,
		// but Field.Base itself needn't leak.
		// if Copy.Dst is UsedBy as the Base of a leaked field, Copy.Src leaks.
		case *Field:
			leak(&todo, leaks, v.Base, "base of leaked field x%d", v.Num())
		case *Case:
			leak(&todo, leaks, v.Base, "base of leaked case x%d", v.Num())
		case *Index:
			leak(&todo, leaks, v.Base, "base of leaked index x%d", v.Num())
		case *Slice:
			leak(&todo, leaks, v.Base, "base of leaked slice x%d", v.Num())
		}

		for _, u := range v.UsedBy() {
			switch u := u.(type) {
			case *Load:
				_, isAddr := u.Type().(*AddrType)
				_, isArray := u.Type().(*ArrayType)
				if isArray || isAddr {
					// v is an address-type location and the location leaks.
					// Any address loaded from v must therefore leak,
					// since the address is stored in leaky v.
					leak(&todo, leaks, u, "load address from leaked x%d", v.Num())
				}
			case *Store:
				if u.Dst == v {
					// v leaks; anything stored into v is stored into a leak,
					// so it itself is also a leak.
					leak(&todo, leaks, u.Src, "stored into leak x%d", u.Dst.Num())
					continue
				}
				// v leaks and is stored into u.Dst,
				// any address loaded from u.Dst
				// is therefore a leaky address
				// (since it might be v).
				for _, uu := range u.Dst.UsedBy() {
					l, ok := uu.(*Load)
					if !ok {
						continue
					}
					leak(&todo, leaks, l, "load of stored leak x%d: %s",
						u.Dst.Num(), u)
				}
			case *Copy:
				if u.Dst == v {
					leak(&todo, leaks, u.Src, "copied to leaked x%d", v.Num())
				}
			case *Field:
				leak(&todo, leaks, u, "field of leaked base x%d", v.Num())
			case *Case:
				leak(&todo, leaks, u, "case of leaked base x%d", v.Num())
			case *Index:
				leak(&todo, leaks, u, "index of leaked base x%d", v.Num())
			case *Slice:
				leak(&todo, leaks, u, "slice of leaked base x%d", v.Num())
			}
		}
	}
	return leaks
}

func leak(todo *[]Value, leaks map[Value]bool, v Value, f string, xs ...interface{}) {
	if leaks[v] {
		return
	}
	leaks[v] = true
	*todo = append(*todo, v)
}

func inlineCalls(fb *funcBuilder) {
	todo := fb.Blocks
	var done []*BasicBlock
	for len(todo) > 0 {
		b := todo[0]
		todo = todo[1:]
		for j, r := range b.Instrs {
			c, ok := r.(*Call)
			if !ok {
				continue
			}
			def := staticFunc(c)
			if def == nil || !canInline(fb, def) {
				continue
			}
			updateRefs(fb, def)
			parms := make(map[*ParmDef]Value)
			for i := range def.Parms {
				parms[def.Parms[i]] = c.Args[i]
			}
			c.delete()
			tail := &BasicBlock{Func: b.Func, Instrs: b.Instrs[j+1:]}
			for _, o := range b.Out() {
				o.rmIn(b)
				o.addIn(tail)
			}
			inlined := copyBlocks(fb, def.Blocks)
			subParms(parms, inlined)
			subCaps(blockCaps(fb.FuncDef, c), inlined)
			subReturn(tail, inlined, strings.HasPrefix(fb.Name, "<block"))
			b.Instrs = append(b.Instrs[:j+1:j+1], &Jump{Dst: inlined[0], L: c.L})
			inlined[0].addIn(b)

			todo = append(append(inlined, tail), todo...)
			break
		}
		done = append(done, b)
	}
	fb.Blocks = done
	rmUnreach(fb.FuncDef)
	rmDeletes(fb.FuncDef)
}

func updateRefs(fb *funcBuilder, ref *FuncDef) {
	for r := range fb.outRefs {
		if r.FuncDef == ref {
			fb.outRefs[r]--
			if fb.outRefs[r] == 0 {
				delete(fb.outRefs, r)
			}
			if fb.outRefs[r] < 0 {
				panic("impossible")
			}
			r.inRefs[fb]--
			if r.inRefs[fb] == 0 {
				delete(r.inRefs, fb)
			}
			if r.inRefs[fb] < 0 {
				panic("impossible")
			}
			if !strings.Contains(ref.Name, "<block") && fb.FuncDef != ref {
				fb.inlineNonBlocks++
			}
			for o := range r.outRefs {
				fb.outRefs[o]++
				o.inRefs[fb]++
			}
			return
		}
	}
	panic(fmt.Sprintf("impossible: %s does not call %s", fb.Name, ref.Name))
}

func canInline(fb *funcBuilder, def *FuncDef) bool {
	var ref *funcBuilder
	for r := range fb.outRefs {
		if r.FuncDef == def {
			ref = r
			break
		}
	}
	if ref == nil {
		panic(fmt.Sprintf("impossible: %s does not call %s", fb.Name, def.Name))
	}
	self := fb.FuncDef == def
	parent := fb.parent == ref
	empty := len(def.Blocks) == 0
	noInline := strings.HasSuffix(def.Name, "no_inline")
	var longRet bool
	if len(def.Blocks) > 0 {
		for _, r := range def.Blocks[0].Instrs {
			if _, ok := r.(*Frame); ok {
				longRet = true
				break
			}
		}
	}
	onlyOneRef := len(ref.inRefs) == 1 && ref.inRefs[fb] == 1 && !ref.Exp
	leafFun := len(ref.outRefs) == 0 && ref.inlineNonBlocks == 0
	return !self && !parent && !empty && !noInline && !longRet && (onlyOneRef || leafFun)
}

func rmSelfTailCalls(fb *funcBuilder) {
	for _, b := range fb.Blocks {
		if len(b.Instrs) < 2 {
			continue
		}
		if !returns(b) {
			continue
		}
		call, ok := b.Instrs[len(b.Instrs)-2].(*Call)
		if !ok || staticFunc(call) != fb.FuncDef {
			continue
		}
		updateRefs(fb, fb.FuncDef)
		for _, o := range b.Out() {
			o.rmIn(b)
		}
		b.Instrs[len(b.Instrs)-2].delete()
		b.Instrs[len(b.Instrs)-1].delete()
		for i := range call.Args {
			// Each parameter has an alloc that it is copied into in block0.
			// We need to find the allocs and store the args into them.
			var alloc *Alloc
			for _, r := range fb.Blocks[0].Instrs {
				p, ok := r.(*Parm)
				if !ok || p.Def != fb.Parms[i] {
					continue
				}
				if len(p.UsedBy()) != 1 {
					panic("impossible")
				}
				ld, ok := p.UsedBy()[0].(*Load)
				if !ok {
					panic("impossible")
				}
				if len(ld.UsedBy()) != 1 {
					panic("impossible")
				}
				var dst Value
				switch init := ld.UsedBy()[0].(type) {
				case *Store:
					dst = init.Dst
				case *Copy:
					dst = init.Dst
				default:
					panic("impossible")
				}
				alloc, ok = dst.(*Alloc)
				if !ok {
					panic("impossible")
				}
				break
			}
			if alloc == nil {
				panic("impossible")
			}
			arg := call.Args[i]
			var storeCopy Instruction
			if fb.Parms[i].ByValue {
				if arg == alloc {
					continue
				}
				storeCopy = &Copy{Dst: alloc, Src: arg, L: call.L}
			} else {
				// We can elide the store if it is storing a
				// singly initialized value back into itself.
				if ld, ok := arg.(*Load); ok && ld.Addr == alloc && singleInit(alloc) != nil {
					continue
				}
				storeCopy = &Store{Dst: alloc, Src: arg, L: call.L}
			}
			arg.addUser(storeCopy)
			alloc.addUser(storeCopy)
			b.Instrs = append(b.Instrs, storeCopy)
		}
		b.Instrs = append(b.Instrs, &Jump{Dst: fb.Blocks[1], L: call.L})
		fb.Blocks[1].addIn(b)
	}
	rmUnreach(fb.FuncDef)
	rmDeletes(fb.FuncDef)
}

func returns(b *BasicBlock) bool {
	seen := make(map[*BasicBlock]bool)
	for {
		if seen[b] {
			return false
		}
		seen[b] = true
		switch tail := b.Instrs[len(b.Instrs)-1].(type) {
		case *Return:
			return true
		case *Jump:
			if len(tail.Dst.Instrs) > 1 {
				return false
			}
			b = tail.Dst
		default:
			return false
		}
	}
}

// staticFunc returns the *FuncDef of the call
// if the *FuncDef can be statically determined
// to be a single definitive function.
func staticFunc(call *Call) *FuncDef {
	if f, ok := call.Func.(*Func); ok {
		// This is a call of a named function.
		return f.Def
	}

	load, ok := call.Func.(*Load)
	if !ok {
		return nil
	}
	field, ok := load.Addr.(*Field)
	if !ok || field.Def.Name != "func" {
		return nil
	}
	funcBase := field.Base

	for {
		init := singleInit(funcBase)
		if init == nil {
			break
		}
		funcBase = init
	}

	// The base is only accessed as either the src of a Copy or Fields.
	for _, user := range funcBase.UsedBy() {
		if user.isDeleted() {
			continue
		}
		switch user := user.(type) {
		case *Field:
			continue
		case *Copy:
			if user.Src == funcBase {
				continue
			}
		}
		return nil
	}
	funcType := funcBase.Type().(*AddrType).Elem.(*StructType)
	f, ok := singleFieldInit(funcBase, funcType.Fields[0]).(*Func)
	if !ok {
		return nil
	}
	return f.Def
}

func blockCaps(f *FuncDef, call *Call) map[*FieldDef]Value {
	if len(call.Args) < 1 {
		return nil
	}
	// Arg[0] is a capture base if it is either the Alloc of a capture struct
	// or the load of a capture field of a func struct.
	var capsBase Value
	if alloc, ok := call.Args[0].(*Alloc); ok {
		if !isCapsStruct(alloc) {
			return nil
		}
		capsBase = alloc
	} else {
		capsLoad, ok := call.Args[0].(*Load)
		if !ok {
			return nil
		}
		capsField, ok := capsLoad.Addr.(*Field)
		if !ok || capsField.Def.Name != "caps" {
			return nil
		}
		// This is the load of a field of a func struct.
		// If it is a capture field, the type is *struct{}.
		// To verify that it's a capture field, it must have a single init,
		// and the init must be a caps struct.
		funcBase := capsField.Base
		for {
			init := singleInit(funcBase)
			if init == nil {
				break
			}
			funcBase = init
		}
		funcType := funcBase.Type().(*AddrType).Elem.(*StructType)
		capsInit := singleFieldInit(funcBase, funcType.Fields[1])
		if capsInit == nil || !isCapsStruct(capsInit) {
			return nil
		}
		capsBase = capsInit
	}

	capsType := capsBase.Type().(*AddrType).Elem.(*StructType)
	caps := make(map[*FieldDef]Value)
	for _, fieldDef := range capsType.Fields {
		init := singleFieldInit(capsBase, fieldDef)
		if init == nil {
			// There should be no way that a field of the block
			// is either uninitialized or initialized more than once.
			panic("impossible")
		}
		caps[fieldDef] = init
	}
	return caps
}

func isCapsStruct(v Value) bool {
	a, ok := v.Type().(*AddrType)
	if !ok {
		return false
	}
	s, ok := a.Elem.(*StructType)
	if !ok {
		return false
	}
	for _, f := range s.Fields {
		if f.Name == "<frame>" {
			return true
		}
	}
	return false
}

func singleFieldInit(base Value, def *FieldDef) Value {
	var init Value
	for _, user := range base.UsedBy() {
		f, ok := user.(*Field)
		if !ok || f.Def != def || isReadOnly(f) {
			continue
		}
		if init != nil {
			return nil
		}
		init = singleInit(f)
	}
	return init
}

func isReadOnly(v Value) bool {
	for _, user := range v.UsedBy() {
		if _, ok := user.(*Load); !ok {
			return false
		}
	}
	return true
}

// singleInit returns the single Value stored or copied into v;
// or nil if there is not exactly one Store or Copy into v,
// or if v used by any instructions that may modify it's value.
func singleInit(v Value) Value {
	var init Value
	for _, user := range v.UsedBy() {
		switch user := user.(type) {
		case *Load:
			continue
		case *Field:
			if !isReadOnly(user) {
				return nil
			}
			continue
		case *Store:
			if user.Src == v {
				continue
			}
			if init != nil {
				return nil
			}
			init = user.Src
		case *Copy:
			if user.Src == v {
				continue
			}
			if init != nil {
				return nil
			}
			init = user.Src
		default:
			return nil
		}
	}
	return init
}

func subParms(sub map[*ParmDef]Value, bs []*BasicBlock) {
	for _, b := range bs {
		for i := range b.Instrs {
			p, ok := b.Instrs[i].(*Parm)
			if !ok {
				continue
			}
			v, ok := sub[p.Def]
			if !ok {
				panic("impossible")
			}

			// The parm is always loaded;
			// it's that load that we need to substitute.
			if len(p.UsedBy()) != 1 {
				panic("impossible")
			}
			l, ok := p.UsedBy()[0].(*Load)
			if !ok {
				panic("impossible")
			}
			p.delete()

			s := make(map[Value]Value)
			s[l] = v
			for _, u := range l.UsedBy() {
				u.subValues(s)
			}
		}
	}
}

func subCaps(sub map[*FieldDef]Value, bs []*BasicBlock) {
	for _, b := range bs {
		for i := range b.Instrs {
			f, ok := b.Instrs[i].(*Field)
			if !ok {
				continue
			}
			v, ok := sub[f.Def]
			if !ok {
				continue
			}
			// The field is always loaded;
			// it is that load thet we need to substitute.
			if len(f.UsedBy()) != 1 {
				panic("impossible")
			}
			l, ok := f.UsedBy()[0].(*Load)
			if !ok {
				panic("impossible")
			}
			f.delete()
			s := make(map[Value]Value)
			s[l] = v
			for _, u := range l.UsedBy() {
				u.subValues(s)
			}
		}
	}
}

func subReturn(dst *BasicBlock, bs []*BasicBlock, isBlock bool) {
	for _, b := range bs {
		for i := range b.Instrs {
			r, ok := b.Instrs[i].(*Return)
			if !ok {
				continue
			}
			if r.Frame == nil {
				b.Instrs[i] = &Jump{Dst: dst, L: r.L}
				dst.addIn(b)
			} else if !isBlock {
				r.Frame.rmUser(r)
				r.Frame = nil
			}
		}
	}
}

func copyBlocks(fb *funcBuilder, bs []*BasicBlock) []*BasicBlock {
	copy := make([]*BasicBlock, len(bs))
	subBlocks := make(map[*BasicBlock]*BasicBlock)
	for i := range copy {
		c := *bs[i]
		fb.mod.nextBlock++
		c.Num = fb.mod.nextBlock - 1
		c.in = append([]*BasicBlock{}, bs[i].in...)
		c.Func = fb.FuncDef
		c.Instrs = append([]Instruction{}, c.Instrs...)
		copy[i] = &c
		subBlocks[bs[i]] = &c
	}
	subValues := make(map[Value]Value)
	subInstrs := make(map[Instruction]Instruction)
	for _, b := range copy {
		for i := range b.in {
			b.in[i] = subBlock(b.in[i], subBlocks)
		}
		for i := range b.Instrs {
			c := b.Instrs[i].shallowCopy()
			subInstrs[b.Instrs[i]] = c
			if v, ok := b.Instrs[i].(Value); ok {
				fb.mod.nextInstr++
				c.(Value).setNum(fb.mod.nextInstr - 1)
				subValues[v] = c.(Value)
			}
			b.Instrs[i] = c
		}
	}
	for _, b := range copy {
		for i := range b.Instrs {
			b.Instrs[i].subValues(subValues)
			if v, ok := b.Instrs[i].(Value); ok {
				v.subUsers(subInstrs)
			}
			if t, ok := b.Instrs[i].(Terminal); ok {
				t.subBlocks(subBlocks)
			}
		}
	}
	return copy
}

func (o Store) shallowCopy() Instruction { return &o }
func (o Copy) shallowCopy() Instruction  { return &o }
func (o Call) shallowCopy() Instruction {
	o.Args = append([]Value{}, o.Args...)
	return &o
}
func (o If) shallowCopy() Instruction     { return &o }
func (o Jump) shallowCopy() Instruction   { return &o }
func (o Return) shallowCopy() Instruction { return &o }
func (o Frame) shallowCopy() Instruction  { return &o }
func (o Alloc) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Load) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Func) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o String) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Var) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Parm) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Field) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Case) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Index) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Slice) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Int) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Float) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Null) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	return &o
}
func (o Op) shallowCopy() Instruction {
	shallowCopyUsedBy(&o.value)
	o.Args = append([]Value{}, o.Args...)
	return &o
}

func shallowCopyUsedBy(v *value) {
	v.users = append([]Instruction{}, v.users...)
}

func (o *If) subBlocks(sub map[*BasicBlock]*BasicBlock) {
	o.Yes = subBlock(o.Yes, sub)
	o.No = subBlock(o.No, sub)
}

func (o *Jump) subBlocks(sub map[*BasicBlock]*BasicBlock) {
	o.Dst = subBlock(o.Dst, sub)
}

func (o *Return) subBlocks(sub map[*BasicBlock]*BasicBlock) {}

func subBlock(old *BasicBlock, sub map[*BasicBlock]*BasicBlock) *BasicBlock {
	if new, ok := sub[old]; ok {
		return new
	}
	panic("no substitution")
}

func (o *Store) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Dst)
	subValue(sub, o, &o.Src)
}

func (o *Copy) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Dst)
	subValue(sub, o, &o.Src)
}

func (o *Call) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Func)
	for i := range o.Args {
		subValue(sub, o, &o.Args[i])
	}
}

func (o *If) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Value)
}

func (o *Jump) subValues(sub map[Value]Value) {}

func (o *Return) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Frame)
}

func (o *Frame) subValues(sub map[Value]Value) {}

func (o *Alloc) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Count)
}

func (o *Load) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Addr)
}

func (o *Func) subValues(sub map[Value]Value) {}

func (o *String) subValues(sub map[Value]Value) {}

func (o *Var) subValues(sub map[Value]Value) {}

func (o *Parm) subValues(sub map[Value]Value) {}

func (o *Field) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Base)
}

func (o *Case) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Base)
}

func (o *Index) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Base)
	subValue(sub, o, &o.Index)
}

func (o *Slice) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Base)
	subValue(sub, o, &o.Index)
}

func (o *Int) subValues(sub map[Value]Value) {}

func (o *Float) subValues(sub map[Value]Value) {}

func (o *Null) subValues(sub map[Value]Value) {}

func (o *Op) subValues(sub map[Value]Value) {
	for i := range o.Args {
		subValue(sub, o, &o.Args[i])
	}
}

func subValue(sub map[Value]Value, r Instruction, v *Value) {
	if *v == nil {
		return
	}
	(*v).rmUser(r)
	for {
		new, ok := sub[*v]
		if !ok {
			break
		}
		*v = new
	}
	(*v).addUser(r)
}

func (v *value) subUsers(sub map[Instruction]Instruction) {
	for i := range v.users {
		u := v.users[i]
		if _, ok := sub[u]; ok {
			v.users[i], ok = sub[u]
			if !ok {
				panic("impossible")
			}
		}
	}
}
