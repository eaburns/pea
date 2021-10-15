package flowgraph

import (
	"fmt"
	"strings"
)

func (mb *modBuilder) optimize() {
	for _, fb := range mb.funcBuilders {
		moveAllocsToStack(fb)
		inlineCalls(fb)
		rmSelfTailCalls(fb)
		rmAllocs(fb)
		mergeBlocks(fb.FuncDef)
		finalMoveAllocsToStack(fb)
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

type tracer interface {
	tr(string, ...interface{})
}

func (mb *modBuilder) tr(f string, vs ...interface{}) {
	if mb.trace {
		fmt.Printf(f+"\n", vs...)
	}
}

func (fb *funcBuilder) tr(f string, vs ...interface{}) {
	fb.mod.tr(f, vs...)
}

func disused(fb *funcBuilder) bool {
	if len(fb.inRefs) > 0 {
		return false
	}
	if strings.Contains(fb.Name, "<block") {
		// Unreferenced blocks must have been fully inlined.
		// It is no longer possible to call them.
		return true
	}
	if fb.mod.Path != fb.Mod {
		// Unreferenced functions from other modules can be removed.
		// They were brough in for inlining. They are likely fully inlined.
		// Any other modules that want to use them
		// will use the version from the defining module.
		return true
	}
	if fb.Inst {
		// Instances of type-parameterized functions
		// can be discarded if disused.
		// Any other module that wants this function
		// will re-generate its own copy of it.
		return true
	}
	return false
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

func rmUnreach(fb *funcBuilder) {
	if len(fb.Blocks) == 0 {
		return
	}
	seen := make(map[*BasicBlock]bool)
	seen[fb.Blocks[0]] = true
	todo := []*BasicBlock{fb.Blocks[0]}
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
	for _, b := range fb.Blocks {
		if seen[b] {
			fb.Blocks[i] = b
			i++
			continue
		}
		for _, o := range b.Out() {
			o.rmIn(b)
		}
		for _, r := range b.Instrs {
			if f, ok := r.(*Func); ok {
				decFuncRef(fb, f.Def)
			}
			for _, u := range r.Uses() {
				u.rmUser(r)
			}
		}
	}
	fb.Blocks = fb.Blocks[:i]
}

func rmDeletes(fb *funcBuilder) {
	var todo []Instruction
	seen := make(map[Instruction]bool)
	for _, b := range fb.Blocks {
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
	for _, b := range fb.Blocks {
		var i int
		for _, r := range b.Instrs {
			if r.isDeleted() {
				if f, ok := r.(*Func); ok {
					decFuncRef(fb, f.Def)
				}
				continue
			}
			b.Instrs[i] = r
			i++
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

func decFuncRef(fb *funcBuilder, ref *FuncDef) {
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
			return
		}
	}
	panic(fmt.Sprintf("impossible: %s does not call %s", fb.Name, ref.Name))
}

func rmAllocs(fb *funcBuilder) {
	subValues := make(map[Value]Value)
	for _, b := range fb.Blocks {
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
	for _, b := range fb.Blocks {
		for _, r := range b.Instrs {
			r.subValues(subValues)
		}
	}
	rmDeletes(fb)
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

// finalMoveAllocsToStack is a very conservative escape analysis pass
// that can run after inlining and tail-call optimization.
// This pass cleans up allocs that, after inlining,
// can be trivially proven not to escape.
func finalMoveAllocsToStack(fb *funcBuilder) {
	fb.mod.trace = fb.mod.traceEsc
	defer func() { fb.mod.trace = false }()

	fb.tr("---- final move allocs to stack:\n%s\n", fb.FuncDef)
	change := true
	for change {
		change = false
		for _, b := range fb.Blocks {
			for _, r := range b.Instrs {
				if a, ok := r.(*Alloc); ok && !a.Stack && !escapesConservative(fb, a) {
					a.Stack = true
					change = true
				}
			}
		}
	}
}

func escapesConservative(tr tracer, a *Alloc) bool {
	if a.Count != nil {
		tr.tr("x%d escapes: array alloc", a.Num())
		return true
	}
	for _, u := range a.UsedBy() {
		switch u := u.(type) {
		case *Load:
			continue
		case *Store:
			if u.Src != a {
				continue
			}
			if d, ok := u.Dst.(*Alloc); ok && d.Stack {
				continue
			}
			tr.tr("x%d escapes: stored into x%d", a.Num(), u.Dst.Num())
			return true
		case *Copy:
			continue
		case *Field:
			if isReadOnly(u) {
				continue
			}
			tr.tr("x%d escapes: non-read-only field x%d", a.Num(), u.Num())
			return true
		case *Case:
			if isReadOnly(u) {
				continue
			}
			tr.tr("x%d escapes: non-read-only case x%d", a.Num(), u.Num())
			return true
		case *Index:
			if isReadOnly(u) {
				continue
			}
			tr.tr("x%d escapes: non-read-only index x%d", a.Num(), u.Num())
			return true
		case *Call:
			for i, arg := range u.Args {
				if arg != a || byValue(u, i) {
					continue
				}
				tr.tr("x%d escapes: call %s non-by value argument %d", a.Num(), u, i)
				return true
			}
		default:
			tr.tr("x%d escapes: used by %s", a.Num(), u)
			return true
		}
	}
	tr.tr("x%d does not escape", a.Num())
	return false
}

func byValue(c *Call, i int) bool {
	f, ok := c.Func.(*Func)
	return ok && f.Def.Parms[i].ByValue
}

// moveAllocsToStack computes leaks and moves allocations to the stack
// that can be proven not to leak.
func moveAllocsToStack(fb *funcBuilder) {
	fb.mod.trace = fb.mod.traceEsc
	defer func() { fb.mod.trace = false }()

	fb.tr("---- moving allocs to stack:\n%s\n", fb.FuncDef)
	leaks := findLeaks(fb)
	fb.tr("-")
	for _, b := range fb.Blocks {
		for _, r := range b.Instrs {
			if a, ok := r.(*Alloc); ok && !a.Stack {
				a.Stack = !escapes(fb.mod, leaks, a)
			}
		}
	}
}

func escapes(tr tracer, leaks map[Value]bool, v Value) bool {
	if a, ok := v.(*Alloc); ok && a.Count != nil {
		tr.tr("x%d escapes: array alloc", v.Num())
		return true
	}
	for _, u := range v.UsedBy() {
		switch u := u.(type) {
		case *Store:
			if u.Src == v && leaks[u.Dst] {
				tr.tr("x%d escapes: stored to leak x%d", v.Num(), u.Dst.Num())
				return true
			}
		case *BitCast:
			if escapes(tr, leaks, u) {
				tr.tr("x%d escapes: bitcast escapes x%d", v.Num(), u.Num())
				return true
			}
		case *Field:
			if escapes(tr, leaks, u) {
				tr.tr("x%d escapes: field escapes x%d", v.Num(), u.Num())
				return true
			}
		case *Case:
			if escapes(tr, leaks, u) {
				tr.tr("x%d escapes: case escapes x%d", v.Num(), u.Num())
				return true
			}
		case *Index:
			if escapes(tr, leaks, u) {
				tr.tr("x%d escapes: index escapes x%d", v.Num(), u.Num())
				return true
			}
		case *Slice:
			if escapes(tr, leaks, u) {
				tr.tr("x%d escapes: slice escapes x%d", v.Num(), u.Num())
				return true
			}
		case *Call:
			s, ok := u.Func.(*Func)
			for i, arg := range u.Args {
				if arg != v {
					continue
				}
				if i == 0 &&
					(!ok || len(s.Def.Parms) > 0 && s.Def.Parms[0].BlockData) {
					// Skip never-escaping block captures.
					continue
				}
				_, isAddr := arg.Type().(*AddrType)
				_, isArray := arg.Type().(*ArrayType)
				if isAddr || isArray {
					tr.tr("x%d escapes: arg %d of call %s", v.Num(), i, u)
					return true
				}
			}
		}
	}
	tr.tr("x%d does not escape", v.Num())
	return false
}

func findLeaks(fb *funcBuilder) map[Value]bool {
	var todo []Value
	leaks := make(map[Value]bool)
	for _, b := range fb.Blocks {
		for _, r := range b.Instrs {
			switch r := r.(type) {
			case *Alloc:
				if r.Count != nil {
					fb.tr("x%d leaks: array alloc", r.Num())
					leak(&todo, leaks, r)
				}
			case *Var:
				fb.tr("x%d leaks: var", r.Num())
				leak(&todo, leaks, r)
			case *Parm:
				fb.tr("x%d leaks: parm", r.Num())
				leak(&todo, leaks, r)
			case *Call:
				for i, arg := range r.Args {
					_, isAddr := arg.Type().(*AddrType)
					_, isArray := arg.Type().(*ArrayType)
					if isAddr || isArray {
						fb.tr("x%d leaks: arg %d to call %s", arg.Num(), i, r)
						leak(&todo, leaks, arg)
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
				fb.tr("x%d leaks: loaded address leaks x%d", v.Addr.Num(), v.Num())
				leak(&todo, leaks, v.Addr)
			}
		case *BitCast:
			fb.tr("x%d leaks: bitcast to leak x%d", v.Src.Num(), v.Num())
			leak(&todo, leaks, v.Src)
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
			fb.tr("x%d leaks: base of leaked field x%d", v.Base.Num(), v.Num())
			leak(&todo, leaks, v.Base)
		case *Case:
			fb.tr("x%d leaks: base of leaked case x%d", v.Base.Num(), v.Num())
			leak(&todo, leaks, v.Base)
		case *Index:
			fb.tr("x%d leaks: base of leaked index x%d", v.Base.Num(), v.Num())
			leak(&todo, leaks, v.Base)
		case *Slice:
			fb.tr("x%d leaks: base of leaked slice x%d", v.Base.Num(), v.Num())
			leak(&todo, leaks, v.Base)
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
					fb.tr("x%d leaks: load address from leaked x%d", u.Num(), v.Num())
					leak(&todo, leaks, u)
				}
			case *Store:
				if u.Dst == v {
					// v leaks; anything stored into v is stored into a leak,
					// so it itself is also a leak.
					fb.tr("x%d leaks: stored into leaked x%d", u.Src.Num(), u.Dst.Num())
					leak(&todo, leaks, u.Src)
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
					fb.tr("x%d leaks: load of stored leak x%d", l.Num(), u.Dst.Num())
					leak(&todo, leaks, l)
				}
			case *Copy:
				if u.Dst == v {
					fb.tr("x%d leaks: copied into leaked x%d", u.Src.Num(), v.Num())
					leak(&todo, leaks, u.Src)
				}
			case *Field:
				fb.tr("x%d leaks: field of leaked base x%d", u.Num(), v.Num())
				leak(&todo, leaks, u)
			case *Case:
				fb.tr("x%d leaks: case of leaked base x%d", u.Num(), v.Num())
				leak(&todo, leaks, u)
			case *Index:
				fb.tr("x%d leaks: index of leaked base x%d", u.Num(), v.Num())
				leak(&todo, leaks, u)
			case *Slice:
				fb.tr("x%d leaks: slice of leaked base x%d", u.Num(), v.Num())
				leak(&todo, leaks, u)
			}
		}
	}
	return leaks
}

func leak(todo *[]Value, leaks map[Value]bool, v Value) {
	if leaks[v] {
		return
	}
	leaks[v] = true
	*todo = append(*todo, v)
}

func inlineCalls(fb *funcBuilder) {
	fb.mod.trace = fb.mod.traceInline
	defer func() { fb.mod.trace = false }()
	fb.tr("---- inlining:\n%s\n", fb.FuncDef)

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
			inlineFuncRefs(fb, def)
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
	rmUnreach(fb)
	rmDeletes(fb)
}

func inlineFuncRefs(fb *funcBuilder, inl *FuncDef) {
	for r := range fb.outRefs {
		if r.FuncDef == inl {
			for o, n := range r.outRefs {
				fb.outRefs[o] += n
				o.inRefs[fb] += n
			}
			fb.inlineNonBlocks += r.inlineNonBlocks
			return
		}
	}
	panic(fmt.Sprintf("impossible: %s does not call %s", fb.Name, inl.Name))
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
	testCall := fb.Test && !strings.Contains("<block", def.Name)
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
	ok := !self && !parent && !empty && !testCall && !noInline && !longRet &&
		(onlyOneRef || leafFun)

	if ok {
		fb.tr("	can inline %s: TRUE", def.Name)
	} else {
		fb.tr("	can inline %s: FALSE", def.Name)
	}
	fb.tr("		self: %v", self)
	fb.tr("		parent: %v", parent)
	fb.tr("		empty: %v", empty)
	fb.tr("		testCall: %v", testCall)
	fb.tr("		noInline: %v", noInline)
	fb.tr("		longRet: %v", longRet)
	fb.tr("		onlyOneRef: %v", onlyOneRef)
	fb.tr("		leafFun: %v", leafFun)
	fb.tr("		exported: %v", ref.Exp)
	fb.tr("		inlineNonBlocks: %d", ref.inlineNonBlocks)
	fb.tr("		inRefs:")
	for r, n := range ref.inRefs {
		fb.tr("			%s: %d", r.Name, n)
	}
	fb.tr("		outRefs:")
	for r, n := range ref.outRefs {
		fb.tr("			%s: %d", r.Name, n)
	}
	return ok
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
				var dst Value
				switch init := p.UsedBy()[0].(type) {
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
	rmUnreach(fb)
	rmDeletes(fb)
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
	return strings.HasPrefix(s.Name, "<caps")
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
		if bc, ok := user.(*BitCast); ok && isReadOnly(bc) {
			continue
		}
		if _, ok := user.(*Load); !ok {
			return false
		}
	}
	return true
}

// singleInit returns the single Value stored or copied into v;
// or nil if there is not exactly one Store or Copy into v,
// or if v used by any instructions that may modify it's value.
// The returned value is never a BitCast; if the single init is a BitCast,
// it source is followed to arrive at the original, non-BitCast Value.
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
	// Follow any number bitcasts to get to the actual initial value.
	for {
		b, ok := init.(*BitCast)
		if !ok {
			break
		}
		init = b.Src
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
			s := make(map[Value]Value)
			s[p] = v
			for _, u := range p.UsedBy() {
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
func (o BitCast) shallowCopy() Instruction {
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
	subValue(sub, o, &o.XValue)
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

func (o *BitCast) subValues(sub map[Value]Value) {
	subValue(sub, o, &o.Src)
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
