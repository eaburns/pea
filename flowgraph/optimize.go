package flowgraph

func Optimize(m *Mod) {
	for _, f := range m.Funcs {
		for _, opt := range Opts {
			opt.Func(f)
		}
	}
}

type Opt struct {
	Name string
	Func func(*FuncDef)
}

var Opts = []Opt{
	{"build", func(*FuncDef) {}},
	{"inline calls", inlineCalls},
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
			if v, ok := r.(Value); ok && len(v.UsedBy()) == 0 {
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
			if !seen[u] && len(u.UsedBy()) == 0 {
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

func inlineCalls(f *FuncDef) {
	todo := f.Blocks
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
			if def == nil {
				continue
			}
			caps := blockCaps(c)
			if caps == nil {
				// For now, we just inline static block calls.
				continue
			}
			parms := make(map[*ParmDef]Value)
			for i := range def.Parms {
				parms[def.Parms[i]] = c.Args[i]
			}

			c.delete()
			if caps != nil {
				// If there is a capture block, delete it.
				c.Args[0].delete()
			}
			if l, ok := c.Func.(*Load); ok {
				// If the called function is a function expression;
				// delete the function struct and caps initialization.
				funcBase := l.Addr.(*Field).Base
				funcBase.delete()
				funcType := funcBase.Type().(*AddrType).Elem.(*StructType)
				capsInit := singleFieldInit(funcBase, funcType.Fields[1])
				capsInit.delete()
			}

			tail := &BasicBlock{Func: b.Func, Instrs: b.Instrs[j+1:]}
			for _, o := range b.Out() {
				o.rmIn(b)
				o.addIn(tail)
			}

			inlined := copyBlocks(f, def.Blocks)
			moveAllocs(f.Blocks[0], inlined[0])
			subParms(parms, inlined)
			subCaps(caps, inlined)
			subReturn(tail, inlined) // sets tail.In if needed

			b.Instrs = append(b.Instrs[:j+1:j+1], &Jump{Dst: inlined[0], L: c.L})
			inlined[0].addIn(b)

			todo = append(append(inlined, tail), todo...)
			break
		}
		done = append(done, b)
	}
	f.Blocks = done
	rmDeletes(f)
	renumber(f)
}

func moveAllocs(dst, src *BasicBlock) {
	instrs := make([]Instruction, 0, len(dst.Instrs)+len(src.Instrs))
	for _, r := range dst.Instrs {
		if _, ok := r.(*Alloc); ok {
			instrs = append(instrs, r)
		}
	}
	for i, r := range src.Instrs {
		if _, ok := r.(*Alloc); ok {
			instrs = append(instrs, r)
		} else {
			src.Instrs = src.Instrs[i:]
			break
		}
	}
	for _, r := range dst.Instrs {
		if _, ok := r.(*Alloc); !ok {
			instrs = append(instrs, r)
		}
	}
	dst.Instrs = instrs
}

// staticFunc returns the *FuncDef of the call
// if the *FuncDef can be statically determined
// to be a single definitive function.
func staticFunc(call *Call) *FuncDef {
	if f, ok := call.Func.(*Func); ok {
		// This is a call of a named function.
		return f.Def
	}

	// Here we are checking if the call is of the func field
	// of a function struct that is initialized only a single time.
	// * Call.Func is a *Load of a *Field,
	// * The *Field.Base is a struct that is only used as the Base of *Fields,
	// * The "func" field of *Field.Base is only initialized once, and
	// * the single init of the field is a *Func;
	// that *Func is our static function.

	load, ok := call.Func.(*Load)
	if !ok {
		return nil
	}
	field, ok := load.Addr.(*Field)
	if !ok || field.Def.Name != "func" {
		return nil
	}
	funcBase := field.Base
	// This is only a block literal call if the function struct base
	// is only used by having its fields accessed.
	// (For example, it must not be the Dst of a Copy.)
	for _, user := range funcBase.UsedBy() {
		if _, ok := user.(*Field); !ok {
			return nil
		}
	}
	funcType := funcBase.Type().(*AddrType).Elem.(*StructType)
	f, ok := singleFieldInit(funcBase, funcType.Fields[0]).(*Func)
	if !ok {
		return nil
	}
	return f.Def
}

func blockCaps(call *Call) map[*FieldDef]Value {
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
		capsInit := singleFieldInit(funcBase, capsField.Def)
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

// singleFieldInit returns the single initialization for a field of a given base value,
// or nil if the field is never initialized or initialized more than once.
func singleFieldInit(base Value, def *FieldDef) Value {
	var init Value
	for _, r := range base.UsedBy() {
		f, ok := r.(*Field)
		if !ok || f.Base != base || f.Def != def || isReadOnly(f) {
			continue
		}
		i := singleInit(f)
		if i == nil || init != nil {
			return nil
		}
		init = i
	}
	return init
}

// isReadOnly returns whether the value is only read.
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
// or if v is used by anything other than
// the destination of a Copy or Store or as a Load.
func singleInit(v Value) Value {
	var init Value
	for _, user := range v.UsedBy() {
		switch user := user.(type) {
		case *Load:
			continue
		case *Store:
			if init != nil || user.Src == v {
				return nil
			}
			init = user.Src
		case *Copy:
			if init != nil || user.Src == v {
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
				l.rmUser(u)
				v.addUser(u)
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
				l.rmUser(u)
				v.addUser(u)
			}
		}
	}
}

func subReturn(dst *BasicBlock, bs []*BasicBlock) {
	for _, b := range bs {
		for i := range b.Instrs {
			r, ok := b.Instrs[i].(*Return)
			if !ok {
				continue
			}
			if r.Frame == nil {
				b.Instrs[i] = &Jump{Dst: dst, L: r.L}
				dst.addIn(b)
			} else {
				r.Frame.rmUser(r)
				r.Frame = nil
			}
		}
	}
}

func copyBlocks(destFunc *FuncDef, bs []*BasicBlock) []*BasicBlock {
	copy := make([]*BasicBlock, len(bs))
	subBlocks := make(map[*BasicBlock]*BasicBlock)
	for i := range copy {
		c := *bs[i]
		c.In = append([]*BasicBlock{}, bs[i].In...)
		c.Func = destFunc
		c.Instrs = append([]Instruction{}, c.Instrs...)
		copy[i] = &c
		subBlocks[bs[i]] = &c
	}
	subValues := make(map[Value]Value)
	subInstrs := make(map[Instruction]Instruction)
	for _, b := range copy {
		for i := range b.In {
			b.In[i] = subBlock(b.In[i], subBlocks)
		}
		for i := range b.Instrs {
			c := b.Instrs[i].shallowCopy()
			subInstrs[b.Instrs[i]] = c
			if v, ok := b.Instrs[i].(Value); ok {
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
	o.Dst = subValue(o.Dst, sub)
	o.Src = subValue(o.Src, sub)
}

func (o *Copy) subValues(sub map[Value]Value) {
	o.Dst = subValue(o.Dst, sub)
	o.Src = subValue(o.Src, sub)
}

func (o *Call) subValues(sub map[Value]Value) {
	o.Func = subValue(o.Func, sub)
	for i := range o.Args {
		o.Args[i] = subValue(o.Args[i], sub)
	}
}

func (o *If) subValues(sub map[Value]Value) {
	o.Value = subValue(o.Value, sub)
}

func (o *Jump) subValues(sub map[Value]Value) {}

func (o *Return) subValues(sub map[Value]Value) {
	o.Frame = subValue(o.Frame, sub)
}

func (o *Frame) subValues(sub map[Value]Value) {}

func (o *Alloc) subValues(sub map[Value]Value) {
	o.Count = subValue(o.Count, sub)
}

func (o *Load) subValues(sub map[Value]Value) {
	o.Addr = subValue(o.Addr, sub)
}

func (o *Func) subValues(sub map[Value]Value) {}

func (o *String) subValues(sub map[Value]Value) {}

func (o *Var) subValues(sub map[Value]Value) {}

func (o *Parm) subValues(sub map[Value]Value) {}

func (o *Field) subValues(sub map[Value]Value) {
	o.Base = subValue(o.Base, sub)
}

func (o *Case) subValues(sub map[Value]Value) {
	o.Base = subValue(o.Base, sub)
}

func (o *Index) subValues(sub map[Value]Value) {
	o.Base = subValue(o.Base, sub)
	o.Index = subValue(o.Index, sub)
}

func (o *Slice) subValues(sub map[Value]Value) {
	o.Base = subValue(o.Base, sub)
	o.Index = subValue(o.Index, sub)
}

func (o *Int) subValues(sub map[Value]Value) {}

func (o *Float) subValues(sub map[Value]Value) {}

func (o *Null) subValues(sub map[Value]Value) {}

func (o *Op) subValues(sub map[Value]Value) {
	for i := range o.Args {
		o.Args[i] = subValue(o.Args[i], sub)
	}
}

func subValue(old Value, sub map[Value]Value) Value {
	if old == nil {
		return nil
	}
	if new, ok := sub[old]; ok {
		return new
	}
	return old
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
