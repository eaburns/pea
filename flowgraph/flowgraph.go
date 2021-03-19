package flowgraph

import (
	"fmt"
	"strings"

	"github.com/eaburns/pea/loc"
)

type Mod struct {
	Path    string
	Types   []*StructType
	Strings []*StrDef
	Vars    []*VarDef
	Funcs   []*FuncDef
	Tests   []*FuncDef
	// Init points to a FuncDef in the Funcs slice
	// that has initialization for the module.
	// It must be called before any defs of the module are used
	// (funcs called, vars/consts read/stored, or tests called).
	Init *FuncDef
}

type Type interface {
	String() string
	buildString(*strings.Builder) *strings.Builder
	isEmpty() bool
	isSmall() bool
	eq(Type) bool
}

type IntType struct {
	Size     int
	Unsigned bool
}

func (*IntType) isEmpty() bool { return false }
func (*IntType) isSmall() bool { return true }

func (t *IntType) eq(other Type) bool {
	o, ok := other.(*IntType)
	return ok && *o == *t
}

type FloatType struct {
	Size int
}

func (*FloatType) isEmpty() bool { return false }
func (*FloatType) isSmall() bool { return true }

func (t *FloatType) eq(other Type) bool {
	o, ok := other.(*FloatType)
	return ok && *o == *t
}

type AddrType struct {
	Elem Type // nil is opaque
}

func (*AddrType) isEmpty() bool { return false }
func (*AddrType) isSmall() bool { return true }

func (t *AddrType) eq(other Type) bool {
	o, ok := other.(*AddrType)
	return ok && (t.Elem.isEmpty() || o.Elem.isEmpty() || t.Elem.eq(o.Elem))
}

type ArrayType struct {
	Elem Type
}

func (*ArrayType) isEmpty() bool { return false }
func (*ArrayType) isSmall() bool { return true }

type FrameType struct{}

func (*FrameType) isEmpty() bool { return false }
func (*FrameType) isSmall() bool { return true }
func (*FrameType) eq(other Type) bool {
	_, ok := other.(*FrameType)
	return ok
}

func (t *ArrayType) eq(other Type) bool {
	o, ok := other.(*ArrayType)
	return ok && t.Elem.eq(o.Elem)
}

type StructType struct {
	// Mod, Args, and Name are set for defined types.
	Mod    string
	Args   []Type
	Name   string
	Fields []*FieldDef
}

type FieldDef struct {
	Num  int
	Name string
	Type Type
}

func (s *StructType) isEmpty() bool { return len(s.Fields) == 0 }
func (*StructType) isSmall() bool   { return false }

func (t *StructType) eq(other Type) bool {
	o, ok := other.(*StructType)
	if !ok || len(t.Fields) != len(o.Fields) {
		return false
	}
	for i := range t.Fields {
		if t.Fields[i].Name != o.Fields[i].Name ||
			!t.Fields[i].Type.eq(o.Fields[i].Type) {
			return false
		}
	}
	return true
}

type UnionType struct {
	Cases []*CaseDef
}

type CaseDef struct {
	Name string
	Type Type
}

func (*UnionType) isEmpty() bool { return false }
func (*UnionType) isSmall() bool { return false }

func (t *UnionType) eq(other Type) bool {
	o, ok := other.(*UnionType)
	if !ok || len(t.Cases) != len(o.Cases) {
		return false
	}
	for i := range t.Cases {
		if t.Cases[i].Name != o.Cases[i].Name ||
			(t.Cases[i].Type == nil) != (o.Cases[i].Type == nil) ||
			t.Cases[i].Type != nil && !t.Cases[i].Type.eq(o.Cases[i].Type) {
			return false
		}
	}
	return true
}

type FuncType struct {
	Parms []Type
	Ret   Type
}

func (*FuncType) isEmpty() bool { return false }
func (*FuncType) isSmall() bool { return true }

func (t *FuncType) eq(other Type) bool {
	o, ok := other.(*FuncType)
	if !ok || len(t.Parms) != len(o.Parms) {
		return false
	}
	for i := range t.Parms {
		if !t.Parms[i].eq(o.Parms[i]) {
			return false
		}
	}
	return t.Ret.eq(o.Ret)
}

type StrDef struct {
	Mod  *Mod
	Num  int
	Text string
}

type VarDef struct {
	Mod  *Mod
	Name string
	Type Type
}

type FuncDef struct {
	Comment string
	Mod     string
	Name    string
	Parms   []*ParmDef
	Type    *FuncType
	Blocks  []*BasicBlock
	L       loc.Loc

	// Iface is the set of iface function arguments.
	// Iface, along with Mod, Name, and Parms
	// form the unique signatuer of the function.
	Iface []IfaceArg
}

type IfaceArg struct {
	// Mod and Name are the module path (or empty for built-in)
	// and name of the interface argument function.
	//
	// For a given, grounded function,
	// the parameter types of an iface arg are fixed.
	// So the only information needed
	// to uniquily determine the function
	// is the Mod and Name.
	Mod  string
	Name string
}

type ParmDef struct {
	Name      string
	Type      Type
	ByValue   bool
	RetValue  bool
	BlockData bool
	L         loc.Loc
}

type BasicBlock struct {
	Num    int
	Func   *FuncDef
	Instrs []Instruction
	in     []*BasicBlock
}

func (b *BasicBlock) addIn(in *BasicBlock) {
	for _, x := range b.in {
		if x == in {
			return
		}
	}
	b.in = append(b.in, in)
}

func (b *BasicBlock) rmIn(in *BasicBlock) {
	var i int
	for _, x := range b.in {
		if x != in {
			b.in[i] = x
			i++
		}
	}
	b.in = b.in[:i]
}

func (b *BasicBlock) In() []*BasicBlock {
	return append([]*BasicBlock{}, b.in...)
}

func (b *BasicBlock) Out() []*BasicBlock {
	if len(b.Instrs) == 0 {
		return nil
	}
	if t, ok := b.Instrs[len(b.Instrs)-1].(Terminal); ok {
		return t.Out()
	}
	return nil
}

type Instruction interface {
	String() string
	Uses() []Value
	Loc() loc.Loc
	Comment() string
	setComment(string, ...interface{})
	delete()
	isDeleted() bool
	buildString(*strings.Builder) *strings.Builder
	shallowCopy() Instruction
	subValues(map[Value]Value)
}

type instruction struct {
	comment string
	deleted bool
}

func (r *instruction) Comment() string                        { return r.comment }
func (r *instruction) setComment(f string, vs ...interface{}) { r.comment = fmt.Sprintf(f, vs...) }
func (r *instruction) delete()                                { r.deleted = true }
func (r *instruction) isDeleted() bool                        { return r.deleted }

type Store struct {
	instruction
	Dst Value
	Src Value
	L   loc.Loc
}

func (s *Store) Uses() []Value { return []Value{s.Dst, s.Src} }
func (s *Store) Loc() loc.Loc  { return s.L }

type Copy struct {
	instruction
	Dst Value
	Src Value
	L   loc.Loc
}

func (c *Copy) Uses() []Value { return []Value{c.Dst, c.Src} }
func (c *Copy) Loc() loc.Loc  { return c.L }

type Call struct {
	instruction
	// Func is the called function.
	// If Func.(*Func), then this is a direct call,
	// otherwise it is an indirect call.
	Func Value
	Args []Value
	L    loc.Loc
}

func (c *Call) Uses() []Value { return append(c.Args, c.Func) }
func (c *Call) Loc() loc.Loc  { return c.L }

type Terminal interface {
	Out() []*BasicBlock
	subBlocks(map[*BasicBlock]*BasicBlock)
}

type If struct {
	instruction
	// Value must be an integer type or an address type.
	// If Value is an address type Op must be Eq and X must be 0.
	Value Value
	// Op must be Eq or Less
	Op  OpKind
	X   int
	Yes *BasicBlock
	No  *BasicBlock
	L   loc.Loc
}

func (r *If) Uses() []Value      { return []Value{r.Value} }
func (r *If) Loc() loc.Loc       { return r.L }
func (r *If) Out() []*BasicBlock { return []*BasicBlock{r.Yes, r.No} }

type Jump struct {
	instruction
	Dst *BasicBlock
	L   loc.Loc
}

func (*Jump) Uses() []Value        { return nil }
func (j *Jump) Loc() loc.Loc       { return j.L }
func (r *Jump) Out() []*BasicBlock { return []*BasicBlock{r.Dst} }

type Return struct {
	instruction
	// Frame is the long return frame value.
	// If Frame is nil, it is not a long return.
	Frame Value
	L     loc.Loc
}

func (r *Return) Uses() []Value {
	if r.Frame == nil {
		return nil
	}
	return []Value{r.Frame}
}

func (r *Return) Loc() loc.Loc       { return r.L }
func (r *Return) Out() []*BasicBlock { return nil }

type Value interface {
	Instruction
	Num() int
	setNum(int)
	Type() Type
	UsedBy() []Instruction
	addUser(Instruction)
	rmUser(Instruction)
	subUsers(map[Instruction]Instruction)
}

type value struct {
	instruction
	num   int
	users []Instruction
}

func (v *value) Num() int     { return v.num }
func (v *value) setNum(x int) { v.num = x }

func (v *value) UsedBy() []Instruction {
	// Return a copy so users can be modified while iterating over UsedBy().
	return append([]Instruction{}, v.users...)
}

func (v *value) addUser(user Instruction) {
	for _, use := range v.users {
		if use == user {
			return
		}
	}
	v.users = append(v.users, user)
}

func (v *value) rmUser(user Instruction) {
	var n int
	for _, use := range v.users {
		if use != user {
			v.users[n] = use
			n++
		}
	}
	v.users = v.users[:n]
}

type Frame struct {
	value
	L loc.Loc
}

func (*Frame) Uses() []Value  { return nil }
func (f *Frame) Loc() loc.Loc { return f.L }
func (*Frame) Type() Type     { return &FrameType{} }

type Alloc struct {
	value
	// Stack is whether the allocation is on the stack,
	// otherwise the heap.
	Stack bool
	// Count and CountImm are mutualaly exclusive.
	// If Count==nil and CountImm<0, then the alloc is for 1.
	Count    Value
	CountImm int
	T        Type // the value type allocated
	L        loc.Loc
}

func (a *Alloc) Uses() []Value {
	if a.Count == nil {
		return nil
	}
	return []Value{a.Count}
}

func (a *Alloc) Loc() loc.Loc { return a.L }
func (a *Alloc) Type() Type   { return a.T }

type Load struct {
	value
	Addr     Value
	AddrType AddrType
	L        loc.Loc
}

func (l *Load) Uses() []Value { return []Value{l.Addr} }
func (l *Load) Loc() loc.Loc  { return l.L }
func (l *Load) Type() Type    { return l.AddrType.Elem }

type Func struct {
	value
	Def *FuncDef
	L   loc.Loc
}

func (*Func) Uses() []Value  { return nil }
func (f *Func) Loc() loc.Loc { return f.L }
func (f *Func) Type() Type   { return f.Def.Type }

type String struct {
	value
	Def *StrDef
	L   loc.Loc
}

func (*String) Uses() []Value  { return nil }
func (v *String) Loc() loc.Loc { return v.L }

func (v *String) Type() Type {
	return &ArrayType{
		Elem: &IntType{Size: 8, Unsigned: true},
	}
}

type Var struct {
	value
	Def *VarDef
	L   loc.Loc
}

func (*Var) Uses() []Value  { return nil }
func (v *Var) Loc() loc.Loc { return v.L }
func (v *Var) Type() Type   { return &AddrType{Elem: v.Def.Type} }

type Parm struct {
	value
	Def *ParmDef
	L   loc.Loc
}

func (*Parm) Uses() []Value  { return nil }
func (p *Parm) Loc() loc.Loc { return p.L }
func (p *Parm) Type() Type   { return &AddrType{Elem: p.Def.Type} }

type Field struct {
	value
	Base     Value
	Def      *FieldDef
	BaseType StructType
	L        loc.Loc
}

func (f *Field) Uses() []Value { return []Value{f.Base} }
func (f *Field) Loc() loc.Loc  { return f.L }
func (f *Field) Type() Type    { return &AddrType{Elem: f.Def.Type} }

type Case struct {
	value
	Base     Value
	Def      *CaseDef
	BaseType UnionType
	L        loc.Loc
}

func (c *Case) Uses() []Value { return []Value{c.Base} }
func (c *Case) Loc() loc.Loc  { return c.L }
func (c *Case) Type() Type    { return &AddrType{Elem: c.Def.Type} }

type Index struct {
	value
	Base     Value
	Index    Value
	BaseType ArrayType
	L        loc.Loc
}

func (x *Index) Uses() []Value { return []Value{x.Base, x.Index} }
func (x *Index) Loc() loc.Loc  { return x.L }
func (x *Index) Type() Type    { return &AddrType{Elem: x.BaseType.Elem} }

type Slice struct {
	value
	Base     Value
	Index    Value
	BaseType ArrayType
	L        loc.Loc
}

func (x *Slice) Uses() []Value { return []Value{x.Base, x.Index} }
func (x *Slice) Loc() loc.Loc  { return x.L }
func (x *Slice) Type() Type    { return &x.BaseType }

type Int struct {
	value
	Text string
	T    IntType
	L    loc.Loc
}

func (*Int) Uses() []Value  { return nil }
func (i *Int) Loc() loc.Loc { return i.L }
func (i *Int) Type() Type   { return &i.T }

type Float struct {
	value
	Text string
	T    FloatType
	L    loc.Loc
}

func (*Float) Uses() []Value  { return nil }
func (f *Float) Loc() loc.Loc { return f.L }
func (f *Float) Type() Type   { return &f.T }

type Null struct {
	value
	T AddrType
	L loc.Loc
}

func (*Null) Uses() []Value  { return nil }
func (n *Null) Loc() loc.Loc { return n.L }
func (n *Null) Type() Type   { return &n.T }

type OpKind int

const (
	BitNot OpKind = iota + 1
	BitXor
	BitAnd
	BitOr
	LeftShift
	RightShift
	Negate
	Minus
	Plus
	Times
	Divide
	Modulus
	Eq
	Neq
	Less
	LessEq
	Greater
	GreaterEq
	NumConvert
	Panic
	Print
)

type Op struct {
	value
	Op   OpKind
	Args []Value
	T    Type
	L    loc.Loc
}

func (o *Op) Uses() []Value {
	// Print and Panic side-effect.
	// This prevents them from being marked as disused.
	if o.Op == Print || o.Op == Panic {
		return append(o.Args, o)
	}
	return o.Args
}

func (o *Op) Loc() loc.Loc { return o.L }
func (o *Op) Type() Type   { return o.T }
