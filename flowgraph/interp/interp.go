package interp

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
	"strings"

	"github.com/eaburns/pea/flowgraph"
)

type Interp struct {
	Out         io.Writer
	Trace       bool
	vars        map[*flowgraph.VarDef]*Obj
	stack       []frame
	nextFrameID int
	n           int
}

type frame struct {
	id int

	args        map[*flowgraph.ParmDef]*Obj
	vals        map[flowgraph.Value]*Obj
	stackAllocs []*Obj
	*flowgraph.FuncDef
	*flowgraph.BasicBlock
	n int
}

func New() *Interp {
	return &Interp{
		Out:  os.Stdout,
		vars: make(map[*flowgraph.VarDef]*Obj),
	}
}

func (interp *Interp) Eval(fun *flowgraph.FuncDef, args ...*Obj) {
	interp.stack = []frame{makeFrame(interp, fun, args)}
	for len(interp.stack) > 0 {
		interp.step()
	}
}

func makeFrame(interp *Interp, fun *flowgraph.FuncDef, args []*Obj) frame {
	if len(args) != len(fun.Parms) {
		panic(fmt.Sprintf("%s: got %d arguments, expected %d",
			fun, len(args), len(fun.Parms)))
	}
	if len(fun.Blocks) == 0 {
		panic(fmt.Sprintf("%s: function body is undefined", fun))
	}
	interp.nextFrameID++
	frame := frame{
		id:         interp.nextFrameID - 1,
		args:       make(map[*flowgraph.ParmDef]*Obj),
		vals:       make(map[flowgraph.Value]*Obj),
		FuncDef:    fun,
		BasicBlock: fun.Blocks[0],
		n:          0,
	}
	for i, parm := range fun.Parms {
		frame.args[parm] = args[i]
	}
	return frame
}

type varVal struct {
	n   int
	val Val
}

func (interp *Interp) step() {
	defer func() { interp.n++ }()

	frame := &interp.stack[len(interp.stack)-1]
	instr := frame.BasicBlock.Instrs[frame.n]
	frame.n++

	if interp.Trace {
		fmt.Printf("\n\n--- step %03d ---\n", interp.n)
		fmt.Printf("block %d, instr %d: %s\n",
			frame.BasicBlock.Num, frame.n, instr)
		defer func() {
			if n := len(interp.stack); n > 0 {
				var vv []varVal
				for k, v := range interp.stack[n-1].vals {
					vv = append(vv, varVal{k.Num(), v.val})
				}
				sort.Slice(vv, func(i, j int) bool { return vv[i].n < vv[j].n })
				for _, v := range vv {
					fmt.Printf("x%d:\t%s\t\t%T\n", v.n, v.val, v.val)
				}
			}
		}()
	}

	switch instr := instr.(type) {
	case *flowgraph.Store:
		dst := frame.vals[instr.Dst].Val().(Pointer)
		src := frame.vals[instr.Src]
		copyObj(dst.Elem, src)
	case *flowgraph.Copy:
		dst := frame.vals[instr.Dst].Val().(Pointer)
		src := frame.vals[instr.Src].Val().(Pointer)
		copyObj(dst.Elem, src.Elem)
	case *flowgraph.Call:
		fun := frame.vals[instr.Func].Val().(Func)
		if len(fun.Def.Blocks) == 0 && fun.Def.Name == "print_int" {
			fmt.Fprintf(interp.Out, "%d", frame.vals[instr.Args[0]].Val().(SignedInt).Int64())
			break
		}
		args := make([]*Obj, len(instr.Args))
		for i, arg := range instr.Args {
			args[i] = frame.vals[arg]
		}
		interp.stack = append(interp.stack, makeFrame(interp, fun.Def, args))
	case *flowgraph.If:
		var yes bool
		x := frame.vals[instr.Value]
		if _, ok := instr.Value.Type().(*flowgraph.AddrType); ok {
			if instr.Op != flowgraph.Eq {
				panic(fmt.Sprintf("got If.Op %s, expected =", instr.Op))
			}
			yes = x.Val().(Pointer).Elem == nil
			if yes {
				frame.BasicBlock = instr.Yes
			} else {
				frame.BasicBlock = instr.No
			}
			frame.n = 0
			return
		}
		switch instr.Op {
		case flowgraph.Eq:
			switch x.Val().(type) {
			case SignedInt:
				yes = x.Val().(SignedInt).Int64() == int64(instr.X)
			case UnsignedInt:
				yes = x.Val().(UnsignedInt).Uint64() == uint64(instr.X)
			default:
				panic(fmt.Sprintf("bad eq type: %T", x.Val()))
			}
		case flowgraph.Less:
			if instr.XValue == nil {
				switch x.Val().(type) {
				case SignedInt:
					yes = x.Val().(SignedInt).Int64() < int64(instr.X)
				case UnsignedInt:
					if instr.X < 0 {
						panic("bad x")
					}
					yes = x.Val().(UnsignedInt).Uint64() < uint64(instr.X)
				}
			} else {
				y := frame.vals[instr.XValue]
				switch x.Val().(type) {
				case SignedInt:
					yes = x.Val().(SignedInt).Int64() < y.Val().(SignedInt).Int64()
				case UnsignedInt:
					yes = x.Val().(UnsignedInt).Uint64() < y.Val().(UnsignedInt).Uint64()
				case Float32:
					yes = x.Val().(Float32) < y.Val().(Float32)
				case Float64:
					yes = x.Val().(Float64) < y.Val().(Float64)
				}
			}
		case flowgraph.LessEq:
			y := frame.vals[instr.XValue]
			switch x.Val().(type) {
			case SignedInt:
				yes = x.Val().(SignedInt).Int64() <= y.Val().(SignedInt).Int64()
			case UnsignedInt:
				yes = x.Val().(UnsignedInt).Uint64() <= y.Val().(UnsignedInt).Uint64()
			case Float32:
				yes = x.Val().(Float32) <= y.Val().(Float32)
			case Float64:
				yes = x.Val().(Float64) <= y.Val().(Float64)
			}
		case flowgraph.Greater:
			y := frame.vals[instr.XValue]
			switch x.Val().(type) {
			case SignedInt:
				yes = x.Val().(SignedInt).Int64() > y.Val().(SignedInt).Int64()
			case UnsignedInt:
				yes = x.Val().(UnsignedInt).Uint64() > y.Val().(UnsignedInt).Uint64()
			case Float32:
				yes = x.Val().(Float32) > y.Val().(Float32)
			case Float64:
				yes = x.Val().(Float64) > y.Val().(Float64)
			}
		default:
			panic(fmt.Sprintf("got If.Op %s, expected = or <", instr.Op))
		}
		if yes {
			frame.BasicBlock = instr.Yes
		} else {
			frame.BasicBlock = instr.No
		}
		frame.n = 0
		return
	case *flowgraph.Jump:
		frame.BasicBlock = instr.Dst
		frame.n = 0
		return
	case *flowgraph.Return:
		if interp.Trace {
			fmt.Printf("returning from %s\n", frame.Func.Name)
		}
		var frameID Val
		if instr.Frame != nil {
			frameID = frame.vals[instr.Frame].Val()
		}
		for {
			popped := interp.stack[len(interp.stack)-1]
			interp.stack = interp.stack[:len(interp.stack)-1]
			for _, del := range popped.stackAllocs {
				del.delete(interp.Trace)
			}
			if frameID == nil || popped.id == int(frameID.(Int64)) {
				break
			}
			if len(interp.stack) == 0 {
				// This case should never happen,
				// because flowgraph building
				// shoud always insert a CheckFrame
				// that would have failed instead.
				panic("impossible")
			}
		}
		return
	case *flowgraph.Frame:
		frame.vals[instr] = &Obj{val: Int64(frame.id)}
	case *flowgraph.Alloc:
		var objs []*Obj
		switch {
		case instr.Count != nil:
			n := frame.vals[instr.Count]
			ary := make([]*Obj, n.Val().(SignedInt).Int64())
			for i := range ary {
				ary[i] = newObj(instr.T.(*flowgraph.ArrayType).Elem)
			}
			objs = ary
			frame.vals[instr] = &Obj{val: Array{Elems: ary}}
		case instr.CountImm >= 0:
			ary := make([]*Obj, instr.CountImm)
			for i := range ary {
				ary[i] = newObj(instr.T.(*flowgraph.ArrayType).Elem)
			}
			objs = ary
			frame.vals[instr] = &Obj{val: Array{Elems: ary}}
		default:
			x := newObj(instr.T.(*flowgraph.AddrType).Elem)
			objs = []*Obj{x}
			frame.vals[instr] = &Obj{val: Pointer{Elem: x}}
		}
		if instr.Stack {
			if interp.Trace {
				for _, o := range objs {
					fmt.Printf("allocating %p on the stack\n", o)
				}
			}
			frame.stackAllocs = append(frame.stackAllocs, objs...)
		}
	case *flowgraph.Load:
		frame.vals[instr] = frame.vals[instr.Addr].Val().(Pointer).Elem
	case *flowgraph.BitCast:
		frame.vals[instr] = frame.vals[instr.Src]
	case *flowgraph.Func:
		frame.vals[instr] = &Obj{val: Func{Def: instr.Def}}
	case *flowgraph.String:
		ary := make([]*Obj, len(instr.Def.Text))
		for i := range instr.Def.Text {
			ary[i] = &Obj{val: Uint8(instr.Def.Text[i])}
		}
		frame.vals[instr] = &Obj{val: Array{Elems: ary}}
	case *flowgraph.Var:
		v, ok := interp.vars[instr.Def]
		if !ok {
			v = newObj(instr.Def.Type)
			interp.vars[instr.Def] = v
		}
		frame.vals[instr] = &Obj{val: Pointer{Elem: v}}
	case *flowgraph.Parm:
		p, ok := frame.args[instr.Def]
		if !ok {
			panic(fmt.Sprintf("no parameter %s", instr.Def.Name))
		}
		if p == nil {
			panic(fmt.Sprintf("nil parameter %s", instr.Def.Name))
		}
		frame.vals[instr] = p
	case *flowgraph.Field:
		base := frame.vals[instr.Base].Val().(Pointer).Elem.Val().(Struct)
		field := base.Fields[instr.Def.Num].Obj
		frame.vals[instr] = &Obj{val: Pointer{Elem: field}}
	case *flowgraph.Case:
		base := frame.vals[instr.Base].Val().(Pointer).Elem.Val().(Union)
		var found bool
		for i := range base.Cases {
			if base.Cases[i].Name != instr.Def.Name {
				continue
			}
			found = true
			if base.Current < 0 {
				base.Current = i
			}
			if base.Current != i {
				panic("accessing bad union case")
			}
			break
		}
		if !found {
			panic("no case")
		}
		frame.vals[instr] = &Obj{
			val: Pointer{Elem: base.Cases[base.Current].Obj},
		}
	case *flowgraph.Index:
		ary := frame.vals[instr.Base].Val().(Array).Elems
		i := frame.vals[instr.Index].Val().(SignedInt).Int64()
		frame.vals[instr] = &Obj{val: Pointer{Elem: ary[i]}}
	case *flowgraph.Slice:
		ary := frame.vals[instr.Base].Val().(Array).Elems
		i := frame.vals[instr.Index].Val().(SignedInt).Int64()
		slice := ary[i:]
		frame.vals[instr] = &Obj{val: Array{Elems: slice}}
	case *flowgraph.Int:
		var x Val
		switch instr.T.Size {
		case 8:
			if instr.T.Unsigned {
				x = Uint8(mustParseUint(instr.Text, 8))
			} else {
				x = Int8(mustParseInt(instr.Text, 8))
			}
		case 16:
			if instr.T.Unsigned {
				x = Uint16(mustParseUint(instr.Text, 16))
			} else {
				x = Int16(mustParseInt(instr.Text, 16))
			}
		case 32:
			if instr.T.Unsigned {
				x = Uint32(mustParseUint(instr.Text, 32))
			} else {
				x = Int32(mustParseInt(instr.Text, 32))
			}
		case 64:
			if instr.T.Unsigned {
				x = Uint64(mustParseUint(instr.Val.String(), 64))
			} else {
				x = Int64(mustParseInt(instr.Val.String(), 64))
			}
		default:
			panic(fmt.Sprintf("bad int size: %d", instr.T.Size))
		}
		frame.vals[instr] = &Obj{val: x}
	case *flowgraph.Float:
		var x Val
		switch instr.T.Size {
		case 32:
			x = Float32(mustParseFloat(instr.Text, 32))
		case 64:
			x = Float64(mustParseFloat(instr.Text, 64))
		default:
			panic(fmt.Sprintf("bad float size: %d", instr.T.Size))
		}
		frame.vals[instr] = &Obj{val: x}
	case *flowgraph.Null:
		if _, ok := instr.Type().(*flowgraph.ArrayType); ok {
			frame.vals[instr] = &Obj{val: Array{}}
		} else {
			frame.vals[instr] = &Obj{val: Pointer{}}
		}
	case *flowgraph.Op:
		switch instr.Op {
		case flowgraph.BitNot, flowgraph.Negate:
			x := frame.vals[instr.Args[0]].Val().(Number)
			frame.vals[instr] = &Obj{val: x.Un(instr.Op)}
		case flowgraph.BitXor, flowgraph.BitAnd, flowgraph.BitOr,
			flowgraph.LeftShift, flowgraph.RightShift, flowgraph.Minus,
			flowgraph.Plus, flowgraph.Times, flowgraph.Divide,
			flowgraph.Modulus, flowgraph.Eq, flowgraph.Neq,
			flowgraph.Less, flowgraph.LessEq, flowgraph.Greater,
			flowgraph.GreaterEq:
			x := frame.vals[instr.Args[0]].Val().(Number)
			y := frame.vals[instr.Args[1]].Val().(Number)
			frame.vals[instr] = &Obj{val: x.Bin(instr.Op, y)}
		case flowgraph.NumConvert:
			x := frame.vals[instr.Args[0]].Val()
			frame.vals[instr] = &Obj{val: convert(x, instr.Type())}
		case flowgraph.Panic:
			panic(goString(frame.vals[instr.Args[0]]))
		case flowgraph.Print:
			fmt.Fprint(interp.Out, goString(frame.vals[instr.Args[0]]))
		case flowgraph.IndexOOBString:
			index := frame.vals[instr.Args[0]].Val()
			length := frame.vals[instr.Args[1]].Val()
			s := fmt.Sprintf("index out of bounds: index=%d, length=%d",
				index.(SignedInt).Int64(), length.(SignedInt).Int64())
			frame.vals[instr] = peaString(s)
		case flowgraph.SliceOOBString:
			start := frame.vals[instr.Args[0]].Val()
			end := frame.vals[instr.Args[1]].Val()
			length := frame.vals[instr.Args[2]].Val()
			s := fmt.Sprintf("slice out of bounds: start=%d, end=%d, length=%d",
				start.(SignedInt).Int64(), end.(SignedInt).Int64(), length.(SignedInt).Int64())
			frame.vals[instr] = peaString(s)
		case flowgraph.CheckFrame:
			frameID := frame.vals[instr.Args[0]].Val()
			ok := false
			for _, frame := range interp.stack {
				if frame.id == int(frameID.(Int64)) {
					ok = true
					break
				}
			}
			if !ok {
				panic("long return across threads or to returned frame")
			}
		default:
			panic(fmt.Sprintf("unknown instruction: %s", instr))
		}
	default:
		panic(fmt.Sprintf("unknown instruction: %T", instr))
	}
}

func peaString(s string) *Obj {
	bytes := make([]*Obj, len(s))
	for i, b := range s {
		bytes[i] = &Obj{val: Uint8(b)}
	}
	return &Obj{
		val: Pointer{
			Elem: &Obj{
				val: Struct{
					Fields: []Field{
						{
							Name: "length",
							// TODO: figure out int size
							Obj: &Obj{val: Int64(int64(len(s)))},
						},
						{
							Name: "data",
							Obj:  &Obj{val: Array{Elems: bytes}},
						},
					},
				},
			},
		},
	}
}

func goString(o *Obj) string {
	str := o.Val().(Pointer).Elem.Val().(Struct)
	var s strings.Builder
	n := int(str.Fields[0].Obj.Val().(SignedInt).Int64())
	for i := 0; i < n; i++ {
		o := str.Fields[1].Obj.Val().(Array).Elems[i]
		s.WriteByte(uint8(o.Val().(Uint8)))
	}
	return s.String()
}

func mustParseInt(text string, bits int) int64 {
	i, err := strconv.ParseInt(text, 10, bits)
	if err != nil {
		panic("bad int: " + err.Error())
	}
	return i
}

func mustParseUint(text string, bits int) uint64 {
	i, err := strconv.ParseUint(text, 10, bits)
	if err != nil {
		panic("bad int: " + err.Error())
	}
	return i
}

func mustParseFloat(text string, bits int) float64 {
	i, err := strconv.ParseFloat(text, bits)
	if err != nil {
		panic("bad int: " + err.Error())
	}
	return i
}
