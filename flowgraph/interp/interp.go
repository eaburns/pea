package interp

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/eaburns/pea/flowgraph"
)

type Interp struct {
	Out         io.Writer
	Trace       bool
	vars        map[*flowgraph.VarDef]Pointer
	stack       []frame
	nextFrameID int
	n           int
}

type frame struct {
	id int

	args map[*flowgraph.ParmDef]*Obj
	vals map[flowgraph.Value]Obj
	*flowgraph.FuncDef
	*flowgraph.BasicBlock
	n int
}

func New() *Interp {
	return &Interp{
		Out:  os.Stdout,
		vars: make(map[*flowgraph.VarDef]Pointer),
	}
}

func (interp *Interp) Eval(fun *flowgraph.FuncDef, args ...Obj) {
	interp.stack = []frame{makeFrame(interp, fun, args)}
	for len(interp.stack) > 0 {
		interp.step()
	}
}

func makeFrame(interp *Interp, fun *flowgraph.FuncDef, args []Obj) frame {
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
		vals:       make(map[flowgraph.Value]Obj),
		FuncDef:    fun,
		BasicBlock: fun.Blocks[0],
		n:          0,
	}
	for i, parm := range fun.Parms {
		frame.args[parm] = &args[i]
	}
	return frame
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
				for k, v := range interp.stack[n-1].vals {
					fmt.Printf("x%d: %s\t\t\t%T\n", k.Num(), v, v)
				}
			}
		}()
	}

	switch instr := instr.(type) {
	case *flowgraph.Store:
		dst := frame.vals[instr.Dst].(Pointer)
		src := frame.vals[instr.Src]
		*dst.Elem = src.ShallowCopy()
	case *flowgraph.Copy:
		dst := frame.vals[instr.Dst].(Pointer)
		src := frame.vals[instr.Src].(Pointer)
		*dst.Elem = (*src.Elem).ShallowCopy()
	case *flowgraph.Call:
		fun := frame.vals[instr.Func].(Func)
		if len(fun.Def.Blocks) == 0 && fun.Def.Name == "print_int" {
			fmt.Printf("%d\n", frame.vals[instr.Args[0]].(SignedInt).Int64())
			break
		}
		args := make([]Obj, len(instr.Args))
		for i, arg := range instr.Args {
			args[i] = frame.vals[arg]
		}
		interp.stack = append(interp.stack, makeFrame(interp, fun.Def, args))
	case *flowgraph.If:
		var yes bool
		x := frame.vals[instr.Value]
		if _, ok := instr.Value.Type().(*flowgraph.AddrType); ok {
			if instr.Op != flowgraph.Eq {
				panic(fmt.Sprintf("got If.Op=%s, expected =", instr.Op))
			}
			yes = x.(Pointer).Elem == nil
		} else {
			switch instr.Op {
			case flowgraph.Eq:
				yes = x.(SignedInt).Int64() == int64(instr.X)
			case flowgraph.Less:
				yes = x.(SignedInt).Int64() < int64(instr.X)
			default:
				panic(fmt.Sprintf("got If.Op=%s, expected = or <", instr.Op))
			}
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
		if instr.Frame == nil {
			interp.stack = interp.stack[:len(interp.stack)-1]
			return
		}
		frameID := frame.vals[instr.Frame].(Int64)
		for {
			popped := interp.stack[len(interp.stack)-1]
			interp.stack = interp.stack[:len(interp.stack)-1]
			if popped.id == int(frameID) {
				break
			}
			if len(interp.stack) == 0 {
				panic("long return to finished frame")
			}
		}
		return
	case *flowgraph.Frame:
		frame.vals[instr] = Int64(frame.id)
	case *flowgraph.Alloc:
		switch {
		case instr.Count != nil:
			n := frame.vals[instr.Count]
			ary := make([]Obj, n.(SignedInt).Int64())
			frame.vals[instr] = Array{Elems: &ary}
		case instr.CountImm >= 0:
			ary := make([]Obj, instr.CountImm)
			frame.vals[instr] = Array{Elems: &ary}
		default:
			x := newObj(instr.T.(*flowgraph.AddrType).Elem)
			frame.vals[instr] = Pointer{Elem: &x}
		}
	case *flowgraph.Load:
		frame.vals[instr] = *frame.vals[instr.Addr].(Pointer).Elem
	case *flowgraph.Func:
		frame.vals[instr] = Func{Def: instr.Def}
	case *flowgraph.String:
		ary := make([]Obj, len(instr.Def.Text))
		for i := range instr.Def.Text {
			ary[i] = Uint8(instr.Def.Text[i])
		}
		frame.vals[instr] = Array{Elems: &ary}
	case *flowgraph.Var:
		frame.vals[instr] = interp.vars[instr.Def]
	case *flowgraph.Parm:
		frame.vals[instr] = Pointer{Elem: frame.args[instr.Def]}
	case *flowgraph.Field:
		base := (*frame.vals[instr.Base].(Pointer).Elem).(Struct)
		field := &base.Fields[instr.Def.Num].Val
		frame.vals[instr] = Pointer{Elem: field}
	case *flowgraph.Case:
		base := (*frame.vals[instr.Base].(Pointer).Elem).(Union)
		frame.vals[instr] = base.Case
	case *flowgraph.Index:
		ary := *frame.vals[instr.Base].(Array).Elems
		i := frame.vals[instr.Index].(SignedInt).Int64()
		frame.vals[instr] = Pointer{Elem: &ary[i]}
	case *flowgraph.Slice:
		ary := *frame.vals[instr.Base].(Array).Elems
		i := frame.vals[instr.Index].(SignedInt).Int64()
		slice := ary[i:]
		frame.vals[instr] = Array{Elems: &slice}
	case *flowgraph.Int:
		var x Obj
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
				x = Uint64(mustParseUint(instr.Text, 64))
			} else {
				x = Int64(mustParseInt(instr.Text, 64))
			}
		default:
			panic(fmt.Sprintf("bad int size: %d", instr.T.Size))
		}
		frame.vals[instr] = x
	case *flowgraph.Float:
		var x Obj
		switch instr.T.Size {
		case 32:
			x = Uint32(mustParseFloat(instr.Text, 32))
		case 64:
			x = Uint64(mustParseFloat(instr.Text, 64))
		default:
			panic(fmt.Sprintf("bad float size: %d", instr.T.Size))
		}
		frame.vals[instr] = x
	case *flowgraph.Op:
		switch instr.Op {
		case flowgraph.BitNot, flowgraph.Negate:
			x := frame.vals[instr.Args[0]].(Number)
			frame.vals[instr] = x.Un(instr.Op)
		case flowgraph.BitXor, flowgraph.BitAnd, flowgraph.BitOr,
			flowgraph.LeftShift, flowgraph.RightShift, flowgraph.Minus,
			flowgraph.Plus, flowgraph.Times, flowgraph.Divide,
			flowgraph.Modulus, flowgraph.Eq, flowgraph.Neq,
			flowgraph.Less, flowgraph.LessEq, flowgraph.Greater,
			flowgraph.GreaterEq:
			x := frame.vals[instr.Args[0]].(Number)
			y := frame.vals[instr.Args[1]].(Number)
			frame.vals[instr] = x.Bin(instr.Op, y)
		case flowgraph.NumConvert:
			frame.vals[instr] = convert(frame.vals[instr.Args[0]], instr.Type())
		case flowgraph.Panic:
			panic(goString(frame.vals[instr.Args[0]]))
		case flowgraph.Print:
			fmt.Fprintf(interp.Out, goString(frame.vals[instr.Args[0]]))
		default:
			panic(fmt.Sprintf("unknown instruction: %s", instr))
		}
	default:
		panic(fmt.Sprintf("unknown instruction: %T", instr))
	}
}

func goString(o Obj) string {
	str := (*o.(Pointer).Elem).(Struct)
	var s strings.Builder
	for _, o := range *str.Fields[1].Val.(Array).Elems {
		s.WriteByte(uint8(o.(Uint8)))
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
