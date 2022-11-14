package interp

import (
	"fmt"
	"reflect"
	"strings"
	"unsafe"

	"github.com/eaburns/pea/flowgraph"
)

type Obj struct {
	deleted bool
	val     Val
}

func newObj(typ flowgraph.Type) *Obj {
	return &Obj{val: newVal(typ)}
}

func (o *Obj) delete(trace bool) {
	if trace {
		fmt.Printf("deleting %p %s\n", o, o)
	}
	if o.deleted {
		panic("deleting deleted object")
	}
	o.deleted = true
	if o.val != nil {
		o.val.delete(trace)
	}
}

func (o Obj) String() string {
	var s strings.Builder
	if o.deleted {
		s.WriteString("deleted ")
	}
	if o.val == nil {
		s.WriteString("<empty>")
	} else {
		s.WriteString(o.val.String())
	}
	return s.String()
}

func (o *Obj) Val() Val {
	if o.deleted {
		panic(fmt.Sprintf("reading deleted object %p", o))
	}
	if o.val == nil {
		panic("impossible")
	}
	return o.val
}

func (o *Obj) SetVal(v Val) {
	if o.deleted {
		panic("writing deleted object")
	}
	if o.val != nil && reflect.TypeOf(o.val) != reflect.TypeOf(v) {
		panic(fmt.Sprintf("want type %T, got %T", o.val, v))
	}
	o.val = v
}

func copyObj(dst, src *Obj) {
	if dst.deleted {
		panic("writing deleted object")
	}
	switch dstVal := dst.Val().(type) {
	case Int8:
		dst.val = src.Val().(Int8)
	case Int16:
		dst.val = src.Val().(Int16)
	case Int32:
		dst.val = src.Val().(Int32)
	case Int64:
		dst.val = src.Val().(Int64)
	case Uint8:
		dst.val = src.Val().(Uint8)
	case Uint16:
		dst.val = src.Val().(Uint16)
	case Uint32:
		dst.val = src.Val().(Uint32)
	case Uint64:
		dst.val = src.Val().(Uint64)
	case Float32:
		dst.val = src.Val().(Float32)
	case Float64:
		dst.val = src.Val().(Float64)
	case Func:
		dst.val = src.Val().(Func)
	case Pointer:
		dst.val = src.Val().(Pointer)
	case Array:
		dst.val = src.Val().(Array)
	case Union:
		srcVal := src.Val().(Union)
		dstVal.Current = srcVal.Current
		for i := range srcVal.Cases {
			copyObj(dstVal.Cases[i].Obj, srcVal.Cases[i].Obj)
		}
	case Struct:
		srcVal := src.Val().(Struct)
		for i := range srcVal.Fields {
			copyObj(dstVal.Fields[i].Obj, srcVal.Fields[i].Obj)
		}
	default:
		panic(fmt.Sprintf("unknown Val type %T", dst.val))
	}
}

type Val interface {
	String() string
	delete(bool)
}

func newVal(typ flowgraph.Type) Val {
	switch typ := typ.(type) {
	case *flowgraph.IntType:
		switch typ.Size {
		case 8:
			if typ.Unsigned {
				return Uint8(0)
			} else {
				return Int8(0)
			}
		case 16:
			if typ.Unsigned {
				return Uint16(0)
			} else {
				return Int16(0)
			}
		case 32:
			if typ.Unsigned {
				return Uint32(0)
			} else {
				return Int32(0)
			}
		case 64:
			if typ.Unsigned {
				return Uint64(0)
			} else {
				return Int64(0)
			}
		default:
			panic(fmt.Sprintf("bad int size: %d", typ.Size))
		}
	case *flowgraph.FloatType:
		switch typ.Size {
		case 32:
			return Float32(0)
		case 64:
			return Float64(0)
		default:
			panic(fmt.Sprintf("bad float size: %d", typ.Size))
		}
	case *flowgraph.AddrType:
		return Pointer{}
	case *flowgraph.ArrayType:
		return Array{}
	case *flowgraph.FrameType:
		return Int64(0)
	case *flowgraph.StructType:
		fields := make([]Field, len(typ.Fields))
		for i := range typ.Fields {
			fields[i].Name = typ.Fields[i].Name
			fields[i].Obj = newObj(typ.Fields[i].Type)
		}
		return Struct{Fields: fields}
	case *flowgraph.UnionType:
		cases := make([]Field, len(typ.Cases))
		for i := range typ.Cases {
			cases[i].Name = typ.Cases[i].Name
			cases[i].Obj = newObj(typ.Cases[i].Type)
		}
		return Union{Cases: cases, Current: -1}
	case *flowgraph.FuncType:
		return Func{}
	default:
		panic(fmt.Sprintf("newObj unknown type: %s", typ))
	}
}

type Int8 int8
type Int16 int16
type Int32 int32
type Int64 int64
type Uint8 uint8
type Uint16 uint16
type Uint32 uint32
type Uint64 uint64
type Float32 float32
type Float64 float64
type Func struct{ Def *flowgraph.FuncDef }
type Pointer struct{ Elem *Obj }
type Array struct{ Elems []*Obj }
type Struct struct{ Fields []Field }
type Union struct {
	Cases   []Field
	Current int
}
type Field struct {
	Name string
	Obj  *Obj
}

func (o Int8) String() string    { return fmt.Sprintf("%d", o) }
func (o Int16) String() string   { return fmt.Sprintf("%d", o) }
func (o Int32) String() string   { return fmt.Sprintf("%d", o) }
func (o Int64) String() string   { return fmt.Sprintf("%d", o) }
func (o Uint8) String() string   { return fmt.Sprintf("%d", o) }
func (o Uint16) String() string  { return fmt.Sprintf("%d", o) }
func (o Uint32) String() string  { return fmt.Sprintf("%d", o) }
func (o Uint64) String() string  { return fmt.Sprintf("%d", o) }
func (o Float32) String() string { return fmt.Sprintf("%f", o) }
func (o Float64) String() string { return fmt.Sprintf("%f", o) }

func (o Func) String() string {
	if o.Def == nil {
		return "null"
	}
	return fmt.Sprintf("&%s.%s", o.Def.Mod, o.Def.Name)
}

func (o Pointer) String() string {
	if o.Elem == nil {
		return "null"
	}
	return fmt.Sprintf("&%s", o.Elem.String())
}

func (o Array) String() string {
	var s strings.Builder
	s.WriteRune('[')
	for i, e := range o.Elems {
		if i > 0 {
			s.WriteString(", ")
		}
		if e == nil {
			s.WriteRune('·')
		} else {
			s.WriteString(e.String())
		}
	}
	s.WriteRune(']')
	return s.String()
}

func (o Union) String() string {
	if o.Current < 0 {
		return "{<uninitialized>}"
	}
	var s strings.Builder
	s.WriteRune('{')
	s.WriteString(o.Cases[o.Current].Name)
	s.WriteString(": ")
	s.WriteString(o.Cases[o.Current].Obj.String())
	s.WriteRune('}')
	return s.String()
}

func (o Struct) String() string {
	var s strings.Builder
	s.WriteRune('{')
	for i, f := range o.Fields {
		if i > 0 {
			s.WriteString(", ")
		}
		s.WriteString(f.Name)
		s.WriteString(": ")
		s.WriteString(f.Obj.String())
	}
	s.WriteRune('}')
	return s.String()
}

func (Int8) delete(bool)        {}
func (Int16) delete(bool)       {}
func (Int32) delete(bool)       {}
func (Int64) delete(bool)       {}
func (Uint8) delete(bool)       {}
func (Uint16) delete(bool)      {}
func (Uint32) delete(bool)      {}
func (Uint64) delete(bool)      {}
func (Float32) delete(bool)     {}
func (Float64) delete(bool)     {}
func (Func) delete(bool)        {}
func (Pointer) delete(bool)     {}
func (Array) delete(trace bool) {}

func (o Union) delete(trace bool) {
	for _, e := range o.Cases {
		e.Obj.delete(trace)
	}
}

func (o Struct) delete(trace bool) {
	for _, e := range o.Fields {
		e.Obj.delete(trace)
	}
}

type SignedInt interface {
	Int64() int64
}

func (o Int8) Int64() int64  { return int64(o) }
func (o Int16) Int64() int64 { return int64(o) }
func (o Int32) Int64() int64 { return int64(o) }
func (o Int64) Int64() int64 { return int64(o) }

type UnsignedInt interface {
	Uint64() uint64
}

func (o Uint8) Uint64() uint64  { return uint64(o) }
func (o Uint16) Uint64() uint64 { return uint64(o) }
func (o Uint32) Uint64() uint64 { return uint64(o) }
func (o Uint64) Uint64() uint64 { return uint64(o) }

type Number interface {
	Val
	Un(flowgraph.OpKind) Number
	Bin(flowgraph.OpKind, Number) Number
}

func (o Int8) Un(op flowgraph.OpKind) Number    { return un(o, op) }
func (o Int16) Un(op flowgraph.OpKind) Number   { return un(o, op) }
func (o Int32) Un(op flowgraph.OpKind) Number   { return un(o, op) }
func (o Int64) Un(op flowgraph.OpKind) Number   { return un(o, op) }
func (o Uint8) Un(op flowgraph.OpKind) Number   { return un(o, op) }
func (o Uint16) Un(op flowgraph.OpKind) Number  { return un(o, op) }
func (o Uint32) Un(op flowgraph.OpKind) Number  { return un(o, op) }
func (o Uint64) Un(op flowgraph.OpKind) Number  { return un(o, op) }
func (o Float32) Un(op flowgraph.OpKind) Number { return un(o, op) }
func (o Float64) Un(op flowgraph.OpKind) Number { return un(o, op) }

func (o Int8) Bin(op flowgraph.OpKind, p Number) Number    { return bin(o, p, op) }
func (o Int16) Bin(op flowgraph.OpKind, p Number) Number   { return bin(o, p, op) }
func (o Int32) Bin(op flowgraph.OpKind, p Number) Number   { return bin(o, p, op) }
func (o Int64) Bin(op flowgraph.OpKind, p Number) Number   { return bin(o, p, op) }
func (o Uint8) Bin(op flowgraph.OpKind, p Number) Number   { return bin(o, p, op) }
func (o Uint16) Bin(op flowgraph.OpKind, p Number) Number  { return bin(o, p, op) }
func (o Uint32) Bin(op flowgraph.OpKind, p Number) Number  { return bin(o, p, op) }
func (o Uint64) Bin(op flowgraph.OpKind, p Number) Number  { return bin(o, p, op) }
func (o Float32) Bin(op flowgraph.OpKind, p Number) Number { return bin(o, p, op) }
func (o Float64) Bin(op flowgraph.OpKind, p Number) Number { return bin(o, p, op) }

func un(o Number, op flowgraph.OpKind) Number {
	switch o.(type) {
	case Int8, Int16, Int32, Int64:
		var x int64
		switch op {
		case flowgraph.BitNot:
			x = ^reflect.ValueOf(o).Int()
		case flowgraph.Negate:
			x = -reflect.ValueOf(o).Int()
		default:
			panic(fmt.Sprintf("bad op: %s", op))
		}
		z := reflect.New(reflect.TypeOf(o)).Elem()
		z.SetInt(x)
		return z.Interface().(Number)
	case Uint8, Uint16, Uint32, Uint64:
		var x uint64
		switch op {
		case flowgraph.BitNot:
			x = ^reflect.ValueOf(o).Uint()
		case flowgraph.Negate:
			x = -reflect.ValueOf(o).Uint()
		default:
			panic(fmt.Sprintf("bad op: %s", op))
		}
		z := reflect.New(reflect.TypeOf(o)).Elem()
		z.SetUint(x)
		return z.Interface().(Number)
	case Float32, Float64:
		var x float64
		switch op {
		case flowgraph.Negate:
			x = -reflect.ValueOf(o).Float()
		default:
			panic(fmt.Sprintf("bad op: %s", op))
		}
		z := reflect.New(reflect.TypeOf(o)).Elem()
		z.SetFloat(x)
		return z.Interface().(Number)
	default:
		panic(fmt.Sprintf("bad number type: %T", o))
	}
}

func bin(o, p Number, op flowgraph.OpKind) Number {
	switch o.(type) {
	case Int8, Int16, Int32, Int64:
		var x int64
		switch op {
		case flowgraph.BitXor:
			x = reflect.ValueOf(o).Int() ^ reflect.ValueOf(p).Int()
		case flowgraph.BitAnd:
			x = reflect.ValueOf(o).Int() & reflect.ValueOf(p).Int()
		case flowgraph.BitOr:
			x = reflect.ValueOf(o).Int() | reflect.ValueOf(p).Int()
		case flowgraph.LeftShift:
			x = reflect.ValueOf(o).Int() << reflect.ValueOf(p).Int()
		case flowgraph.RightShift:
			x = reflect.ValueOf(o).Int() >> reflect.ValueOf(p).Int()
		case flowgraph.Minus:
			x = reflect.ValueOf(o).Int() - reflect.ValueOf(p).Int()
		case flowgraph.Plus:
			x = reflect.ValueOf(o).Int() + reflect.ValueOf(p).Int()
		case flowgraph.Times:
			x = reflect.ValueOf(o).Int() * reflect.ValueOf(p).Int()
		case flowgraph.Divide:
			x = reflect.ValueOf(o).Int() / reflect.ValueOf(p).Int()
		case flowgraph.Modulus:
			x = reflect.ValueOf(o).Int() % reflect.ValueOf(p).Int()
		default:
			var b bool
			switch op {
			case flowgraph.Eq:
				b = reflect.ValueOf(o).Int() == reflect.ValueOf(p).Int()
			case flowgraph.Neq:
				b = reflect.ValueOf(o).Int() != reflect.ValueOf(p).Int()
			case flowgraph.Less:
				b = reflect.ValueOf(o).Int() < reflect.ValueOf(p).Int()
			case flowgraph.LessEq:
				b = reflect.ValueOf(o).Int() <= reflect.ValueOf(p).Int()
			case flowgraph.Greater:
				b = reflect.ValueOf(o).Int() > reflect.ValueOf(p).Int()
			case flowgraph.GreaterEq:
				b = reflect.ValueOf(o).Int() >= reflect.ValueOf(p).Int()
			default:
				panic(fmt.Sprintf("bad op: %s", op))
			}
			return Bool(b)
		}
		z := reflect.New(reflect.TypeOf(o)).Elem()
		z.SetInt(x)
		return z.Interface().(Number)
	case Uint8, Uint16, Uint32, Uint64:
		var x uint64
		switch op {
		case flowgraph.BitXor:
			x = reflect.ValueOf(o).Uint() ^ reflect.ValueOf(p).Uint()
		case flowgraph.BitAnd:
			x = reflect.ValueOf(o).Uint() & reflect.ValueOf(p).Uint()
		case flowgraph.BitOr:
			x = reflect.ValueOf(o).Uint() | reflect.ValueOf(p).Uint()
		case flowgraph.LeftShift:
			x = reflect.ValueOf(o).Uint() << reflect.ValueOf(p).Uint()
		case flowgraph.RightShift:
			x = reflect.ValueOf(o).Uint() >> reflect.ValueOf(p).Uint()
		case flowgraph.Minus:
			x = reflect.ValueOf(o).Uint() - reflect.ValueOf(p).Uint()
		case flowgraph.Plus:
			x = reflect.ValueOf(o).Uint() + reflect.ValueOf(p).Uint()
		case flowgraph.Times:
			x = reflect.ValueOf(o).Uint() * reflect.ValueOf(p).Uint()
		case flowgraph.Divide:
			x = reflect.ValueOf(o).Uint() / reflect.ValueOf(p).Uint()
		case flowgraph.Modulus:
			x = reflect.ValueOf(o).Uint() % reflect.ValueOf(p).Uint()
		default:
			var b bool
			switch op {
			case flowgraph.Eq:
				b = reflect.ValueOf(o).Uint() == reflect.ValueOf(p).Uint()
			case flowgraph.Neq:
				b = reflect.ValueOf(o).Uint() != reflect.ValueOf(p).Uint()
			case flowgraph.Less:
				b = reflect.ValueOf(o).Uint() < reflect.ValueOf(p).Uint()
			case flowgraph.LessEq:
				b = reflect.ValueOf(o).Uint() <= reflect.ValueOf(p).Uint()
			case flowgraph.Greater:
				b = reflect.ValueOf(o).Uint() > reflect.ValueOf(p).Uint()
			case flowgraph.GreaterEq:
				b = reflect.ValueOf(o).Uint() >= reflect.ValueOf(p).Uint()
			default:
				panic(fmt.Sprintf("bad op: %s", op))
			}
			return Bool(b)
		}
		z := reflect.New(reflect.TypeOf(o)).Elem()
		z.SetUint(x)
		return z.Interface().(Number)
	case Float32, Float64:
		var x float64
		switch op {
		case flowgraph.Minus:
			x = reflect.ValueOf(o).Float() - reflect.ValueOf(p).Float()
		case flowgraph.Plus:
			x = reflect.ValueOf(o).Float() + reflect.ValueOf(p).Float()
		case flowgraph.Times:
			x = reflect.ValueOf(o).Float() * reflect.ValueOf(p).Float()
		case flowgraph.Divide:
			x = reflect.ValueOf(o).Float() / reflect.ValueOf(p).Float()
		default:
			var b bool
			switch op {
			case flowgraph.Eq:
				b = reflect.ValueOf(o).Float() == reflect.ValueOf(p).Float()
			case flowgraph.Neq:
				b = reflect.ValueOf(o).Float() != reflect.ValueOf(p).Float()
			case flowgraph.Less:
				b = reflect.ValueOf(o).Float() < reflect.ValueOf(p).Float()
			case flowgraph.LessEq:
				b = reflect.ValueOf(o).Float() <= reflect.ValueOf(p).Float()
			case flowgraph.Greater:
				b = reflect.ValueOf(o).Float() > reflect.ValueOf(p).Float()
			case flowgraph.GreaterEq:
				b = reflect.ValueOf(o).Float() >= reflect.ValueOf(p).Float()
			default:
				panic(fmt.Sprintf("bad op: %s", op))
			}
			return Bool(b)
		}
		z := reflect.New(reflect.TypeOf(o)).Elem()
		z.SetFloat(x)
		return z.Interface().(Number)
	default:
		panic(fmt.Sprintf("bad number type: %T", o))
	}
}

// Instead, we should probably make bool always be int8 on all archs.
func Bool(b bool) Number {
	if b {
		return Uint8(1)
	}
	return Uint8(0)
}

func convert(obj Val, typ flowgraph.Type) Val {
	switch typ := typ.(type) {
	case *flowgraph.IntType:
		switch {
		case typ.Size == 8 && typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Int8(obj)
			case Float64:
				return Int8(obj)
			case SignedInt:
				return Int8(obj.Int64())
			case UnsignedInt:
				return Int8(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 16 && !typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Int16(obj)
			case Float64:
				return Int16(obj)
			case SignedInt:
				return Int16(obj.Int64())
			case UnsignedInt:
				return Int16(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 32 && !typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Int32(obj)
			case Float64:
				return Int32(obj)
			case SignedInt:
				return Int32(obj.Int64())
			case UnsignedInt:
				return Int32(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 64 && !typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Int64(obj)
			case Float64:
				return Int64(obj)
			case SignedInt:
				return Int64(obj.Int64())
			case UnsignedInt:
				return Int64(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 8 && typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Uint8(obj)
			case Float64:
				return Uint8(obj)
			case SignedInt:
				return Uint8(obj.Int64())
			case UnsignedInt:
				return Uint8(obj.Uint64())
			case Pointer:
				return Uint8(uintptr(unsafe.Pointer(obj.Elem)))
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 16 && typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Uint16(obj)
			case Float64:
				return Uint16(obj)
			case SignedInt:
				return Uint16(obj.Int64())
			case UnsignedInt:
				return Uint16(obj.Uint64())
			case Pointer:
				return Uint16(uintptr(unsafe.Pointer(obj.Elem)))
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 32 && typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Uint32(obj)
			case Float64:
				return Uint32(obj)
			case SignedInt:
				return Uint32(obj.Int64())
			case UnsignedInt:
				return Uint32(obj.Uint64())
			case Pointer:
				return Uint32(uintptr(unsafe.Pointer(obj.Elem)))
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 64 && typ.Unsigned:
			switch obj := obj.(type) {
			case Float32:
				return Uint64(obj)
			case Float64:
				return Uint64(obj)
			case SignedInt:
				return Uint64(obj.Int64())
			case UnsignedInt:
				return Uint64(obj.Uint64())
			case Pointer:
				return Uint64(uintptr(unsafe.Pointer(obj.Elem)))
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		}
	case *flowgraph.FloatType:
		switch {
		case typ.Size == 32:
			switch obj := obj.(type) {
			case Float32:
				return Float32(obj)
			case Float64:
				return Float32(obj)
			case SignedInt:
				return Float32(obj.Int64())
			case UnsignedInt:
				return Float32(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 64:
			switch obj := obj.(type) {
			case Float32:
				return Float64(obj)
			case Float64:
				return Float64(obj)
			case SignedInt:
				return Float64(obj.Int64())
			case UnsignedInt:
				return Float64(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		}
	}
	panic(fmt.Sprintf("impossible convert to %s", typ))
}
