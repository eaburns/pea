package interp

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/eaburns/pea/flowgraph"
)

type Obj interface {
	String() string
	ShallowCopy() Obj
}

func newObj(typ flowgraph.Type) Obj {
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
			return Uint32(0)
		case 64:
			return Uint64(0)
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
			fields[i].Val = newObj(typ.Fields[i].Type)
		}
		return Struct{Fields: fields}
	case *flowgraph.UnionType:
		return Union{}
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
type Array struct{ Elems *[]Obj }
type Struct struct{ Fields []Field }
type Field struct {
	Name string
	Val  Obj
}
type Union struct{ Case Obj }

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
	return fmt.Sprintf("&%s", *o.Elem)
}

func (o Array) String() string {
	if o.Elems == nil {
		return "[]"
	}
	var s strings.Builder
	s.WriteRune('[')
	for i, e := range *o.Elems {
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
	var s strings.Builder
	s.WriteRune('{')
	s.WriteString(o.Case.String())
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
		s.WriteString(f.Val.String())
	}
	s.WriteRune('}')
	return s.String()
}

func (o Int8) ShallowCopy() Obj    { return o }
func (o Int16) ShallowCopy() Obj   { return o }
func (o Int32) ShallowCopy() Obj   { return o }
func (o Int64) ShallowCopy() Obj   { return o }
func (o Uint8) ShallowCopy() Obj   { return o }
func (o Uint16) ShallowCopy() Obj  { return o }
func (o Uint32) ShallowCopy() Obj  { return o }
func (o Uint64) ShallowCopy() Obj  { return o }
func (o Float32) ShallowCopy() Obj { return o }
func (o Float64) ShallowCopy() Obj { return o }
func (o Func) ShallowCopy() Obj    { return o }
func (o Pointer) ShallowCopy() Obj { return o }
func (o Array) ShallowCopy() Obj   { return o }
func (o Union) ShallowCopy() Obj   { return o }

func (o Struct) ShallowCopy() Obj {
	fields := make([]Field, len(o.Fields))
	for i := range o.Fields {
		fields[i] = o.Fields[i]
	}
	return Struct{Fields: fields}
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
	Obj
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

// TODO: Bool may not always be Int64.
// Instead, we should probably make bool always be int8 on all archs.
func Bool(b bool) Number {
	if b {
		return Int64(1)
	}
	return Int64(0)
}

func convert(obj Obj, typ flowgraph.Type) Obj {
	switch typ := typ.(type) {
	case *flowgraph.IntType:
		switch {
		case typ.Size == 8 && typ.Unsigned == false:
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
		case typ.Size == 16 && typ.Unsigned == false:
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
		case typ.Size == 32 && typ.Unsigned == false:
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
		case typ.Size == 64 && typ.Unsigned == false:
			switch obj := obj.(type) {
			case Float32:
				return Int64(obj)
			case Float64:
				return Int64(obj)
			case SignedInt:
				return Int64(obj.Int64())
			case UnsignedInt:
				return Int32(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 8 && typ.Unsigned == true:
			switch obj := obj.(type) {
			case Float32:
				return Uint8(obj)
			case Float64:
				return Uint8(obj)
			case SignedInt:
				return Uint8(obj.Int64())
			case UnsignedInt:
				return Uint8(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 16 && typ.Unsigned == true:
			switch obj := obj.(type) {
			case Float32:
				return Uint16(obj)
			case Float64:
				return Uint16(obj)
			case SignedInt:
				return Uint16(obj.Int64())
			case UnsignedInt:
				return Uint16(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 32 && typ.Unsigned == true:
			switch obj := obj.(type) {
			case Float32:
				return Uint32(obj)
			case Float64:
				return Uint32(obj)
			case SignedInt:
				return Uint32(obj.Int64())
			case UnsignedInt:
				return Uint32(obj.Uint64())
			default:
				panic(fmt.Sprintf("impossible convert from %T", obj))
			}
		case typ.Size == 64 && typ.Unsigned == true:
			switch obj := obj.(type) {
			case Float32:
				return Uint64(obj)
			case Float64:
				return Uint64(obj)
			case SignedInt:
				return Uint64(obj.Int64())
			case UnsignedInt:
				return Uint64(obj.Uint64())
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
