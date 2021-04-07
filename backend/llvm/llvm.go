package llvm

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/eaburns/pea/flowgraph"
	"github.com/eaburns/pea/loc"
)

type Option func(*gen)

// TestMain generates a test binary instead of a normal object file.
var TestMain Option = func(g *gen) { g.test = true }

// LocFiles specifies the source files for location information.
func LocFiles(files loc.Files) Option {
	return func(g *gen) { g.files = files }
}

func Generate(w io.Writer, mod *flowgraph.Mod, opts ...Option) error {
	g := &gen{
		w:        w,
		intBits:  64,
		boolBits: 64,
		panicNum: make(map[*flowgraph.Op]int),
		callNum:  make(map[*flowgraph.Call]int),
	}
	for _, o := range opts {
		o(g)
	}
	return g.generate(w, mod)
}

func (g *gen) generate(w io.Writer, mod *flowgraph.Mod) (err error) {
	defer func() {
		if r := recover(); r != nil {
			if ioErr, ok := r.(ioError); ok {
				err = ioErr.error
			} else {
				panic(r)
			}
		}
	}()
	if g.files != nil {
		g.declarePanicLocStrings(mod)
		if g.test {
			g.declareTestCallLocStrings(mod)
		}
	}
	g.write(
		"%string = type {", g.int(), ", i8*}\n",
		"declare i8* @pea_malloc(", g.int(), ")\n",
		"declare i8* @pea_new_frame()\n",
		"declare void @pea_finish_frame(i8* nocapture)\n",
		"declare void @pea_long_return(i8* nocapture) noreturn\n",
		"declare void @pea_set_test_call_loc(i8*, i32)\n",
		"declare void @pea_panic(%string* nocapture, i8* nocapture, i32) readonly noreturn\n",
		"declare void @pea_print(%string* nocapture) readonly\n",
		"declare void @pea_print_int(i64)\n",
		"declare void @llvm.memcpy.p0i8.p0i8.", g.int(), "(i8*, i8*, ", g.int(), ", i1)\n",
		"declare i32 @setjmp(i8*) returns_twice\n")
	for _, t := range mod.Types {
		unnamed := *t
		unnamed.Mod = ""
		unnamed.Args = nil
		unnamed.Name = ""
		g.write(t, " = type ", &unnamed, "\n")
	}
	for _, s := range mod.Strings {
		g.write("@str", s.Num, " = private unnamed_addr constant [", len(s.Text), " x i8] c", quote(s.Text), "\n")
	}
	for _, v := range mod.Vars {
		g.write("@\"", v.Mod.Path, " ", v.Name, "\" = global ", v.Type, " zeroinitializer\n")
	}
	for _, f := range mod.Funcs {
		if f.Mod == "main" && f.Name == "print_int" {
			// print_int declared above.
			continue
		}
		if len(f.Blocks) == 0 {
			g.write("declare void ", f, "(", f.Parms, ")\n")
			continue
		}
		if f.Test {
			continue
		}
		g.writeFuncDef(f)
	}
	if g.test {
		g.writeTestMain(mod)
		return nil
	}
	var main *flowgraph.FuncDef
	for _, f := range mod.Funcs {
		if f.Mod == "main" && f.Name == "main" && len(f.Parms) == 0 {
			main = f
			break
		}
	}
	g.writeMain(mod, main)
	return nil
}

type gen struct {
	w        io.Writer
	test     bool
	files    loc.Files
	intBits  int
	boolBits int
	panicNum map[*flowgraph.Op]int
	callNum  map[*flowgraph.Call]int
	next     int
}

type ioError struct{ error }

type tmp int

func (g *gen) tmp() tmp {
	g.next++
	return tmp(g.next - 1)
}

type intType int

func (g *gen) int() intType  { return intType(g.intBits) }
func (g *gen) bool() intType { return intType(g.boolBits) }

type typeVal struct{ Value flowgraph.Value }

func (g *gen) writeMain(mod *flowgraph.Mod, main *flowgraph.FuncDef) {
	if main != nil {
		g.write("define i32 @main() {\n")
		if mod.Init != nil {
			g.write("	call void ", mod.Init, "()\n")
		}
		g.write(
			"	call void ", main, "()\n",
			"	ret i32 0\n",
			"}\n")
	}
}

func (g *gen) writeTestMain(mod *flowgraph.Mod) {
	for _, t := range mod.Funcs {
		if !t.Test {
			continue
		}
		name := t.Name + "\x00"
		g.write("@test.", t.Name, " = private unnamed_addr constant [", len(name), " x i8] c", quote(name), "\n")
		g.writeFuncDef(t)
	}
	g.write(
		"@fail_str = private unnamed_addr constant [5 x i8] c\"FAIL\\00\"\n",
		"declare i32 @puts(i8* nocapture)\n",
		"declare i32 @pea_run_test(void()* nocapture, i8* nocapture)\n",
		"define i32 @main() {\n")
	if mod.Init != nil {
		g.write("	call void ", mod.Init, "()\n")
	}
	var fail tmp
	first := true
	for _, t := range mod.Funcs {
		if !t.Test {
			continue
		}
		s := g.tmp()
		n := len(t.Name) + 1
		g.line(s, " = getelementptr [", n, " x i8], [", n, " x i8]* @test.", t.Name, ", i64 0, i64 0")
		r := g.tmp()
		g.line(r, " = call i32 @pea_run_test(void()* ", t, ", i8* ", s, ")")
		if first {
			fail = r
			first = false
		} else {
			failNext := g.tmp()
			g.line(failNext, " = or i32 ", fail, ", ", r)
			fail = failNext
		}
	}
	cond := g.tmp()
	str := g.tmp()
	g.write(
		"	", cond, " = trunc i32 ", fail, " to i1\n",
		"	br i1 ", cond, ", label %fail, label %pass\n",
		"fail:\n",
		"	", str, " = getelementptr [5 x i8], [5 x i8]* @fail_str, i64 0, i64 0\n",
		"	", g.tmp(), " = call i32 @puts(i8* ", str, ")\n",
		"	ret i32 0\n",
		"pass:\n",
		"	ret i32 1\n",
		"}\n")
}

func (g *gen) declarePanicLocStrings(mod *flowgraph.Mod) {
	var panics []*flowgraph.Op
	for _, f := range mod.Funcs {
		for _, b := range f.Blocks {
			for _, r := range b.Instrs {
				p, ok := r.(*flowgraph.Op)
				if !ok || p.Op != flowgraph.Panic {
					continue
				}
				g.panicNum[p] = len(panics)
				panics = append(panics, p)
			}
		}
	}
	for _, p := range panics {
		i := g.panicNum[p]
		l := g.files.Location(p.L)
		n := len(l.Path) + 1
		g.write("@panic_file", i, " = private unnamed_addr constant [", n, " x i8] c", quote(l.Path+"\x00"), "\n")
	}
}

func (g *gen) declareTestCallLocStrings(mod *flowgraph.Mod) {
	var calls []*flowgraph.Call
	for _, f := range mod.Funcs {
		if !f.Test {
			continue
		}
		for _, b := range f.Blocks {
			for _, r := range b.Instrs {
				c, ok := r.(*flowgraph.Call)
				if !ok {
					continue
				}
				g.callNum[c] = len(calls)
				calls = append(calls, c)
			}
		}
	}
	for _, c := range calls {
		i := g.callNum[c]
		l := g.files.Location(c.L)
		n := len(l.Path) + 1
		g.write("@call_file", i, " = private unnamed_addr constant [", n, " x i8] c", quote(l.Path+"\x00"), "\n")
	}
}

func (g *gen) writeFuncDef(f *flowgraph.FuncDef) {
	if strings.HasSuffix(f.Name, "no_inline") {
		g.write("define linkonce_odr void ", f, "(", f.Parms, ") noinline {\n")
	} else {
		g.write("define linkonce_odr void ", f, "(", f.Parms, ") {\n")
	}
	for _, b := range f.Blocks {
		g.write(b.Num, ":\n")
		for _, r := range b.Instrs {
			g.writeInstr(f, r)
		}
	}
	g.write("}\n")
}

func (g *gen) write(vs ...interface{}) {
	for _, v := range vs {
		switch v := v.(type) {
		case int:
			g.writeString(strconv.Itoa(v))
		case string:
			g.writeString(v)
		case flowgraph.Type:
			g.writeType(v)
		case []flowgraph.Type:
			for i, t := range v {
				if i > 0 {
					g.writeString(", ")
				}
				g.writeType(t)
			}
		case flowgraph.Value:
			g.writeValue(v)
		case []flowgraph.Value:
			for i, v := range v {
				if i > 0 {
					g.writeString(", ")
				}
				g.write(v.Type(), " ", v)
			}
		case typeVal:
			g.write(v.Value.Type(), " ", v.Value)
		case tmp:
			g.write("%t", int(v))
		case intType:
			g.write("i", int(v))
		case *flowgraph.FuncDef:
			g.write(`@"`, v.SourceName, `"`)
		case []*flowgraph.ParmDef:
			for i, p := range v {
				if i > 0 {
					g.write(", ")
				}
				g.write(p.Type, " %\"", p.Name, "\"")
			}
		default:
			panic(fmt.Sprintf("unknown print type: %T", v))
		}
	}
}

func (g *gen) writeInstr(f *flowgraph.FuncDef, r flowgraph.Instruction) {
	switch r := r.(type) {
	case *flowgraph.Store:
		// flowgraph considers any pointer equal to struct{}*; LLVM does not.
		// If we are storing a pointer to a non-equal dst type, we add a bitcast.
		st, dt := r.Src.Type(), r.Dst.Type()
		if isAddr(st) && st.String() != elemType(dt).String() {
			tmp := g.tmp()
			g.line(tmp, " = bitcast ", typeVal{r.Src}, " to ", elemType(dt))
			g.line("store ", elemType(dt), " ", tmp, ", ", typeVal{r.Dst})
			break
		}
		g.line("store ", typeVal{r.Src}, ", ", typeVal{r.Dst}, "")
	case *flowgraph.Copy:
		d := g.tmp()
		g.line(d, " = bitcast ", typeVal{r.Dst}, " to i8*")
		s := g.tmp()
		g.line(s, " = bitcast ", typeVal{r.Src}, " to i8*")
		l := g.sizeOf(elemType(r.Dst.Type()))
		g.line("call void @llvm.memcpy.p0i8.p0i8.", g.int(), "(i8* ", d, ", i8* ", s, ", ", g.int(), " ", l, ", i1 0)")
	case *flowgraph.Call:
		if i, ok := g.callNum[r]; ok {
			l := g.files.Location(r.L)
			n := len(l.Path) + 1
			p := g.tmp()
			g.line(p, " = getelementptr [", n, " x i8], [", n, " x i8]* @call_file", i, ", i64 0, i64 0")
			g.line("call void @pea_set_test_call_loc(i8* ", p, ", i32 ", l.Line[0], ")")
		}
		if fun, ok := r.Func.(*flowgraph.Func); ok {
			if fun.Def.Mod == "main" && fun.Def.Name == "print_int" {
				g.line("call void @pea_print_int(", r.Args, ")")
				break
			}
			g.line("call void ", fun.Def, "(", r.Args, ")")
		} else {
			g.line("call void ", r.Func, "(", r.Args, ")")
		}
	case *flowgraph.If:
		cond := g.tmp()
		switch r.Op {
		case flowgraph.Eq:
			if isAddr(r.Value.Type()) {
				if r.X != 0 {
					panic("impossible")
				}
				g.line(cond, " = icmp eq ", typeVal{r.Value}, ", null")
			} else {
				g.line(cond, " = icmp eq ", typeVal{r.Value}, ", ", r.X)
			}
		case flowgraph.Less:
			g.line(cond, " = icmp slt ", typeVal{r.Value}, ", ", r.X)
		default:
			panic(fmt.Sprintf("bad If.Op: %s", r.Op))
		}
		g.line("br i1 ", cond, ", label %", r.Yes.Num, ", label %", r.No.Num, "")
	case *flowgraph.Jump:
		g.line("br label %", r.Dst.Num, "")
	case *flowgraph.Return:
		if r.Frame != nil {
			g.write(
				"	call void @pea_long_return(i8* ", r.Frame, ")\n",
				"	unreachable\n")
			return
		}
		for _, r := range f.Blocks[0].Instrs {
			fr, ok := r.(*flowgraph.Frame)
			if !ok {
				continue
			}
			g.line("call void @pea_finish_frame(i8* ", fr, ")")
			break
		}
		g.line("ret void")
	case *flowgraph.Frame:
		t, c := g.tmp(), g.tmp()
		g.write(
			"	", r, " = call i8* @pea_new_frame()\n",
			"	", t, " = call i32 @setjmp(i8* ", r, ")\n",
			"	", c, " = icmp eq i32 ", t, ", 0\n",
			"	br i1 ", c, ", label %func, label %ret\n",
			"ret:\n",
			"	ret void\n",
			"func:\n")
	case *flowgraph.Alloc:
		switch {
		case !r.Stack:
			l := g.sizeOf(elemType(r.Type()))
			t := g.tmp()
			switch {
			case r.Count != nil:
				x := g.tmp()
				g.line(x, " = mul ", g.int(), " ", l, ", ", r.Count)
				g.line(t, " = call i8* @pea_malloc(", g.int(), " ", x, ")")
			case r.CountImm >= 0:
				x := g.tmp()
				g.line(x, " = mul ", g.int(), " ", l, ", ", r.CountImm)
				g.line(t, " = call i8* @pea_malloc(", g.int(), " ", x, ")")
			default:
				g.line(t, " = call i8* @pea_malloc(", g.int(), " ", l, ")")
			}
			g.line(r, " = bitcast i8* ", t, " to ", r.Type())
		case r.Count != nil:
			panic("impossible")
		case r.CountImm >= 0:
			g.line(r, " = alloca ", elemType(r.Type()), ", i32 ", r.CountImm)
		default:
			g.line(r, " = alloca ", elemType(r.Type()))
		}
	case *flowgraph.Load:
		g.line(r, " = load ", r.Type(), ", ", typeVal{r.Addr})
	case *flowgraph.Func:
		break
	case *flowgraph.String:
		n := len(r.Def.Text)
		g.line(r, " = getelementptr [", n, " x i8], [", n, " x i8]* @str", r.Def.Num, ", i64 0, i64 0")
	case *flowgraph.Var:
		break
	case *flowgraph.Parm:
		break
	case *flowgraph.Field:
		t := r.Base.Type()
		g.line(r, " = getelementptr ", elemType(t), ", ", t, " ", r.Base, ", i32 0, i32 ", r.Def.Num)
	case *flowgraph.Case:
		// TODO: Set Num on union cases to the field number.
		// The idea was that they would all be offset 0, so set all to 0.
		// But this just adds complexity in places that wished they knew the number.
		// It's easy enough to set the Num to the index and just ignore it later.
		n := -1
		for i, c := range r.BaseType.Cases {
			if c == r.Def {
				n = i
				break
			}
		}
		if n < 0 {
			panic("no case")
		}
		g.line(r, " = getelementptr ", elemType(r.Base.Type()), ", ", typeVal{r.Base}, ", i32 0, i32 ", n)
	case *flowgraph.Index:
		g.line(r, " = getelementptr ", elemType(r.Base.Type()), ", ", typeVal{r.Base}, ", ", typeVal{r.Index})
	case *flowgraph.Slice:
		g.line(r, " = getelementptr ", elemType(r.Base.Type()), ", ", typeVal{r.Base}, ", ", typeVal{r.Index})
	case *flowgraph.Int:
		break
	case *flowgraph.Float:
		break
	case *flowgraph.Null:
		break
	case *flowgraph.Op:
		switch r.Op {
		case flowgraph.BitNot:
			g.line(r, " = xor ", typeVal{r.Args[0]})
		case flowgraph.BitXor:
			g.line(r, " = xor ", typeVal{r.Args[0]}, ", ", r.Args[1])
		case flowgraph.BitAnd:
			g.line(r, " = and ", typeVal{r.Args[0]}, ", ", r.Args[1])
		case flowgraph.BitOr:
			g.line(r, " = or ", typeVal{r.Args[0]}, ", ", r.Args[1])
		case flowgraph.LeftShift:
			g.line(r, " = shl ", typeVal{r.Args[0]}, ", ", r.Args[1])
		case flowgraph.RightShift:
			g.line(r, " = lshr ", typeVal{r.Args[0]}, ", ", r.Args[1])
		case flowgraph.Negate:
			if isFloat(r) {
				g.line(r, " = fneg ", typeVal{r.Args[0]})
			} else {
				g.line(r, " = sub ", r.Args[0].Type(), " 0, ", r.Args[0])
			}
		case flowgraph.Minus:
			if isFloat(r) {
				g.line(r, " = fsub ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else {
				g.line(r, " = sub ", typeVal{r.Args[0]}, ", ", r.Args[1])
			}
		case flowgraph.Plus:
			if isFloat(r) {
				g.line(r, " = fadd ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else {
				g.line(r, " = add ", typeVal{r.Args[0]}, ", ", r.Args[1])
			}
		case flowgraph.Times:
			if isFloat(r) {
				g.line(r, " = fmul ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else {
				g.line(r, " = mul ", typeVal{r.Args[0]}, ", ", r.Args[1])
			}
		case flowgraph.Divide:
			if isFloat(r) {
				g.line(r, " = fdiv ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else if isUnsigned(r) {
				g.line(r, " = udiv ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else {
				g.line(r, " = sdiv ", typeVal{r.Args[0]}, ", ", r.Args[1])
			}
		case flowgraph.Modulus:
			if isFloat(r) {
				g.line(r, " = frem ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else if isUnsigned(r) {
				g.line(r, " = urem ", typeVal{r.Args[0]}, ", ", r.Args[1])
			} else {
				g.line(r, " = srem ", typeVal{r.Args[0]}, ", ", r.Args[1])
			}
		case flowgraph.Eq, flowgraph.Neq, flowgraph.Less, flowgraph.LessEq,
			flowgraph.Greater, flowgraph.GreaterEq:
			op := "icmp"
			sign := "s"
			if isUnsigned(r.Args[0]) {
				sign = "u"
			}
			var cond string
			switch r.Op {
			case flowgraph.Eq:
				sign = ""
				cond = "eq"
			case flowgraph.Neq:
				sign = ""
				cond = "ne"
			case flowgraph.Less:
				cond = "lt"
			case flowgraph.LessEq:
				cond = "le"
			case flowgraph.Greater:
				cond = "gt"
			case flowgraph.GreaterEq:
				cond = "ge"
			default:
				panic("impossible")
			}
			if isFloat(r.Args[0]) {
				op = "fcmp"
				sign = "u"
			}
			c := g.tmp()
			g.line(c, " = ", op, " ", sign, cond, " ", typeVal{r.Args[0]}, ", ", r.Args[1])
			g.line(r, " = zext i1 ", c, " to ", g.bool())
		case flowgraph.NumConvert:
			var op string
			switch dstType := r.Type().(type) {
			case *flowgraph.IntType:
				switch srcType := r.Args[0].Type().(type) {
				case *flowgraph.IntType:
					switch {
					case dstType.Size == srcType.Size:
						op = "bitcast"
					case dstType.Size < srcType.Size:
						op = "trunc"
					case !dstType.Unsigned && !srcType.Unsigned:
						op = "sext"
					default:
						op = "zext"
					}
				case *flowgraph.FloatType:
					if dstType.Unsigned {
						op = "fptoui"
					} else {
						op = "fptosi"
					}
				default:
					panic(fmt.Sprintf("bad numconvert src type: %T", srcType))
				}
			case *flowgraph.FloatType:
				switch srcType := r.Args[0].Type().(type) {
				case *flowgraph.IntType:
					if srcType.Unsigned {
						op = "uitofp"
					} else {
						op = "sitofp"
					}
				case *flowgraph.FloatType:
					if dstType.Size < srcType.Size {
						op = "fptrunc"
					} else {
						op = "fpext"
					}
				default:
					panic(fmt.Sprintf("bad numconvert src type: %T", srcType))
				}
			default:
				panic(fmt.Sprintf("bad numconvert dest type: %T", dstType))
			}
			g.line(r, " = ", op, " ", typeVal{r.Args[0]}, " to ", r.Type())
		case flowgraph.Panic:
			if g.files != nil {
				i := g.panicNum[r]
				l := g.files.Location(r.L)
				n := len(l.Path) + 1
				p := g.tmp()
				g.line(p, " = getelementptr [", n, " x i8], [", n, " x i8]* @panic_file", i, ", i64 0, i64 0")
				if f.Test {
					g.line("call void @pea_set_test_call_loc(i8* ", p, ", i32 ", l.Line[0], ")")
				}
				g.line("call void @pea_panic(", typeVal{r.Args[0]}, ", i8* ", p, ", i32 ", l.Line[0], ")")
			} else {
				g.line("call void @pea_panic(", typeVal{r.Args[0]}, ", i8* null, i32 0)")
			}
		case flowgraph.Print:
			g.line("call void @pea_print(", typeVal{r.Args[0]}, ")")
		default:
			panic(fmt.Sprintf("unknown op: %s", r.Op))
		}
	default:
		panic(fmt.Sprintf("unknown instruction type: %T", r))
	}
}

func (g *gen) line(vs ...interface{}) {
	g.writeString("	")
	g.write(vs...)
	g.writeString("\n")
}

func (g *gen) sizeOf(t flowgraph.Type) tmp {
	p := g.tmp()
	g.write("	", p, " = getelementptr ", t, ", ", t, "* null, i32 1\n")
	s := g.tmp()
	g.write("	", s, " = ptrtoint ", t, "* ", p, " to ", g.int(), "\n")
	return s
}

func (g *gen) writeType(t flowgraph.Type) {
	switch t := t.(type) {
	case *flowgraph.IntType:
		g.write("i", t.Size)
	case *flowgraph.FloatType:
		if t.Size == 32 {
			g.writeString("float")
		} else {
			g.writeString("double")
		}
	case *flowgraph.AddrType:
		if isEmpty(elemType(t)) {
			g.writeString("i8*")
		} else {
			g.write(elemType(t), "*")
		}
	case *flowgraph.ArrayType:
		if isEmpty(elemType(t)) {
			g.writeString("i8*")
		} else {
			g.write(elemType(t), "*")
		}
	case *flowgraph.FrameType:
		g.writeString("i8*")
	case *flowgraph.StructType:
		if t.Mod != "" {
			g.write(`%"`, t.Mod, " ", t.Name)
			if len(t.Args) > 0 {
				g.write("<", t.Args, ">")
			}
			g.writeString(`"`)
			return
		}
		if t.Name == "string" {
			g.writeString("%string")
			return
		}
		if len(t.Fields) == 0 {
			g.writeString("void")
			return
		}
		g.writeString("{")
		for i, f := range t.Fields {
			if i > 0 {
				g.writeString(", ")
			}
			g.writeType(f.Type)
		}
		g.writeString("}")
	case *flowgraph.UnionType:
		// TODO: Implement proper unions.
		// A proper union would be a struct
		// that has the most-strictly-aligned
		// union case as a member,
		// followed by a byte array (or nothing)
		// with the number of bytes of
		// the size of the biggest element
		// minus the size of the most-strictly-aligned.
		//
		// However, at this point in codegen
		// we do not know the size or alignment
		// of anything.
		// Best we can do is just emit a struct.
		g.writeString("{")
		for i, c := range t.Cases {
			if i > 0 {
				g.writeString(", ")
			}
			g.writeType(c.Type)
		}
		g.writeString("}")
	case *flowgraph.FuncType:
		if isEmpty(t.Ret) {
			g.write("void (", t.Parms, ")*")
		} else {
			g.write("void (", t.Parms, ", ", t.Ret, "*)*")
		}
	default:
		panic(fmt.Sprintf("unknown Type type: %T", t))
	}
}

func (g *gen) writeValue(v flowgraph.Value) {
	switch v := v.(type) {
	case *flowgraph.Func:
		g.write(v.Def)
	case *flowgraph.Var:
		g.write(`@"`, v.Def.Mod.Path, " ", v.Def.Name, `"`)
	case *flowgraph.Parm:
		g.write(`%"`, v.Def.Name, `"`)
	case *flowgraph.Int:
		g.write(v.Text)
	case *flowgraph.Float:
		g.write(v.Text)
	case *flowgraph.Null:
		g.write("null")
	default:
		g.write("%x", v.Num())
	}
}

func (g *gen) writeString(s string) {
	if _, err := io.WriteString(g.w, s); err != nil {
		panic(ioError{err})
	}
}

func quote(s string) string {
	var out strings.Builder
	out.WriteRune('"')
	for i := 0; i < len(s); i++ {
		c := s[i]
		if ' ' <= c && c <= '~' { // ASCII letter/number/punct
			out.WriteRune(rune(c))
		} else {
			fmt.Fprintf(&out, "\\%02X", c)
		}
	}
	out.WriteRune('"')
	return out.String()
}

func isAddr(t flowgraph.Type) bool {
	switch t.(type) {
	case *flowgraph.AddrType:
		return true
	case *flowgraph.ArrayType:
		return true
	case *flowgraph.FuncType:
		return true
	default:
		return false
	}
}

func isFloat(v flowgraph.Value) bool {
	_, ok := v.Type().(*flowgraph.FloatType)
	return ok
}

func isUnsigned(v flowgraph.Value) bool {
	i, ok := v.Type().(*flowgraph.IntType)
	return ok && i.Unsigned
}

func isEmpty(t flowgraph.Type) bool {
	s, ok := t.(*flowgraph.StructType)
	return ok && len(s.Fields) == 0
}

func elemType(t flowgraph.Type) flowgraph.Type {
	switch t := t.(type) {
	case *flowgraph.ArrayType:
		return t.Elem
	case *flowgraph.AddrType:
		return t.Elem
	default:
		panic(fmt.Sprintf("elem of non-pointer type %s", t))
	}
}
