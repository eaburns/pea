package checker

import (
	"errors"
	"fmt"
	"math/big"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type fail struct {
	msg   string
	loc   loc.Loc
	notes []note
}

type note struct {
	msg string
	loc loc.Loc // empty for built-in
}

func (f *fail) error(files loc.Files) error {
	var s strings.Builder
	s.WriteString(files.Location(f.loc).String())
	s.WriteString(": ")
	s.WriteString(f.msg)
	for _, note := range f.notes {
		s.WriteString("\n\t")
		s.WriteString(note.msg)
		if note.loc != (loc.Loc{}) {
			s.WriteString(" (")
			s.WriteString(files.Location(note.loc).String())
			s.WriteRune(')')
		}
	}
	return errors.New(s.String())
}

func notFound(name string, l loc.Loc) *fail {
	return &fail{
		msg: fmt.Sprintf("%s: not found", name),
		loc: l,
	}
}

func redef(l loc.Loc, name string, prev loc.Loc) *fail {
	return &fail{
		msg:   name + " redefined",
		loc:   l,
		notes: []note{{msg: "previous", loc: prev}},
	}
}

// Check does semantic checking, and returns a *Mod on success.
func Check(modPath string, files []*parser.File, importer Importer) (*Mod, loc.Files, []error) {
	var fails []*fail
	idNames := make(map[string]loc.Loc)
	defs := make(map[parser.Def]Def)
	typeNames := make(map[string]loc.Loc)
	mod := &Mod{Path: modPath}
	if importer == nil {
		importer = newDefaultImporter(files)
	}
	for _, parserFile := range files {
		var imports []*Import
		for _, parserImport := range parserFile.Imports {
			m, err := importer.Load(parserImport.Path)
			if err != nil {
				fails = append(fails, &fail{
					msg: err.Error(),
					loc: parserImport.L,
				})
				continue
			}
			name := filepath.Base(parserImport.Path)
			if parserImport.Name != nil {
				name = parserImport.Name.Name
			}
			if prev, ok := idNames[name]; ok {
				fails = append(fails, redef(parserImport.L, name, prev))
				continue
			}
			idNames[name] = parserImport.L
			imp := &Import{
				Name: name,
				Path: parserImport.Path,
				Exp:  parserImport.Exp,
				L:    parserImport.L,
				Defs: m.Defs,
			}
			imports = append(imports, imp)
		}
		file := &File{
			FilePath: parserFile.Path(),
			Nls:      parserFile.NewLines(),
			Length:   parserFile.Len(),
			Mod:      mod,
			Imports:  imports,
		}
		mod.Files = append(mod.Files, file)
		// Make TypeDefs, but not their Types yet.
		// The Types cannot be made until all TypeDefs are made,
		// because making Types requires looking up the def
		// for each type name.
		for _, parserDef := range parserFile.Defs {
			parserTypeDef, ok := parserDef.(*parser.TypeDef)
			if !ok {
				continue
			}
			name := parserTypeDef.Name.Name
			if prev, ok := typeNames[name]; ok {
				fails = append(fails, redef(parserTypeDef.L, name, prev))
				continue
			}
			typeNames[name] = parserTypeDef.L
			parms, fs := makeTypeParms(importer.Files(), parserTypeDef.TypeParms)
			if len(fs) > 0 {
				fails = append(fails, fs...)
			}
			typeDef := &TypeDef{
				File:  file,
				Alias: parserTypeDef.Alias,
				Mod:   modPath,
				Name:  name,
				Parms: parms,
				Exp:   parserTypeDef.Exp,
				L:     parserTypeDef.L,
			}
			defs[parserDef] = typeDef
			mod.Defs = append(mod.Defs, typeDef)
		}
	}
	// Make TypeDef.Types, but don't instantiate them.
	// They cannot be instantiated until all are made,
	// because instantiation requires the instantiated def
	// to have it's type already built.
	for _, parserFile := range files {
		for _, parserDef := range parserFile.Defs {
			parserTypeDef, ok := parserDef.(*parser.TypeDef)
			if !ok {
				continue
			}
			typeDef := defs[parserTypeDef].(*TypeDef)
			t, fs := _makeType(typeDef, parserTypeDef.Type, false)
			if len(fs) > 0 {
				fails = append(fails, fs...)
			}
			typeDef.Type = t
		}
	}
	// Type instantiation assumes no alias cycles,
	// so we check them here.
	// Alias cycles are an error, but this loop will
	// report and then break any cycles it finds
	// in order to allow checking to continue
	// reporting more errors.
	for _, def := range mod.Defs {
		if typeDef, ok := def.(*TypeDef); ok && typeDef.Alias {
			if fail := checkAliasCycle(typeDef); fail != nil {
				fails = append(fails, fail)
			}
		}
	}
	// At this point, all TypeDefs and their types are made.
	// From here on, we can fully make and instantiate types.
	testNames := make(map[string]loc.Loc)
	for i, parserFile := range files {
		file := mod.Files[i]
		for _, parserDef := range parserFile.Defs {
			switch parserDef := parserDef.(type) {
			case *parser.TypeDef:
				// Now all the types are built, so we can inst them.
				typeDef := defs[parserDef].(*TypeDef)
				typeDef.Type = instType(typeDef.Type)
			case *parser.VarDef:
				name := parserDef.Name.Name
				if prev, ok := idNames[name]; ok {
					fails = append(fails, redef(parserDef.L, name, prev))
					continue
				}
				idNames[name] = parserDef.L
				t, fs := makeType(file, parserDef.Type)
				if len(fs) > 0 {
					fails = append(fails, fs...)
				}
				varDef := &VarDef{
					File:  file,
					Mod:   modPath,
					Name:  name,
					T:     t,
					Const: parserDef.Const,
					Exp:   parserDef.Exp,
					L:     parserDef.L,
				}
				mod.Defs = append(mod.Defs, varDef)
				defs[parserDef] = varDef
			case *parser.FuncDef:
				typeParms := findTypeParms(importer.Files(), parserDef)
				funDef := &FuncDef{
					File:      file,
					Mod:       modPath,
					Name:      parserDef.Name.Name,
					TypeParms: typeParms,
					Exp:       parserDef.Exp,
					L:         parserDef.L,
				}
				var fs []*fail
				if funDef.Parms, fs = makeFuncParms(funDef, parserDef.Parms); len(fs) > 0 {
					fails = append(fails, fs...)
				}
				var parmTypes []Type
				for i := range funDef.Parms {
					parmTypes = append(parmTypes, funDef.Parms[i].T)
				}
				if funDef.Ret, fs = makeType(funDef, parserDef.Ret); len(fs) > 0 {
					fails = append(fails, fs...)
				}
				if funDef.Iface, fs = makeFuncDecls(funDef, parserDef.Iface); len(fs) > 0 {
					fails = append(fails, fs...)
				}
				funDef.T = &FuncType{Parms: parmTypes, Ret: funDef.Ret, L: funDef.L}
				mod.Defs = append(mod.Defs, funDef)
				defs[parserDef] = funDef
			case *parser.TestDef:
				name := parserDef.Name.Name
				if prev, ok := testNames[name]; ok {
					fails = append(fails, redef(parserDef.L, name, prev))
					continue
				}
				testNames[name] = parserDef.L
				testDef := &TestDef{
					File: file,
					Mod:  modPath,
					Name: name,
					L:    parserDef.L,
				}
				mod.Defs = append(mod.Defs, testDef)
				defs[parserDef] = testDef
			default:
				panic(fmt.Sprintf("bad def type: %T", parserDef))
			}
		}
	}
	for _, parserFile := range files {
		var fs []*fail
		for _, parserDef := range parserFile.Defs {
			switch def := defs[parserDef].(type) {
			case *TypeDef:
				break // nothing to do really.
			case *VarDef:
				fs = checkVarDef(def, parserDef.(*parser.VarDef))
			case *FuncDef:
				// TODO
			case *TestDef:
				// TODO
			}
		}
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
	}
	if len(fails) > 0 {
		var errs []error
		for _, fail := range fails {
			errs = append(errs, fail.error(importer.Files()))
		}
		return nil, nil, errs
	}
	return mod, importer.Files(), nil
}

func makeTypeParms(files loc.Files, parserTypeVars []parser.TypeVar) ([]TypeParm, []*fail) {
	var fails []*fail
	seen := make(map[string]loc.Loc)
	var typeParms []TypeParm
	for _, parserTypeVar := range parserTypeVars {
		name := parserTypeVar.Name
		if prev, ok := seen[name]; ok {
			fails = append(fails, redef(parserTypeVar.L, name, prev))
		} else {
			seen[name] = parserTypeVar.L
		}
		typeParms = append(typeParms, TypeParm{
			Name: name,
			L:    parserTypeVar.L,
		})
	}
	return typeParms, fails
}

func makeType(x scope, parserType parser.Type) (typ Type, fails []*fail) {
	return _makeType(x, parserType, true)
}

func _makeType(x scope, parserType parser.Type, inst bool) (typ Type, fails []*fail) {
	switch parserType := parserType.(type) {
	case nil:
		return nil, nil
	case *parser.RefType:
		typ, fails = _makeType(x, parserType.Type, inst)
		typ = &RefType{Type: typ, L: parserType.L}
	case *parser.NamedType:
		var args []Type
		for _, parserArg := range parserType.Args {
			arg, fs := _makeType(x, parserArg, inst)
			if len(fs) > 0 {
				fails = append(fails, fs...)
				continue
			}
			args = append(args, arg)
		}
		if len(fails) > 0 {
			break
		}
		if parserType.Mod != nil {
			modName := parserType.Mod.Name
			modLoc := parserType.Mod.L
			imp := x.findMod(parserType.Mod.Name)
			if imp == nil {
				fails = append(fails, notFound(modName, modLoc))
				return nil, fails
			}
			x = imp
		}
		name := parserType.Name.Name
		if typ = x.findType(args, name, parserType.L); typ == nil {
			fails = append(fails, &fail{
				msg: fmt.Sprintf("undefined: %s", name),
				loc: parserType.L,
			})
		}
		if dt, ok := typ.(*DefType); ok && inst {
			typ = instType(dt)
		}
	case *parser.ArrayType:
		var elemType Type
		elemType, fails = _makeType(x, parserType.ElemType, inst)
		typ = &ArrayType{ElemType: elemType, L: parserType.L}
	case *parser.StructType:
		var fields []FieldDef
		for _, parserField := range parserType.Fields {
			t, fs := _makeType(x, parserField.Type, inst)
			if len(fs) > 0 {
				fails = append(fails, fs...)
			}
			fields = append(fields, FieldDef{
				Name: parserField.Name.Name,
				Type: t,
				L:    parserField.L,
			})
		}
		typ = &StructType{Fields: fields, L: parserType.L}
	case *parser.UnionType:
		var cases []CaseDef
		for _, parserCase := range parserType.Cases {
			t, fs := _makeType(x, parserCase.Type, inst)
			if len(fs) > 0 {
				fails = append(fails, fs...)
			}
			cases = append(cases, CaseDef{
				Name: parserCase.Name.Name,
				Type: t,
				L:    parserCase.L,
			})
		}
		typ = &UnionType{Cases: cases, L: parserType.L}
	case *parser.FuncType:
		parms, fs := _makeTypes(x, parserType.Parms, inst)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		ret, fs := _makeType(x, parserType.Ret, inst)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		typ = &FuncType{Parms: parms, Ret: ret, L: parserType.L}
	case parser.TypeVar:
		name := parserType.Name
		if typ = x.findType(nil, name, parserType.L); typ == nil {
			fails = append(fails, &fail{
				msg: fmt.Sprintf("undefined: %s", name),
				loc: parserType.L,
			})
		}
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", parserType))
	}
	if len(fails) > 0 {
		return nil, fails
	}
	return typ, nil
}

func makeTypes(x scope, parserTypes []parser.Type) ([]Type, []*fail) {
	return _makeTypes(x, parserTypes, true)
}

func _makeTypes(x scope, parserTypes []parser.Type, inst bool) ([]Type, []*fail) {
	var fails []*fail
	var types []Type
	for _, parserType := range parserTypes {
		t, fs := _makeType(x, parserType, inst)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		types = append(types, t)
	}
	return types, fails
}

func checkAliasCycle(root *TypeDef) *fail {
	var path []*TypeDef
	seen := make(map[*TypeDef]bool)
	var check func(*TypeDef) bool
	check = func(typeDef *TypeDef) bool {
		path = append(path, typeDef)
		if seen[typeDef] {
			return false
		}
		seen[typeDef] = true
		defType, ok := typeDef.Type.(*DefType)
		if !ok || !defType.Def.Alias || check(defType.Def) {
			path = path[:len(path)-1]
			return true
		}
		return false
	}
	if !check(root) {
		var notes []note
		for i := 0; i < len(path); i++ {
			if path[i] == path[len(path)-1] {
				path = path[i:]
				break
			}
		}
		for _, def := range path {
			notes = append(notes, note{
				msg: def.Type.String(),
				loc: def.Type.(*DefType).L,
			})
		}
		// Break the alias so that checking can continue reporting more errors.
		root.Alias = false
		return &fail{
			msg:   "alias cycle",
			loc:   root.L,
			notes: notes,
		}
	}
	return nil
}

func instType(typ Type) Type {
	typ = resolveAlias(typ)
	switch typ := typ.(type) {
	case nil:
		break
	case *RefType:
		typ.Type = instType(typ.Type)
	case *DefType:
		if typ.Def == nil {
			return typ
		}
		if typ.Inst = findInst(typ.Def, typ.Args); typ.Inst == nil {
			typ.Inst = newInst(typ.Def, typ.Args)
		}
	case *ArrayType:
		typ.ElemType = instType(typ.ElemType)
	case *StructType:
		for i := range typ.Fields {
			typ.Fields[i].Type = instType(typ.Fields[i].Type)
		}
	case *UnionType:
		for i := range typ.Cases {
			typ.Cases[i].Type = instType(typ.Cases[i].Type)
		}
	case *FuncType:
		for i := range typ.Parms {
			typ.Parms[i] = instType(typ.Parms[i])
		}
		typ.Ret = instType(typ.Ret)
	case *TypeVar:
		break
	case *BasicType:
		break
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
	return typ
}

func resolveAlias(typ Type) Type {
	defType, ok := typ.(*DefType)
	if !ok || !defType.Def.Alias {
		return typ
	}
	aliased := copyTypeWithLoc(defType.Def.Type, defType.L)
	sub := make(map[*TypeParm]Type)
	for i, arg := range defType.Args {
		sub[&defType.Def.Parms[i]] = arg
	}
	return resolveAlias(subType(sub, aliased))
}

func findInst(def *TypeDef, args []Type) *TypeInst {
next:
	for _, inst := range def.Insts {
		for i, a := range inst.Args {
			if !eq(args[i], a) {
				continue next
			}
		}
		return inst
	}
	return nil
}

func newInst(def *TypeDef, args []Type) *TypeInst {
	if def.Alias {
		panic(fmt.Sprintf("should-be resolved alias: %s", def.Name))
	}
	inst := &TypeInst{Args: make([]Type, len(args))}
	sub := make(map[*TypeParm]Type, len(def.Parms))
	for i, arg := range args {
		// We are making one canonical instance.
		// For location-independence,
		// set the canonical instance's arguments
		// to the corresponding parameter locations.
		inst.Args[i] = copyTypeWithLoc(arg, def.Parms[i].L)
		sub[&def.Parms[i]] = inst.Args[i]
	}
	def.Insts = append(def.Insts, inst)
	inst.Type = subType(sub, def.Type)
	return inst
}

func subType(sub map[*TypeParm]Type, typ Type) Type {
	switch typ := typ.(type) {
	case nil:
		return nil
	case *RefType:
		copy := *typ
		copy.Type = subType(sub, typ.Type)
		return &copy
	case *DefType:
		copy := *typ
		copy.Args = nil
		for _, arg := range typ.Args {
			copy.Args = append(copy.Args, subType(sub, arg))
		}
		if typ.Inst != nil {
			return instType(&copy)
		}
		return &copy
	case *ArrayType:
		copy := *typ
		copy.ElemType = subType(sub, typ.ElemType)
		return &copy
	case *StructType:
		var copy StructType
		for i := range typ.Fields {
			f := typ.Fields[i]
			f.Type = subType(sub, f.Type)
			copy.Fields = append(copy.Fields, f)
		}
		return &copy
	case *UnionType:
		var copy UnionType
		for i := range typ.Cases {
			c := typ.Cases[i]
			c.Type = subType(sub, c.Type)
			copy.Cases = append(copy.Cases, c)
		}
		return &copy
	case *FuncType:
		var copy FuncType
		for _, p := range typ.Parms {
			copy.Parms = append(copy.Parms, subType(sub, p))
		}
		copy.Ret = subType(sub, typ.Ret)
		return &copy
	case *TypeVar:
		if s, ok := sub[typ.Def]; ok {
			return s
		}
		copy := *typ
		return &copy
	case *BasicType:
		copy := *typ
		return &copy
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}

func copyTypeWithLoc(typ Type, l loc.Loc) Type {
	switch typ := typ.(type) {
	case nil:
		return nil
	case *RefType:
		return &RefType{
			Type: copyTypeWithLoc(typ.Type, l),
			L:    l,
		}
	case *DefType:
		copy := *typ
		for i := range copy.Args {
			copy.Args[i] = copyTypeWithLoc(copy.Args[i], l)
		}
		copy.L = l
		return &copy
	case *ArrayType:
		return &ArrayType{
			ElemType: copyTypeWithLoc(typ.ElemType, l),
			L:        l,
		}
	case *StructType:
		var copy StructType
		for i := range typ.Fields {
			f := typ.Fields[i]
			f.L = l
			f.Type = copyTypeWithLoc(f.Type, l)
			copy.Fields = append(copy.Fields, f)
		}
		copy.L = l
		return &copy
	case *UnionType:
		var copy UnionType
		for i := range typ.Cases {
			c := typ.Cases[i]
			c.L = l
			c.Type = copyTypeWithLoc(c.Type, l)
			copy.Cases = append(copy.Cases, c)
		}
		copy.L = l
		return &copy
	case *FuncType:
		var copy FuncType
		for _, p := range typ.Parms {
			copy.Parms = append(copy.Parms, copyTypeWithLoc(p, l))
		}
		copy.Ret = copyTypeWithLoc(typ.Ret, l)
		copy.L = l
		return &copy
	case *TypeVar:
		copy := *typ
		copy.L = l
		return &copy
	case *BasicType:
		copy := *typ
		copy.L = l
		return &copy
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", typ))
	}
}

func findTypeParms(files loc.Files, parserFuncDef *parser.FuncDef) []TypeParm {
	typeVars := make(map[string]loc.Loc)
	for _, parserFuncParm := range parserFuncDef.Parms {
		findTypeVars(parserFuncParm.Type, typeVars)
	}
	findTypeVars(parserFuncDef.Ret, typeVars)
	var typeParms []TypeParm
	for name, l := range typeVars {
		typeParms = append(typeParms, TypeParm{
			Name: name,
			L:    l,
		})
	}
	sort.Slice(typeParms, func(i, j int) bool {
		li := typeParms[i].L
		lj := typeParms[j].L
		if li[0] == lj[0] {
			return li[1] < lj[1]
		}
		return li[0] < lj[0]
	})
	return typeParms
}

func findTypeVars(parserType parser.Type, typeVars map[string]loc.Loc) {
	switch parserType := parserType.(type) {
	case nil:
		return
	case *parser.RefType:
		findTypeVars(parserType.Type, typeVars)
	case *parser.NamedType:
		for _, parserArg := range parserType.Args {
			findTypeVars(parserArg, typeVars)
		}
	case *parser.ArrayType:
		findTypeVars(parserType.ElemType, typeVars)
	case *parser.StructType:
		for _, parserField := range parserType.Fields {
			findTypeVars(parserField.Type, typeVars)
		}
	case *parser.UnionType:
		for _, parserCase := range parserType.Cases {
			findTypeVars(parserCase.Type, typeVars)
		}
	case *parser.FuncType:
		for _, parserParm := range parserType.Parms {
			findTypeVars(parserParm, typeVars)
		}
		findTypeVars(parserType.Ret, typeVars)
	case parser.TypeVar:
		if _, ok := typeVars[parserType.Name]; !ok {
			typeVars[parserType.Name] = parserType.L
		}
	default:
		panic(fmt.Sprintf("unsupported Type type: %T %v", parserType, parserType))
	}
}

func makeFuncParms(x scope, parserParms []parser.FuncParm) ([]FuncParm, []*fail) {
	seen := make(map[string]loc.Loc)
	var fails []*fail
	var parms []FuncParm
	for _, parserParm := range parserParms {
		name := parserParm.Name.Name
		if prev, ok := seen[name]; ok {
			fails = append(fails, redef(parserParm.L, name, prev))
		} else {
			seen[name] = parserParm.L
		}
		var t Type
		var fs []*fail
		if parserParm.Type != nil {
			if t, fs = makeType(x, parserParm.Type); len(fs) > 0 {
				fails = append(fails, fs...)
			}
		}
		parms = append(parms, FuncParm{Name: name, T: t, L: parserParm.L})
	}
	return parms, fails
}

func makeFuncDecls(x scope, parserDecls []parser.FuncDecl) ([]FuncDecl, []*fail) {
	var fails []*fail
	var decls []FuncDecl
	for _, parserDecl := range parserDecls {
		parms, fs := makeTypes(x, parserDecl.Parms)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		ret, fs := makeType(x, parserDecl.Ret)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		decls = append(decls, FuncDecl{
			Name:  parserDecl.Name.Name,
			Parms: parms,
			Ret:   ret,
			L:     parserDecl.L,
		})
	}
	return decls, fails
}

func checkVarDef(def *VarDef, parserDef *parser.VarDef) []*fail {
	var fails []*fail
	if parserDef.Expr != nil {
		expr, fs := checkExpr(def, parserDef.Expr, def.T)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		def.Expr = expr
	}
	if def.T == nil && def.Expr != nil {
		def.T = def.Expr.Type()
	}
	if def.T == nil && def.Expr == nil {
		fails = append(fails, &fail{
			msg: "cannot infer variable type",
			loc: def.L,
		})
	}
	return fails
}

func checkExpr(x scope, parserExpr parser.Expr, want Type) (Expr, []*fail) {
	var expr Expr
	var fails []*fail
	switch parserExpr := parserExpr.(type) {
	case *parser.Call:
		// TODO
	case *parser.Convert:
		expr, fails = checkConvert(x, parserExpr)
	case *parser.SubExpr:
		expr, fails = checkExpr(x, parserExpr.Expr, want)
	case *parser.ArrayLit:
		expr, fails = checkArrayLit(x, parserExpr, want)
	case *parser.StructLit:
		expr, fails = checkStructLit(x, parserExpr, want)
	case *parser.UnionLit:
		expr, fails = checkUnionLit(x, parserExpr, want)
	case *parser.BlockLit:
		expr, fails = checkBlockLit(x, parserExpr, want)
	case *parser.StrLit:
		expr, fails = checkStrLit(parserExpr, want)
	case *parser.CharLit:
		expr, fails = checkCharLit(parserExpr, want)
	case *parser.IntLit:
		expr, fails = checkIntLit(parserExpr, want)
	case *parser.FloatLit:
		expr, fails = checkFloatLit(parserExpr, want)
	case *parser.ModSel:
		// TODO
	case parser.Id:
		expr, fails = checkId(x, parserExpr, want)
	default:
		panic(fmt.Sprintf("impossible expr type: %T", parserExpr))
	}
	if expr != nil && want != nil {
		var fs []*fail
		expr, fs = convert(expr, want)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
	}
	return expr, fails
}

// want is the type expected for the last expression, or nil.
func checkExprs(x scope, parserExprs []parser.Expr, want Type) ([]Expr, []*fail) {
	var fails []*fail
	var exprs []Expr
	for i, parserExpr := range parserExprs {
		var fs []*fail
		var expr Expr
		if i == len(parserExprs)-1 {
			expr, fs = checkExpr(x, parserExpr, want)
		} else {
			expr, fs = checkExpr(x, parserExpr, nil)
		}
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		if expr != nil {
			exprs = append(exprs, expr)
		}
	}
	return exprs, fails
}

func convert(expr Expr, typ Type) (Expr, []*fail) {
	var fails []*fail
	dst, dstRefDepth := valueType(typ)
	src, srcRefDepth := valueType(expr.Type())
	if !eq(dst, src) {
		goto mismatch
	}
	if dstRefDepth > srcRefDepth {
		// Consider whether the expression is "addressable".
		if dstRefDepth > srcRefDepth+1 {
			goto mismatch
		}
		deref, ok := expr.(*Deref)
		if !ok {
			fails = append(fails, &fail{
				msg: "expression cannot be referenced",
				loc: expr.Loc(),
			})
			return expr, fails
		}
		expr = deref.Expr
	} else {
		// Automatic dereference.
		for i := 0; i < srcRefDepth-dstRefDepth; i++ {
			expr = deref(expr)
		}
	}
	return expr, fails

mismatch:
	fails = append(fails, &fail{
		msg: fmt.Sprintf("type mismatch: got %s, want %s", expr.Type(), typ),
		loc: expr.Loc(),
	})
	return expr, fails
}

// valueType is the type with all leading references removed.
func valueType(typ Type) (Type, int) {
	var n int
	for {
		if ref, ok := typ.(*RefType); !ok {
			return typ, n
		} else {
			n++
			typ = ref.Type
		}
	}
}

func eq(a, b Type) bool {
	if a == nil || b == nil {
		// nil indicates an error; just be equal to anything.
		return true
	}
	switch a := a.(type) {
	case *DefType:
		if a.Def.Alias {
			panic("impossible")
		}
		b, ok := b.(*DefType)
		if !ok {
			return false
		}
		if b.Def.Alias {
			panic("impossible")
		}
		if len(a.Args) != len(b.Args) || a.Def != b.Def {
			return false
		}
		for i, aArg := range a.Args {
			bArg := b.Args[i]
			if !eq(aArg, bArg) {
				return false
			}
		}
		return true
	case *RefType:
		b, ok := b.(*RefType)
		return ok && eq(a.Type, b.Type)
	case *ArrayType:
		b, ok := b.(*ArrayType)
		return ok && eq(a.ElemType, b.ElemType)
	case *StructType:
		b, ok := b.(*StructType)
		if !ok || len(a.Fields) != len(b.Fields) {
			return false
		}
		for i := range a.Fields {
			aField := &a.Fields[i]
			bField := &b.Fields[i]
			if aField.Name != bField.Name || !eq(aField.Type, bField.Type) {
				return false
			}
		}
		return true
	case *UnionType:
		b, ok := b.(*UnionType)
		if !ok || len(a.Cases) != len(b.Cases) {
			return false
		}
		for i := range a.Cases {
			aCase := &a.Cases[i]
			bCase := &b.Cases[i]
			if aCase.Name != bCase.Name ||
				(aCase.Type == nil) != (bCase.Type == nil) ||
				(aCase.Type != nil && !eq(aCase.Type, bCase.Type)) {
				return false
			}
		}
		return true
	case *FuncType:
		b, ok := b.(*FuncType)
		if !ok || len(a.Parms) != len(b.Parms) || (a.Ret == nil) != (b.Ret == nil) {
			return false
		}
		for i, aParm := range a.Parms {
			bParm := b.Parms[i]
			if !eq(aParm, bParm) {
				return false
			}
		}
		return a.Ret == nil || eq(a.Ret, b.Ret)
	case *BasicType:
		b, ok := b.(*BasicType)
		return ok && a.Kind == b.Kind
	case *TypeVar:
		b, ok := b.(*TypeVar)
		return ok && a.Def == b.Def
	default:
		panic(fmt.Sprintf("impossible Type type: %T", a))
	}
}

func checkId(x scope, parserId parser.Id, want Type) (Expr, []*fail) {
	var ids []id
	for _, id := range x.find(parserId.Name) {
		// TODO: handle grounding for functions.
		if t := id.Type(); t == nil || !isGround(t) || (want != nil && !eq(want, t)) {
			continue
		}
		ids = append(ids, id)
	}
	switch {
	case len(ids) == 0:
		return nil, []*fail{notFound(parserId.Name, parserId.L)}
	case len(ids) > 1 && want == nil:
		return nil, []*fail{{
			msg: fmt.Sprintf("%s is ambiguous", parserId.Name),
			loc: parserId.L,
		}}
	case len(ids) > 1:
		return nil, []*fail{{
			msg: fmt.Sprintf("%s is ambiguous for type %s", parserId.Name, want),
			loc: parserId.L,
		}}
	}
	switch id := ids[0].(type) {
	case *VarDef:
		return &Var{Def: id, T: id.T, L: parserId.L}, nil
	case *FuncParm:
		// TODO: handle capture
		return &Parm{Def: id, T: id.T, L: parserId.L}, nil
	case *FuncLocal:
		// TODO: handle capture
		return &Local{Def: id, T: id.T, L: parserId.L}, nil
	case *BlockCap:
		// TODO: handle capture
		return &Cap{Def: id, T: id.T, L: parserId.L}, nil
	case *FuncDef:
		inst := instFunc(id, id.Type().(*FuncType))
		return wrapCallInBlock(inst, parserId.L), nil
	case Callable:
		return wrapCallInBlock(id, parserId.L), nil
	default:
		panic(fmt.Sprintf("impossible id type: %T", id))
	}
}

func instFunc(def *FuncDef, typ *FuncType) *FuncInst {
	for _, inst := range def.Insts {
		if eq(inst.T, typ) {
			return inst
		}
	}
	inst := &FuncInst{T: typ, Def: def}
	def.Insts = append(def.Insts, inst)
	return inst
}

func wrapCallInBlock(fun Callable, l loc.Loc) *BlockLit {
	typ := fun.Type().(*FuncType)
	blk := &BlockLit{Ret: typ.Ret, T: typ, L: l}
	call := &Call{Fun: fun, T: typ.Ret, L: l}
	call.Args = make([]Expr, len(typ.Parms))
	blk.Exprs = []Expr{call}
	blk.Parms = make([]FuncParm, len(typ.Parms))
	for i := range typ.Parms {
		blk.Parms[i].Name = fmt.Sprintf("x%d", i)
		blk.Parms[i].T = typ.Parms[i]
		blk.Parms[i].L = l
		call.Args[i] = &Parm{Def: &blk.Parms[i], T: typ.Parms[i], L: l}
	}
	return blk
}

func isGround(typ Type) bool {
	switch typ := typ.(type) {
	case *DefType:
		for _, a := range typ.Args {
			if !isGround(a) {
				return false
			}
		}
		return true
	case *RefType:
		return isGround(typ.Type)
	case *ArrayType:
		return isGround(typ.ElemType)
	case *StructType:
		for i := range typ.Fields {
			if !isGround(typ.Fields[i].Type) {
				return false
			}
		}
		return true
	case *UnionType:
		for i := range typ.Cases {
			if !isGround(typ.Cases[i].Type) {
				return false
			}
		}
		return true
	case *FuncType:
		for i := range typ.Parms {
			if !isGround(typ.Parms[i]) {
				return false
			}
		}
		return isGround(typ.Ret)
	case *BasicType:
		return true
	case *TypeVar:
		return false
	default:
		panic(fmt.Sprintf("impossible type type: %T", typ))
	}
}

// checkConvert checks a type conversion.
// 	* The expected type of the subexpression is the conversion type,
// 	  and it is an error if the subexpression type is not convertible
// 	  to the conversion type.
var fails []*fail

func checkConvert(x scope, parserConvert *parser.Convert) (Expr, []*fail) {
	typ, fs := makeType(x, parserConvert.Type)
	if len(fs) > 0 {
		fails = append(fails, fs...)
	}
	expr, fs := checkExpr(x, parserConvert.Expr, typ)
	if len(fs) > 0 {
		fails = append(fails, fs...)
	}
	return expr, fails
}

// checkStructLit checks a struct literal.
// 	* If the expected type is appropriate to the literal,
// 	  then the literal's type is the expected type.
// 	  The expected type of each element is the element type,
// 	  and it is an error if the value's type is not convertible
// 	  to the element type.
// 	* Otherwise, the literal's type is an unnamed array type.
// 	  It is an error if the literal has not element expressions.
// 	  The array element type is the type of the element at index 0
// 	  with no expected type.
// 	  The expected type of elements at indices greater than 0
// 	  is the array element type.
//
// A type is apropriate to an array literal if
// 	* its literal type is an array type or a reference to an array type.
func checkArrayLit(x scope, parserLit *parser.ArrayLit, want Type) (Expr, []*fail) {
	var fails []*fail
	lit := &ArrayLit{L: parserLit.L}
	if lit.Array, _ = trim1Ref(literal(want)).(*ArrayType); lit.Array != nil {
		for _, parserExpr := range parserLit.Exprs {
			expr, fs := checkExpr(x, parserExpr, lit.Array.ElemType)
			if len(fs) > 0 {
				fails = append(fails, fs...)
			}
			lit.Elems = append(lit.Elems, expr)
		}
		if !isRef(want) {
			lit.T = ref(copyTypeWithLoc(want, lit.L))
			return deref(lit), fails
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, fails
	}

	var elemType Type
	for _, parserExpr := range parserLit.Exprs {
		expr, fs := checkExpr(x, parserExpr, elemType)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		lit.Elems = append(lit.Elems, expr)
		if elemType == nil {
			elemType = expr.Type()
		}
	}
	if elemType == nil {
		fails = append(fails, &fail{
			msg: "unable to infer array type",
			loc: lit.L,
		})
		return lit, fails
	}
	lit.Array = &ArrayType{ElemType: elemType, L: lit.L}
	lit.T = ref(lit.Array)
	return deref(lit), fails
}

// checkStructLit checks a struct literal.
// 	* If the expected type is appropriate to the literal,
// 	  then the literal's type is the expected type.
// 	  The expected type of each literal field value
// 	  is the type of the corresponding field,
// 	  and it is an error if the value's type is not convertible
// 	  to the field value.
// 	* Otherwise, the literal's type is an unnamed struct type
// 	  with a field corresponding to each of the literal's fields.
// 	  The type of each field is the type of its corresponding value
// 	  with no expected type.
//
// A type is apropriate to a struct literal if
// 	* its literal type is a struct type or a reference to a struct type,
// 	* it has the same number of fields as the literal,
// 	* each of the fields, in order, has the same name as the corresponding literal field.
func checkStructLit(x scope, parserLit *parser.StructLit, want Type) (Expr, []*fail) {
	var fails []*fail
	lit := &StructLit{L: parserLit.L}

	if lit.Struct = appropriateStruct(want, parserLit); lit.Struct != nil {
		for i, parserField := range parserLit.FieldVals {
			expr, fs := checkExpr(x, parserField.Val, lit.Struct.Fields[i].Type)
			if len(fs) > 0 {
				fails = append(fails, fs...)
			}
			lit.Fields = append(lit.Fields, expr)
		}
		if !isRef(want) {
			lit.T = ref(copyTypeWithLoc(want, lit.L))
			return deref(lit), fails
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, fails
	}

	lit.Struct = &StructType{L: lit.L}
	for _, parserField := range parserLit.FieldVals {
		expr, fs := checkExpr(x, parserField.Val, nil)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		lit.Fields = append(lit.Fields, expr)
		lit.Struct.Fields = append(lit.Struct.Fields, FieldDef{
			Name: parserField.Name.Name,
			Type: expr.Type(),
			L:    parserField.L,
		})
	}
	lit.T = ref(lit.Struct)
	return deref(lit), fails
}

func appropriateStruct(typ Type, lit *parser.StructLit) *StructType {
	s, ok := trim1Ref(literal(typ)).(*StructType)
	if !ok || len(s.Fields) != len(lit.FieldVals) {
		return nil
	}
	for i := range s.Fields {
		if s.Fields[i].Name != lit.FieldVals[i].Name.Name {
			return nil
		}
	}
	return s
}

// checkUnionLit checks a union literal.
// 	* If the expected type is appropriate to the literal,
// 	  then the literal's type is the expected type.
// 	  If the literal has a value,
// 	  it's expected type is the corresponding case type,
// 	  and it is an error if the value is not convertible
// 	  to the case type.
// 	* Otherwise, the literal's type is an unnamed union type
// 	  with a single case of the name of the literal case.
// 	  If the literal has a value, the type of the case
// 	  is the type of the value with no expected type.
//
// A type is apropriate to a union literal if
// 	* its literal type is a union type or a reference to a union type,
// 	* it has a case with the same name as the literal case,
// 	* if the literal has a value, the corresponding case has a type,
// 	* or if the literal has no value, the corresponding case has no type.
func checkUnionLit(x scope, parserLit *parser.UnionLit, want Type) (Expr, []*fail) {
	var fails []*fail
	lit := &UnionLit{L: parserLit.L}
	if lit.Union, lit.Case = appropriateUnion(want, parserLit); lit.Union != nil {
		if parserLit.CaseVal.Val != nil {
			lit.Val, fails = checkExpr(x, parserLit.CaseVal.Val, lit.Case.Type)
		}
		if !isRef(want) {
			lit.T = ref(copyTypeWithLoc(want, lit.L))
			return deref(lit), fails
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, fails
	}

	lit.Union = &UnionType{
		Cases: []CaseDef{{
			Name: parserLit.CaseVal.Name.Name,
			L:    parserLit.CaseVal.L,
		}},
		L: parserLit.L,
	}
	if parserLit.CaseVal.Val != nil {
		lit.Val, fails = checkExpr(x, parserLit.CaseVal.Val, nil)
	}
	lit.Case = &lit.Union.Cases[0]
	if lit.Val != nil {
		lit.Case.Type = lit.Val.Type()
	}
	lit.T = ref(lit.Union)
	return deref(lit), fails
}

func appropriateUnion(typ Type, lit *parser.UnionLit) (*UnionType, *CaseDef) {
	u, ok := trim1Ref(literal(typ)).(*UnionType)
	if !ok {
		return nil, nil
	}
	c := findCase(lit.CaseVal.Name.Name, u)
	if c == nil || (c.Type == nil) != (lit.CaseVal.Val == nil) {
		return nil, nil
	}
	return u, c
}

func findCase(name string, u *UnionType) *CaseDef {
	for i := range u.Cases {
		if u.Cases[i].Name == name {
			return &u.Cases[i]
		}
	}
	return nil
}

// checkBlockLit checks a block literal.
// 	* If the expected type is appropriate to the literal,
// 	  then the literal's type is the expected type.
// 	  If the type has a return type,
// 	  it is an error if there are no expressions in the block.
// 	  The expected type of the last expression is the return type,
// 	  and it is an error if the type of the last expression
// 	  is not convertible to the return type.
// 	* Otherwise, the literal's type is an unnamed function type
// 	  with parameters corresponding to the explicity type
// 	  of each of the literal's parameters.
// 	  It is an error if any of the parameter's type is elided.
// 	  If there are no expressions in the block, the type has no return type.
// 	  Otherwise the return type is the type of the last expression
// 	  in the block with no expected type.
//
// A type is apropriate to a block literal if
// 	* its literal type is a function type or a reference to a function type,
// 	* it has the same number of parameters as the literal, and
// 	* all explicit parameter types of the literal
// 	  equal the corresponding parameter type of the function type.
func checkBlockLit(x scope, parserLit *parser.BlockLit, want Type) (Expr, []*fail) {
	var fails []*fail
	lit := &BlockLit{L: parserLit.L}
	lit.Parms, fails = makeFuncParms(x, parserLit.Parms)
	x = &blockLitScope{parent: x, BlockLit: lit}

	if f := appropriateBlock(want, lit.Parms); f != nil {
		if f.Ret != nil && len(parserLit.Exprs) == 0 {
			fails = append(fails, &fail{
				msg: fmt.Sprintf("want return type %s: no experssions", f.Ret),
				loc: lit.L,
			})
		}
		for i := range lit.Parms {
			if lit.Parms[i].T != nil {
				continue
			}
			lit.Parms[i].T = copyTypeWithLoc(f.Parms[i], lit.Parms[i].L)
		}
		var fs []*fail
		lit.Exprs, fs = checkExprs(x, parserLit.Exprs, f.Ret)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		if !isRef(want) {
			lit.T = ref(copyTypeWithLoc(want, lit.L))
			return deref(lit), fails
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, fails
	}

	// Remove parameters with elided types and report an error.
	var n int
	parmTypes := make([]Type, 0, len(lit.Parms))
	for _, p := range lit.Parms {
		if p.T == nil {
			fails = append(fails, &fail{
				msg: fmt.Sprintf("cannot infer type of parameter %s", p.Name),
				loc: p.L,
			})
			continue
		}
		parmTypes = append(parmTypes, p.T)
		lit.Parms[n] = p
		n++
	}
	lit.Parms = lit.Parms[:n]

	var fs []*fail
	lit.Exprs, fs = checkExprs(x, parserLit.Exprs, nil)
	if len(fs) > 0 {
		fails = append(fails, fs...)
	}

	var retType Type
	if len(lit.Exprs) > 0 {
		retType = lit.Exprs[len(lit.Exprs)-1].Type()
	}
	lit.T = ref(&FuncType{Parms: parmTypes, Ret: retType, L: lit.L})
	return deref(lit), fails
}

func appropriateBlock(typ Type, litParms []FuncParm) *FuncType {
	f, ok := trim1Ref(literal(typ)).(*FuncType)
	if !ok || len(f.Parms) != len(litParms) {
		return nil
	}
	for i := range f.Parms {
		if t := litParms[i].T; t != nil && !eq(f.Parms[i], t) {
			return nil
		}
	}
	return f
}

// checkStrLit checks a string literal.
// 	* If the expected type's literal types is the built-in string type
// 	  or a reference to the built-in string type,
// 	  then the type of the literal is the expected type.
// 	* Otherwise the type is string.
func checkStrLit(parserLit *parser.StrLit, want Type) (Expr, []*fail) {
	lit := &StrLit{Text: parserLit.Data, L: parserLit.L}
	switch b, ok := trim1Ref(literal(want)).(*BasicType); {
	case want == nil:
		fallthrough
	case !ok:
		fallthrough
	default:
		lit.T = ref(&BasicType{Kind: String, L: parserLit.L})
		return deref(lit), nil
	case b.Kind == String:
		if !isRef(want) {
			lit.T = ref(copyTypeWithLoc(want, lit.L))
			return deref(lit), nil
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, nil
	}
}

// checkCharLit checks a character literal.
// 	* Characeter literals are checked just as int literals
// 	  with the literal value being the unicode code point value
// 	  of the character.
// TODO: should default to int32, not int.
func checkCharLit(parserLit *parser.CharLit, want Type) (Expr, []*fail) {
	return checkIntLit(&parser.IntLit{
		Text: strconv.FormatInt(int64(parserLit.Rune), 10),
		L:    parserLit.L,
	}, want)
}

// checkIntLit checks an integer literal.
// 	* If the expected type's literal type is a built-in int type
// 	  or a reference to a built-in int type,
// 	  then the type of the literal is the expected type.
// 	  It is an error if the value is not representable by the int type.
// 	* If the expected type's literal type is a built-in float type,
// 	  or a reference to a built-in float type,
// 	  then the type of the literal is the expected type.
// 	* Otherwise the type of the literal is int.
func checkIntLit(parserLit *parser.IntLit, want Type) (Expr, []*fail) {
	lit := &IntLit{Text: parserLit.Text, L: parserLit.L}
	if _, ok := lit.Val.SetString(parserLit.Text, 0); !ok {
		panic("malformed int")
	}
	var bits uint
	var signed bool
	switch b, ok := trim1Ref(literal(want)).(*BasicType); {
	case want == nil:
		fallthrough
	case !ok:
		fallthrough
	default:
		want = &BasicType{Kind: Int, L: parserLit.L}
		fallthrough
	case b.Kind == Int:
		bits = 64 // TODO: set by a flag
		signed = true
	case b.Kind == Int8:
		bits = 8
		signed = true
	case b.Kind == Int16:
		bits = 16
		signed = true
	case b.Kind == Int32:
		bits = 32
		signed = true
	case b.Kind == Int64:
		bits = 64
		signed = true
	case b.Kind == Uint:
		bits = 64 // TODO: set by a flag
		signed = false
	case b.Kind == Uint8:
		bits = 8
		signed = false
	case b.Kind == Uint16:
		bits = 16
		signed = false
	case b.Kind == Uint32:
		bits = 32
		signed = false
	case b.Kind == Uint64:
		bits = 64
		signed = false
	case b.Kind == Float32 || b.Kind == Float64:
		return checkFloatLit(&parser.FloatLit{Text: parserLit.Text, L: lit.L}, want)
	}
	var min, max *big.Int
	if signed {
		bits--
		min = big.NewInt(1)
		min = min.Lsh(min, bits)
		min = min.Neg(min)
		max = big.NewInt(1)
		max = max.Lsh(max, bits)
		max = max.Sub(max, big.NewInt(1))
	} else {
		min = big.NewInt(0)
		max = big.NewInt(1)
		max = max.Lsh(max, bits)
		max = max.Sub(max, big.NewInt(1))
	}
	var fails []*fail
	switch {
	case lit.Val.Cmp(min) < 0:
		fails = append(fails, &fail{
			msg: fmt.Sprintf("%s underflows type %s", lit.Text, want),
			loc: lit.L,
		})
	case lit.Val.Cmp(max) > 0:
		fails = append(fails, &fail{
			msg: fmt.Sprintf("%s overflows type %s", lit.Text, want),
			loc: lit.L,
		})
	}
	if !isRef(want) {
		lit.T = ref(copyTypeWithLoc(want, lit.L))
		return deref(lit), fails
	}
	lit.T = copyTypeWithLoc(want, lit.L)
	return lit, fails
}

// checkFloatLit checks a float literal.
// 	* If the expected type's literal type is a built-in float type
// 	  or a reference to a built-in float type,
// 	  then the type of the expression is the expected type.
// 	* If the expected type's literal type is a built-in int type
// 	  or a reference to a built-in int type,
// 	  then the type of the experssion is the expected type.
// 	  It is an error if the literal value is not a whole integer value
// 	  representable by the int typ.
// 	* Otherwise, the type is float64.
func checkFloatLit(parserLit *parser.FloatLit, want Type) (Expr, []*fail) {
	lit := &FloatLit{Text: parserLit.Text, L: parserLit.L}
	if _, _, err := lit.Val.Parse(parserLit.Text, 10); err != nil {
		panic("malformed float")
	}
	switch b, ok := trim1Ref(literal(want)).(*BasicType); {
	case want == nil:
		fallthrough
	case !ok:
		fallthrough
	default:
		lit.T = ref(&BasicType{Kind: Float64, L: parserLit.L})
		return deref(lit), nil
	case b.Kind == Int ||
		b.Kind == Int8 ||
		b.Kind == Int16 ||
		b.Kind == Int32 ||
		b.Kind == Int64 ||
		b.Kind == Uint ||
		b.Kind == Uint8 ||
		b.Kind == Uint16 ||
		b.Kind == Uint32 ||
		b.Kind == Uint64:
		var i big.Int
		var fails []*fail
		if _, acc := lit.Val.Int(&i); acc != big.Exact {
			fails = append(fails, &fail{
				msg: fmt.Sprintf("%s truncates %s", want, lit.Text),
				loc: lit.L,
			})
		}
		intLit, fs := checkIntLit(&parser.IntLit{Text: i.String(), L: parserLit.L}, want)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		return intLit, fails
	case b.Kind == Float32 || b.Kind == Float64:
		if !isRef(want) {
			lit.T = ref(copyTypeWithLoc(want, lit.L))
			return deref(lit), nil
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, nil
	}
}

func deref(expr Expr) Expr {
	var t Type
	switch ref := expr.Type().(type) {
	case *RefType:
		t = ref.Type
	case *DefType:
		if ref.Inst == nil || ref.Inst.Type == nil {
			return expr
		}
		t = ref.Inst.Type.(*RefType).Type
	default:
		panic(fmt.Sprintf("impossible type: %s", expr.Type()))
	}
	return &Deref{Expr: expr, T: t, L: expr.Loc()}
}

func ref(typ Type) Type {
	return &RefType{Type: typ, L: typ.Loc()}
}

func isRef(typ Type) bool {
	switch typ := typ.(type) {
	case nil:
		return false
	case *RefType:
		return true
	case *DefType:
		return typ.Inst != nil && isRef(typ.Inst.Type)
	default:
		return false
	}
}

func trim1Ref(typ Type) Type {
	if typ == nil {
		return nil
	}
	if ref, ok := typ.(*RefType); ok {
		return ref.Type
	}
	return typ
}

func literal(typ Type) Type {
	if typ == nil {
		return nil
	}
	switch typ := typ.(type) {
	case *RefType:
		if typ.Type == nil {
			return nil
		}
		return &RefType{Type: literal(typ.Type), L: typ.L}
	case *DefType:
		if typ.Inst == nil || typ.Inst.Type == nil {
			return nil
		}
		return literal(typ.Inst.Type)
	default:
		return typ
	}
}
