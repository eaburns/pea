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
					Type:  t,
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

				if funDef.Ret, fs = makeType(funDef, parserDef.Ret); len(fs) > 0 {
					fails = append(fails, fs...)
				}
				if funDef.Iface, fs = makeFuncDecls(funDef, parserDef.Iface); len(fs) > 0 {
					fails = append(fails, fs...)
				}
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
		if nt, ok := typ.(*NamedType); ok && inst {
			typ = instType(nt)
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
		namedType, ok := typeDef.Type.(*NamedType)
		if !ok || !namedType.Def.Alias || check(namedType.Def) {
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
				loc: def.Type.(*NamedType).L,
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
	case *NamedType:
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
	namedType, ok := typ.(*NamedType)
	if !ok || !namedType.Def.Alias {
		return typ
	}
	aliased := copyTypeWithLoc(namedType.Def.Type, namedType.L)
	sub := make(map[*TypeParm]Type)
	for i, arg := range namedType.Args {
		sub[&namedType.Def.Parms[i]] = arg
	}
	return resolveAlias(subType(sub, aliased))
}

func findInst(def *TypeDef, args []Type) *TypeInst {
next:
	for _, inst := range def.Insts {
		for i, a := range inst.Args {
			if !args[i].eq(a) {
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
	case *NamedType:
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
	case *NamedType:
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
		t, fs := makeType(x, parserParm.Type)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		parms = append(parms, FuncParm{
			Name: name,
			Type: t,
			L:    parserParm.L,
		})
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
		_, expr, fs := checkExpr(def, parserDef.Expr, def.Type)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		def.Expr = expr
	}
	if def.Type == nil && def.Expr != nil {
		def.Type = def.Expr.Type()
	}
	if def.Type == nil && def.Expr == nil {
		fails = append(fails, &fail{
			msg: "cannot infer variable type",
			loc: def.L,
		})
	}
	return fails
}

func checkExpr(x scope, parserExpr parser.Expr, sug Type) (scope, Expr, []*fail) {
	var expr Expr
	var fails []*fail
	switch parserExpr := parserExpr.(type) {
	case *parser.Call:
		// TODO
	case *parser.Convert:
		// TODO
	case *parser.SubExpr:
		x, expr, fails = checkExpr(x, parserExpr.Expr, sug)
	case *parser.ModSel:
		// TODO
	case *parser.ArrayLit:
		// TODO
	case *parser.StructLit:
		x, expr, fails = checkStructLit(x, parserExpr, sug)
	case *parser.UnionLit:
		x, expr, fails = checkUnionLit(x, parserExpr, sug)
	case *parser.BlockLit:
		expr, fails = checkBlockLit(x, parserExpr, sug)
	case *parser.StrLit:
		expr, fails = checkStrLit(parserExpr, sug)
	case *parser.CharLit:
		expr, fails = checkCharLit(parserExpr, sug)
	case *parser.IntLit:
		expr, fails = checkIntLit(parserExpr, sug)
	case *parser.FloatLit:
		expr, fails = checkFloatLit(parserExpr, sug)
	case parser.Id:
		// TODO
	default:
		panic(fmt.Sprintf("impossible expr type: %T", parserExpr))
	}
	return x, expr, fails
}

// sug is the type suggested for the last expression, or nil.
func checkExprs(x scope, parserExprs []parser.Expr, sug Type) (scope, []Expr, []*fail) {
	var fails []*fail
	var exprs []Expr
	for i, parserExpr := range parserExprs {
		var fs []*fail
		var expr Expr
		if i == len(parserExprs)-1 {
			x, expr, fs = checkExpr(x, parserExpr, sug)
		} else {
			x, expr, fs = checkExpr(x, parserExpr, nil)
		}
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		if expr != nil {
			exprs = append(exprs, expr)
		}
	}
	return x, exprs, fails
}

func checkStructLit(x scope, parserStructLit *parser.StructLit, sug Type) (scope, Expr, []*fail) {
	var fails []*fail
	structLit := &StructLit{L: parserStructLit.L}

	var sugFieldTypes []Type
	sugStructType, _ := trim1Ref(literal(sug)).(*StructType)
	if sugStructType != nil &&
		len(sugStructType.Fields) == len(parserStructLit.FieldVals) {
		for i := range sugStructType.Fields {
			typeField := sugStructType.Fields[i]
			litFieldName := parserStructLit.FieldVals[i].Name.Name
			if typeField.Name != litFieldName {
				sugFieldTypes = nil
				break
			}
			sugFieldTypes = append(sugFieldTypes, typeField.Type)
		}
	}
	for i, parserFieldVal := range parserStructLit.FieldVals {
		var sugFieldType Type
		if sugFieldTypes != nil {
			sugFieldType = sugFieldTypes[i]
		}
		var fs []*fail
		var expr Expr
		x, expr, fs = checkExpr(x, parserFieldVal.Val, sugFieldType)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		if expr != nil {
			structLit.Fields = append(structLit.Fields, expr)
		}
	}

	sugOK := false
	if sugFieldTypes != nil && len(structLit.Fields) == len(sugFieldTypes) {
		sugOK = true
		for i, sugFieldType := range sugFieldTypes {
			if !sugFieldType.eq(structLit.Fields[i].Type()) {
				sugOK = false
				break
			}
		}
	}
	if !sugOK {
		structType := &StructType{L: parserStructLit.L}
		for i, field := range structLit.Fields {
			name := parserStructLit.FieldVals[i].Name.Name
			structType.Fields = append(structType.Fields, FieldDef{
				Name: name,
				Type: field.Type(),
				L:    parserStructLit.FieldVals[i].L,
			})
		}
		structLit.Struct = structType
		structLit.T = ref(structType)
		return x, deref(structLit), fails
	}

	structLit.Struct = sugStructType
	if !isRef(sug) {
		structLit.T = ref(copyTypeWithLoc(sug, structLit.L))
		return x, deref(structLit), fails
	}
	structLit.T = copyTypeWithLoc(sug, structLit.L)
	return x, structLit, fails
}

func checkUnionLit(x scope, parserUnionLit *parser.UnionLit, sug Type) (scope, Expr, []*fail) {
	var fails []*fail
	unionLit := &UnionLit{L: parserUnionLit.L}

	var sugCase *CaseDef
	sugUnionType, _ := trim1Ref(literal(sug)).(*UnionType)
	if sugUnionType != nil {
		caseName := parserUnionLit.CaseVal.Name.Name
		sugCase = findCase(caseName, sugUnionType)
	}

	if parserVal := parserUnionLit.CaseVal.Val; parserVal != nil {
		var sugValType Type
		if sugCase != nil {
			sugValType = sugCase.Type
		}
		x, unionLit.Val, fails = checkExpr(x, parserVal, sugValType)
	}

	if sugCase == nil ||
		(sugCase.Type == nil) != (unionLit.Val == nil) ||
		sugCase.Type != nil && !sugCase.Type.eq(unionLit.Val.Type()) {
		unionLit.Union = &UnionType{
			Cases: []CaseDef{{
				Name: parserUnionLit.CaseVal.Name.Name,
				L:    parserUnionLit.CaseVal.L,
			}},
			L: parserUnionLit.L,
		}
		unionLit.Case = &unionLit.Union.Cases[0]
		if unionLit.Val != nil {
			unionLit.Case.Type = unionLit.Val.Type()
		}
		unionLit.T = ref(unionLit.Union)
		return x, deref(unionLit), fails
	}

	unionLit.Union = sugUnionType
	unionLit.Case = sugCase
	if !isRef(sug) {
		unionLit.T = ref(copyTypeWithLoc(sug, unionLit.L))
		return x, deref(unionLit), fails
	}
	unionLit.T = copyTypeWithLoc(sug, unionLit.L)
	return x, unionLit, fails
}

func findCase(name string, u *UnionType) *CaseDef {
	for i := range u.Cases {
		if u.Cases[i].Name == name {
			return &u.Cases[i]
		}
	}
	return nil
}

func checkBlockLit(x scope, parserBlockLit *parser.BlockLit, sug Type) (Expr, []*fail) {
	var fails []*fail
	parms, fs := makeFuncParms(x, parserBlockLit.Parms)
	if len(fs) > 0 {
		fails = append(fails, fs...)
	}
	parmTypes := make([]Type, len(parms))
	for i, parm := range parms {
		parmTypes[i] = parm.Type
	}

	blockLit := &BlockLit{
		Parms: parms,
		L:     parserBlockLit.L,
	}
	x = &blockLitScope{parent: x, BlockLit: blockLit}

	var retType Type
	if b, ok := trim1Ref(literal(sug)).(*FuncType); ok {
		retType = b.Ret
	}
	_, blockLit.Exprs, fs = checkExprs(x, parserBlockLit.Exprs, retType)
	if len(fs) > 0 {
		fails = append(fails, fs...)
	}

	if n := len(blockLit.Exprs); n == 0 {
		retType = nil
	} else {
		retType = blockLit.Exprs[n-1].Type()
	}
	blockLit.T = &FuncType{
		Parms: parmTypes,
		Ret:   retType,
		L:     parserBlockLit.L,
	}

	if base := trim1Ref(literal(sug)); base == nil || !blockLit.T.eq(base) {
		return blockLit, fails
	}
	if !isRef(sug) {
		blockLit.T = ref(copyTypeWithLoc(sug, blockLit.L))
		return deref(blockLit), fails
	}
	blockLit.T = copyTypeWithLoc(sug, blockLit.L)
	return blockLit, fails
}

func checkStrLit(parserStrLit *parser.StrLit, sug Type) (Expr, []*fail) {
	strLit := &StrLit{Text: parserStrLit.Data, L: parserStrLit.L}
	switch b, ok := trim1Ref(literal(sug)).(*BasicType); {
	case sug == nil:
		fallthrough
	case !ok:
		fallthrough
	default:
		strLit.T = ref(&BasicType{Kind: String, L: parserStrLit.L})
		return deref(strLit), nil
	case b.Kind == String:
		if !isRef(sug) {
			strLit.T = ref(copyTypeWithLoc(sug, parserStrLit.L))
			return deref(strLit), nil
		}
		strLit.T = copyTypeWithLoc(sug, parserStrLit.L)
		return strLit, nil
	}
}

func checkCharLit(parserCharLit *parser.CharLit, sug Type) (Expr, []*fail) {
	parserIntLit := &parser.IntLit{
		Text: strconv.FormatInt(int64(parserCharLit.Rune), 10),
		L:    parserCharLit.L,
	}
	return checkIntLit(parserIntLit, sug)
}

func checkIntLit(parserIntLit *parser.IntLit, sug Type) (Expr, []*fail) {
	intLit := &IntLit{Text: parserIntLit.Text, L: parserIntLit.L}
	if _, ok := intLit.Val.SetString(parserIntLit.Text, 0); !ok {
		panic("malformed int")
	}
	var bits uint
	var signed bool
	switch b, ok := trim1Ref(literal(sug)).(*BasicType); {
	case sug == nil:
		fallthrough
	case !ok:
		fallthrough
	default:
		intLit.T = ref(&BasicType{Kind: Int, L: parserIntLit.L})
		return deref(intLit), nil
	case b.Kind == Float32 || b.Kind == Float64:
		parserFloatLit := &parser.FloatLit{
			Text: parserIntLit.Text,
			L:    parserIntLit.L,
		}
		return checkFloatLit(parserFloatLit, sug)
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
	switch {
	case intLit.Val.Cmp(min) < 0:
		return nil, []*fail{{
			msg: fmt.Sprintf("%s underflows type %s", intLit.Text, sug),
			loc: intLit.L,
		}}
	case intLit.Val.Cmp(max) > 0:
		return nil, []*fail{{
			msg: fmt.Sprintf("%s overflows type %s", intLit.Text, sug),
			loc: intLit.L,
		}}
	default:
		if !isRef(sug) {
			intLit.T = ref(copyTypeWithLoc(sug, intLit.L))
			return deref(intLit), nil
		}
		intLit.T = copyTypeWithLoc(sug, intLit.L)
		return intLit, nil
	}
}

func checkFloatLit(parserFloatLit *parser.FloatLit, sug Type) (Expr, []*fail) {
	floatLit := &FloatLit{Text: parserFloatLit.Text, L: parserFloatLit.L}
	if _, _, err := floatLit.Val.Parse(parserFloatLit.Text, 10); err != nil {
		panic("malformed float")
	}
	switch b, ok := trim1Ref(literal(sug)).(*BasicType); {
	case sug == nil:
		fallthrough
	case !ok:
		fallthrough
	default:
		floatLit.T = ref(&BasicType{Kind: Float64, L: parserFloatLit.L})
		return deref(floatLit), nil
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
		if _, acc := floatLit.Val.Int(&i); acc != big.Exact {
			return nil, []*fail{{
				msg: fmt.Sprintf("%s truncates %s", sug, floatLit.Text),
				loc: floatLit.L,
			}}
		}
		parserIntLit := &parser.IntLit{
			Text: i.String(),
			L:    parserFloatLit.L,
		}
		return checkIntLit(parserIntLit, sug)
	case b.Kind == Float32 || b.Kind == Float64:
		if !isRef(sug) {
			floatLit.T = ref(copyTypeWithLoc(sug, floatLit.L))
			return deref(floatLit), nil
		}
		floatLit.T = copyTypeWithLoc(sug, floatLit.L)
		return floatLit, nil
	}
}

func deref(expr Expr) Expr {
	var t Type
	switch ref := expr.Type().(type) {
	case *RefType:
		t = ref.Type
	case *NamedType:
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
	case *NamedType:
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
	return typ.literal()
}

func (r *RefType) literal() Type {
	if r.Type == nil {
		return nil
	}
	return &RefType{Type: r.Type.literal(), L: r.Loc()}
}

func (n *NamedType) literal() Type {
	if n.Inst == nil || n.Inst.Type == nil {
		return nil
	}
	return n.Inst.Type.literal()
}

func (a *ArrayType) literal() Type  { return a }
func (s *StructType) literal() Type { return s }
func (u *UnionType) literal() Type  { return u }
func (f *FuncType) literal() Type   { return f }
func (b *BasicType) literal() Type  { return b }
func (t *TypeVar) literal() Type    { return t }
