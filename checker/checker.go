package checker

import (
	"errors"
	"fmt"
	"math/big"
	"path/filepath"
	"sort"
	"strconv"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

// Check does semantic checking, and returns a *Mod on success.
func Check(modPath string, files []*parser.File, importer Importer) (*Mod, loc.Files, []error) {
	var errs []Error
	idNames := make(map[string]loc.Loc)
	defs := make(map[parser.Def]Def)
	typeNames := make(map[string]loc.Loc)
	mod := &Mod{Path: modPath}
	if importer == nil {
		importer = NewImporter(".", files)
	}
	var importedMods []*Mod
	for _, parserFile := range files {
		var imports []*Import
		for _, parserImport := range parserFile.Imports {
			m, err := importer.Load(parserImport.Path)
			if err != nil {
				errs = append(errs, newError(parserImport.L, err.Error()))
				continue
			}
			importedMods = append(importedMods, m)
			name := filepath.Base(parserImport.Path)
			if parserImport.Name != nil {
				name = parserImport.Name.Name
			}
			/*
				// TODO: bad handling of re-defined imports.
				// The re-definition should be per-file, not module.
				if name != "_" {
					if prev, ok := idNames[name]; ok {
						errs = append(errs, redef(parserImport.L, name, prev))
						continue
					}
					idNames[name] = parserImport.L
				}
			*/
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
				errs = append(errs, redef(parserTypeDef.L, name, prev))
				continue
			}
			typeNames[name] = parserTypeDef.L
			parms, fs := makeTypeParms(importer.Files(), parserTypeDef.TypeParms)
			if len(fs) > 0 {
				errs = append(errs, fs...)
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
				errs = append(errs, fs...)
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
			if err := checkAliasCycle(typeDef); err != nil {
				errs = append(errs, err)
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
				if name != "_" {
					if prev, ok := idNames[name]; ok {
						errs = append(errs, redef(parserDef.L, name, prev))
						continue
					}
					idNames[name] = parserDef.L
				}
				t, fs := makeType(file, parserDef.Type)
				if len(fs) > 0 {
					errs = append(errs, fs...)
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
				var fs []Error
				if funDef.Parms, fs = makeFuncParms(funDef, parserDef.Parms); len(fs) > 0 {
					errs = append(errs, fs...)
				}
				if funDef.Ret, fs = makeType(funDef, parserDef.Ret); len(fs) > 0 {
					errs = append(errs, fs...)
				}
				if funDef.Ret == nil {
					funDef.Ret = &StructType{L: parserDef.L}
				}
				if funDef.Iface, fs = makeFuncDecls(funDef, parserDef.Iface); len(fs) > 0 {
					errs = append(errs, fs...)
				}
				mod.Defs = append(mod.Defs, funDef)
				defs[parserDef] = funDef
			case *parser.TestDef:
				name := parserDef.Name.Name
				if prev, ok := testNames[name]; ok {
					errs = append(errs, redef(parserDef.L, name, prev))
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
		for _, parserDef := range parserFile.Defs {
			var es []Error
			switch def := defs[parserDef].(type) {
			case *TypeDef:
				break // nothing to do really.
			case *VarDef:
				es = checkVarDef(def, parserDef.(*parser.VarDef))
			case *FuncDef:
				es = checkFuncDef(def, parserDef.(*parser.FuncDef))
			case *TestDef:
				es = checkTestDef(def, parserDef.(*parser.TestDef))
			}
			if len(es) > 0 {
				errs = append(errs, es...)
			}
		}
	}
	if es := topoSortVars(mod); len(es) > 0 {
		errs = append(errs, es...)
	}
	if len(errs) > 0 {
		var es []error
		for _, err := range errs {
			err.done(importer.Files(), 5)
			es = append(es, err)
		}
		return nil, nil, es
	}

	for i := 0; i < 5; i++ {
		toSub := mod.toSub
		mod.toSub = nil
		for _, imp := range importedMods {
			toSub = append(toSub, imp.toSub...)
			imp.toSub = nil
		}
		for _, inst := range toSub {
			subFuncInst(inst)
		}
	}
	if len(mod.toSub) > 0 {
		// TODO: improve too much substitution error message
		return nil, nil, []error{errors.New("too much substitution")}
	}

	mod.Deps = importer.Deps()

	return mod, importer.Files(), nil
}

func makeTypeParms(files loc.Files, parserTypeVars []parser.TypeVar) ([]TypeParm, []Error) {
	var errs []Error
	seen := make(map[string]loc.Loc)
	var typeParms []TypeParm
	for _, parserTypeVar := range parserTypeVars {
		name := parserTypeVar.Name
		if prev, ok := seen[name]; ok {
			errs = append(errs, redef(parserTypeVar.L, name, prev))
		} else {
			seen[name] = parserTypeVar.L
		}
		typeParms = append(typeParms, TypeParm{
			Name: name,
			L:    parserTypeVar.L,
		})
	}
	return typeParms, errs
}

func makeType(x scope, parserType parser.Type) (typ Type, errs []Error) {
	return _makeType(x, parserType, true)
}

func _makeType(x scope, parserType parser.Type, inst bool) (typ Type, errs []Error) {
	switch parserType := parserType.(type) {
	case nil:
		return nil, nil
	case *parser.RefType:
		typ, errs = _makeType(x, parserType.Type, inst)
		typ = &RefType{Type: typ, L: parserType.L}
	case *parser.NamedType:
		var args []Type
		for _, parserArg := range parserType.Args {
			arg, fs := _makeType(x, parserArg, inst)
			if len(fs) > 0 {
				errs = append(errs, fs...)
				continue
			}
			args = append(args, arg)
		}
		if len(errs) > 0 {
			break
		}
		if parserType.Mod != nil {
			modName := parserType.Mod.Name
			modLoc := parserType.Mod.L
			imp := x.findMod(parserType.Mod.Name)
			if imp == nil {
				errs = append(errs, notFound(modName, modLoc))
				return nil, errs
			}
			x = imp
		}
		name := parserType.Name.Name
		switch types := x.findType(args, name, parserType.L); {
		case len(types) == 0:
			errs = append(errs, notFound(name, parserType.L))
		case len(types) > 1:
			errs = append(errs, ambigType(name, parserType.L, types))
		default:
			typ = types[0]
		}
		if dt, ok := typ.(*DefType); ok && inst {
			typ = instType(dt)
		}
	case *parser.ArrayType:
		var elemType Type
		elemType, errs = _makeType(x, parserType.ElemType, inst)
		typ = &ArrayType{ElemType: elemType, L: parserType.L}
	case *parser.StructType:
		var fields []FieldDef
		for _, parserField := range parserType.Fields {
			t, fs := _makeType(x, parserField.Type, inst)
			if len(fs) > 0 {
				errs = append(errs, fs...)
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
				errs = append(errs, fs...)
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
			errs = append(errs, fs...)
		}
		ret, fs := _makeType(x, parserType.Ret, inst)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		if ret == nil {
			ret = &StructType{L: parserType.L}
		}
		typ = &FuncType{Parms: parms, Ret: ret, L: parserType.L}
	case parser.TypeVar:
		name := parserType.Name
		switch types := x.findType(nil, name, parserType.L); {
		case len(types) == 0:
			errs = append(errs, notFound(name, parserType.L))
		case len(types) > 1:
			errs = append(errs, ambigType(name, parserType.L, types))
		default:
			typ = types[0]
		}
	default:
		panic(fmt.Sprintf("unsupported Type type: %T", parserType))
	}
	if len(errs) > 0 {
		return nil, errs
	}
	return typ, nil
}

func makeTypes(x scope, parserTypes []parser.Type) ([]Type, []Error) {
	return _makeTypes(x, parserTypes, true)
}

func _makeTypes(x scope, parserTypes []parser.Type, inst bool) ([]Type, []Error) {
	var errs []Error
	var types []Type
	for _, parserType := range parserTypes {
		t, fs := _makeType(x, parserType, inst)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		types = append(types, t)
	}
	return types, errs
}

func checkAliasCycle(root *TypeDef) Error {
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
			notes = append(notes, newNote(def.Type.String()).setLoc(def.Type))
		}
		// Break the alias so that checking can continue reporting more errors.
		root.Alias = false
		err := newError(root.L, "alias cycle")
		err.setNotes(notes)
		return err
	}
	return nil
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

func makeFuncParms(x scope, parserParms []parser.FuncParm) ([]ParmDef, []Error) {
	seen := make(map[string]loc.Loc)
	var errs []Error
	var parms []ParmDef
	for _, parserParm := range parserParms {
		name := parserParm.Name.Name
		if prev, ok := seen[name]; name != "_" && ok {
			errs = append(errs, redef(parserParm.L, name, prev))
		} else {
			seen[name] = parserParm.L
		}
		var t Type
		var fs []Error
		if parserParm.Type != nil {
			if t, fs = makeType(x, parserParm.Type); len(fs) > 0 {
				errs = append(errs, fs...)
			}
		}
		parms = append(parms, ParmDef{Name: name, T: t, L: parserParm.L})
	}
	return parms, errs
}

func makeFuncDecls(x scope, parserDecls []parser.FuncDecl) ([]FuncDecl, []Error) {
	var errs []Error
	var decls []FuncDecl
	for _, parserDecl := range parserDecls {
		parms, fs := makeTypes(x, parserDecl.Parms)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		ret, fs := makeType(x, parserDecl.Ret)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		if ret == nil {
			ret = &StructType{L: parserDecl.L}
		}
		decls = append(decls, FuncDecl{
			Name:  parserDecl.Name.Name,
			Parms: parms,
			Ret:   ret,
			L:     parserDecl.L,
		})
	}
	return decls, errs
}

func checkVarDef(def *VarDef, parserDef *parser.VarDef) []Error {
	var errs []Error
	if parserDef.Expr != nil {
		expr, fs := checkAndConvertExpr(def, parserDef.Expr, def.T)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		def.Expr = &Call{
			Func: &Builtin{
				Op:    Assign,
				Parms: []Type{refType(def.T), expr.Type()},
				Ret:   expr.Type(),
			},
			Args: []Expr{&Var{Def: def, T: refType(def.T), L: def.L}, expr},
			T:    expr.Type(),
			L:    def.L,
		}
	}
	if def.T == nil && def.Expr != nil {
		def.T = def.Expr.Type()
	}
	if def.T == nil && def.Expr == nil {
		errs = append(errs, newError(def, "cannot infer variable type"))
	}
	return errs
}

func topoSortVars(mod *Mod) []Error {
	var errs []Error
	var sorted []Def
	var path []interface{}
	onPath := make(map[*VarDef]bool)
	seenVar := make(map[*VarDef]bool)
	seenFunc := make(map[*FuncDef]bool)

	var sortVar func(loc.Loc, *VarDef) bool
	var sortFunc func(loc.Loc, funcUse) bool
	sortVar = func(l loc.Loc, vr *VarDef) bool {
		path = append(path, varUse{Var: vr, L: l})
		defer func() { path = path[:len(path)-1] }()
		if onPath[vr] {
			var i int
			for ; i < len(path); i++ {
				if vu, ok := path[i].(varUse); ok && vu.Var == vr {
					break
				}
			}
			var notes []note
			prev := vr.Name
			for i++; i < len(path); i++ {
				switch use := path[i].(type) {
				case varUse:
					note := newNote("%s uses %s", prev, use.Var.Name)
					note.setLoc(use.L)
					notes = append(notes, note)
					prev = use.Var.Name
				case funcUse:
					note := newNote("%s calls %s", prev, use.Func.Name)
					prev = use.Func.Name
					note.setLoc(use.L)
					notes = append(notes, note)
					if use.Arg != nil {
						note = newNote("%s calls %s", prev, use.Arg.Name)
						prev = use.Arg.Name
						note.setLoc(use.Parm.L)
						notes = append(notes, note)
					}
				default:
					panic("impossible")
				}
			}
			err := newError(vr, "%s has a cyclic initialization", vr.Name)
			err.setNotes(notes)
			errs = append(errs, err)
			return false
		}
		onPath[vr] = true
		defer func() { delete(onPath, vr) }()
		if seenVar[vr] {
			return true
		}
		seenVar[vr] = true
		for _, v := range vr.usedVars {
			if !sortVar(v.L, v.Var) {
				return false
			}
		}
		for _, f := range vr.usedFuncs {
			if !sortFunc(f.L, f) {
				return false
			}
		}
		sorted = append(sorted, vr)
		return true
	}
	sortFunc = func(l loc.Loc, use funcUse) bool {
		fun := use.Func
		if use.Arg != nil {
			fun = use.Arg
		}
		if seenFunc[fun] {
			return true
		}
		seenFunc[fun] = true
		path = append(path, use)
		defer func() { path = path[:len(path)-1] }()
		for _, v := range fun.usedVars {
			if !sortVar(v.L, v.Var) {
				return false
			}
		}
		for _, f := range fun.usedFuncs {
			if !sortFunc(f.L, f) {
				return false
			}
		}
		return true
	}

	var n int
	for _, def := range mod.Defs {
		v, ok := def.(*VarDef)
		if !ok {
			mod.Defs[n] = def
			n++
			continue
		}
		sortVar(v.L, v)
	}
	mod.Defs = append(mod.Defs[:n], sorted...)
	return errs
}

func checkFuncDef(def *FuncDef, parserDef *parser.FuncDef) []Error {
	var errs []Error
	def.Exprs, errs = checkExprs(def, true, parserDef.Exprs, nil)
	if len(parserDef.Exprs) == 0 && parserDef.Exprs != nil {
		def.Exprs = []Expr{}
	}
	if !isEmptyStruct(def.Ret) &&
		def.Exprs != nil &&
		(len(def.Exprs) == 0 || !isBuiltin(def.Exprs[len(def.Exprs)-1], Return)) {
		errs = append(errs, newError(def, "function must end in a return"))
	}
	for _, l := range def.Locals {
		if l.Name != "_" && !l.used {
			errs = append(errs, newError(l, "%s unused", l.Name))
		}
	}
	if len(def.TypeParms) == 0 && len(def.Iface) == 0 {
		// This is a not-parameterized function.
		// Make sure we build an instance of it even if never called.
		canonicalFuncInst(newFuncInst(def, nil, def.Loc()))
	}
	return errs
}

func isBuiltin(expr Expr, op Op) bool {
	for {
		convert, ok := expr.(*Convert)
		if !ok || convert.Kind != Deref {
			break
		}
		expr = convert.Expr
	}
	call, ok := expr.(*Call)
	if !ok {
		return false
	}
	if call.Func == nil {
		// The call is already an error.
		// Let's not report another error;
		// just assume it would have been a return.
		return true
	}
	b, ok := call.Func.(*Builtin)
	return ok && b.Op == op
}

func checkTestDef(def *TestDef, parserDef *parser.TestDef) []Error {
	var errs []Error
	def.Exprs, errs = checkExprs(def, true, parserDef.Exprs, nil)
	return errs
}

func checkExpr(x scope, parserExpr parser.Expr, want Type) (Expr, []Error) {
	switch parserExpr := parserExpr.(type) {
	case *parser.Call:
		return checkCall(x, parserExpr, want)
	case *parser.Convert:
		return checkConvert(x, parserExpr)
	case *parser.SubExpr:
		return checkExpr(x, parserExpr.Expr, want)
	case *parser.ArrayLit:
		return checkArrayLit(x, parserExpr, want)
	case *parser.StructLit:
		return checkStructLit(x, parserExpr, want)
	case *parser.UnionLit:
		return checkUnionLit(x, parserExpr, want)
	case *parser.BlockLit:
		return checkBlockLit(x, parserExpr, want)
	case *parser.StrLit:
		return checkStrLit(parserExpr, want)
	case *parser.CharLit:
		return checkCharLit(parserExpr, want)
	case *parser.IntLit:
		return checkIntLit(parserExpr, want)
	case *parser.FloatLit:
		return checkFloatLit(parserExpr, want)
	case *parser.ModSel:
		return checkModSel(x, parserExpr, true, want)
	case parser.Ident:
		return checkID(x, parserExpr, true, want)
	default:
		panic(fmt.Sprintf("impossible expr type: %T", parserExpr))
	}
}

func checkAndConvertExpr(x scope, parserExpr parser.Expr, want Type) (Expr, []Error) {
	expr, errs := checkExpr(x, parserExpr, want)
	if expr != nil && want != nil {
		var err Error
		expr, err = convert(expr, want, false)
		if err != nil {
			errs = append(errs, err)
		}
	}
	return expr, errs
}

// newLocals indicates whether new local variables may be created.
// want is the type expected for the last expression, or nil.
func checkExprs(x scope, newLocals bool, parserExprs []parser.Expr, want Type) ([]Expr, []Error) {
	var errs []Error
	var exprs []Expr
	for i, parserExpr := range parserExprs {
		if call, ok := isAssign(parserExpr); ok && newLocals {
			if id, ok := isNewID(x, call.Args[0]); ok {
				local, assign, es := newLocal(x, call, id)
				if len(es) > 0 {
					errs = append(errs, es...)
				}
				if local == nil || assign == nil {
					continue
				}
				exprs = append(exprs, assign)
				x = &localScope{parent: x, LocalDef: local}
				continue
			}
		}
		var expr Expr
		var es []Error
		if i == len(parserExprs)-1 && want != nil && !isEmptyStruct(want) {
			expr, es = checkExpr(x, parserExpr, want)
			if expr != nil && !isBuiltin(expr, Panic) && !isBuiltin(expr, Return) {
				var err Error
				if expr, err = convert(expr, want, false); err != nil {
					es = append(es, err)
				}
			}
		} else {
			expr, es = checkExpr(x, parserExpr, nil)
		}
		if len(es) > 0 {
			errs = append(errs, es...)
		}
		if expr != nil {
			exprs = append(exprs, expr)
		}
	}
	return exprs, errs
}

func isAssign(parserExpr parser.Expr) (call *parser.Call, ok bool) {
	if call, ok = parserExpr.(*parser.Call); !ok {
		return nil, false
	}
	if id, ok := call.Fun.(parser.Ident); !ok || id.Name != ":=" {
		return nil, false
	}
	return call, true
}

func isNewID(x scope, parserExpr parser.Expr) (parser.Ident, bool) {
	parserID, ok := parserExpr.(parser.Ident)
	if !ok {
		return parser.Ident{}, false
	}
	for _, id := range x.find(parserID.Name) {
		switch id.(type) {
		case *VarDef:
			return parser.Ident{}, false
		case *ParmDef:
			return parser.Ident{}, false
		case *LocalDef:
			return parser.Ident{}, false
		}
	}
	return parserID, true
}

func newLocal(x scope, call *parser.Call, id parser.Ident) (*LocalDef, Expr, []Error) {
	expr, errs := checkExpr(x, call.Args[1], nil)
	if expr == nil {
		return nil, nil, errs
	}
	local := x.newLocal(id.Name, expr.Type(), id.L)
	if local == nil {
		errs = append(errs, newError(call.L, "local defined outside of a block"))
		return nil, nil, errs
	}
	assign := &Convert{
		Kind: Deref,
		Expr: &Call{
			Func: &Builtin{
				Op:    Assign,
				Parms: []Type{refType(expr.Type()), expr.Type()},
				Ret:   expr.Type(),
			},
			Args: []Expr{
				&Local{Def: local, T: refType(expr.Type()), L: id.L},
				expr,
			},
			T: &RefType{Type: &StructType{L: id.L}, L: id.L},
			L: call.L,
		},
		T: &StructType{L: id.L},
		L: id.L,
	}
	return local, assign, errs
}

func convert(expr Expr, typ Type, explicit bool) (Expr, Error) {
	if typ == nil {
		panic("convert to no type")
	}
	if expr == nil || expr.Type() == nil {
		return expr, nil
	}
	srcType := expr.Type()
	dstType := typ

	if convert, ok := expr.(*Convert); !explicit && ok && convert.Explicit {
		if eqType(srcType, dstType) {
			return expr, nil
		}
		return expr, newError(expr, "cannot convert %s (%s) to type %s",
			expr, expr.Type(), typ)
	}

	if isLiteralType(srcType) {
		if dstLitType := literalType(dstType); dstLitType != nil {
			dstType = dstLitType
		}
	} else if isLiteralType(dstType) {
		if srcLitType := literalType(srcType); srcLitType != nil {
			srcType = srcLitType
		}
	}

	srcValueType := valueType(srcType)
	dstValueType := valueType(dstType)
	if !eqType(dstValueType, srcValueType) {
		return expr, newError(expr, "cannot convert %s (%s) to type %s",
			expr, expr.Type(), typ)
	}

	expr0 := expr
	srcRefDepth, dstRefDepth := refDepth(srcType), refDepth(dstType)
	for srcRefDepth < dstRefDepth {
		convert, ok := expr.(*Convert)
		if !ok || convert.Kind != Deref {
			return expr0, newError(expr0, "cannot convert %s (%s) to type %s",
				expr0, expr0.Type(), typ)
		}
		expr = convert.Expr
		srcRefDepth++
	}
	for dstRefDepth < srcRefDepth {
		expr = deref(expr)
		srcRefDepth--
	}
	return expr, nil
}

func checkCall(x scope, parserCall *parser.Call, want Type) (Expr, []Error) {
	switch fun := parserCall.Fun.(type) {
	case parser.Ident:
		ids := x.find(fun.Name)
		return resolveIDCall(x, fun, parserCall, want, ids)
	case *parser.ModSel:
		imp := x.findMod(fun.Mod.Name)
		if imp == nil {
			return nil, []Error{notFound(fun.Mod.Name, fun.L)}
		}
		ids := imp.find(fun.Name.Name)
		return resolveIDCall(x, fun.Name, parserCall, want, ids)
	default:
		return checkExprCall(x, parserCall, want)
	}
}

func resolveIDCall(x scope, parserID parser.Ident, parserCall *parser.Call, want Type, ids []id) (Expr, []Error) {
	if len(ids) == 0 {
		args, errs := checkArgsFallback(x, parserCall.Args)
		errs = append(errs, notFound(parserID.Name, parserID.L))
		return &Call{Args: args, L: parserCall.L}, errs
	}
	funcs, notes := filterToFuncs(ids, parserID.L)
	if len(funcs) == 0 {
		args, errs := checkArgsFallback(x, parserCall.Args)
		err := newError(parserID.L, "%s is not callable", parserID.Name)
		err.setNotes(notes)
		errs = append(errs, err)
		return &Call{Args: args, L: parserCall.L}, errs
	}
	funcs, ns := filterByArity(funcs, len(parserCall.Args))
	if len(ns) > 0 {
		notes = append(notes, ns...)
	}
	var errs []Error
	var args []Expr
	for i, parserArg := range parserCall.Args {
		t := commonGroundParmType(funcs, i)
		if t == nil {
			var arg Expr
			var es []Error
			// If this is the LHS of an assignment to an Ident,
			// don't mark it as "used" if it is a local variable.
			if lhs, ok := parserArg.(parser.Ident); ok && i == 0 && parserID.Name == ":=" {
				arg, es = checkID(x, lhs, false, t)
			} else {
				arg, es = checkAndConvertExpr(x, parserArg, nil)
			}
			if arg == nil || arg.Type() == nil || len(es) > 0 {
				errs = append(errs, es...)
				as, es := checkArgsFallback(x, parserCall.Args[i+1:])
				args = append(args, as...)
				errs = append(errs, es...)
				return &Call{Args: args, L: parserCall.L}, errs
			}
			args = append(args, arg)
			var ns []note
			funcs, ns = filterByGroundedArg(funcs, i, arg)
			notes = append(notes, ns...)
			continue
		}
		arg, es := checkExpr(x, parserArg, t)
		// TODO: these errors should become notes and be appended.
		if len(es) > 0 {
			errs = append(errs, es...)
			as, es := checkArgsFallback(x, parserCall.Args[i+1:])
			args = append(args, as...)
			errs = append(errs, es...)
			return &Call{Args: args, L: parserCall.L}, errs
		}
		if arg, err := convert(arg, t, false); err != nil {
			var notes []note
			for _, f := range funcs {
				notes = append(notes, newNote("%s parameter %d is type %s", f, i, t).setLoc(f.groundParm(i)))
			}
			err.setNotes(notes)
			errs = append(errs, err)
			as, fs := checkArgsFallback(x, parserCall.Args)
			if len(fs) > 0 {
				errs = append(errs, fs...)
			}
			args = append(args, as...)
			return &Call{Args: args, L: parserCall.L}, errs
		} else if arg != nil {
			args = append(args, arg)
		}
	}

	if want != nil {
		var ns []note
		funcs, ns = filterByReturn(funcs, want)
		notes = append(notes, ns...)
	} else {
		funcs, ns = filterUngroundReturns(funcs)
		notes = append(notes, ns...)
	}

	funcs, ns = filterIfaceConstraints(x, parserCall.L, funcs)
	notes = append(notes, ns...)
	switch {
	case len(funcs) == 0:
		err := notFound(parserID.Name, parserID.L)
		err.setNotes(notes)
		errs = append(errs, err)
		return &Call{Args: args, L: parserCall.L}, errs
	case len(funcs) > 1:
		errs = append(errs, ambiguousCall(parserID.Name, funcs, parserID.L))
		return &Call{Args: args, L: parserCall.L}, errs
	}

	fun := useFunc(x, parserCall.L, funcs[0])
	ret := fun.groundRet()
	for i, arg := range args {
		args[i], _ = convert(arg, fun.groundParm(i), false)
	}
	var expr Expr = &Call{
		Func: fun,
		Args: args,
		T:    &RefType{Type: ret, L: ret.Loc()},
		L:    parserCall.L,
	}
	for isRefType(expr.Type()) {
		expr = deref(expr)
	}
	return expr, errs
}

func filterToFuncs(ids []id, l loc.Loc) ([]Func, []note) {
	var funcs []Func
	var notes []note
	for _, id := range ids {
		var fun Func
		switch id := id.(type) {
		case *VarDef:
			if t := funcType(id.T); t != nil {
				fun = &idFunc{id: id, funcType: t, l: l}
			}
		case *ParmDef:
			if t := funcType(id.T); t != nil {
				fun = &idFunc{id: id, funcType: t, l: l}
			}
		case *LocalDef:
			if t := funcType(id.T); t != nil {
				fun = &idFunc{id: id, funcType: t, l: l}
			}
		case *BlockCap:
			// find() does not return *BlockCap, so this is not possible.
			panic("impossible")
		case Func:
			fun = id
		default:
			panic(fmt.Sprintf("impossible id type: %T", id))
		}
		if fun == nil {
			note := newNote("%s (%s) is not a function", id, id.Type()).setLoc(id)
			notes = append(notes, note)
			continue
		}
		funcs = append(funcs, fun)
	}
	return funcs, notes
}

func filterByArity(funcs []Func, arity int) ([]Func, []note) {
	var notes []note
	var n int
	for _, f := range funcs {
		if f.arity() == arity {
			funcs[n] = f
			n++
			continue
		}
		notes = append(notes, newNote("%s: expects %d arguments, got %d", f, f.arity(), arity).setLoc(f))
	}
	return funcs[:n], notes
}

func checkArgsFallback(x scope, parserArgs []parser.Expr) ([]Expr, []Error) {
	var args []Expr
	var errs []Error
	for _, parserArg := range parserArgs {
		arg, fs := checkAndConvertExpr(x, parserArg, nil)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		if arg != nil {
			args = append(args, arg)
		}
	}
	return args, errs
}

func filterByReturn(funcs []Func, want Type) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		if note := f.unifyRet(want); note != nil {
			notes = append(notes, note)
			continue
		}
		retType := f.groundRet()
		if retType == nil {
			continue
		}
		// If there is only 1 function, don't bother checking conversion.
		// If it cannot convert, it will fail upstream.
		// The reason for this special case is that canConvertReturn
		// only accepts implicit conversions.
		// If the parent node is an explicit conversion,
		// we want it to be accepted in the common case
		// that there is only one function overload acceptable at this point.
		if len(funcs) > 1 && !canConvertReturn(retType, want) {
			notes = append(notes, newNote("%s: cannot convert returned %s to %s", f, retType, want).setLoc(f))
			continue
		}
		funcs[n] = f
		n++
	}
	return funcs[:n], notes
}

func commonGroundParmType(funcs []Func, i int) Type {
	var t Type
	for _, f := range funcs {
		switch {
		case f.groundParm(i) == nil:
			return nil
		case t == nil:
			t = f.groundParm(i)
		case !eqType(t, f.groundParm(i)):
			return nil
		}
	}
	return t
}

func filterByGroundedArg(funcs []Func, i int, arg Expr) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		if note := f.unifyParm(i, arg.Type()); note != nil {
			notes = append(notes, note)
			continue
		}
		parmType := f.groundParm(i)
		if parmType == nil {
			continue
		}
		if _, err := convert(arg, parmType, false); err == nil {
			funcs[n] = f
			n++
			continue
		}
		notes = append(notes, newNote("%s: cannot convert argument %s (%s) to %s",
			f, arg, arg.Type(), parmType).setLoc(parmType))
	}
	return funcs[:n], notes
}

func filterUngroundReturns(funcs []Func) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		if f.groundRet() != nil {
			funcs[n] = f
			n++
			continue
		}
		notes = append(notes, newNote("%s: cannot infer return type", f).setLoc(f))
	}
	return funcs[:n], notes
}

func filterIfaceConstraints(x scope, l loc.Loc, funcs []Func) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		if note := instIface(x, l, f); note != nil {
			notes = append(notes, note)
			continue
		}
		funcs[n] = f
		n++
	}
	return funcs[:n], notes
}

func useFunc(x scope, l loc.Loc, fun Func) Func {
	switch fun := fun.(type) {
	case *FuncInst:
		x.useFunc(l, fun.Def, nil, nil)
		return canonicalFuncInst(fun)
	case *idFunc:
		return &ExprFunc{
			Expr:     idToExpr(useID(x, l, true, fun.id), fun.l),
			FuncType: fun.funcType,
		}
	default:
		return fun
	}
}

func ambiguousCall(name string, funcs []Func, l loc.Loc) Error {
	var notes []note
	for _, f := range funcs {
		notes = append(notes, newNote("%s", f).setLoc(f))
	}
	err := newError(l, "%s: ambiguous call", name)
	err.setNotes(notes)
	return err
}

func canConvertReturn(src, dst Type) bool {
	someNonZeroLoc := loc.Loc{1, 1}
	// To test whether a return type is convertable,
	// we create a dummy Deref
	// and try to convert it to the desired type.
	// If there is no error, then we've got it.
	// We use Deref, because the result of a call
	// is always a Deref node.
	//
	// Our dummy node needs a non-zero loc,
	// since errors can only be created with a non-zero loc.
	// We are going to ignore the error, so any non-zero loc works.
	_, err := convert(&Convert{Kind: Deref, T: src, L: someNonZeroLoc}, dst, false)
	return err == nil
}

func checkExprCall(x scope, parserCall *parser.Call, want Type) (Expr, []Error) {
	var errs []Error
	var fun *ExprFunc
	// TODO: should checkExprCall just use checkExpr, without the convert?
	expr, fs := checkAndConvertExpr(x, parserCall.Fun, want)
	if len(fs) > 0 {
		errs = append(errs, fs...)
	}
	if expr != nil && expr.Type() != nil {
		if t := funcType(expr.Type()); t == nil {
			err := newError(expr, "%s (%s) is not callable", expr, expr.Type())
			errs = append(errs, err)
		} else {
			fun = &ExprFunc{Expr: expr, FuncType: t}
		}
	}
	if fun != nil && len(parserCall.Args) != len(fun.FuncType.Parms) {
		err := newError(parserCall.L, "got %d arguments, expected %d",
			len(parserCall.Args), len(fun.FuncType.Parms))
		errs = append(errs, err)
	}
	var args []Expr
	for i, parserArg := range parserCall.Args {
		var arg Expr
		var fs []Error
		if fun != nil && i < len(fun.FuncType.Parms) {
			arg, fs = checkAndConvertExpr(x, parserArg, fun.FuncType.Parms[i])
		} else {
			arg, fs = checkAndConvertExpr(x, parserArg, nil)
		}
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		if arg != nil {
			args = append(args, arg)
		}
	}
	var ret Type
	if fun != nil {
		ret = &RefType{Type: fun.FuncType.Ret, L: fun.FuncType.Ret.Loc()}
	}
	expr = &Call{Func: fun, Args: args, T: ret, L: parserCall.L}
	for isRefType(expr.Type()) {
		expr = deref(expr)
	}
	return expr, errs
}

func checkModSel(x scope, parserSel *parser.ModSel, useLocal bool, want Type) (Expr, []Error) {
	imp := x.findMod(parserSel.Mod.Name)
	if imp == nil {
		return nil, []Error{notFound(parserSel.Mod.Name, parserSel.L)}
	}
	parserID := parserSel.Name
	ids := imp.find(parserID.Name)
	return resolveID(x, parserID, useLocal, want, ids)
}

func checkID(x scope, parserID parser.Ident, useLocal bool, want Type) (Expr, []Error) {
	ids := x.find(parserID.Name)
	return resolveID(x, parserID, useLocal, want, ids)
}

func resolveID(x scope, parserID parser.Ident, useLocal bool, want Type, ids []id) (Expr, []Error) {
	if len(ids) == 1 {
		return idToExpr(useID(x, parserID.L, useLocal, ids[0]), parserID.L), nil
	}
	var n int
	var ambigNotes []note
	var notFoundNotes []note
	for _, id := range ids {
		if !isGround(id) {
			if n := unifyFunc(x, parserID.L, id.(Func), want); n != nil {
				notFoundNotes = append(notFoundNotes, n)
				continue
			}
		}
		if want != nil {
			expr := idToExpr(id, parserID.L)
			if _, err := convert(expr, want, false); err != nil {
				notFoundNotes = append(notFoundNotes,
					newNote("cannot convert %s (%s) to type %s",
						expr, expr.Type(), want).setLoc(expr))
				continue
			}
		}
		var l loc.Loc
		if locer, ok := id.(interface{ Loc() loc.Loc }); ok {
			l = locer.Loc()
		}
		ambigNotes = append(ambigNotes, newNote(id.String()).setLoc(l))
		ids[n] = id
		n++
	}
	ids = ids[:n]
	switch {
	case len(ids) == 0:
		err := notFound(parserID.Name, parserID.L)
		err.setNotes(notFoundNotes)
		return nil, []Error{err}
	case len(ids) > 1:
		var err Error
		if want != nil {
			err = newError(parserID.L, "%s is ambiguous", parserID.Name)
		} else {
			err = newError(parserID.L, "%s is ambiguous for type %s", parserID.Name, want)
		}
		err.setNotes(ambigNotes)
		return nil, []Error{err}
	default:
		return idToExpr(useID(x, parserID.L, useLocal, ids[0]), parserID.L), nil
	}
}

func isGround(id id) bool {
	switch id := id.(type) {
	case Func:
		for i := 0; i < id.arity(); i++ {
			if id.groundParm(i) == nil {
				return false
			}
		}
		return id.groundRet() != nil
	default:
		return true
	}
}

func useID(x scope, l loc.Loc, useLocal bool, id id) id {
	switch id := id.(type) {
	case *VarDef:
		x.useVar(l, id)
		return id
	case *ParmDef:
		return x.capture(id)
	case *LocalDef:
		if useLocal {
			id.used = true
		}
		return x.capture(id)
	case *BlockCap:
		return x.capture(id)
	case *FuncInst:
		x.useFunc(l, id.Def, nil, nil)
		return canonicalFuncInst(id)
	default:
		return id
	}
}

func idToExpr(id id, l loc.Loc) Expr {
	switch id := id.(type) {
	case *VarDef:
		return deref(&Var{Def: id, T: refType(id.T), L: l})
	case *ParmDef:
		return deref(&Parm{Def: id, T: refType(id.T), L: l})
	case *LocalDef:
		return deref(&Local{Def: id, T: refType(id.T), L: l})
	case *BlockCap:
		return deref(&Cap{Def: id, T: refType(id.T), L: l})
	case Func:
		return wrapCallInBlock(id, l)
	default:
		panic(fmt.Sprintf("impossible id type: %T", id))
	}
}

func wrapCallInBlock(fun Func, l loc.Loc) Expr {
	var parms []Type
	for i := 0; i < fun.arity(); i++ {
		p := fun.groundParm(i)
		if p == nil {
			panic("impossible")
		}
		parms = append(parms, p)
	}
	ret := fun.groundRet()
	if ret == nil {
		panic("impossible")
	}
	typ := &FuncType{Parms: parms, Ret: ret, L: l}
	blk := &BlockLit{Ret: typ.Ret, T: refType(typ), L: l}
	call := &Call{Func: fun, T: &RefType{Type: typ.Ret, L: l}, L: l}
	call.Args = make([]Expr, len(typ.Parms))
	expr := deref(call)
	for isRefType(expr.Type()) {
		expr = deref(expr)
	}
	expr, err := convert(expr, typ.Ret, false)
	if err != nil {
		panic("impossible")
	}
	blk.Exprs = []Expr{expr}
	blk.Parms = make([]ParmDef, len(typ.Parms))
	for i := range typ.Parms {
		blk.Parms[i].Name = fmt.Sprintf("x%d", i)
		blk.Parms[i].T = typ.Parms[i]
		blk.Parms[i].L = l
		call.Args[i] = deref(&Parm{
			Def: &blk.Parms[i],
			T:   refType(typ.Parms[i]),
			L:   l,
		})
	}
	return deref(blk)
}

// checkConvert checks a type conversion.
// 	* The expected type of the subexpression is the conversion type,
// 	  and it is an error if the subexpression type is not explicitly convertible
// 	  to the conversion type.
func checkConvert(x scope, parserConvert *parser.Convert) (Expr, []Error) {
	typ, errs := makeType(x, parserConvert.Type)
	expr, es := checkExpr(x, parserConvert.Expr, typ)
	if len(es) > 0 {
		errs = append(errs, es...)
	}
	if expr == nil || expr.Type() == nil {
		return expr, errs
	}
	cvt := &Convert{
		Explicit: true,
		Expr:     expr,
		T:        typ,
		L:        parserConvert.L,
	}
	switch basicKind(typ) {
	case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Float32, Float64:
		if !isRefType(typ) && (isIntType(expr.Type()) || isFloatType(expr.Type())) {
			cvt.Kind = NumConvert
			return cvt, errs
		}
	case String:
		if !isRefType(typ) && isByteArray(expr.Type()) {
			cvt.Kind = StrConvert
			return cvt, errs
		}
	}
	// The conversion would happen implicitly,
	// but it was made explicit in the code.
	// Here we do the implicit conversion,
	// and stick a Noop conversion node on top of it
	// to track that it was requested explicitly.
	expr, err := convert(expr, typ, true)
	if err != nil {
		errs = append(errs, err)
	}
	cvt.Expr = expr
	cvt.Kind = Noop
	return cvt, errs
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
// A type is appropriate to an array literal if
// 	* its literal type is an array type or a reference to an array type.
func checkArrayLit(x scope, parserLit *parser.ArrayLit, want Type) (Expr, []Error) {
	var errs []Error
	lit := &ArrayLit{L: parserLit.L}
	if lit.Array, _ = trim1Ref(literalType(want)).(*ArrayType); lit.Array != nil {
		for _, parserExpr := range parserLit.Exprs {
			expr, fs := checkAndConvertExpr(x, parserExpr, lit.Array.ElemType)
			if len(fs) > 0 {
				errs = append(errs, fs...)
			}
			lit.Elems = append(lit.Elems, expr)
		}
		if !isRefType(want) {
			lit.T = refType(copyTypeWithLoc(want, lit.L))
			return deref(lit), errs
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, errs
	}

	var elemType Type
	for _, parserExpr := range parserLit.Exprs {
		expr, fs := checkAndConvertExpr(x, parserExpr, elemType)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		lit.Elems = append(lit.Elems, expr)
		if elemType == nil {
			elemType = expr.Type()
		}
	}
	if elemType == nil {
		errs = append(errs, newError(lit, "unable to infer array type"))
		return lit, errs
	}
	lit.Array = &ArrayType{ElemType: elemType, L: lit.L}
	lit.T = refType(lit.Array)
	return deref(lit), errs
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
// A type is appropriate to a struct literal if
// 	* its literal type is a struct type or a reference to a struct type,
// 	* it has the same number of fields as the literal,
// 	* each of the fields, in order, has the same name as the corresponding literal field.
func checkStructLit(x scope, parserLit *parser.StructLit, want Type) (Expr, []Error) {
	var errs []Error
	lit := &StructLit{L: parserLit.L}

	if lit.Struct = appropriateStruct(want, parserLit); lit.Struct != nil {
		for i, parserField := range parserLit.FieldVals {
			expr, fs := checkAndConvertExpr(x, parserField.Val, lit.Struct.Fields[i].Type)
			if len(fs) > 0 {
				errs = append(errs, fs...)
			}
			lit.Fields = append(lit.Fields, expr)
		}
		if !isRefType(literalType(want)) {
			lit.T = refType(copyTypeWithLoc(want, lit.L))
			return deref(lit), errs
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, errs
	}

	lit.Struct = &StructType{L: lit.L}
	for _, parserField := range parserLit.FieldVals {
		expr, fs := checkAndConvertExpr(x, parserField.Val, nil)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		lit.Fields = append(lit.Fields, expr)
		lit.Struct.Fields = append(lit.Struct.Fields, FieldDef{
			Name: parserField.Name.Name,
			Type: expr.Type(),
			L:    parserField.L,
		})
	}
	lit.T = refType(lit.Struct)
	return deref(lit), errs
}

func appropriateStruct(typ Type, lit *parser.StructLit) *StructType {
	s, ok := trim1Ref(literalType(typ)).(*StructType)
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
// A type is appropriate to a union literal if
// 	* its literal type is a union type or a reference to a union type,
// 	* it has a case with the same name as the literal case,
// 	* if the literal has a value, the corresponding case has a type,
// 	* or if the literal has no value, the corresponding case has no type.
func checkUnionLit(x scope, parserLit *parser.UnionLit, want Type) (Expr, []Error) {
	var errs []Error
	lit := &UnionLit{L: parserLit.L}
	if lit.Union, lit.Case = appropriateUnion(want, parserLit); lit.Union != nil {
		if parserLit.CaseVal.Val != nil {
			lit.Val, errs = checkAndConvertExpr(x, parserLit.CaseVal.Val, lit.Case.Type)
		}
		if !isRefType(literalType(want)) {
			lit.T = refType(copyTypeWithLoc(want, lit.L))
			return deref(lit), errs
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, errs
	}

	lit.Union = &UnionType{
		Cases: []CaseDef{{
			Name: parserLit.CaseVal.Name.Name,
			L:    parserLit.CaseVal.L,
		}},
		L: parserLit.L,
	}
	if parserLit.CaseVal.Val != nil {
		lit.Val, errs = checkAndConvertExpr(x, parserLit.CaseVal.Val, nil)
	}
	lit.Case = &lit.Union.Cases[0]
	if lit.Val != nil {
		lit.Case.Type = lit.Val.Type()
	}
	lit.T = refType(lit.Union)
	return deref(lit), errs
}

func appropriateUnion(typ Type, lit *parser.UnionLit) (*UnionType, *CaseDef) {
	u, ok := trim1Ref(literalType(typ)).(*UnionType)
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
// 	  with parameters corresponding to the explicitly type
// 	  of each of the literal's parameters.
// 	  It is an error if any of the parameter's type is elided.
// 	  If there are no expressions in the block, the type has no return type.
// 	  Otherwise the return type is the type of the last expression
// 	  in the block with no expected type.
//
// A type is appropriate to a block literal if
// 	* its literal type is a function type or a reference to a function type,
// 	* it has the same number of parameters as the literal, and
// 	* all explicit parameter types of the literal
// 	  equal the corresponding parameter type of the function type.
func checkBlockLit(x scope, parserLit *parser.BlockLit, want Type) (Expr, []Error) {
	var errs []Error
	lit := &BlockLit{L: parserLit.L}
	lit.Parms, errs = makeFuncParms(x, parserLit.Parms)
	x = &blockLitScope{parent: x, BlockLit: lit}

	if f := appropriateBlock(want, lit.Parms); f != nil {
		for i := range lit.Parms {
			if lit.Parms[i].T != nil {
				continue
			}
			lit.Parms[i].T = copyTypeWithLoc(f.Parms[i], lit.Parms[i].L)
		}
		lit.Ret = f.Ret
		var fs []Error
		lit.Exprs, fs = checkExprs(x, true, parserLit.Exprs, f.Ret)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		if !isRefType(want) {
			lit.T = refType(copyTypeWithLoc(want, lit.L))
			return deref(lit), errs
		}
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, errs
	}

	// Remove parameters with elided types and report an error.
	var n int
	var parmTypes []Type
	for _, p := range lit.Parms {
		if p.T == nil {
			errs = append(errs, newError(p.L, "cannot infer type of parameter %s", p.Name))
			continue
		}
		parmTypes = append(parmTypes, p.T)
		lit.Parms[n] = p
		n++
	}
	lit.Parms = lit.Parms[:n]

	var fs []Error
	lit.Exprs, fs = checkExprs(x, true, parserLit.Exprs, nil)
	if len(fs) > 0 {
		errs = append(errs, fs...)
	}
	for _, l := range lit.Locals {
		if l.Name != "_" && !l.used {
			errs = append(errs, newError(l, "%s unused", l.Name))
		}
	}

	if len(lit.Exprs) > 0 {
		lit.Ret = lit.Exprs[len(lit.Exprs)-1].Type()
	}
	if lit.Ret == nil {
		lit.Ret = &StructType{L: lit.L}
	}
	lit.T = refType(&FuncType{Parms: parmTypes, Ret: lit.Ret, L: lit.L})
	return deref(lit), errs
}

func appropriateBlock(typ Type, litParms []ParmDef) *FuncType {
	f, ok := trim1Ref(literalType(typ)).(*FuncType)
	if !ok || len(f.Parms) != len(litParms) {
		return nil
	}
	for i := range f.Parms {
		if t := litParms[i].T; t != nil && !eqType(f.Parms[i], t) {
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
func checkStrLit(parserLit *parser.StrLit, want Type) (Expr, []Error) {
	lit := &StrLit{Text: parserLit.Data, L: parserLit.L}
	switch {
	case want == nil:
		fallthrough
	default:
		lit.T = refType(&BasicType{Kind: String, L: parserLit.L})
		return deref(lit), nil
	case isStringRefType(want):
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, nil
	case isStringType(want):
		lit.T = refType(copyTypeWithLoc(want, lit.L))
		return deref(lit), nil
	}
}

// checkCharLit checks a character literal.
// 	* Characeter literals are checked just as int literals
// 	  with the literal value being the unicode code point value
// 	  of the character.
// TODO: should default to int32, not int.
func checkCharLit(parserLit *parser.CharLit, want Type) (Expr, []Error) {
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
func checkIntLit(parserLit *parser.IntLit, want Type) (Expr, []Error) {
	switch {
	case want == nil:
		fallthrough
	default:
		t := &BasicType{Kind: Int, L: parserLit.L}
		lit, errs := newIntLit(parserLit, t)
		lit.T = refType(t)
		return deref(lit), errs
	case isFloatType(want) || isFloatRefType(want):
		floatLit := &parser.FloatLit{Text: parserLit.Text, L: parserLit.L}
		return checkFloatLit(floatLit, want)
	case isIntRefType(want):
		lit, errs := newIntLit(parserLit, want)
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, errs
	case isIntType(want):
		lit, errs := newIntLit(parserLit, want)
		lit.T = refType(copyTypeWithLoc(want, lit.L))
		return deref(lit), errs
	}
}

func newIntLit(parserLit *parser.IntLit, want Type) (*IntLit, []Error) {
	var bits uint
	var signed bool
	switch basicKind(want) {
	case Int:
		bits = 64 // TODO: set by a flag
		signed = true
	case Int8:
		bits = 8
		signed = true
	case Int16:
		bits = 16
		signed = true
	case Int32:
		bits = 32
		signed = true
	case Int64:
		bits = 64
		signed = true
	case Uint:
		bits = 64 // TODO: set by a flag
		signed = false
	case Uint8:
		bits = 8
		signed = false
	case Uint16:
		bits = 16
		signed = false
	case Uint32:
		bits = 32
		signed = false
	case Uint64:
		bits = 64
		signed = false
	default:
		panic(fmt.Sprintf("impossible int kind: %s", want))
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
	lit := &IntLit{Text: parserLit.Text, L: parserLit.L}
	if _, ok := lit.Val.SetString(parserLit.Text, 0); !ok {
		panic("malformed int")
	}
	var errs []Error
	switch {
	case lit.Val.Cmp(min) < 0:
		errs = append(errs, newError(lit, "%s underflows type %s", lit.Text, want))
	case lit.Val.Cmp(max) > 0:
		errs = append(errs, newError(lit, "%s overflows type %s", lit.Text, want))
	}
	return lit, errs
}

// checkFloatLit checks a float literal.
// 	* If the expected type's literal type is a built-in float type
// 	  or a reference to a built-in float type,
// 	  then the type of the expression is the expected type.
// 	* If the expected type's literal type is a built-in int type
// 	  or a reference to a built-in int type,
// 	  then the type of the expression is the expected type.
// 	  It is an error if the literal value is not a whole integer value
// 	  representable by the int typ.
// 	* Otherwise, the type is float64.
func checkFloatLit(parserLit *parser.FloatLit, want Type) (Expr, []Error) {
	lit := &FloatLit{Text: parserLit.Text, L: parserLit.L}
	if _, _, err := lit.Val.Parse(parserLit.Text, 10); err != nil {
		panic("malformed float")
	}
	switch {
	case want == nil:
		fallthrough
	default:
		lit.T = refType(&BasicType{Kind: Float64, L: parserLit.L})
		return deref(lit), nil
	case isIntType(want) || isIntRefType(want):
		var i big.Int
		var errs []Error
		if _, acc := lit.Val.Int(&i); acc != big.Exact {
			errs = append(errs, newError(lit, "%s truncates %s", want, lit.Text))
		}
		intLit, es := checkIntLit(&parser.IntLit{Text: i.String(), L: parserLit.L}, want)
		if len(es) > 0 {
			errs = append(errs, es...)
		}
		return intLit, errs
	case isFloatRefType(want):
		lit.T = copyTypeWithLoc(want, lit.L)
		return lit, nil
	case isFloatType(want):
		lit.T = refType(copyTypeWithLoc(want, lit.L))
		return deref(lit), nil
	}
}

func deref(expr Expr) Expr {
	var t Type
	switch ref := expr.Type().(type) {
	case nil:
		return nil
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
	return &Convert{Kind: Deref, Expr: expr, T: t, L: expr.Loc()}
}
