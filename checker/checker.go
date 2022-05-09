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
	"github.com/eaburns/pea/mod"
	"github.com/eaburns/pea/parser"
)

type checker struct {
	importer            Importer
	maxErrorDepth       int
	verboseNotes        bool
	trimErrorPathPrefix string
}

// Option is an option to Check.
type Option func(*checker)

// UseImporter returns an Option that sets the importer to use for checking.
// By default checker uses an importer loads modules from the current directory.
func UseImporter(imp Importer) Option {
	return func(c *checker) { c.importer = imp }
}

// TrimErrorPathPrefix returns an Option that trims the given prefix
// from file paths reported in error messages.
func TrimErrorPathPrefix(p string) Option {
	return func(c *checker) { c.trimErrorPathPrefix = p }
}

// MaxErrorDepth returns an Option that sets the max nesting depth for reported errors.
// A value of -1 indicates no maximum depth.
func MaxErrorDepth(m int) Option {
	return func(c *checker) { c.maxErrorDepth = m }
}

// VerboseNotes returns an Option that sets whether to
// suppresses truncation of error notes.
// Notes can be truncated to those most likely to be relevant.
// If VerboseNotes is true, there is no truncation.
// By default VerboseNotes is false.
func VerboseNotes(b bool) Option {
	return func(c *checker) { c.verboseNotes = b }
}

// Check does semantic checking, and returns a *Mod on success.
func Check(modPath string, files []*parser.File, opts ...Option) (*Mod, loc.Files, []error) {
	checker := checker{
		maxErrorDepth: 3,
		verboseNotes:  false,
	}
	for _, opt := range opts {
		opt(&checker)
	}
	if checker.importer == nil {
		r := mod.NewRoot(".")
		checker.importer = NewImporter(r, files, checker.trimErrorPathPrefix)
	}

	modPath = cleanImportPath(modPath)

	var errs []Error
	idNames := make(map[string]loc.Loc)
	defs := make(map[parser.Def]Def)
	typeNames := make(map[string]loc.Loc)
	mod := &Mod{Path: modPath}
	var importedMods []*Mod
	for _, parserFile := range files {
		var imports []*Import
		for _, parserImport := range parserFile.Imports {
			m, err := checker.importer.Load(parserImport.Path)
			if err != nil {
				errs = append(errs, newError(parserImport.L, err.Error()))
				continue
			}
			importedMods = append(importedMods, m)
			name := importName(parserImport.Path)
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
				Path: cleanImportPath(parserImport.Path),
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
			parms, fs := makeTypeParms(checker.importer.Files(), parserTypeDef.TypeParms)
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
			if defs[parserTypeDef] == nil {
				// There was an error with this def;
				// it was probably redefined.
				// Just move along.
				continue
			}
			typeDef := defs[parserTypeDef].(*TypeDef)
			t, fs := _makeType(typeDef, parserTypeDef.Type, false, false)
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
				if defs[parserDef] == nil {
					// There was an error with this def;
					// it was probably redefined.
					// Just move along.
					break
				}
				typeDef := defs[parserDef].(*TypeDef)
				typeDef.Type = instType(typeDef.Type)
			case *parser.VarDef:
				origErrorCount := len(errs)
				name := parserDef.Name.Name
				if name != "_" {
					if prev, ok := idNames[name]; ok {
						errs = append(errs, redef(parserDef.L, name, prev))
						continue
					}
					idNames[name] = parserDef.L
				}
				t, es := makeType(file, parserDef.Type)
				if len(es) > 0 {
					errs = append(errs, es...)
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
				defs[parserDef] = varDef
				if len(errs) == origErrorCount {
					// Only add to the mod defs list used for scope lookup
					// if there were no errors in the variable signature.
					// If there was an error, the types likely contain nil,
					// which downstream code assumes is non-nil.
					mod.Defs = append(mod.Defs, varDef)
				}
			case *parser.FuncDef:
				origErrorCount := len(errs)
				typeParms := findTypeParms(checker.importer.Files(), parserDef)
				funDef := &FuncDef{
					File:      file,
					Mod:       modPath,
					Name:      parserDef.Name.Name,
					TypeParms: typeParms,
					Exp:       parserDef.Exp,
					L:         parserDef.L,
				}
				var es []Error
				if funDef.Parms, es = makeFuncParms(funDef, parserDef.Parms); len(es) > 0 {
					errs = append(errs, es...)
				}
				if funDef.Ret, es = makeType(funDef, parserDef.Ret); len(es) > 0 {
					errs = append(errs, es...)
				}
				if funDef.Ret == nil {
					funDef.Ret = &StructType{L: parserDef.L}
				}
				if funDef.Iface, es = makeFuncDecls(funDef, parserDef.Iface); len(es) > 0 {
					errs = append(errs, es...)
				}
				defs[parserDef] = funDef
				if len(errs) == origErrorCount {
					// Only add to the mod defs list used for scope lookup
					// if there were no errors in the function signature.
					// If there was an error, the types likely contain nil,
					// which downstream code assumes is non-nil.
					mod.Defs = append(mod.Defs, funDef)
				}
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
			err.done(&checker)
			es = append(es, err)
		}
		return nil, nil, es
	}

	// TODO: only limit sub depth of already-seenfunctions.
	//
	// Currently we limit to 5 rounds of substitution.
	// This means if there is a chain of 5 functions
	// one calls another calls another,
	// and they have type parameters, this will error.
	//
	// Actually 5 is not an unreasonable call depth,
	// and this can happen in practice,
	// for example, with type-parameterized data structs.
	//
	// Instead, what we want to do is catch and break
	// infinite recursions.
	// A recursion only happens between
	// functions definitions that have already been seen.
	//
	// Instead of doing N rounds,
	// we should do infinite rounds,
	// but track a round-count for each func def substituted,
	// and break that at N.
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

	mod.Deps = checker.importer.Deps()

	return mod, checker.importer.Files(), nil
}

func importName(path string) string {
	if i := strings.LastIndex(path, "//"); i >= 0 {
		return strings.Replace(path[i+2:], "/", "#", -1)
	}
	return filepath.Base(path)
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
	return _makeType(x, parserType, true, false)
}

func _makeType(x scope, parserType parser.Type, inst, allowUnboundForTest bool) (typ Type, errs []Error) {
	switch parserType := parserType.(type) {
	case nil:
		return nil, nil
	case *parser.RefType:
		typ, errs = _makeType(x, parserType.Type, inst, allowUnboundForTest)
		typ = &RefType{Type: typ, L: parserType.L}
	case *parser.NamedType:
		var args []Type
		for _, parserArg := range parserType.Args {
			arg, fs := _makeType(x, parserArg, inst, allowUnboundForTest)
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
			imp := findImport(x, parserType.Mod.Name)
			if imp == nil {
				errs = append(errs, notFound(modName, modLoc))
				return nil, errs
			}
			x = imp
		}
		name := parserType.Name.Name
		switch types := findType(x, args, name, parserType.L); {
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
		elemType, errs = _makeType(x, parserType.ElemType, inst, allowUnboundForTest)
		typ = &ArrayType{ElemType: elemType, L: parserType.L}
	case *parser.StructType:
		var fields []FieldDef
		for _, parserField := range parserType.Fields {
			t, fs := _makeType(x, parserField.Type, inst, allowUnboundForTest)
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
			t, fs := _makeType(x, parserCase.Type, inst, allowUnboundForTest)
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
		parms, fs := _makeTypes(x, parserType.Parms, inst, allowUnboundForTest)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		ret, fs := _makeType(x, parserType.Ret, inst, allowUnboundForTest)
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		if ret == nil {
			ret = &StructType{L: parserType.L}
		}
		typ = &FuncType{Parms: parms, Ret: ret, L: parserType.L}
	case parser.TypeVar:
		name := parserType.Name
		switch types := findType(x, nil, name, parserType.L); {
		case len(types) == 0:
			if allowUnboundForTest {
				typ = &TypeVar{Name: name, L: parserType.L}
			} else {
				errs = append(errs, notFound(name, parserType.L))
			}
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
	return _makeTypes(x, parserTypes, true, false)
}

func _makeTypes(x scope, parserTypes []parser.Type, inst bool, allowUnboundForTest bool) ([]Type, []Error) {
	var errs []Error
	var types []Type
	for _, parserType := range parserTypes {
		t, fs := _makeType(x, parserType, inst, allowUnboundForTest)
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
	if parserDef.Expr == nil {
		// This is a variable declaration.
		// If the type was erronous, that error is reported elsewhere.
		return nil
	}
	expr, errs := checkAndConvertExpr(def, parserDef.Expr, patternOrAny(def.T))
	if def.T == nil || expr.Type() == nil {
		return errs
	}
	def.Expr = &Call{
		Func: &Builtin{
			Op:    Assign,
			Parms: []Type{refType(def.T), expr.Type()},
			Ret:   expr.Type(),
		},
		Args: []Expr{&Var{Def: def, T: refType(def.T), L: def.L}, expr},
		T:    &StructType{L: def.L},
		L:    def.L,
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
	def.Exprs, errs = checkExprs(def, true, parserDef.Exprs, any())
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
	def.Exprs, errs = checkExprs(def, true, parserDef.Exprs, any())
	return errs
}

func checkExpr(x scope, parserExpr parser.Expr, pat typePattern) (Expr, []Error) {
	switch parserExpr := parserExpr.(type) {
	case *parser.Call:
		return checkCall(x, parserExpr, pat)
	case *parser.Convert:
		return checkConvert(x, parserExpr)
	case *parser.SubExpr:
		return checkExpr(x, parserExpr.Expr, pat)
	case *parser.ArrayLit:
		return checkArrayLit(x, parserExpr, pat)
	case *parser.StructLit:
		return checkStructLit(x, parserExpr, pat)
	case *parser.UnionLit:
		return checkUnionLit(x, parserExpr, pat)
	case *parser.BlockLit:
		return checkBlockLit(x, parserExpr, pat)
	case *parser.StrLit:
		return checkStrLit(parserExpr, pat)
	case *parser.CharLit:
		return checkCharLit(parserExpr, pat)
	case *parser.IntLit:
		return checkIntLit(parserExpr, pat)
	case *parser.FloatLit:
		return checkFloatLit(parserExpr, pat)
	case *parser.ModSel:
		return checkModSel(x, parserExpr, true, pat)
	case parser.Ident:
		return checkID(x, parserExpr, true, pat)
	default:
		panic(fmt.Sprintf("impossible expr type: %T", parserExpr))
	}
}

func checkAndConvertExpr(x scope, parserExpr parser.Expr, pat typePattern) (Expr, []Error) {
	expr, errs := checkExpr(x, parserExpr, pat)
	if expr == nil || !pat.isGroundType() {
		return expr, errs
	}
	expr, err := convert(expr, pat.groundType(), false)
	if err != nil {
		errs = append(errs, err)
	}
	return expr, errs
}

// newLocals indicates whether new local variables may be created.
// pat is the type pattern for the last expression.
func checkExprs(x scope, newLocals bool, parserExprs []parser.Expr, pat typePattern) ([]Expr, []Error) {
	var errs []Error
	var exprs []Expr
	for i, parserExpr := range parserExprs {
		if call, ok := isAssign(parserExpr); ok && newLocals {
			if id, ok := isNewID(x, call.Args[0]); ok {
				local, assign, es := newLocalAssign(x, call, id)
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
		if i == len(parserExprs)-1 && pat.isGroundType() && !isEmptyStruct(pat.groundType()) {
			expr, es = checkExpr(x, parserExpr, pat)
			if expr != nil && !isBuiltin(expr, Panic) && !isBuiltin(expr, Return) {
				var err Error
				if expr, err = convert(expr, pat.groundType(), false); err != nil {
					es = append(es, err)
				}
			}
		} else {
			expr, es = checkExpr(x, parserExpr, any())
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
	for _, id := range findIDs(x, parserID.Name) {
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

func newLocalAssign(x scope, call *parser.Call, id parser.Ident) (*LocalDef, Expr, []Error) {
	expr, errs := checkExpr(x, call.Args[1], any())
	if expr == nil {
		return nil, nil, errs
	}
	local := newLocal(x, id.Name, expr.Type(), id.L)
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
			T: &StructType{L: call.L},
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

	if isEmptyStruct(dstType) && !isEmptyStruct(srcType) {
		return &Convert{
			Kind:     Drop,
			Explicit: explicit,
			Expr:     expr,
			T:        dstType,
			L:        expr.Loc(),
		}, nil
	}

	if isLiteralType(srcType) {
		if dstLitType := literalType(dstType); dstLitType != nil {
			dstType = dstLitType
		}
		if isUnionSubsetConvertible(dstType, srcType) {
			// Only consider union convert if the valpe types are not equal;
			// if they are equal, then a noop convert is fine.
			if !eqType(valueType(srcType), valueType(dstType)) {
				return unionConvert(expr, typ, explicit), nil
			}
		}
	} else if isLiteralType(dstType) {
		if srcLitType := literalType(srcType); srcLitType != nil {
			srcType = srcLitType
		}
		if isUnionSubsetConvertible(dstType, srcType) {
			// Only consider union convert if the valpe types are not equal;
			// if they are equal, then a noop convert is fine.
			if !eqType(valueType(srcType), valueType(dstType)) {
				return unionConvert(expr, typ, explicit), nil
			}
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

func unionConvert(expr Expr, typ Type, explicit bool) Expr {
	if unionLit, ok := expr.(*UnionLit); ok {
		unionLit.T = typ
		return unionLit
	}
	return &Convert{
		Kind:     UnionConvert,
		Explicit: explicit,
		Expr:     expr,
		T:        typ,
		L:        expr.Loc(),
	}
}

func checkCall(x scope, parserCall *parser.Call, pat typePattern) (Expr, []Error) {
	switch fun := parserCall.Fun.(type) {
	case parser.Ident:
		ids := findIDs(x, fun.Name)
		return resolveIDCall(x, nil, fun, parserCall, pat, ids)
	case *parser.ModSel:
		imp := findImport(x, fun.Mod.Name)
		if imp == nil {
			return nil, []Error{notFound(fun.Mod.Name, fun.L)}
		}
		var addMod *Import
		if !imp.Exp {
			addMod = imp
			x = addImportScope(x, imp)
		}
		ids := findIDs(imp, fun.Name.Name)
		return resolveIDCall(x, addMod, fun.Name, parserCall, pat, ids)
	default:
		return checkExprCall(x, parserCall, pat)
	}
}

func resolveIDCall(x scope, mod *Import, parserID parser.Ident, parserCall *parser.Call, pat typePattern, ids []id) (Expr, []Error) {
	if pat.isGroundType() {
		if defType, ok := pat.groundType().(*DefType); ok && mod == nil {
			ids = append(ids, adModuleIDs(x, defType.Def.Mod, parserID.Name)...)
		}
	}

	funcs, notes := filterToFuncs(ids, parserID.L)
	funcs, ns := filterByArity(funcs, len(parserCall.Args))
	notes = append(notes, ns...)
	if len(funcs) > 0 {
		markVerbose(notes)
	}

	var args []Expr
	for i, parserArg := range parserCall.Args {
		arg, fs, ns, errs := checkArgAndFilter(x, parserID, i, parserArg, funcs)
		if len(errs) > 0 {
			errs = append(errs, checkArgsFallback(x, parserCall.Args[i+1:])...)
			return &Call{L: parserCall.L}, errs
		}
		funcs = fs
		args = append(args, arg)
		notes = append(notes, ns...)

		if mod == nil {
			// Do argument-dependent lookup if the call is not tagged with a module.
			fs, ns = adLookup(x, parserID, len(parserCall.Args), args, pat)
			funcs = append(funcs, fs...)
			notes = append(notes, ns...)
		}
		if len(funcs) > 0 {
			markVerbose(notes)
		}
	}

	if pat.isGroundType() {
		var ns []note
		funcs, ns = filterByReturn(funcs, pat.groundType())
		notes = append(notes, ns...)
	} else {
		funcs, ns = filterUngroundReturns(funcs)
		notes = append(notes, ns...)
	}
	if len(funcs) > 0 {
		markVerbose(notes)
	}

	funcs, ns = filterIfaceConstraints(x, parserCall.L, funcs)
	notes = append(notes, ns...)

	switch {
	case len(funcs) == 0:
		err := notFound(parserID.Name, parserID.L)
		err.setNotes(notes)
		return &Call{Args: args, L: parserCall.L}, []Error{err}
	case len(funcs) > 1:
		err := ambiguousCall(parserID.Name, funcs, parserID.L)
		return &Call{Args: args, L: parserCall.L}, []Error{err}
	}

	fun := useFunc(x, parserCall.L, funcs[0])
	ret := fun.ret().groundType()
	for i, arg := range args {
		p := fun.parm(i).groundType()
		args[i], _ = convert(arg, p, false)
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
	return expr, nil
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

func checkArgAndFilter(x scope, parserID parser.Ident, i int, parserArg parser.Expr, funcs []Func) (Expr, []Func, []note, []Error) {
	var notes []note
	if t := commonGroundParmType(funcs, i); t != nil {
		arg, errs := checkExpr(x, parserArg, pattern(t))
		if len(errs) > 0 {
			return nil, nil, nil, errs
		}
		arg, err := convert(arg, t, false)
		if err != nil {
			funcs, notes = filterByGroundedArg(funcs, i, arg)
		}
		return arg, funcs, notes, errs
	}
	var arg Expr
	var errs []Error
	// If this is the LHS of an assignment to an Ident,
	// don't mark it as "used" if it is a local variable.
	if lhs, ok := parserArg.(parser.Ident); ok && i == 0 && parserID.Name == ":=" {
		arg, errs = checkID(x, lhs, false, any())
	} else {
		arg, errs = checkAndConvertExpr(x, parserArg, any())
	}
	if len(errs) > 0 {
		return arg, nil, nil, errs
	}
	funcs, notes = filterByGroundedArg(funcs, i, arg)
	return arg, funcs, notes, errs
}

func adLookup(x scope, parserID parser.Ident, arity int, args []Expr, pat typePattern) ([]Func, []note) {
	// Only called after args have been added, so args cannot be empty.
	defType, ok := args[len(args)-1].Type().(*DefType)
	if !ok {
		return nil, nil
	}
	if pat.isGroundType() {
		if dt, ok := pat.groundType().(*DefType); ok && defType.Def.Mod == dt.Def.Mod {
			return nil, nil
		}
	}
	for _, prevArg := range args[:len(args)-1] {
		dt, ok := prevArg.Type().(*DefType)
		if ok && defType.Def.Mod == dt.Def.Mod {
			// Already added by ADL on previous argument.
			return nil, nil
		}
	}

	ids := adModuleIDs(x, defType.Def.Mod, parserID.Name)
	funcs, notes := filterToFuncs(ids, parserID.L)
	var ns []note
	funcs, ns = filterByArity(funcs, arity)
	notes = append(notes, ns...)

	for i, arg := range args {
		funcs, ns = filterByGroundedArg(funcs, i, arg)
		notes = append(notes, ns...)
	}
	return funcs, notes
}

// adModuleIDs returns IDs from a module for argument-dependent lookup.
// If the importPath of the module is either the current module
// or is capital Imported into the current module, nil is returned.
// Otherwise, the IDs from the module matching idName are returned.
func adModuleIDs(x scope, importPath, idName string) []id {
	if isModuleInScope(x, importPath) {
		// These are already in scope, we don't need to re-add them.
		return nil
	}
	file := file(x)
	if file == nil {
		return nil
	}
	for _, imp := range file.Imports {
		if imp.Path != importPath {
			continue
		}
		if imp.Exp {
			break
		}
		return findIDs(imp, idName)
	}
	return nil
}

func checkArgsFallback(x scope, parserArgs []parser.Expr) []Error {
	var errs []Error
	for _, parserArg := range parserArgs {
		_, es := checkAndConvertExpr(x, parserArg, any())
		errs = append(errs, es...)
	}
	return errs
}

func commonGroundParmType(funcs []Func, i int) Type {
	var t Type
	for _, f := range funcs {
		if !f.parm(i).isGroundType() {
			return nil
		}
		parmType := f.parm(i).groundType()
		if t == nil {
			t = parmType
			continue
		}
		if !eqType(t, parmType) {
			return nil
		}
	}
	return t
}

func filterByGroundedArg(funcs []Func, i int, arg Expr) ([]Func, []note) {
	if arg.Type() == nil {
		// There was an error checking the argument.
		// Just silently filter out all functions.
		return nil, nil
	}
	var n int
	var notes []note
	for _, f := range funcs {
		// For the moment, we don't bother trying to unify if the type is already grounded.
		// The difference is just in who reports the error: unify or converson.
		// TODO: always unify types and change the expected error in tests.
		if pat := f.parm(i); !pat.isGroundType() {
			bind := unify(pat, arg.Type())
			if bind == nil {
				n := newNote("%s: cannot unify argument %d: %s and %s", f, i, pat, arg.Type()).setLoc(arg)
				notes = append(notes, n)
				continue
			}
			if n := f.sub(bind); n != nil {
				notes = append(notes, n)
				continue
			}
		}
		parmType := f.parm(i).groundType()
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

func filterByReturn(funcs []Func, typ Type) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		// For the moment, we don't bother trying to unify if the type is already grounded.
		// The difference is just in who reports the error: unify or converson.
		// TODO: always unify types and change the expected error in tests.
		if pat := f.ret(); !pat.isGroundType() {
			bind := unify(pat, typ)
			if bind == nil {
				// TODO: the location here should be the call location, not the want location.
				n := newNote("%s: cannot unify return: %s and %s", f, pat, typ).setLoc(typ)
				notes = append(notes, n)
				continue
			}
			if n := f.sub(bind); n != nil {
				notes = append(notes, n)
				continue
			}
		}
		retType := f.ret().groundType()

		// If there is only 1 function, don't bother checking conversion.
		// If it cannot convert, it will fail upstream.
		// The reason for this special case is that canImplicitConvert
		// only accepts implicit conversions.
		// If the parent node is an explicit conversion,
		// we want it to be accepted in the common case
		// that there is only one function overload acceptable at this point.
		if len(funcs) > 1 && !canImplicitConvert(retType, typ) {
			notes = append(notes, newNote("%s: cannot convert returned %s to %s", f, retType, typ).setLoc(f))
			continue
		}
		funcs[n] = f
		n++
	}
	return funcs[:n], notes
}

func filterUngroundReturns(funcs []Func) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		if f.ret().isGroundType() {
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
		useFuncInst(x, l, fun.Def, nil, nil)
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

// canImplicitConvert returns whether the src type
// can implicitly convert to the dst type.
func canImplicitConvert(src, dst Type) bool {
	/*
		This works by calling convert()
		on a Deref expression of the src type
		to the dst type.
		Deref allows for &-conversion,
		which works in convert()
		by looking for and pealing off
		a top-level Deref expression.

		This cannot lead to risk of addressing
		a non-addressable expression:

		The only non-addressable expression
		is a &-conversion.
		If S is the result of an &-conversion,
		then S must be the & of some type, say T:
		S=&T.

		For an implicit conversion to add too many &,
		it must be an implicit &-conversion.
		This means that D = &S, so D = &S = &&T.
		However &&T is not gramatical,
		so there is no way to write such a type D.

		Note that implicit conversions
		cannot convert into a defined type.
		So there is no risk of D being &&T
		written as some defined type U=&T with D=&U,
		because such an implicit conversion
		isn't allowed, and convert will fail.
	*/

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

func checkExprCall(x scope, parserCall *parser.Call, pat typePattern) (Expr, []Error) {
	var errs []Error
	var fun *ExprFunc
	expr, fs := checkExpr(x, parserCall.Fun, any())
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
			arg, fs = checkAndConvertExpr(x, parserArg, pattern(fun.FuncType.Parms[i]))
		} else {
			arg, fs = checkAndConvertExpr(x, parserArg, any())
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

func checkModSel(x scope, parserSel *parser.ModSel, useLocal bool, pat typePattern) (Expr, []Error) {
	imp := findImport(x, parserSel.Mod.Name)
	if imp == nil {
		return nil, []Error{notFound(parserSel.Mod.Name, parserSel.L)}
	}
	parserID := parserSel.Name
	ids := findIDs(imp, parserID.Name)
	return resolveID(x, parserID, useLocal, pat, ids)
}

func checkID(x scope, parserID parser.Ident, useLocal bool, pat typePattern) (Expr, []Error) {
	ids := findIDs(x, parserID.Name)
	return resolveID(x, parserID, useLocal, pat, ids)
}

func resolveID(x scope, parserID parser.Ident, useLocal bool, pat typePattern, ids []id) (Expr, []Error) {
	var n int
	var notFoundNotes []note
	for _, id := range ids {
		if !isGround(id) {
			if !pat.isGroundType() {
				n := newNote("cannot ground %s", id).setLoc(id)
				notFoundNotes = append(notFoundNotes, n)
				continue
			}
			if fail := unifyFunc(x, parserID.L, id.(Func), pat.groundType()); fail != nil {
				notFoundNotes = append(notFoundNotes, fail.note)
				continue
			}
		}
		ids[n] = id
		n++
	}
	ids = ids[:n]

	// If there is more than one ID, filter to only those
	// implicitly convertable to the pattern type.
	// We don't filter in the 1-ID case,
	// to allow explicit conversions to work
	// when the ID is unambiguous.
	// If we filtered here, the explicit conversion
	// would get a "not found" instead of
	// the convertible expression.
	var ambigNotes []note
	if len(ids) > 1 && pat.isGroundType() {
		var n int
		for _, id := range ids {
			expr := idToExpr(id, parserID.L)
			if _, err := convert(expr, pat.groundType(), false); err != nil {
				n := newNote("cannot convert %s (%s) to type %s", expr, expr.Type(), pat.groundType()).setLoc(expr)
				notFoundNotes = append(notFoundNotes, n)
				continue
			}
			ambigNotes = append(ambigNotes, newNote(id.String()).setLoc(id))
			ids[n] = id
			n++
		}
		ids = ids[:n]
	}
	if len(ids) > 1 && !pat.isGroundType() {
		for _, id := range ids {
			note := newNote(id.String()).setLoc(id)
			ambigNotes = append(ambigNotes, note)
		}
	}

	switch {
	case len(ids) == 0:
		err := notFound(parserID.Name, parserID.L)
		err.setNotes(notFoundNotes)
		return nil, []Error{err}
	case len(ids) > 1:
		err := newError(parserID.L, "%s is ambiguous for pattern %s", parserID.Name, pat)
		err.setNotes(ambigNotes)
		return nil, []Error{err}
	default:
		id := useID(x, parserID.L, useLocal, ids[0])
		expr := idToExpr(id, parserID.L)
		return expr, nil
	}
}

func isGround(id id) bool {
	switch id := id.(type) {
	case Func:
		for i := 0; i < id.arity(); i++ {
			if !id.parm(i).isGroundType() {
				return false
			}
		}
		return id.ret().isGroundType()
	default:
		return true
	}
}

func useID(x scope, l loc.Loc, useLocal bool, id id) id {
	switch id := id.(type) {
	case *VarDef:
		useVar(x, l, id)
		return id
	case *ParmDef:
		return capture(x, id)
	case *LocalDef:
		if useLocal {
			id.used = true
		}
		return capture(x, id)
	case *BlockCap:
		return capture(x, id)
	case *FuncInst:
		useFuncInst(x, l, id.Def, nil, nil)
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
		return wrapCallInBlock(id, id.ret().groundType(), l)
	default:
		panic(fmt.Sprintf("impossible id type: %T", id))
	}
}

// Convert a Func f into a block literal of the form
// 	(p…){ res := f(p…), res }
// where p… are parameters with the same type as f.
//
// The call is assigned to a varible,
// and that variable is the result of the block.
// This allows wantRet to have one more reference
// than the normal result of calling fun.Ret.
// This reference feature is needed by iface call substitution,
// where the return of an iface function may need to have
// up to one additional reference added to it.
func wrapCallInBlock(fun Func, wantRet Type, l loc.Loc) Expr {
	var parms []Type
	for i := 0; i < fun.arity(); i++ {
		p := fun.parm(i).groundType()
		parms = append(parms, p)
	}
	retType := fun.ret().groundType()
	call := &Call{
		Func: fun,
		Args: make([]Expr, len(parms)),
		T:    &RefType{Type: retType, L: l},
		L:    l,
	}
	localDef := &LocalDef{
		Name: "ret",
		T:    call.Type(),
		L:    l,
	}
	assign := &Call{
		Func: &Builtin{
			Op:    Assign,
			Parms: []Type{refType(localDef.T), call.Type()},
			Ret:   call.Type(),
		},
		Args: []Expr{
			&Local{Def: localDef, T: refType(localDef.T), L: l},
			call,
		},
		T: &StructType{L: l},
		L: l,
	}
	local := deref(&Local{Def: localDef, T: refType(localDef.T), L: l})
	result, err := convert(local, wantRet, false)
	if err != nil {
		panic(fmt.Sprintf("impossible: %s", err))
	}
	blk := &BlockLit{
		Parms:  make([]ParmDef, len(parms)),
		Locals: []*LocalDef{localDef},
		Ret:    wantRet,
		Exprs:  []Expr{assign, result},
		T:      refType(&FuncType{Parms: parms, Ret: wantRet, L: l}),
		L:      l,
	}
	for i := range parms {
		blk.Parms[i].Name = fmt.Sprintf("x%d", i)
		blk.Parms[i].T = parms[i]
		blk.Parms[i].L = l
		call.Args[i] = deref(&Parm{
			Def: &blk.Parms[i],
			T:   refType(parms[i]),
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
	// typ may be nil if there was an error in the type, so use patternOrAny.
	expr, es := checkExpr(x, parserConvert.Expr, patternOrAny(typ))
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

	if eqType(definedBaseType(expr.Type()), definedBaseType(typ)) {
		// Explicit conversion between equivalent defined types is OK and a noop.
		cvt.Expr = expr
		cvt.Kind = Noop
		return cvt, errs
	}

	if isUnionSubsetConvertible(literalType(typ), literalType(expr.Type())) {
		return unionConvert(expr, typ, true), nil
	}

	switch basicKind(typ) {
	case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Float32, Float64:
		if !isRefType(typ) && (isIntType(expr.Type()) || isFloatType(expr.Type())) {
			cvt.Kind = NumConvert
			return cvt, errs
		}
	case UintRef:
		if !isRefType(typ) && isRefType(definedBaseType(expr.Type())) {
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

// checkArrayLit checks an array literal.
//
// If the pattern's type is an array type or reference to an array type,
// the 0th element is checked with the pattern's array element pattern.
// Subsequent expressions are checked with the pattern of element 0's type.
//
// Otherwise, it is an error if there are no expressions in the literal.
// The 0th element is checked with a new pattern with a single bound type variable.
// Subsequent expressions are checked with the pattern of element 0's type.
//
// The type of the literal is the unification of a literal array type with the pattern.
// The element type of the literal array is the type of expression 0, or
// if there are no elements and the pattern is an array or array reference type,
// then the pattern's array element type.
// It is an error if the unification fails or contains type variables bound in the pattern.
//
// The returned Expr is never nil even if there are errors.
func checkArrayLit(x scope, parserLit *parser.ArrayLit, pat typePattern) (Expr, []Error) {
	lit := &ArrayLit{Array: &ArrayType{L: parserLit.L}, L: parserLit.L}
	elemPat := any()
	if isArrayType(pat.typ) || isArrayRefType(pat.typ) {
		ary := trim1Ref(literalType(pat.typ)).(*ArrayType)
		elemPat = pat.withType(ary.ElemType)
	} else if len(parserLit.Exprs) == 0 {
		return lit, []Error{newError(lit, "cannot infer array type")}
	}
	var errs []Error
	for _, parserExpr := range parserLit.Exprs {
		expr, es := checkAndConvertExpr(x, parserExpr, elemPat)
		if len(es) > 0 {
			errs = append(errs, es...)
		}
		// TODO: there should be no nil exprs
		if expr == nil {
			continue
		}
		lit.Elems = append(lit.Elems, expr)
		if expr.Type() != nil {
			elemPat = pat.withType(expr.Type())
		}
	}
	if len(errs) > 0 {
		return lit, errs
	}
	lit.Array.ElemType = elemPat.typ
	switch bind := unify(pat, lit.Array); {
	case bind == nil:
		return lit, []Error{newError(lit, "cannot unify %s with %s", lit.Array, pat)}
	case isRefType(literalType(pat.typ)):
		lit.T = subType(bind, pat.typ)
	default:
		// The underlying literal is always a reference, so add a ref here,
		// but we will deref it below before returning the expression.
		lit.T = subType(bind, refType(pat.typ))
	}
	if !pat.withType(lit.T).isGroundType() {
		return lit, []Error{newError(lit, "cannot infer array type, got %s", lit.T)}
	}
	if isRefType(literalType(pat.typ)) {
		return lit, nil
	}
	return deref(lit), nil
}

// checkStructLit checks a struct literal.
//
// If the pattern's type is a struct type or reference to a struct type
// and the number, names, and order of fields match the literal expression, then
// the field expressions are checked with the pattern's corresponding field pattern.
// Otherwise, the field expressions are checked with a new pattern
// with a single bound type variable.
//
// The type of the literal is the unification of a literal struct type with the pattern.
// The field types of the literal struct type used in the unification
// are the types of each corresponding field expression.
// It is an error if the unification fails or contains type variables bound in the pattern.
//
// The returned Expr is never nil even if there are errors.
func checkStructLit(x scope, parserLit *parser.StructLit, pat typePattern) (Expr, []Error) {
	lit := &StructLit{L: parserLit.L}
	var fieldPats []typePattern
	if isStructType(pat.typ) || isStructRefType(pat.typ) {
		str := trim1Ref(literalType(pat.typ)).(*StructType)
		if len(str.Fields) == len(parserLit.FieldVals) {
			for i := range parserLit.FieldVals {
				if str.Fields[i].Name != parserLit.FieldVals[i].Name.Name {
					fieldPats = nil
					break
				}
				fieldPats = append(fieldPats, pat.withType(str.Fields[i].Type))
			}
		}
	}
	if fieldPats == nil {
		for range parserLit.FieldVals {
			fieldPats = append(fieldPats, any())
		}
	}
	var errs []Error
	lit.Struct = &StructType{L: lit.L}
	for i, parserField := range parserLit.FieldVals {
		expr, fs := checkAndConvertExpr(x, parserField.Val, fieldPats[i])
		if len(fs) > 0 {
			errs = append(errs, fs...)
		}
		// TODO: there should be no nil exprs
		if expr == nil {
			continue
		}
		lit.Fields = append(lit.Fields, expr)
		lit.Struct.Fields = append(lit.Struct.Fields, FieldDef{
			Name: parserField.Name.Name,
			Type: expr.Type(),
			L:    parserField.L,
		})
	}
	if len(errs) > 0 {
		return lit, errs
	}
	switch bind := unify(pat, lit.Struct); {
	case bind == nil:
		return lit, []Error{newError(lit, "cannot unify %s with %s", lit.Struct, pat)}
	case isRefType(literalType(pat.typ)):
		lit.T = subType(bind, pat.typ)
	default:
		// The underlying literal is always a reference, so add a ref here,
		// but we will deref it below before returning the expression.
		lit.T = subType(bind, refType(pat.typ))
	}
	if !pat.withType(lit.T).isGroundType() {
		// I believe this is impossible. If not, it should be an error.
		// However, I'd like to see a case that produces this panic
		// before bothering to return the error.
		panic("impossible")
		// return lit, []Error{newError(lit, "cannot infer struct type, got %s", lit.T)}
	}
	if isRefType(literalType(pat.typ)) {
		return lit, nil
	}
	return deref(lit), nil
}

// checkUnionLit checks a union literal.
//
// If the literal has an expresson and
// the pattern's type is a union type or reference to a union type and
// the union type has a case that is typed,
// the expression is checked with the pattern's corresponding case pattern.
// Otherwise, the case expression is checked with a new pattern
// with a single bound type variable.
//
// If the pattern is a union type or a reference to a union type,
// the type of the literal is the unification with the pattern
// of the literal union type of the pattern
// with the type of the case corresponding to the literal, if typed,
// replaced with the type of the literal expression.
//
// Otherwise if the pattern is not a union type or reference to a union type,
// the type of the literal is the unification with the pattern
// of the literal union type that consists of just the literal case;
// if the literal has an expression,
// the case is typed with the type of the expression,
// otherwise it is untyped.
//
// It is an error if the unification fails or contains type variables bound in the pattern.
//
// The returned Expr is never nil even if there are errors.
func checkUnionLit(x scope, parserLit *parser.UnionLit, pat typePattern) (Expr, []Error) {
	caseName := parserLit.CaseVal.Name.Name
	lit := &UnionLit{L: parserLit.L}
	exprPat := any()
	if isUnionType(pat.typ) || isUnionRefType(pat.typ) {
		uni := trim1Ref(literalType(pat.typ)).(*UnionType)
		if c := findCase(caseName, uni); c != nil && (c.Type == nil) == (parserLit.CaseVal.Val == nil) {
			if c.Type != nil {
				exprPat = pat.withType(c.Type)
			}
			// Copy the type, because we will later replace the type for the expr case
			// with the type of the checked expression.
			lit.Union = copyTypeWithLoc(uni, pat.typ.Loc()).(*UnionType)
		}
	}
	if lit.Union == nil {
		lit.Union = &UnionType{
			Cases: []CaseDef{{Name: caseName, L: parserLit.CaseVal.Name.L}},
			L:     lit.L,
		}
	}
	lit.Case = findCase(caseName, lit.Union)
	if parserLit.CaseVal.Val != nil {
		var errs []Error
		lit.Val, errs = checkAndConvertExpr(x, parserLit.CaseVal.Val, exprPat)
		if len(errs) > 0 {
			return lit, errs
		}
		lit.Case.Type = lit.Val.Type()
	}
	switch bind := unify(pat, lit.Union); {
	case bind == nil:
		return lit, []Error{newError(lit, "cannot unify %s with %s", lit.Union, pat)}
	case isRefType(literalType(pat.typ)):
		lit.T = subType(bind, pat.typ)
	default:
		// The underlying literal is always a reference, so add a ref here,
		// but we will deref it below before returning the expression.
		lit.T = subType(bind, refType(pat.typ))
	}
	if !pat.withType(lit.T).isGroundType() {
		return lit, []Error{newError(lit, "cannot infer union type, got %s", lit.T)}
	}
	if isRefType(literalType(pat.typ)) {
		return lit, nil
	}
	return deref(lit), nil
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
//
// If the pattern is not a function type or reference to a function type,
// it is an error if any parameters of the literal do not have explicit types.
//
// The expressions in the literal body are checked with a new pattern
// containing a single, bound type variable.
// However, if the pattern is a function type or reference to a function type,
// the last expression in the literal body is checked with the type
// of the pattern's function return pattern.
//
// The type of the literal is the unification of a literal function type with the pattern.
// The parameter types of the literal function type used in the unification
// are the explicit parameter type for each parameter where specified;
// for parameters without an explicitly specified type,
// the type is that of the pattern's corresponding parameter
// (recall that the pattern must be a function or reference to a function
// in order for non-explicitly-typed parameters to be allowed).
// The return type of the function literal is the type of the last expression in the body,
// or if the pattern is a function or reference to a function
// and the pattern's function return type is [.], then the return type is [.],
// or if the pattern's function return type a ground type in the pattern
// and the last expression in the body  is a panic or return,
// then the type is the pattern's return type.
// It is an error if the unification fails or contains type variables bound in the pattern.
//
// TODO: move [.] special case into unify.
// TODO: panic/return special case for block literals goes away by adding !-type from return and panic, and moving the handling into unify.
//
// The returned Expr is never nil even if there are errors.
func checkBlockLit(x scope, parserLit *parser.BlockLit, pat typePattern) (Expr, []Error) {
	lit := &BlockLit{L: parserLit.L}
	var errs []Error
	lit.Parms, errs = makeFuncParms(x, parserLit.Parms)
	retPat := any()
	funType := &FuncType{L: lit.L}
	if isFuncType(pat.typ) || isFuncRefType(pat.typ) {
		fun := trim1Ref(literalType(pat.typ)).(*FuncType)
		if len(fun.Parms) == len(lit.Parms) {
			retPat = pat.withType(fun.Ret)
			for i := range lit.Parms {
				p := &lit.Parms[i]
				if lit.Parms[i].T == nil {
					lit.Parms[i].T = fun.Parms[i]
					continue
				}
				parmPat := pat.withType(fun.Parms[i])
				bind := unify(parmPat, p.T)
				if bind == nil {
					err := newError(p, "cannot unify %s with %s", p.T, parmPat)
					errs = append(errs, err)
				}
				lit.Parms[i].T = subType(bind, parmPat.typ)
			}
		}
	}
	for _, p := range lit.Parms {
		if p.T == nil || !pat.withType(p.T).isGroundType() {
			err := newError(p.L, "cannot infer type of parameter %s", p.Name)
			errs = append(errs, err)
			continue
		}
		funType.Parms = append(funType.Parms, p.T)
	}
	var es []Error
	x = &blockLitScope{parent: x, BlockLit: lit}
	lit.Exprs, es = checkExprs(x, true, parserLit.Exprs, retPat)
	if len(es) > 0 {
		errs = append(errs, es...)
	}
	for _, l := range lit.Locals {
		if l.Name != "_" && !l.used {
			errs = append(errs, newError(l, "%s unused", l.Name))
		}
	}
	if isFuncType(pat.typ) || isFuncRefType(pat.typ) {
		fun := trim1Ref(literalType(pat.typ)).(*FuncType)
		if isEmptyStruct(fun.Ret) ||
			(pat.withType(fun.Ret).isGroundType() && endsInReturnOrPanic(lit.Exprs)) {
			lit.Ret = copyTypeWithLoc(fun.Ret, lit.L)
		}
	}
	if lit.Ret == nil {
		if len(lit.Exprs) > 0 {
			lit.Ret = lit.Exprs[len(lit.Exprs)-1].Type()
		}
		if lit.Ret == nil {
			lit.Ret = &StructType{L: lit.L}
		}
	}
	funType.Ret = lit.Ret

	if len(errs) > 0 {
		return lit, errs
	}
	switch bind := unify(pat, funType); {
	case bind == nil:
		return lit, []Error{newError(lit, "cannot unify %s with %s", funType, pat)}
	case isRefType(literalType(pat.typ)):
		lit.T = subType(bind, pat.typ)
	default:
		// The underlying literal is always a reference, so add a ref here,
		// but we will deref it below before returning the expression.
		lit.T = subType(bind, refType(pat.typ))
	}
	if !pat.withType(lit.T).isGroundType() {
		// I believe this is impossible. If not, it should be an error.
		// However, I'd like to see a case that produces this panic
		// before bothering to return the error.
		panic("impossible")
		//return lit, []Error{newError(lit, "cannot infer block type, got %s", lit.T)}
	}
	if isRefType(literalType(pat.typ)) {
		return lit, nil
	}
	return deref(lit), nil
}

func endsInReturnOrPanic(es []Expr) bool {
	if len(es) == 0 {
		return false
	}
	e := es[len(es)-1]
	for {
		if c, ok := e.(*Convert); !ok {
			break
		} else {
			e = c.Expr
		}
	}
	c, ok := e.(*Call)
	if !ok {
		return false
	}
	b, ok := c.Func.(*Builtin)
	if !ok {
		return false
	}
	return b.Op == Return || b.Op == Panic
}

// checkStrLit checks a string literal.
//
// If the pattern's type is a string type or reference to a string type,
// the type of the literal is the pattern's type.
// Otherwise the type of the literal is the unification
// of the built-in type string and the pattern.
//
// The returned Expr is never nil even if there are errors.
func checkStrLit(parserLit *parser.StrLit, pat typePattern) (Expr, []Error) {
	lit := &StrLit{Text: parserLit.Data, L: parserLit.L}
	switch {
	case isStringRefType(pat.typ):
		lit.T = copyTypeWithLoc(pat.groundType(), lit.L)
		return lit, nil
	case isStringType(pat.typ):
		lit.T = refType(copyTypeWithLoc(pat.groundType(), lit.L))
		return deref(lit), nil
	default:
		bind := unify(pat, &BasicType{Kind: String, L: parserLit.L})
		switch {
		case bind == nil:
			return lit, []Error{newError(lit, "cannot unify string with %s", pat)}
		case isRefType(literalType(pat.typ)):
			lit.T = subType(bind, pat.typ)
			return lit, nil
		default:
			lit.T = subType(bind, refType(pat.typ))
			return deref(lit), nil
		}
	}
}

// checkCharLit checks a character literal.
//
// The same as an integer literal, but with the default type being int32, not int.
func checkCharLit(parserLit *parser.CharLit, pat typePattern) (Expr, []Error) {
	parserIntLit := &parser.IntLit{
		Text: strconv.FormatInt(int64(parserLit.Rune), 10),
		L:    parserLit.L,
	}
	return _checkIntLit(parserIntLit, pat, Int32)
}

// checkIntLit checks an integer literal.
//
// If the pattern's type is an integer type or reference to an integer type,
// the type of the literal is the pattern's type.
// If the pattern's type is a floating point type or reference to a floating point type,
// the type of the literal is the pattern's type.
// Otherwise the type of the literal is the unification
// of the built-in type int and the pattern.
//
// It is an error if the value is not representable by the type.
//
// The returned Expr is never nil even if there are errors.
func checkIntLit(parserLit *parser.IntLit, pat typePattern) (Expr, []Error) {
	return _checkIntLit(parserLit, pat, Int)
}

func _checkIntLit(parserLit *parser.IntLit, pat typePattern, defaultKind BasicTypeKind) (expr Expr, errs []Error) {
	if isFloatType(pat.typ) || isFloatRefType(pat.typ) {
		floatLit := &parser.FloatLit{Text: parserLit.Text, L: parserLit.L}
		return checkFloatLit(floatLit, pat)
	}

	lit := &IntLit{Text: parserLit.Text, L: parserLit.L}
	if _, ok := lit.Val.SetString(lit.Text, 0); !ok {
		panic("malformed int")
	}
	defer func() {
		if err := checkValueSize(lit); err != nil {
			errs = append(errs, err)
		}
	}()
	switch {
	case isIntRefType(pat.typ):
		lit.T = copyTypeWithLoc(pat.groundType(), lit.L)
		return lit, nil
	case isIntType(pat.typ):
		lit.T = refType(copyTypeWithLoc(pat.groundType(), lit.L))
		return deref(lit), nil
	default:
		switch bind := unify(pat, &BasicType{Kind: defaultKind, L: lit.L}); {
		case bind == nil:
			return lit, []Error{newError(lit, "cannot unify %s with %s", defaultKind, pat)}
		case isRefType(literalType(pat.typ)):
			lit.T = subType(bind, pat.typ)
			return lit, nil
		default:
			lit.T = subType(bind, refType(pat.typ))
			return deref(lit), nil
		}
	}
}

func checkValueSize(lit *IntLit) Error {
	var bits uint
	var signed bool
	kind := basicKind(lit.T)
	switch kind {
	case noBasicTypeKind:
		return nil
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
	case UintRef:
		bits = 64 // TODO: set by a flag
		signed = false
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
		panic(fmt.Sprintf("impossible int kind: %s", kind))
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
	case lit.Val.Cmp(min) < 0:
		return newError(lit, "%s underflows %s", lit.Text, kind)
	case lit.Val.Cmp(max) > 0:
		return newError(lit, "%s overflows %s", lit.Text, kind)
	}
	return nil
}

// checkFloatLit checks a float literal.
//
// If the pattern's type is a floating point type or reference to a floating point type,
// the type of the literal is the pattern's type.
// If the pattern's type is an integer type or reference to an integer type,
// the type of the literal is the pattern's type.
// Otherwise the type of the literal is the unification
// of the built-in type float64 and the pattern.
//
// It is an error if the value is not representable by the type.
//
// The returned Expr is never nil even if there are errors.
func checkFloatLit(parserLit *parser.FloatLit, pat typePattern) (Expr, []Error) {
	lit := &FloatLit{Text: parserLit.Text, L: parserLit.L}
	if _, _, err := lit.Val.Parse(parserLit.Text, 10); err != nil {
		panic("malformed float")
	}
	switch {
	case isIntType(pat.typ) || isIntRefType(pat.typ):
		var i big.Int
		var errs []Error
		if _, acc := lit.Val.Int(&i); acc != big.Exact {
			errs = append(errs, newError(lit, "%s truncates %s", pat.groundType(), lit.Text))
		}
		intLit, es := checkIntLit(&parser.IntLit{Text: i.String(), L: parserLit.L}, pat)
		if len(es) > 0 {
			errs = append(errs, es...)
		}
		return intLit, errs
	case isFloatRefType(pat.typ):
		lit.T = copyTypeWithLoc(pat.groundType(), lit.L)
		return lit, nil
	case isFloatType(pat.typ):
		lit.T = refType(copyTypeWithLoc(pat.groundType(), lit.L))
		return deref(lit), nil
	default:
		switch bind := unify(pat, &BasicType{Kind: Float64, L: lit.L}); {
		case bind == nil:
			return lit, []Error{newError(lit, "cannot unify %s with %s", Float64, pat)}
		case isRefType(literalType(pat.typ)):
			lit.T = subType(bind, pat.typ)
			return lit, nil
		default:
			lit.T = subType(bind, refType(pat.typ))
			return deref(lit), nil
		}
	}
}

func deref(expr Expr) Expr {
	var t Type
	switch ref := expr.Type().(type) {
	case nil:
		// The expression's type had some kind of error.
		// It is reported elsewhere; just propagate the nil.
		break
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
