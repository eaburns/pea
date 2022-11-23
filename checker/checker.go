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
	type typeKey struct {
		arity int
		name  string
	}
	seenTypes := map[typeKey]loc.Loc{
		{0, "int"}:     {},
		{0, "int8"}:    {},
		{0, "int16"}:   {},
		{0, "int32"}:   {},
		{0, "int64"}:   {},
		{0, "uint"}:    {},
		{0, "uint8"}:   {},
		{0, "uint16"}:  {},
		{0, "uint32"}:  {},
		{0, "uint64"}:  {},
		{0, "uintref"}: {},
		{0, "float32"}: {},
		{0, "float64"}: {},
		{0, "string"}:  {},
	}
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
		// Make TypeDefs and IfaceDefs, but not their Types yet.
		// The Types cannot be made until all TypeDefs are made,
		// because making Types requires looking up the def
		// for each type name.
		for _, parserDef := range parserFile.Defs {
			var def Def
			var arity int
			var name string
			switch parserDef := parserDef.(type) {
			default:
				continue
			case *parser.TypeDef:
				arity = len(parserDef.TypeParms)
				name = parserDef.Name.Name
				parms, es := makeTypeParms(checker.importer.Files(), parserDef.TypeParms)
				errs = append(errs, es...)
				def = &TypeDef{
					File:   file,
					Alias:  parserDef.Alias,
					Mod:    modPath,
					Name:   name,
					Parms:  parms,
					Exp:    parserDef.Exp,
					Opaque: parserDef.Opaque,
					L:      parserDef.L,
				}
			case *parser.IfaceDef:
				arity = len(parserDef.TypeParms)
				name = parserDef.Name.Name
				parms, es := makeTypeParms(checker.importer.Files(), parserDef.TypeParms)
				errs = append(errs, es...)
				def = &IfaceDef{
					File:   file,
					Mod:    modPath,
					Name:   name,
					Parms:  parms,
					Exp:    parserDef.Exp,
					Opaque: parserDef.Opaque,
					L:      parserDef.L,
				}
			}
			defs[parserDef] = def
			key := typeKey{arity, name}
			if prev, ok := seenTypes[key]; ok {
				errorName := name
				if arity > 0 {
					errorName = fmt.Sprintf("(%d)%s", arity, name)
				}
				errs = append(errs, redef(def.Loc(), errorName, prev))
				continue
			}
			seenTypes[key] = def.Loc()
			mod.Defs = append(mod.Defs, def)
		}
	}

	errs = append(errs, checkTypeDefs(files, defs)...)

	// At this point, all TypeDefs and their types are made.
	// From here on, we can fully make and instantiate types.

	// New that we can instantiate types we can finish checking IfaceDefs.
	errs = append(errs, checkIfaceDefs(files, defs)...)

	testNames := make(map[string]loc.Loc)
	for i, parserFile := range files {
		file := mod.Files[i]
		for _, parserDef := range parserFile.Defs {
			switch parserDef := parserDef.(type) {
			case *parser.TypeDef:
				// already done
			case *parser.IfaceDef:
				// already done
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

				for _, elem := range parserDef.Iface {
					switch elem := elem.(type) {
					case *parser.FuncDecl:
						decl, es := makeFuncDecl(funDef, elem)
						errs = append(errs, es...)
						funDef.Iface = appendFuncDecl(funDef.Iface, decl)
					case *parser.NamedType:
						args, es := makeTypes(funDef, elem.Args)
						errs = append(errs, es...)
						inst, err := newIfaceInst(funDef, elem, args)
						if err != nil {
							errs = append(errs, err)
							break
						}
						inst = canonicalIfaceInst(inst.Def, inst.Args)
						for i := range inst.Funcs {
							funDef.Iface = appendFuncDecl(funDef.Iface, &inst.Funcs[i])
						}
					default:
						panic(fmt.Sprintf("bad iface element type: %T", elem))
					}
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
				// already done
			case *IfaceDef:
				// already done
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

func checkTypeDefs(files []*parser.File, defs map[parser.Def]Def) []Error {
	// Checking TypeDefs happens in three stages:
	// 1) Set the TypeDef.Type field to an uninstantiate type.
	// 	We cannot instantiate types until we are sure there are no alias cycles.
	// 2) Check for alias cycles, reporting any as errors and breaking them abrtrarily
	// 	so that checking can continue.
	// 3) TypeDef.Type, which can be done now that we know there are no cycles.
	var errs []Error
	for _, parserFile := range files {
		for _, parserDef := range parserFile.Defs {
			parserTypeDef, ok := parserDef.(*parser.TypeDef)
			if !ok {
				continue
			}
			typeDef := defs[parserTypeDef].(*TypeDef)
			t, es := _makeType(typeDef, parserTypeDef.Type, false, false)
			errs = append(errs, es...)
			typeDef.Type = t
		}
	}
	// Type instantiation assumes no alias cycles,
	// so we check them here.
	// Alias cycles are an error, but this loop will
	// report and then break any cycles it finds
	// in order to allow checking to continue
	// reporting more errors.
	for _, def := range defs {
		if typeDef, ok := def.(*TypeDef); ok && typeDef.Alias {
			if err := checkTypeAliasCycle(typeDef); err != nil {
				errs = append(errs, err)
			}
		}
	}
	for _, def := range defs {
		if typeDef, ok := def.(*TypeDef); ok {
			typeDef.Type = instType(typeDef.Type)
		}
	}
	return errs
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
		case len(types) == 0 && parserType.Mod == nil:
			// If the type is not found,
			// the type name does not have a module specified,
			// there is a module with the same name as the type,
			// and that module has a single type of the same name
			// with the correct arity; use that type.
			imp := findImport(x, name)
			if imp != nil {
				types = findType(imp, args, name, parserType.L)
				if len(types) == 1 {
					typ = types[0]
					break
				}
			}
			fallthrough
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

func checkTypeAliasCycle(root *TypeDef) Error {
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
		err := newError(root.L, "type alias cycle")
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
	for _, elem := range parserFuncDef.Iface {
		switch elem := elem.(type) {
		case *parser.FuncDecl:
			for _, parserIfaceParm := range elem.Parms {
				findTypeVars(parserIfaceParm, typeVars)
			}
			if elem.Ret != nil {
				findTypeVars(elem.Ret, typeVars)
			}
		case *parser.NamedType:
			for _, parserTypeArg := range elem.Args {
				findTypeVars(parserTypeArg, typeVars)
			}
		default:
			panic(fmt.Sprintf("bad iface elem type: %T", elem))
		}
	}
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

// checkIfaceDefs assumes that it is called from late enough during checking
// that it is possible to fully instantiate types.
func checkIfaceDefs(files []*parser.File, defs map[parser.Def]Def) []Error {
	// Checking IfaceDefs happens in two steps:
	// 1) Populate the "head", which is the IfaceDef.Iface and IfaceDef.Alias fields.
	// 	In so doing, any *IfaceInsts are temporary instances,
	// 	they contain only the .Def and .Args fields, but not .Funcs,
	// 	and they are never a "canonical" instance from .Def.Insts,
	// 	as we cannot yet compute Funcs.
	// 2) Recursively check the "body" of each IfaceDef, which is the .Funcs.
	// 	In doing this, any temporary *IfaceInsts are replaced by their canonical.
	// 	Any cycles are detected, reported as errors, and are arbitrarily broken
	// 	so that checking can continue to report other errors.
	var errs []Error
	for _, parserFile := range files {
		for _, parserDef := range parserFile.Defs {
			parserDef, ok := parserDef.(*parser.IfaceDef)
			if !ok {
				continue
			}
			def := defs[parserDef].(*IfaceDef)
			errs = append(errs, checkIfaceDefHead(def, parserDef)...)
		}
	}
	for _, def := range defs {
		if ifaceDef, ok := def.(*IfaceDef); ok {
			errs = append(errs, checkIfaceDefBody(ifaceDef, nil)...)
		}
	}
	return errs
}

func checkIfaceDefHead(def *IfaceDef, parserDef *parser.IfaceDef) []Error {
	if parserDef.Alias != nil {
		args, errs := makeTypes(def, parserDef.Alias.Args)
		var err Error
		if def.Alias, err = newIfaceInst(def, parserDef.Alias, args); err != nil {
			errs = append(errs, err)
		}
		return errs
	}

	var errs []Error
	for _, elem := range parserDef.Iface {
		switch elem := elem.(type) {
		case *parser.FuncDecl:
			decl, es := makeFuncDecl(def, elem)
			errs = append(errs, es...)
			def.Iface = append(def.Iface, decl)
		case *parser.NamedType:
			args, es := makeTypes(def, elem.Args)
			errs = append(errs, es...)
			if inst, err := newIfaceInst(def, elem, args); err != nil {
				errs = append(errs, err)
			} else {
				def.Iface = append(def.Iface, inst)
			}
		default:
			panic(fmt.Sprintf("bad iface element type: %T", elem))
		}
	}
	return errs
}

func makeFuncDecl(x scope, parserDecl *parser.FuncDecl) (*FuncDecl, []Error) {
	parms, errs := makeTypes(x, parserDecl.Parms)
	ret, es := makeType(x, parserDecl.Ret)
	errs = append(errs, es...)
	if ret == nil {
		ret = &StructType{L: parserDecl.L}
	}
	decl := &FuncDecl{
		Name:  parserDecl.Name.Name,
		Parms: parms,
		Ret:   ret,
		L:     parserDecl.L,
	}
	return decl, errs
}

func checkIfaceDefBody(def *IfaceDef, path []*IfaceDef) (errs []Error) {
	if def.Funcs != nil {
		return nil // Already checked.
	}
	defer func() {
		if def.Funcs == nil {
			// Mark it as checked, so we don't keep doing it.
			def.Funcs = []FuncDecl{}
		}
	}()
	for _, p := range path {
		if p != def {
			continue
		}
		err := newError(p, "interface cycle")
		var notes []note
		for _, d := range path[1:] {
			notes = append(notes, newNote(d.Name).setLoc(d))
		}
		notes = append(notes, newNote(path[0].Name).setLoc(path[0]))
		err.setNotes(notes)
		return []Error{err}
	}

	if def.Alias != nil {
		errs = append(errs, checkIfaceDefBody(def.Alias.Def, append(path, def))...)
		// If there were errors, it may be because an interface cycle;
		// we cannot get the canonicalIfaceInst if there was a cycle,
		// so just return.
		if len(errs) != 0 {
			return errs
		}
		def.Alias = canonicalIfaceInst(def.Alias.Def, def.Alias.Args)
		def.Funcs = def.Alias.Funcs
		return errs
	}

	for i, elem := range def.Iface {
		switch elem := elem.(type) {
		case *IfaceInst:
			errs = append(errs, checkIfaceDefBody(elem.Def, append(path, def))...)
			inst := canonicalIfaceInst(elem.Def, elem.Args)
			def.Iface[i] = inst
			for i := range inst.Funcs {
				def.Funcs = appendFuncDecl(def.Funcs, &inst.Funcs[i])
			}
		case *FuncDecl:
			def.Funcs = appendFuncDecl(def.Funcs, elem)
		default:
			panic(fmt.Sprintf("bad iface elem type: %T", elem))
		}
	}
	return errs
}

func appendFuncDecl(funcs []FuncDecl, decl *FuncDecl) []FuncDecl {
	for i := range funcs {
		if eqFuncDecl(&funcs[i], decl) {
			return funcs
		}
	}
	return append(funcs, *decl)
}

func eqFuncDecl(a, b *FuncDecl) bool {
	if a.Name != b.Name || len(a.Parms) != len(b.Parms) {
		return false
	}
	for i := range a.Parms {
		if !eqType(a.Parms[i], b.Parms[i]) {
			return false
		}
	}
	return eqType(a.Ret, b.Ret)
}

func newIfaceInst(x scope, parserType *parser.NamedType, args []Type) (*IfaceInst, Error) {
	if parserType.Mod != nil {
		imp := findImport(x, parserType.Mod.Name)
		if imp == nil {
			return nil, notFound(parserType.Mod.Name, parserType.L)
		}
		x = imp
	}
	name := parserType.Name.Name
	l := parserType.L
	switch defs := findIfaceDef(x, len(parserType.Args), name, l); {
	case len(defs) == 0:
		return nil, notFound(name, l)
	case len(defs) > 1:
		return nil, ambigIface(name, l, defs)
	case defs[0].File.Mod.Imported && defs[0].Opaque:
		modName := parserType.Mod.Name
		return nil, newError(l, "interface %s#%s is opaque", modName, name)
	default:
		return &IfaceInst{Def: defs[0], Args: args}, nil
	}
}

func canonicalIfaceInst(def *IfaceDef, args []Type) *IfaceInst {
nextInst:
	for _, inst := range def.Insts {
		for i, arg := range inst.Args {
			if !eqType(arg, args[i]) {
				continue nextInst
			}
		}
		return inst
	}
	bind := make(map[*TypeParm]Type)
	for i := range def.Parms {
		bind[&def.Parms[i]] = args[i]
	}

	if def.Alias != nil {
		return canonicalIfaceInst(def.Alias.Def, subTypes(bind, def.Alias.Args))
	}

	var funcs []FuncDecl
	for i := range def.Funcs {
		funcs = append(funcs, *subFuncDecl(bind, &def.Funcs[i]))
	}
	inst := &IfaceInst{Def: def, Args: args, Funcs: funcs}
	for _, arg := range args {
		if hasTypeVariable(arg) {
			// If the args have a type variable, don't bother saving this inst.
			return inst
		}
	}
	def.Insts = append(def.Insts, inst)
	return inst
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
			Parms: []Type{refLiteral(def.T), expr.Type()},
			Ret:   expr.Type(),
		},
		Args: []Expr{&Var{Def: def, T: refLiteral(def.T), L: def.L}, expr},
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
	if n := len(def.Exprs); n > 0 {
		// checkExprs removes outer conversions for all but the last expression.
		// The reason it keeps them on the last is because it is called for block literals,
		// where the last expression's value is used as the result value.
		// For a FuncDef, the last expression value is not used,
		// so we just remove it here.
		def.Exprs[n-1] = removeOuterConverts(def.Exprs[n-1])
	}
	if !isEmptyStruct(def.Ret) &&
		def.Exprs != nil &&
		(len(def.Exprs) == 0 || !neverReturns(def.Exprs[len(def.Exprs)-1])) {
		errs = append(errs, newError(def, "function must end in a return"))
	}
	for _, l := range def.Locals {
		if l.Name != "_" && !l.used {
			errs = append(errs, newError(l, "%s unused", l.Name))
		}
	}
	if len(def.TypeParms) == 0 && len(def.Iface) == 0 {
		// This is a not-parameterized function.
		// Make sure we add it to the Insts memo table,
		// so its Exprs get built.
		memoizeFuncInst(newFuncInst(def, def.Loc()))
	}
	return errs
}

func neverReturns(expr Expr) bool {
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
	return isEnd(call.Func.ret().typ)
}

func checkTestDef(def *TestDef, parserDef *parser.TestDef) []Error {
	var errs []Error
	def.Exprs, errs = checkExprs(def, true, parserDef.Exprs, any())
	return errs
}

func checkExpr(x scope, parserExpr parser.Expr, pat typePattern) (Expr, []Error) {
	return _checkExpr(x, parserExpr, pat, implicit)
}

func _checkExpr(x scope, parserExpr parser.Expr, pat typePattern, mode convertMode) (Expr, []Error) {
	switch parserExpr := parserExpr.(type) {
	case *parser.Call:
		return checkCall(x, parserExpr, pat, mode)
	case *parser.Convert:
		return checkConvert(x, parserExpr)
	case *parser.SubExpr:
		return checkExpr(x, parserExpr.Expr, pat)
	case *parser.ArrayLit, *parser.StructLit, *parser.UnionLit, *parser.BlockLit,
		*parser.StrLit, *parser.CharLit, *parser.IntLit, *parser.FloatLit:
		return checkLit(x, parserExpr, pat)
	case *parser.ModSel:
		return checkModSel(x, parserExpr, pat)
	case parser.Ident:
		return checkID(x, parserExpr, false, pat)
	default:
		panic(fmt.Sprintf("impossible expr type: %T", parserExpr))
	}
}

func checkAndConvertExpr(x scope, parserExpr parser.Expr, pat typePattern) (Expr, []Error) {
	expr, errs := checkExpr(x, parserExpr, pat)
	if expr == nil || !pat.isGroundType() {
		return expr, errs
	}
	expr, _, err := convertExpr(expr, pat, implicit)
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
			if expr != nil && !neverReturns(expr) {
				var err Error
				if expr, _, err = convertExpr(expr, pat, implicit); err != nil {
					es = append(es, err)
				}
			}
		} else {
			expr, es = checkExpr(x, parserExpr, any())
		}
		if i < len(parserExprs)-1 {
			// Remove noisy top-level conversions for all but the last expression.
			// The last expression must keep the conversion,
			// because it may be the result expression of a block.
			expr = removeOuterConverts(expr)
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

func removeOuterConverts(expr Expr) Expr {
	for {
		cvt, ok := expr.(*Convert)
		if !ok {
			break
		}
		expr = cvt.Expr
	}
	return expr
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
				Parms: []Type{refLiteral(expr.Type()), expr.Type()},
				Ret:   expr.Type(),
			},
			Args: []Expr{
				&Local{Def: local, T: refLiteral(expr.Type()), L: id.L},
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

func checkCall(x scope, parserCall *parser.Call, pat typePattern, mode convertMode) (Expr, []Error) {
	switch fun := parserCall.Fun.(type) {
	case parser.Ident:
		ids := findIDs(x, fun.Name)
		return resolveIDCall(x, nil, fun, parserCall, pat, ids, mode)
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
		return resolveIDCall(x, addMod, fun.Name, parserCall, pat, ids, mode)
	default:
		return checkExprCall(x, parserCall, pat)
	}
}

func resolveIDCall(x scope, mod *Import, parserID parser.Ident, parserCall *parser.Call, pat typePattern, ids []id, mode convertMode) (Expr, []Error) {
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
	var firstArgConvertError Error
	for i, parserArg := range parserCall.Args {
		parmPats := make([]typePattern, 0, len(funcs))
		for _, f := range funcs {
			parmPats = append(parmPats, f.parm(i))
		}
		parmPat := common(parmPats...)

		var arg Expr
		var errs []Error
		// If this is the LHS of an assignment to an Ident,
		// don't mark it as "used" if it is a local variable.
		if lhs, ok := parserArg.(parser.Ident); ok && i == 0 && parserID.Name == ":=" {
			arg, errs = checkID(x, lhs, true, parmPat)
		} else {
			arg, errs = checkExpr(x, parserArg, parmPat)
		}
		if len(errs) > 0 {
			errs = append(errs, checkArgsFallback(x, parserCall.Args[i+1:])...)
			return &Call{L: parserCall.L}, errs
		}
		funcs, ns = filterByArg(funcs, i, arg)
		notes = append(notes, ns...)
		args = append(args, arg)

		if mod == nil {
			// Do argument-dependent lookup if the call is not tagged with a module.
			fs, ns := adLookup(x, parserID, len(parserCall.Args), args, pat)
			funcs = append(funcs, fs...)
			notes = append(notes, ns...)
		}
		if len(funcs) > 0 {
			markVerbose(notes)
		}
		if len(funcs) == 0 && firstArgConvertError == nil {
			// This is just for simpler error messages:
			// If all functions have the same pattern (eg, if len(funcs)==1),
			// we try to convert the argument expression to that pattern.
			// If the conversion fails, we keep track of the error;
			// and if there are no more function candidates after checking args,
			// we can return the first such error instead of returning "not found"
			// on the called function.
			//
			// Note that we do not return the error right away,
			// as to give ADL a chance to add additional candidate functions.
			samePat := true
			for _, f := range funcs {
				if !eqType(f.parm(i).typ, parmPat.typ) {
					samePat = false
					break
				}
			}
			if samePat {
				_, _, firstArgConvertError = convertExpr(arg, parmPat, implicit)
			}
		}
	}

	// No candidates left.
	// Consider reporting a simpler error if an argument failed to convert
	// to a type that was agreed by all considered functions.
	if len(funcs) == 0 && firstArgConvertError != nil {
		return &Call{L: parserCall.L}, []Error{firstArgConvertError}
	}

	funcs, ns = filterByReturnType(funcs, pat, mode)
	notes = append(notes, ns...)
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
		args[i], _, _ = convertExpr(arg, fun.parm(i), implicit)
	}
	var expr Expr = &Call{
		Func: fun,
		Args: args,
		T:    &RefType{Type: ret, L: ret.Loc()},
		L:    parserCall.L,
	}
	for isRefLiteral(expr.Type()) {
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
		funcs, ns = filterByArg(funcs, i, arg)
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

func filterByArg(funcs []Func, i int, arg Expr) ([]Func, []note) {
	if arg.Type() == nil {
		// There was an error checking the argument.
		// Just silently filter out all functions.
		return nil, nil
	}

	var n int
	var notes []note
	for _, f := range funcs {
		_, bind, err := convertExpr(arg, f.parm(i), implicit)
		if err != nil {
			notes = append(notes, err.(note))
			continue
		}
		if note := f.sub(nil, bind); note != nil {
			notes = append(notes, note)
			continue
		}
		funcs[n] = f
		n++
	}
	return funcs[:n], notes
}

func filterByReturnType(funcs []Func, pat typePattern, mode convertMode) ([]Func, []note) {
	var notes []note
	if len(funcs) == 1 {
		f := funcs[0]
		// Just try to substitute it, ignoring conversion errors.
		// If it fails to convert the parent expression will report the error,
		// with more information that we would report here,
		// because it will use convertExpr which has the expression
		// printed in the error message, but here we just have the type.
		bind, convertNote := convertType(f.ret(), pat, mode)
		if note := f.sub(nil, bind); note != nil {
			return nil, append(notes, note)
		}
		if !f.ret().isGroundType() {
			// If the ret is not grounded, it means bind was nil;
			// the conversion failed and we could not infer the return type.
			if convertNote == nil {
				convertNote = newNote("%s: cannot infer return type %s", f, f.ret())
			}
			notes = append(notes, convertNote)
			return nil, notes
		}
		return funcs, nil
	}

	var n int
	for _, f := range funcs {
		bind, note := convertType(f.ret(), pat, mode)
		if note != nil {
			notes = append(notes, note)
			continue
		}
		if note := f.sub(nil, bind); note != nil {
			notes = append(notes, note)
			continue
		}
		funcs[n] = f
		n++
	}
	return funcs[:n], notes
}

func filterIfaceConstraints(x scope, l loc.Loc, funcs []Func) ([]Func, []note) {
	var n int
	var notes []note
	for _, f := range funcs {
		if note := instFuncConstraints(x, l, f); note != nil {
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
		return captureExprIfaceArgs(fun)
	case *idFunc:
		return &ExprFunc{
			Expr:     useID(x, l, false, fun.id),
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
	if fun != nil && fun.FuncType != nil && fun.FuncType.Ret != nil {
		ret = &RefType{Type: fun.FuncType.Ret, L: fun.FuncType.Ret.Loc()}
	}
	expr = &Call{Func: fun, Args: args, T: ret, L: parserCall.L}
	for isRefLiteral(expr.Type()) {
		expr = deref(expr)
	}
	return expr, errs
}

func checkModSel(x scope, parserSel *parser.ModSel, pat typePattern) (Expr, []Error) {
	imp := findImport(x, parserSel.Mod.Name)
	if imp == nil {
		return nil, []Error{notFound(parserSel.Mod.Name, parserSel.L)}
	}
	parserID := parserSel.Name
	ids := findIDs(imp, parserID.Name)
	return resolveID(x, parserID, false, pat, ids)
}

func checkID(x scope, parserID parser.Ident, assignLHS bool, pat typePattern) (Expr, []Error) {
	ids := findIDs(x, parserID.Name)
	// If the pattern wants a FuncType, expand the set of IDs to consider
	// to include those from the pattern's param and return type modules too.
	if funType, ok := pat.typ.(*FuncType); ok {
		seen := make(map[string]bool)
		for i := 0; i < len(funType.Parms); i++ {
			parm := funType.Parms[i]
			defType, ok := parm.(*DefType)
			if !ok || seen[defType.Def.Mod] {
				continue
			}
			seen[defType.Def.Mod] = true
			ids = append(ids, adModuleIDs(x, defType.Def.Mod, parserID.Name)...)
		}
		if defType, ok := funType.Ret.(*DefType); ok && !seen[defType.Def.Mod] {
			ids = append(ids, adModuleIDs(x, defType.Def.Mod, parserID.Name)...)
		}
	}
	return resolveID(x, parserID, assignLHS, pat, ids)
}

func resolveID(x scope, parserID parser.Ident, assignLHS bool, pat typePattern, ids []id) (Expr, []Error) {
	var n int
	var notFoundNotes []note
	for _, id := range ids {
		fun, ok := id.(Func)
		if !ok {
			ids[n] = id
			n++
			continue
		}
		if !isGround(id) {
			if !pat.isGroundType() {
				n := newNote("cannot ground %s", id).setLoc(id)
				notFoundNotes = append(notFoundNotes, n)
				continue
			}
			if _, fail := unifyFunc(x, parserID.L, fun, nil, pat); fail != nil {
				notFoundNotes = append(notFoundNotes, fail.note)
				continue
			}
		}
		if note := instFuncConstraints(x, parserID.L, fun); note != nil {
			notFoundNotes = append(notFoundNotes, note)
			continue
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
			if _, n0 := convertType(pattern(id.Type()), pat, implicit); n0 != nil {
				n := newNote("cannot convert %s (%s) to %s", id, id.Type(), pat)
				n.setLoc(id.Type().Loc())
				n.setNotes([]note{n0})
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
		return useID(x, parserID.L, assignLHS, ids[0]), nil
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

// useID marks the ID as used, converts it to a Cap if it is actually captured
// and not accessed directly, and returns it as an Expr.
// If the ID as a FuncInst, it is canonicalized
// and a BlockLit is returned wrapping its call.
func useID(x scope, l loc.Loc, assignLHS bool, id id) Expr {
	switch id := id.(type) {
	case *VarDef:
		useVar(x, l, id)
	case *LocalDef:
		// Assigning to a local doesn't use it.
		// However, if the local's type is a reference,
		// then we aren't assigning to the local,
		// but the referent of the local,
		// in which case we are using it;
		// we are dereferencing it.
		if !assignLHS || isRefLiteral(id.Type()) {
			id.used = true
		}
	}

	switch id := capture(x, id).(type) {
	case *VarDef:
		return deref(&Var{Def: id, T: refLiteral(id.T), L: l})
	case *ParmDef:
		return deref(&Parm{Def: id, T: refLiteral(id.T), L: l})
	case *LocalDef:
		return deref(&Local{Def: id, T: refLiteral(id.T), L: l})
	case *BlockCap:
		return deref(&Cap{Def: id, T: refLiteral(id.T), L: l})
	case *FuncInst:
		useFuncInst(x, l, id.Def, nil, nil)
		switch f := captureExprIfaceArgs(id).(type) {
		case *ExprFunc:
			return f.Expr
		default:
			return wrapCallInBlock(id, id.ret().groundType(), l)
		}
	case Func:
		return wrapCallInBlock(id, id.ret().groundType(), l)
	default:
		panic(fmt.Sprintf("impossible id type: %T", id))
	}
}

// Convert a Func f into a block literal of the form
//
//	(p…){ res := f(p…), res }
//
// where p… are parameters with the same type as f.
//
// The call is assigned to a varible,
// and that variable is the result of the block.
// This allows wantRet to have one more reference
// than the normal result of calling fun.Ret.
// This reference feature is needed by iface call substitution,
// where the return of an iface function may need to have
// up to one additional reference added to it.
func wrapCallInBlock(fun Func, wantRet Type, l loc.Loc) *BlockLit {
	block, _ := _wrapCallInBlock(fun, wantRet, l)
	return block
}

func _wrapCallInBlock(fun Func, wantRet Type, l loc.Loc) (*BlockLit, *Call) {
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
		T:    retType,
		L:    l,
	}
	assign := &Call{
		Func: &Builtin{
			Op:    Assign,
			Parms: []Type{refLiteral(localDef.T), retType},
			Ret:   retType,
		},
		Args: []Expr{
			&Local{Def: localDef, T: refLiteral(localDef.T), L: l},
			deref(call),
		},
		T: &StructType{L: l},
		L: l,
	}
	local := deref(&Local{Def: localDef, T: refLiteral(localDef.T), L: l})
	result, _, err := convertExpr(local, pattern(wantRet), implicit)
	if err != nil {
		panic(fmt.Sprintf("impossible: %s", err))
	}
	typ := &FuncType{Parms: parms, Ret: wantRet, L: l}
	blk := &BlockLit{
		Parms:  make([]ParmDef, len(parms)),
		Locals: []*LocalDef{localDef},
		Ret:    wantRet,
		Exprs:  []Expr{assign, result},
		Func:   typ,
		T:      typ,
		L:      l,
	}
	for i := range parms {
		blk.Parms[i].Name = fmt.Sprintf("x%d", i)
		blk.Parms[i].T = parms[i]
		blk.Parms[i].L = l
		call.Args[i] = deref(&Parm{
			Def: &blk.Parms[i],
			T:   refLiteral(parms[i]),
			L:   l,
		})
	}
	return blk, call
}

func checkConvert(x scope, parserConvert *parser.Convert) (Expr, []Error) {
	typ, errs := makeType(x, parserConvert.Type)
	// typ may be nil if there was an error in the type, so use patternOrAny.
	expr, es := _checkExpr(x, parserConvert.Expr, patternOrAny(typ), explicit)
	if len(es) > 0 {
		errs = append(errs, es...)
	}
	if typ == nil {
		return expr, errs
	}
	expr, _, err := convertExpr(expr, pattern(typ), explicit)
	if err != nil {
		errs = append(errs, err)
	}
	return expr, errs
}

func checkLit(x scope, parserExpr parser.Expr, pat typePattern) (Expr, []Error) {
	expr, errs := _checkLit(x, parserExpr, pat)
	if len(errs) > 0 {
		return expr, errs
	}
	switch expr, _, err := convertExpr(expr, pat, explicit); {
	case err != nil:
		return expr, []Error{err}
	case !pat.withType(expr.Type()).isGroundType():
		err := newError(parserExpr, "cannot infer literal type, got %s",
			pat.withType(expr.Type()))
		return expr, []Error{err}
	default:
		// The convert succeeded, but the result is marked as an explicit conversion.
		// This is not an explicit conversion expression, so we need to
		// remove any Noops added solely to mark the Explicit bit,
		// and clear the Explicit bit on the top-most remaining conversion, if any.
		// This will allow the resulting expression to be implicitly converted.
		for {
			cvt, ok := expr.(*Convert)
			if !ok {
				break
			}
			if cvt.Kind != Noop || !eqType(cvt.Type(), cvt.Expr.Type()) {
				cvt.Explicit = false
				break
			}
			// Remove excess noops added solely to mark explicit conversion.
			expr = cvt.Expr
		}
		return expr, nil
	}
}

func _checkLit(x scope, parserExpr parser.Expr, pat typePattern) (Expr, []Error) {
	switch {
	case isRefLiteral(pat.typ):
		return _checkLit(x, parserExpr, pat.refElem())
	case isVisibleDefinedType(pat.typ):
		return _checkLit(x, parserExpr, pat.instType())
	}
	switch parserExpr := parserExpr.(type) {
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
	default:
		panic(fmt.Sprintf("impossible literal type: %T", parserExpr))
	}
}

func checkArrayLit(x scope, parserLit *parser.ArrayLit, pat typePattern) (Expr, []Error) {
	elemPat := any()
	lit := &ArrayLit{Array: &ArrayType{L: parserLit.L}, L: parserLit.L}
	if _, ok := pat.typ.(*ArrayType); ok {
		elemPat = pat.arrayElem()
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
	lit.Array.ElemType = elemPat.typ
	lit.T = lit.Array
	return lit, errs
}

func checkStructLit(x scope, parserLit *parser.StructLit, pat typePattern) (Expr, []Error) {
	var errs []Error
	var fieldPats []typePattern
	if st, ok := pat.typ.(*StructType); ok {
		if isEmptyStruct(st) && len(parserLit.FieldVals) > 0 {
			errs = append(errs, newError(parserLit, "invalid empty struct literal"))
		}
		if len(st.Fields) == len(parserLit.FieldVals) {
			for i := range parserLit.FieldVals {
				if st.Fields[i].Name != parserLit.FieldVals[i].Name.Name {
					fieldPats = nil
					break
				}
				fieldPats = append(fieldPats, pat.field(i))
			}
		}
	}
	if fieldPats == nil {
		for range parserLit.FieldVals {
			fieldPats = append(fieldPats, any())
		}
	}

	lit := &StructLit{
		Struct: &StructType{L: parserLit.L},
		L:      parserLit.L,
	}
	lit.T = lit.Struct
	for i, parserField := range parserLit.FieldVals {
		expr, es := checkAndConvertExpr(x, parserField.Val, fieldPats[i])
		if len(es) > 0 {
			errs = append(errs, es...)
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
	return lit, errs
}

func checkUnionLit(x scope, parserLit *parser.UnionLit, pat typePattern) (Expr, []Error) {
	exprPat := any()
	lit := &UnionLit{L: parserLit.L}
	caseName := parserLit.CaseVal.Name.Name
	if uni, ok := pat.typ.(*UnionType); ok {
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
	var errs []Error
	lit.Case = findCase(caseName, lit.Union)
	if parserLit.CaseVal.Val != nil {
		lit.Val, errs = checkAndConvertExpr(x, parserLit.CaseVal.Val, exprPat)
		if len(errs) > 0 {
			return lit, errs
		}
		lit.Case.Type = lit.Val.Type()
	}
	lit.T = lit.Union
	return lit, nil
}

func findCase(name string, u *UnionType) *CaseDef {
	name = strings.TrimSuffix(name, "?")
	name = strings.TrimSuffix(name, ":")
	for i := range u.Cases {
		if strings.TrimSuffix(u.Cases[i].Name, "?") == name {
			return &u.Cases[i]
		}
	}
	return nil
}

func checkBlockLit(x scope, parserLit *parser.BlockLit, pat typePattern) (Expr, []Error) {
	lit := &BlockLit{
		L:    parserLit.L,
		Func: &FuncType{L: parserLit.L},
	}
	var errs []Error
	lit.Parms, errs = makeFuncParms(x, parserLit.Parms)

	retPat := any()
	if fun, ok := pat.typ.(*FuncType); ok && len(fun.Parms) == len(lit.Parms) {
		retPat = pat.withType(fun.Ret)
		for i := range lit.Parms {
			litParm := any()
			if lit.Parms[i].T != nil {
				litParm = pattern(lit.Parms[i].T)
			}
			patParm := pat.withType(fun.Parms[i])
			bind, note := convertType(litParm, patParm, implicit)
			if note != nil {
				// We need to ensure the note has a non-0 Loc before making it an Error.
				note.setLoc(lit.Parms[i].L)
				errs = append(errs, note.(Error))
			}
			lit.Parms[i].T = subType(bind, patParm.typ)
		}
	}
	for _, p := range lit.Parms {
		if p.T == nil || !pat.withType(p.T).isGroundType() {
			err := newError(p.L, "cannot infer type of parameter %s", p.Name)
			errs = append(errs, err)
			continue
		}
		lit.Func.Parms = append(lit.Func.Parms, p.T)
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
	if len(lit.Exprs) == 0 {
		lit.Ret = &StructType{L: lit.L}
	} else {
		lit.Ret = lit.Exprs[len(lit.Exprs)-1].Type()
	}
	lit.Func.Ret = lit.Ret
	lit.T = lit.Func
	return lit, errs
}

func checkStrLit(parserLit *parser.StrLit, pat typePattern) (Expr, []Error) {
	lit := &StrLit{
		Text: parserLit.Data,
		T:    &BasicType{Kind: String, L: parserLit.L},
		L:    parserLit.L,
	}
	return lit, nil
}

func checkCharLit(parserLit *parser.CharLit, pat typePattern) (Expr, []Error) {
	parserIntLit := &parser.IntLit{
		Text: strconv.FormatInt(int64(parserLit.Rune), 10),
		L:    parserLit.L,
	}
	return _checkIntLit(parserIntLit, pat, Int32)
}

func checkIntLit(parserLit *parser.IntLit, pat typePattern) (Expr, []Error) {
	return _checkIntLit(parserLit, pat, Int)
}

func _checkIntLit(parserLit *parser.IntLit, pat typePattern, kind BasicTypeKind) (expr Expr, errs []Error) {
	if basic, ok := pat.typ.(*BasicType); ok {
		switch basic.Kind {
		case Float32, Float64:
			floatLit := &parser.FloatLit{Text: parserLit.Text, L: parserLit.L}
			return checkFloatLit(floatLit, pat)
		case Int, Int8, Int16, Int32, Int64, UintRef, Uint, Uint8, Uint16, Uint32, Uint64:
			kind = basic.Kind
		}
	}
	lit := &IntLit{
		Text: parserLit.Text,
		T:    &BasicType{Kind: kind, L: parserLit.L},
		L:    parserLit.L,
	}
	if _, ok := lit.Val.SetString(lit.Text, 0); !ok {
		panic("malformed int")
	}
	defer func() {
		if err := checkValueSize(lit); err != nil {
			errs = append(errs, err)
		}
	}()
	return lit, nil
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

func checkFloatLit(parserLit *parser.FloatLit, pat typePattern) (Expr, []Error) {
	var val big.Float
	if _, _, err := val.Parse(parserLit.Text, 10); err != nil {
		panic(fmt.Sprintf("malformed float: %s", err))
	}
	kind := Float64
	if basic, ok := pat.typ.(*BasicType); ok {
		switch basic.Kind {
		case Float32, Float64:
			kind = basic.Kind
		case Int, Int8, Int16, Int32, Int64, UintRef, Uint, Uint8, Uint16, Uint32, Uint64:
			var i big.Int
			var errs []Error
			if _, acc := val.Int(&i); acc != big.Exact {
				err := newError(parserLit, "%s truncates %s", basic.Kind, parserLit.Text)
				errs = append(errs, err)
			}
			intLit, es := checkIntLit(&parser.IntLit{Text: i.String(), L: parserLit.L}, pat)
			if len(es) > 0 {
				errs = append(errs, es...)
			}
			return intLit, errs
		}
	}
	lit := &FloatLit{
		Text: parserLit.Text,
		T:    &BasicType{Kind: kind, L: parserLit.L},
		Val:  val,
		L:    parserLit.L,
	}
	return lit, nil
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
