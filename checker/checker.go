package checker

import (
	"fmt"
	"path/filepath"
	"sort"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
)

type fail struct {
	msg   string
	loc   loc.Loc
	notes []note
	cause []*fail
}

type note struct {
	msg string
	loc loc.Loc // empty for built-in
}

func (f *fail) error(files loc.Files) error {
	return fmt.Errorf("%v: %s", files.Location(f.loc), f.msg)
}

func redef(l loc.Loc, name string, prev loc.Loc) *fail {
	return &fail{
		msg:   name + " redefined",
		loc:   l,
		notes: []note{{msg: "previous", loc: prev}},
	}
}

// Check does semantic checking, and returns a *Mod on success.
func Check(modPath string, files []*parser.File) (*Mod, loc.Files, []error) {
	var fails []*fail
	idNames := make(map[string]loc.Loc)
	typeDefs := make(map[*parser.TypeDef]*TypeDef)
	typeNames := make(map[string]loc.Loc)
	mod := &Mod{Path: modPath}
	importer := newDefaultImporter(files)
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
			path:    parserFile.Path(),
			nls:     parserFile.NewLines(),
			len:     parserFile.Len(),
			Mod:     mod,
			Imports: imports,
		}
		mod.Files = append(mod.Files, file)
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
			parms := makeTypeParms(importer.Files(), parserTypeDef.TypeParms)
			typeDef := &TypeDef{
				File:  file,
				Mod:   modPath,
				Name:  name,
				Parms: parms,
				Exp:   parserTypeDef.Exp,
				L:     parserTypeDef.L,
			}
			typeDefs[parserTypeDef] = typeDef
			mod.Defs = append(mod.Defs, typeDef)
		}
	}

	testNames := make(map[string]loc.Loc)
	for i, file := range mod.Files {
		parserFile := files[i]
		for _, parserDef := range parserFile.Defs {
			switch parserDef := parserDef.(type) {
			case *parser.TypeDef:
				typeDef := typeDefs[parserDef]
				t, fs := makeType(typeDef, parserDef.Type)
				if len(fs) > 0 {
					fails = append(fails, fs...)
				}
				typeDef.Type = t
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
				mod.Defs = append(mod.Defs, &VarDef{
					File:  file,
					Mod:   modPath,
					Name:  name,
					Type:  t,
					Const: parserDef.Const,
					Exp:   parserDef.Exp,
					L:     parserDef.L,
				})
			case *parser.FuncDef:
				typeParms := findTypeParms(importer.Files(), parserDef)
				fun := &FuncDef{
					File:      file,
					Mod:       modPath,
					Name:      parserDef.Name.Name,
					TypeParms: typeParms,
					Exp:       parserDef.Exp,
					L:         parserDef.L,
				}
				var fs []*fail
				if fun.Parms, fs = makeFuncParms(fun, parserDef.Parms); len(fs) > 0 {
					fails = append(fails, fs...)
				}

				if fun.Ret, fs = makeType(fun, parserDef.Ret); len(fs) > 0 {
					fails = append(fails, fs...)
				}
				if fun.Iface, fs = makeFuncDecls(fun, parserDef.Iface); len(fs) > 0 {
					fails = append(fails, fs...)
				}
				mod.Defs = append(mod.Defs, fun)
			case *parser.TestDef:
				name := parserDef.Name.Name
				if prev, ok := testNames[name]; ok {
					fails = append(fails, redef(parserDef.L, name, prev))
					continue
				}
				testNames[name] = parserDef.L
				mod.Defs = append(mod.Defs, &TestDef{
					File: file,
					Mod:  modPath,
					Name: name,
					L:    parserDef.L,
				})
			default:
				panic(fmt.Sprintf("bad def type: %T", parserDef))
			}
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

func makeTypeParms(files loc.Files, parserTypeVars []parser.TypeVar) []TypeParm {
	var typeParms []TypeParm
	for _, parserTypeVar := range parserTypeVars {
		typeParms = append(typeParms, TypeParm{
			Name:     parserTypeVar.Name,
			L:        parserTypeVar.L,
			location: files.Location(parserTypeVar.L),
		})
	}
	return typeParms
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
			Name:     name,
			L:        l,
			location: files.Location(l),
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

func makeType(x scope, parserType parser.Type) (typ Type, fails []*fail) {
	switch parserType := parserType.(type) {
	case nil:
		return nil, nil
	case *parser.RefType:
		typ, fails = makeType(x, parserType.Type)
		typ = &RefType{Type: typ, L: parserType.L}
	case *parser.NamedType:
		var args []Type
		for _, parserArg := range parserType.Args {
			arg, fs := makeType(x, parserArg)
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
			ds := x.find(parserType.Mod.Name)
			switch {
			case len(ds) > 1:
				fails = append(fails, ambig(modName, modLoc))
				return nil, fails
			case len(ds) == 0:
				fails = append(fails, notFound(modName, modLoc))
				return nil, fails
			}
			imp, ok := ds[0].(*Import)
			if !ok {
				fails = append(fails, notImport(modName, modLoc))
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
	case *parser.ArrayType:
		var elemType Type
		elemType, fails = makeType(x, parserType.ElemType)
		typ = &ArrayType{ElemType: elemType, L: parserType.L}
	case *parser.StructType:
		var fields []Field
		fields, fails = makeFields(x, parserType.Fields)
		typ = &StructType{Fields: fields, L: parserType.L}
	case *parser.UnionType:
		var cases []Case
		cases, fails = makeCases(x, parserType.Cases)
		typ = &UnionType{Cases: cases, L: parserType.L}
	case *parser.FuncType:
		parms, fs := makeTypes(x, parserType.Parms)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		ret, fs := makeType(x, parserType.Ret)
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
	var fails []*fail
	var types []Type
	for _, parserType := range parserTypes {
		t, fs := makeType(x, parserType)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		types = append(types, t)
	}
	return types, fails
}

func makeFuncParms(x scope, parserParms []parser.FuncParm) ([]FuncParm, []*fail) {
	var fails []*fail
	var parms []FuncParm
	for _, parserParm := range parserParms {
		t, fs := makeType(x, parserParm.Type)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		parms = append(parms, FuncParm{
			Name: parserParm.Name.Name,
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

func makeFields(x scope, parserFields []parser.Field) ([]Field, []*fail) {
	var fails []*fail
	var fields []Field
	for _, parserField := range parserFields {
		t, fs := makeType(x, parserField.Type)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		fields = append(fields, Field{
			Name: parserField.Name.Name,
			Type: t,
			L:    parserField.L,
		})
	}
	return fields, fails
}

func makeCases(x scope, parserCases []parser.Case) ([]Case, []*fail) {
	var fails []*fail
	var cases []Case
	for _, parserCase := range parserCases {
		t, fs := makeType(x, parserCase.Type)
		if len(fs) > 0 {
			fails = append(fails, fs...)
		}
		cases = append(cases, Case{
			Name: parserCase.Name.Name,
			Type: t,
			L:    parserCase.L,
		})
	}
	return cases, fails
}
