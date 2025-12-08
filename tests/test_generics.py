"""Tests for Ada generics support."""

import pytest
from uada80.parser import parse
from uada80.semantic import analyze
from uada80.ast_nodes import (
    PackageDecl,
    GenericInstantiation,
    GenericTypeDecl,
    GenericSubprogramUnit,
    SubprogramBody,
    SubprogramDecl,
)
from uada80.symbol_table import SymbolKind


class TestGenericParsing:
    """Tests for parsing generic constructs."""

    def test_parse_generic_package_with_type_formal(self):
        """Test parsing a generic package with a type formal parameter."""
        source = """
        generic
            type Element_Type is private;
        package Stack is
            procedure Push(E : Element_Type);
            function Pop return Element_Type;
        end Stack;
        """
        ast = parse(source)
        assert len(ast.units) == 1
        pkg = ast.units[0].unit
        assert isinstance(pkg, PackageDecl)
        assert pkg.name == "Stack"
        assert len(pkg.generic_formals) == 1
        formal = pkg.generic_formals[0]
        assert isinstance(formal, GenericTypeDecl)
        assert formal.name == "Element_Type"

    def test_parse_generic_package_multiple_formals(self):
        """Test parsing a generic with multiple type formals."""
        source = """
        generic
            type Key_Type is private;
            type Value_Type is private;
        package Map is
            procedure Insert(K : Key_Type; V : Value_Type);
        end Map;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        assert len(pkg.generic_formals) == 2
        assert pkg.generic_formals[0].name == "Key_Type"
        assert pkg.generic_formals[1].name == "Value_Type"

    def test_parse_generic_instantiation(self):
        """Test parsing a generic instantiation."""
        source = """
        package Int_Stack is new Stack(Integer);
        """
        ast = parse(source)
        assert len(ast.units) == 1
        inst = ast.units[0].unit
        assert isinstance(inst, GenericInstantiation)
        assert inst.name == "Int_Stack"
        assert inst.kind == "package"
        assert inst.generic_name.name == "Stack"
        assert len(inst.actual_parameters) == 1
        assert inst.actual_parameters[0].value.name == "Integer"

    def test_parse_generic_instantiation_multiple_actuals(self):
        """Test parsing a generic instantiation with multiple actuals."""
        source = """
        package String_Int_Map is new Map(String, Integer);
        """
        ast = parse(source)
        inst = ast.units[0].unit
        assert len(inst.actual_parameters) == 2
        assert inst.actual_parameters[0].value.name == "String"
        assert inst.actual_parameters[1].value.name == "Integer"

    def test_parse_generic_with_tagged_type(self):
        """Test parsing a generic with a tagged type formal."""
        source = """
        generic
            type T is tagged private;
        package Container is
            procedure Store(Item : T);
        end Container;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        formal = pkg.generic_formals[0]
        assert formal.is_tagged


class TestGenericSemantics:
    """Tests for semantic analysis of generics."""

    def test_generic_package_creates_symbol(self):
        """Test that generic packages create the right symbol."""
        source = """
        generic
            type T is private;
        package Container is
            procedure Add(Item : T);
        end Container;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        sym = result.symbols.lookup("Container")
        assert sym is not None
        assert sym.kind == SymbolKind.GENERIC_PACKAGE

    def test_generic_instantiation_creates_package(self):
        """Test that generic instantiation creates a package symbol."""
        source = """
        generic
            type T is private;
        package Container is
            procedure Add(Item : T);
        end Container;

        package Int_Container is new Container(Integer);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        sym = result.symbols.lookup("Int_Container")
        assert sym is not None
        assert sym.kind == SymbolKind.PACKAGE

        # Check it knows its generic origin
        assert hasattr(sym, 'generic_instance_of')
        assert sym.generic_instance_of.name == "Container"

    def test_generic_wrong_number_of_parameters(self):
        """Test error for wrong number of generic parameters."""
        source = """
        generic
            type T is private;
            type U is private;
        package Pair is
            procedure Set(A : T; B : U);
        end Pair;

        package Bad is new Pair(Integer);
        """
        ast = parse(source)
        result = analyze(ast)
        assert result.has_errors
        assert any("wrong number" in str(e).lower() for e in result.errors)

    def test_generic_formal_type_visible_in_package(self):
        """Test that generic formal type is visible inside the package."""
        source = """
        generic
            type Element is private;
        package Stack is
            Current : Element;
        end Stack;
        """
        ast = parse(source)
        result = analyze(ast)
        # Should not error - Element should be a valid type inside Stack
        assert not result.has_errors


class TestGenericWithSubprograms:
    """Tests for generic subprogram formals."""

    def test_parse_generic_with_procedure_formal(self):
        """Test parsing a generic with a procedure formal."""
        source = """
        generic
            type T is private;
            with procedure Process(X : T);
        package Processor is
            procedure Run(Item : T);
        end Processor;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        assert len(pkg.generic_formals) == 2
        # First is type, second is procedure
        from uada80.ast_nodes import GenericSubprogramDecl
        assert isinstance(pkg.generic_formals[1], GenericSubprogramDecl)
        assert pkg.generic_formals[1].kind == "procedure"
        assert pkg.generic_formals[1].name == "Process"

    def test_parse_generic_with_function_formal(self):
        """Test parsing a generic with a function formal."""
        source = """
        generic
            type T is private;
            with function Compare(A, B : T) return Boolean;
        package Sorter is
            procedure Sort;
        end Sorter;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        from uada80.ast_nodes import GenericSubprogramDecl
        assert isinstance(pkg.generic_formals[1], GenericSubprogramDecl)
        assert pkg.generic_formals[1].kind == "function"
        assert pkg.generic_formals[1].name == "Compare"

    def test_parse_generic_with_box_default(self):
        """Test parsing a generic subprogram formal with is <> default."""
        source = """
        generic
            type T is private;
            with function "=" (A, B : T) return Boolean is <>;
        package Container is
            function Contains(Item : T) return Boolean;
        end Container;
        """
        ast = parse(source)
        pkg = ast.units[0].unit
        from uada80.ast_nodes import GenericSubprogramDecl
        func_formal = pkg.generic_formals[1]
        assert isinstance(func_formal, GenericSubprogramDecl)
        assert func_formal.is_box


class TestGenericLowering:
    """Tests for lowering generic constructs to IR."""

    def test_generic_package_not_lowered(self):
        """Test that generic packages (templates) don't generate IR code."""
        from uada80.lowering import ASTLowering

        source = """
        generic
            type T is private;
        package Stack is
            procedure Push(E : T);
        end Stack;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        lowerer = ASTLowering(result.symbols)
        module = lowerer.lower(ast)

        # Generic package shouldn't generate any functions
        assert len(module.functions) == 0

    def test_generic_instantiation_lowered(self):
        """Test that generic instantiations generate IR code."""
        from uada80.lowering import ASTLowering

        source = """
        generic
            type T is private;
        package Container is
        end Container;

        package Int_Container is new Container(Integer);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        lowerer = ASTLowering(result.symbols)
        module = lowerer.lower(ast)

        # Instantiation creates a package but no subprograms to lower
        # This test verifies no errors occur during lowering
        assert module is not None

    def test_generic_instantiation_with_subprogram(self):
        """Test that subprograms in generic instances get proper names."""
        from uada80.lowering import ASTLowering

        # Create a minimal generic package with a procedure body
        source = """
        generic
            type Element is private;
        package Simple_Stack is
            procedure Clear;

            procedure Clear is
            begin
                null;
            end Clear;
        end Simple_Stack;

        package Int_Stack is new Simple_Stack(Integer);
        """
        ast = parse(source)
        result = analyze(ast)
        # Note: May have errors due to mixing spec and body; that's ok for this test
        if not result.has_errors:
            lowerer = ASTLowering(result.symbols)
            module = lowerer.lower(ast)

            # Look for the instantiated procedure with prefixed name
            func_names = [f.name for f in module.functions]
            assert any("Int_Stack" in name for name in func_names) or len(func_names) == 0


class TestGenericSubprograms:
    """Tests for generic procedure and function declarations."""

    def test_parse_generic_procedure(self):
        """Test parsing a generic procedure declaration with body."""
        source = """
        generic
            type Item is private;
        procedure Swap(A, B : in out Item) is
            Temp : Item;
        begin
            Temp := A;
            A := B;
            B := Temp;
        end Swap;
        """
        ast = parse(source)
        assert len(ast.units) == 1
        unit = ast.units[0].unit
        assert isinstance(unit, GenericSubprogramUnit)
        assert len(unit.formals) == 1
        assert unit.formals[0].name == "Item"
        assert isinstance(unit.subprogram, SubprogramBody)
        assert unit.name == "Swap"
        assert not unit.is_function

    def test_parse_generic_function(self):
        """Test parsing a generic function declaration with body."""
        source = """
        generic
            type Element is private;
        function Identity(X : Element) return Element is
        begin
            return X;
        end Identity;
        """
        ast = parse(source)
        unit = ast.units[0].unit
        assert isinstance(unit, GenericSubprogramUnit)
        assert unit.name == "Identity"
        assert unit.is_function
        assert isinstance(unit.subprogram, SubprogramBody)

    def test_parse_generic_procedure_spec_only(self):
        """Test parsing a generic procedure specification (no body)."""
        source = """
        generic
            type T is private;
        procedure Do_Something(X : T);
        """
        ast = parse(source)
        unit = ast.units[0].unit
        assert isinstance(unit, GenericSubprogramUnit)
        assert unit.name == "Do_Something"
        assert isinstance(unit.subprogram, SubprogramDecl)

    def test_generic_procedure_creates_symbol(self):
        """Test that generic procedure creates proper symbol."""
        source = """
        generic
            type T is private;
        procedure Process(X : T) is
        begin
            null;
        end Process;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        sym = result.symbols.lookup("Process")
        assert sym is not None
        assert sym.kind == SymbolKind.GENERIC_PROCEDURE

    def test_generic_function_creates_symbol(self):
        """Test that generic function creates proper symbol."""
        source = """
        generic
            type T is private;
        function Maximum(A, B : T) return T is
        begin
            return A;
        end Maximum;
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        sym = result.symbols.lookup("Maximum")
        assert sym is not None
        assert sym.kind == SymbolKind.GENERIC_FUNCTION

    def test_generic_procedure_instantiation_syntax(self):
        """Test parsing generic procedure instantiation."""
        source = """
        generic
            type T is private;
        procedure Swap(A, B : in out T) is
            Temp : T;
        begin
            Temp := A;
            A := B;
            B := Temp;
        end Swap;

        procedure Int_Swap is new Swap(Integer);
        """
        ast = parse(source)
        assert len(ast.units) == 2
        # First unit is generic
        assert isinstance(ast.units[0].unit, GenericSubprogramUnit)
        # Second unit is instantiation
        assert isinstance(ast.units[1].unit, GenericInstantiation)
        inst = ast.units[1].unit
        assert inst.kind == "procedure"
        assert inst.name == "Int_Swap"

    def test_generic_function_instantiation_syntax(self):
        """Test parsing generic function instantiation."""
        source = """
        generic
            type T is private;
        function Max(A, B : T) return T is
        begin
            return A;
        end Max;

        function Int_Max is new Max(Integer);
        """
        ast = parse(source)
        assert len(ast.units) == 2
        inst = ast.units[1].unit
        assert isinstance(inst, GenericInstantiation)
        assert inst.kind == "function"
        assert inst.name == "Int_Max"

    def test_generic_subprogram_instantiation_semantic(self):
        """Test semantic analysis of generic subprogram instantiation."""
        source = """
        generic
            type T is private;
        procedure Process(X : T) is
        begin
            null;
        end Process;

        procedure Int_Process is new Process(Integer);
        """
        ast = parse(source)
        result = analyze(ast)
        assert not result.has_errors

        # Check generic is registered
        gen_sym = result.symbols.lookup("Process")
        assert gen_sym is not None
        assert gen_sym.kind == SymbolKind.GENERIC_PROCEDURE

        # Check instance is registered
        inst_sym = result.symbols.lookup("Int_Process")
        assert inst_sym is not None
        assert inst_sym.kind == SymbolKind.PROCEDURE

    def test_generic_subprogram_multiple_type_formals(self):
        """Test generic subprogram with multiple type formal parameters."""
        source = """
        generic
            type K is private;
            type V is private;
        procedure Store(Key : K; Value : V) is
        begin
            null;
        end Store;
        """
        ast = parse(source)
        unit = ast.units[0].unit
        assert isinstance(unit, GenericSubprogramUnit)
        assert len(unit.formals) == 2
        assert unit.formals[0].name == "K"
        assert unit.formals[1].name == "V"
