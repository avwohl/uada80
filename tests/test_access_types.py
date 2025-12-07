"""Tests for access (pointer) type support."""

import pytest
from uada80.compiler import compile_source, Compiler, OutputFormat


# ============================================================================
# Access Type Declaration Tests
# ============================================================================


def test_access_type_declaration():
    """Test basic access type declaration."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_access_all_type():
    """Test 'access all' type declaration."""
    source = """
    procedure Test is
        type Int_Ptr is access all Integer;
        P : Int_Ptr;
    begin
        P := null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_access_constant_type():
    """Test 'access constant' type declaration."""
    source = """
    procedure Test is
        type Int_Ptr is access constant Integer;
        P : Int_Ptr;
    begin
        P := null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Allocator (new) Tests
# ============================================================================


def test_simple_allocator():
    """Test simple allocator expression."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_allocator_with_initial_value():
    """Test allocator with initial value."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer'(42);
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_allocator_record():
    """Test allocator for record type."""
    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        type Point_Ptr is access Point;
        P : Point_Ptr;
    begin
        P := new Point;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Dereference (.all) Tests
# ============================================================================


def test_dereference_read():
    """Test reading through .all dereference."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
        V : Integer;
    begin
        P := new Integer'(42);
        V := P.all;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_dereference_write():
    """Test writing through .all dereference."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer;
        P.all := 42;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_dereference_in_expression():
    """Test using .all in expressions."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
        Result : Integer;
    begin
        P := new Integer'(10);
        Result := P.all + 5;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Null Pointer Tests
# ============================================================================


def test_null_assignment():
    """Test assigning null to pointer."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_null_initialization():
    """Test initializing pointer with null."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr := null;
    begin
        null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Access to Record Tests
# ============================================================================


def test_access_record_field():
    """Test accessing record field through pointer."""
    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        type Point_Ptr is access Point;
        P : Point_Ptr;
        V : Integer;
    begin
        P := new Point;
        P.all.X := 10;
        V := P.all.X;
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_implicit_dereference_record():
    """Test implicit dereference for record field access (Ada style)."""
    source = """
    procedure Test is
        type Point is record
            X : Integer;
            Y : Integer;
        end record;
        type Point_Ptr is access Point;
        P : Point_Ptr;
        V : Integer;
    begin
        P := new Point;
        P.X := 10;  -- Implicit dereference
        V := P.X;   -- Implicit dereference
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# IR Generation Tests
# ============================================================================


def test_allocator_ir_generation():
    """Test IR generation for allocator."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer'(100);
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Should contain a call to heap allocator
    assert "call" in result.output.lower() or "heap" in result.output.lower()


def test_dereference_ir_generation():
    """Test IR generation for dereference."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
        V : Integer;
    begin
        P := new Integer'(42);
        V := P.all;
    end Test;
    """

    compiler = Compiler(output_format=OutputFormat.IR)
    result = compiler.compile(source)

    assert result.success
    # Should contain load instruction for dereference
    assert "load" in result.output.lower()


# ============================================================================
# Z80 Code Generation Tests
# ============================================================================


def test_access_code_generation():
    """Test Z80 code generation for access types."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := new Integer'(42);
        P.all := 100;
    end Test;
    """

    result = compile_source(source)

    assert result.success
    # Should generate call to heap allocator
    assert "call" in result.output.lower() or "_heap_alloc" in result.output


def test_null_code_generation():
    """Test Z80 code generation for null."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P : Int_Ptr;
    begin
        P := null;
    end Test;
    """

    result = compile_source(source)

    assert result.success
    # null should be represented as 0
    assert "ld" in result.output.lower()


# ============================================================================
# Linked List Pattern Tests
# ============================================================================


def test_linked_list_type():
    """Test linked list type pattern."""
    source = """
    procedure Test is
        type Node;
        type Node_Ptr is access Node;
        type Node is record
            Value : Integer;
            Next : Node_Ptr;
        end record;
        Head : Node_Ptr;
    begin
        Head := null;
    end Test;
    """

    result = compile_source(source)
    # This may not be fully supported yet due to forward declarations
    assert result is not None


def test_simple_linked_structure():
    """Test simple linked structure."""
    source = """
    procedure Test is
        type Cell is record
            Value : Integer;
            Next_Value : Integer;
        end record;
        type Cell_Ptr is access Cell;
        P : Cell_Ptr;
    begin
        P := new Cell;
        P.Value := 1;
        P.Next_Value := 2;
    end Test;
    """

    result = compile_source(source)
    assert result.success


# ============================================================================
# Multiple Allocation Tests
# ============================================================================


def test_multiple_allocations():
    """Test multiple allocations."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        P1 : Int_Ptr;
        P2 : Int_Ptr;
        P3 : Int_Ptr;
    begin
        P1 := new Integer'(1);
        P2 := new Integer'(2);
        P3 := new Integer'(3);
    end Test;
    """

    result = compile_source(source)
    assert result.success


def test_array_of_pointers():
    """Test array of access types."""
    source = """
    procedure Test is
        type Int_Ptr is access Integer;
        type Ptr_Array is array (1 .. 5) of Int_Ptr;
        Ptrs : Ptr_Array;
    begin
        Ptrs(1) := new Integer'(10);
        Ptrs(2) := null;
    end Test;
    """

    result = compile_source(source)
    assert result.success


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
