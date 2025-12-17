"""Tests for the Ada type system."""

import pytest
from uada80.type_system import (
    TypeKind,
    AdaType,
    IntegerType,
    ModularType,
    EnumerationType,
    ArrayType,
    RecordType,
    RecordComponent,
    AccessType,
    PREDEFINED_TYPES,
    same_type,
    is_subtype_of,
    types_compatible,
    can_convert,
    common_type,
    get_attribute,
)


# ============================================================================
# Predefined Types Tests
# ============================================================================

def test_predefined_boolean():
    """Test predefined Boolean type."""
    bool_type = PREDEFINED_TYPES["Boolean"]

    assert bool_type.name == "Boolean"
    assert bool_type.kind == TypeKind.ENUMERATION
    assert bool_type.size_bits == 8
    assert isinstance(bool_type, EnumerationType)
    assert bool_type.literals == ["False", "True"]
    assert bool_type.pos("False") == 0
    assert bool_type.pos("True") == 1


def test_predefined_integer():
    """Test predefined Integer type."""
    int_type = PREDEFINED_TYPES["Integer"]

    assert int_type.name == "Integer"
    assert int_type.kind == TypeKind.INTEGER
    assert int_type.size_bits == 16
    assert isinstance(int_type, IntegerType)
    assert int_type.low == -32768
    assert int_type.high == 32767
    assert int_type.contains(0)
    assert int_type.contains(-32768)
    assert int_type.contains(32767)
    assert not int_type.contains(32768)


def test_predefined_natural():
    """Test predefined Natural subtype."""
    nat_type = PREDEFINED_TYPES["Natural"]

    assert nat_type.name == "Natural"
    assert nat_type.low == 0
    assert nat_type.high == 32767
    assert nat_type.base_type == PREDEFINED_TYPES["Integer"]
    assert nat_type.contains(0)
    assert not nat_type.contains(-1)


def test_predefined_positive():
    """Test predefined Positive subtype."""
    pos_type = PREDEFINED_TYPES["Positive"]

    assert pos_type.name == "Positive"
    assert pos_type.low == 1
    assert pos_type.high == 32767
    assert not pos_type.contains(0)
    assert pos_type.contains(1)


def test_predefined_character():
    """Test predefined Character type."""
    char_type = PREDEFINED_TYPES["Character"]

    assert char_type.name == "Character"
    assert char_type.kind == TypeKind.ENUMERATION
    assert char_type.size_bits == 8
    assert char_type.pos("A") == 65
    assert char_type.pos("\0") == 0


def test_predefined_short_integer():
    """Test predefined Short_Integer type."""
    short_type = PREDEFINED_TYPES["Short_Integer"]

    assert short_type.size_bits == 8
    assert short_type.low == -128
    assert short_type.high == 127


def test_predefined_unsigned_8():
    """Test predefined Unsigned_8 type."""
    u8_type = PREDEFINED_TYPES["Unsigned_8"]

    assert isinstance(u8_type, ModularType)
    assert u8_type.size_bits == 8
    assert u8_type.modulus == 256
    assert u8_type.low == 0
    assert u8_type.high == 255


def test_predefined_unsigned_16():
    """Test predefined Unsigned_16 type."""
    u16_type = PREDEFINED_TYPES["Unsigned_16"]

    assert isinstance(u16_type, ModularType)
    assert u16_type.size_bits == 16
    assert u16_type.modulus == 65536


def test_predefined_string():
    """Test predefined String type."""
    str_type = PREDEFINED_TYPES["String"]

    assert str_type.name == "String"
    assert str_type.kind == TypeKind.ARRAY
    assert not str_type.is_constrained
    assert str_type.component_type == PREDEFINED_TYPES["Character"]


# ============================================================================
# Integer Type Tests
# ============================================================================

def test_integer_type_size_computation():
    """Test automatic size computation for integer types."""
    # Small positive range fits in 8 bits
    small = IntegerType(name="Small", size_bits=0, low=0, high=100)
    assert small.size_bits == 8

    # Signed range needing 16 bits
    medium = IntegerType(name="Medium", size_bits=0, low=-1000, high=1000)
    assert medium.size_bits == 16

    # Large range needing 32 bits
    large = IntegerType(name="Large", size_bits=0, low=0, high=100000)
    assert large.size_bits == 32


def test_integer_type_contains():
    """Test range checking."""
    byte_range = IntegerType(name="Byte", size_bits=8, low=0, high=255)

    assert byte_range.contains(0)
    assert byte_range.contains(255)
    assert byte_range.contains(100)
    assert not byte_range.contains(-1)
    assert not byte_range.contains(256)


# ============================================================================
# Modular Type Tests
# ============================================================================

def test_modular_type():
    """Test modular (unsigned) type."""
    mod256 = ModularType(name="Mod256", size_bits=0, modulus=256)

    assert mod256.size_bits == 8
    assert mod256.low == 0
    assert mod256.high == 255
    assert mod256.contains(0)
    assert mod256.contains(255)
    assert not mod256.contains(256)
    assert not mod256.contains(-1)


def test_modular_type_large():
    """Test large modular type."""
    mod1000 = ModularType(name="Mod1000", size_bits=0, modulus=1000)

    assert mod1000.size_bits == 16  # Needs more than 8 bits
    assert mod1000.high == 999


# ============================================================================
# Enumeration Type Tests
# ============================================================================

def test_enumeration_type():
    """Test custom enumeration type."""
    color = EnumerationType(
        name="Color",
        size_bits=0,
        literals=["Red", "Green", "Blue"],
    )

    assert color.kind == TypeKind.ENUMERATION
    assert color.size_bits == 8
    assert color.pos("Red") == 0
    assert color.pos("Green") == 1
    assert color.pos("Blue") == 2
    assert color.val(0) == "Red"
    assert color.val(2) == "Blue"
    assert color.low == 0
    assert color.high == 2


def test_enumeration_custom_positions():
    """Test enumeration with custom position values."""
    status = EnumerationType(
        name="Status",
        size_bits=8,
        literals=["Off", "On", "Error"],
        positions={"Off": 0, "On": 1, "Error": 255},
    )

    assert status.pos("Error") == 255
    assert status.val(255) == "Error"


# ============================================================================
# Array Type Tests
# ============================================================================

def test_array_type_constrained():
    """Test constrained array type."""
    int_type = PREDEFINED_TYPES["Integer"]

    vec10 = ArrayType(
        name="Vector10",
        size_bits=0,
        index_types=[int_type],
        component_type=int_type,
        is_constrained=True,
        bounds=[(1, 10)],
    )

    assert vec10.kind == TypeKind.ARRAY
    assert vec10.dimensions() == 1
    assert vec10.length(0) == 10
    assert vec10.size_bits == 10 * 16  # 10 elements * 16 bits


def test_array_type_multidimensional():
    """Test multi-dimensional array type."""
    int_type = PREDEFINED_TYPES["Integer"]

    matrix = ArrayType(
        name="Matrix",
        size_bits=0,
        index_types=[int_type, int_type],
        component_type=int_type,
        is_constrained=True,
        bounds=[(1, 3), (1, 4)],
    )

    assert matrix.dimensions() == 2
    assert matrix.length(0) == 3
    assert matrix.length(1) == 4
    assert matrix.size_bits == 3 * 4 * 16  # 12 elements * 16 bits


def test_array_type_unconstrained():
    """Test unconstrained array type."""
    char_type = PREDEFINED_TYPES["Character"]
    pos_type = PREDEFINED_TYPES["Positive"]

    string = ArrayType(
        name="String",
        size_bits=0,
        index_types=[pos_type],
        component_type=char_type,
        is_constrained=False,
    )

    assert not string.is_constrained
    assert string.size_bits == 0  # Unknown until constrained


# ============================================================================
# Record Type Tests
# ============================================================================

def test_record_type():
    """Test record type."""
    int_type = PREDEFINED_TYPES["Integer"]

    point = RecordType(
        name="Point",
        size_bits=0,
        components=[
            RecordComponent(name="X", component_type=int_type),
            RecordComponent(name="Y", component_type=int_type),
        ],
    )

    assert point.kind == TypeKind.RECORD
    assert point.size_bits == 32  # Two 16-bit integers

    x_comp = point.get_component("X")
    assert x_comp is not None
    assert x_comp.offset_bits == 0

    y_comp = point.get_component("Y")
    assert y_comp is not None
    assert y_comp.offset_bits == 16


def test_record_type_mixed():
    """Test record with mixed component sizes."""
    int_type = PREDEFINED_TYPES["Integer"]
    bool_type = PREDEFINED_TYPES["Boolean"]

    rec = RecordType(
        name="MixedRecord",
        size_bits=0,
        components=[
            RecordComponent(name="Flag", component_type=bool_type),
            RecordComponent(name="Count", component_type=int_type),
        ],
    )

    # Flag is 8 bits, Count is 16 bits
    # Total should be 24 bits (3 bytes)
    assert rec.size_bits == 24


# ============================================================================
# Access Type Tests
# ============================================================================

def test_access_type():
    """Test access (pointer) type."""
    int_type = PREDEFINED_TYPES["Integer"]

    int_ptr = AccessType(
        name="Integer_Ptr",
        size_bits=0,
        designated_type=int_type,
    )

    assert int_ptr.kind == TypeKind.ACCESS
    assert int_ptr.size_bits == 16  # Z80 address size
    assert int_ptr.designated_type == int_type
    assert int_ptr.is_access()


# ============================================================================
# Type Classification Tests
# ============================================================================

def test_type_classification():
    """Test type classification methods."""
    int_type = PREDEFINED_TYPES["Integer"]
    bool_type = PREDEFINED_TYPES["Boolean"]
    str_type = PREDEFINED_TYPES["String"]

    assert int_type.is_scalar()
    assert int_type.is_discrete()
    assert int_type.is_numeric()
    assert not int_type.is_composite()

    assert bool_type.is_scalar()
    assert bool_type.is_discrete()
    assert not bool_type.is_numeric()

    assert str_type.is_composite()
    assert not str_type.is_scalar()


# ============================================================================
# Type Compatibility Tests
# ============================================================================

def test_same_type():
    """Test same_type function."""
    int1 = PREDEFINED_TYPES["Integer"]
    int2 = PREDEFINED_TYPES["Integer"]
    nat = PREDEFINED_TYPES["Natural"]

    assert same_type(int1, int2)
    assert not same_type(int1, nat)


def test_is_subtype_of():
    """Test subtype relationship."""
    integer = PREDEFINED_TYPES["Integer"]
    natural = PREDEFINED_TYPES["Natural"]
    positive = PREDEFINED_TYPES["Positive"]

    assert is_subtype_of(natural, integer)
    assert is_subtype_of(positive, integer)
    assert not is_subtype_of(integer, natural)


def test_types_compatible():
    """Test type compatibility."""
    integer = PREDEFINED_TYPES["Integer"]
    natural = PREDEFINED_TYPES["Natural"]
    boolean = PREDEFINED_TYPES["Boolean"]
    universal = PREDEFINED_TYPES["Universal_Integer"]

    # Same type
    assert types_compatible(integer, integer)

    # Subtype relationship
    assert types_compatible(integer, natural)
    assert types_compatible(natural, integer)

    # Incompatible types
    assert not types_compatible(integer, boolean)

    # Universal integer compatible with integer types
    assert types_compatible(universal, integer)
    assert types_compatible(integer, universal)
    # But not with Boolean
    assert not types_compatible(universal, boolean)


def test_can_convert():
    """Test type conversion rules."""
    integer = PREDEFINED_TYPES["Integer"]
    short = PREDEFINED_TYPES["Short_Integer"]
    unsigned = PREDEFINED_TYPES["Unsigned_8"]
    boolean = PREDEFINED_TYPES["Boolean"]

    # Numeric to numeric is allowed
    assert can_convert(integer, short)
    assert can_convert(short, integer)
    assert can_convert(integer, unsigned)

    # Enumeration to enumeration not allowed
    assert not can_convert(boolean, PREDEFINED_TYPES["Character"])


def test_common_type():
    """Test finding common type."""
    integer = PREDEFINED_TYPES["Integer"]
    natural = PREDEFINED_TYPES["Natural"]
    universal = PREDEFINED_TYPES["Universal_Integer"]
    boolean = PREDEFINED_TYPES["Boolean"]

    # Same type
    assert common_type(integer, integer) == integer

    # Subtype -> base type
    assert common_type(integer, natural) == integer

    # Universal integer
    assert common_type(universal, integer) == integer

    # No common type
    assert common_type(integer, boolean) is None


# ============================================================================
# Type Attribute Tests
# ============================================================================

def test_integer_attributes():
    """Test integer type attributes."""
    integer = PREDEFINED_TYPES["Integer"]

    assert get_attribute(integer, "First") == -32768
    assert get_attribute(integer, "Last") == 32767
    assert get_attribute(integer, "Size") == 16


def test_enumeration_attributes():
    """Test enumeration type attributes."""
    boolean = PREDEFINED_TYPES["Boolean"]

    assert get_attribute(boolean, "First") == "False"
    assert get_attribute(boolean, "Last") == "True"
    assert get_attribute(boolean, "Size") == 8


def test_array_attributes():
    """Test array type attributes."""
    int_type = PREDEFINED_TYPES["Integer"]

    arr = ArrayType(
        name="TestArray",
        size_bits=0,
        index_types=[int_type],
        component_type=int_type,
        is_constrained=True,
        bounds=[(1, 10)],
    )

    assert get_attribute(arr, "Length") == 10
    assert get_attribute(arr, "First") == 1
    assert get_attribute(arr, "Last") == 10


def test_predefined_float():
    """Test predefined Float type (32-bit single precision)."""
    float_type = PREDEFINED_TYPES["Float"]

    assert float_type.name == "Float"
    assert float_type.kind == TypeKind.FLOAT
    assert float_type.size_bits == 32
    assert float_type.digits == 6  # Single precision has ~6 decimal digits


def test_predefined_long_float():
    """Test predefined Long_Float type (64-bit double precision)."""
    long_float_type = PREDEFINED_TYPES["Long_Float"]

    assert long_float_type.name == "Long_Float"
    assert long_float_type.kind == TypeKind.FLOAT
    assert long_float_type.size_bits == 64
    assert long_float_type.digits == 15  # Double precision has ~15 decimal digits


def test_predefined_long_long_float():
    """Test predefined Long_Long_Float type (64-bit on Z80)."""
    llf_type = PREDEFINED_TYPES["Long_Long_Float"]

    assert llf_type.name == "Long_Long_Float"
    assert llf_type.kind == TypeKind.FLOAT
    assert llf_type.size_bits == 64
    assert llf_type.digits == 15
