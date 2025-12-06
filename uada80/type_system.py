"""
Ada type system for Z80 target.

Implements Ada's strong type system with support for:
- Predefined types (Integer, Boolean, Character, etc.)
- Constrained integer types and subtypes
- Enumeration types
- Array types (constrained and unconstrained)
- Record types (including discriminated records)
- Access (pointer) types
- Derived types

Z80-specific considerations:
- 8-bit byte is the smallest addressable unit
- 16-bit addresses (64KB address space)
- Integer operations are 8-bit or 16-bit
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional


class TypeKind(Enum):
    """Classification of Ada types."""

    # Scalar types
    INTEGER = auto()  # Signed integer
    MODULAR = auto()  # Unsigned integer with wraparound
    ENUMERATION = auto()  # Enumeration (including Boolean, Character)
    FLOAT = auto()  # Floating point (limited on Z80)
    FIXED = auto()  # Fixed point

    # Composite types
    ARRAY = auto()
    RECORD = auto()

    # Access types
    ACCESS = auto()

    # Special types
    PRIVATE = auto()  # Private type (opaque)
    INCOMPLETE = auto()  # Forward declaration
    UNIVERSAL_INTEGER = auto()  # Compile-time integer
    UNIVERSAL_REAL = auto()  # Compile-time real


@dataclass
class AdaType:
    """Base class for all Ada types."""

    name: str
    kind: TypeKind = TypeKind.INTEGER  # Default, overridden by subclasses
    size_bits: int = 0  # Size in bits
    alignment: int = 1  # Alignment in bytes

    def size_bytes(self) -> int:
        """Return size in bytes, rounded up."""
        return (self.size_bits + 7) // 8

    def is_scalar(self) -> bool:
        """Check if this is a scalar type."""
        return self.kind in (
            TypeKind.INTEGER,
            TypeKind.MODULAR,
            TypeKind.ENUMERATION,
            TypeKind.FLOAT,
            TypeKind.FIXED,
        )

    def is_discrete(self) -> bool:
        """Check if this is a discrete type (integer or enumeration)."""
        return self.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.ENUMERATION)

    def is_numeric(self) -> bool:
        """Check if this is a numeric type."""
        return self.kind in (
            TypeKind.INTEGER,
            TypeKind.MODULAR,
            TypeKind.FLOAT,
            TypeKind.FIXED,
        )

    def is_composite(self) -> bool:
        """Check if this is a composite type."""
        return self.kind in (TypeKind.ARRAY, TypeKind.RECORD)

    def is_access(self) -> bool:
        """Check if this is an access type."""
        return self.kind == TypeKind.ACCESS


@dataclass
class IntegerType(AdaType):
    """Signed integer type with range constraint."""

    low: int = 0
    high: int = 0
    base_type: Optional["IntegerType"] = None  # For subtypes

    def __post_init__(self) -> None:
        self.kind = TypeKind.INTEGER
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute minimum size needed for this range."""
        # For signed: need to fit both low and high
        max_abs = max(abs(self.low), abs(self.high))
        if self.low >= 0:
            # Positive only range
            if max_abs <= 127:
                return 8
            elif max_abs <= 32767:
                return 16
            else:
                return 32
        else:
            # Signed range
            if self.low >= -128 and self.high <= 127:
                return 8
            elif self.low >= -32768 and self.high <= 32767:
                return 16
            else:
                return 32

    def contains(self, value: int) -> bool:
        """Check if value is within range."""
        return self.low <= value <= self.high


@dataclass
class ModularType(AdaType):
    """Unsigned modular type with wraparound arithmetic."""

    modulus: int = 256  # Values are 0 .. modulus-1

    def __post_init__(self) -> None:
        self.kind = TypeKind.MODULAR
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute minimum size for modulus."""
        if self.modulus <= 256:
            return 8
        elif self.modulus <= 65536:
            return 16
        else:
            return 32

    @property
    def low(self) -> int:
        return 0

    @property
    def high(self) -> int:
        return self.modulus - 1

    def contains(self, value: int) -> bool:
        """Check if value is within range."""
        return 0 <= value < self.modulus


@dataclass
class EnumerationType(AdaType):
    """Enumeration type."""

    literals: list[str] = field(default_factory=list)
    # Position values (usually 0, 1, 2, ... but can be customized via rep clause)
    positions: dict[str, int] = field(default_factory=dict)

    def __post_init__(self) -> None:
        self.kind = TypeKind.ENUMERATION
        # Initialize positions if not set
        if not self.positions and self.literals:
            self.positions = {lit: i for i, lit in enumerate(self.literals)}
        # Compute size based on number of literals
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute minimum size for enumeration."""
        count = len(self.literals)
        if count <= 256:
            return 8
        elif count <= 65536:
            return 16
        else:
            return 32

    @property
    def low(self) -> int:
        return min(self.positions.values()) if self.positions else 0

    @property
    def high(self) -> int:
        return max(self.positions.values()) if self.positions else 0

    def pos(self, literal: str) -> int:
        """Get position value of literal."""
        return self.positions.get(literal, -1)

    def val(self, position: int) -> Optional[str]:
        """Get literal at position."""
        for lit, pos in self.positions.items():
            if pos == position:
                return lit
        return None


@dataclass
class ArrayType(AdaType):
    """Array type."""

    index_types: list[AdaType] = field(default_factory=list)  # Index type(s)
    component_type: Optional[AdaType] = None
    is_constrained: bool = True
    # For constrained arrays, the bounds
    bounds: list[tuple[int, int]] = field(default_factory=list)

    def __post_init__(self) -> None:
        self.kind = TypeKind.ARRAY
        if self.size_bits == 0 and self.is_constrained:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute array size in bits."""
        if not self.is_constrained or not self.component_type:
            return 0  # Unknown size for unconstrained
        total_elements = 1
        for low, high in self.bounds:
            total_elements *= (high - low + 1)
        return total_elements * self.component_type.size_bits

    def dimensions(self) -> int:
        """Return number of dimensions."""
        return len(self.index_types)

    def length(self, dimension: int = 0) -> int:
        """Return length of specified dimension."""
        if dimension < len(self.bounds):
            low, high = self.bounds[dimension]
            return high - low + 1
        return 0


@dataclass
class RecordComponent:
    """A component (field) of a record type."""

    name: str
    component_type: AdaType
    offset_bits: int = 0  # Offset from start of record
    default_value: Optional[any] = None


@dataclass
class RecordType(AdaType):
    """Record type."""

    components: list[RecordComponent] = field(default_factory=list)
    discriminants: list[RecordComponent] = field(default_factory=list)
    is_tagged: bool = False
    parent_type: Optional["RecordType"] = None  # For derived types

    def __post_init__(self) -> None:
        self.kind = TypeKind.RECORD
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute record size based on components."""
        if not self.components and not self.discriminants:
            return 0
        # Simple layout: pack components sequentially
        total_bits = 0
        for comp in self.discriminants + self.components:
            # Align to byte boundary for simplicity
            if total_bits % 8 != 0:
                total_bits = ((total_bits + 7) // 8) * 8
            comp.offset_bits = total_bits
            total_bits += comp.component_type.size_bits
        return total_bits

    def get_component(self, name: str) -> Optional[RecordComponent]:
        """Look up component by name."""
        for comp in self.discriminants + self.components:
            if comp.name.lower() == name.lower():
                return comp
        # Check parent type for derived records
        if self.parent_type:
            return self.parent_type.get_component(name)
        return None


@dataclass
class AccessType(AdaType):
    """Access (pointer) type."""

    designated_type: Optional[AdaType] = None
    is_access_all: bool = False  # 'access all' can point to aliased objects
    is_access_constant: bool = False  # 'access constant' for read-only

    def __post_init__(self) -> None:
        self.kind = TypeKind.ACCESS
        # Z80 has 16-bit addresses
        self.size_bits = 16


@dataclass
class SubtypeInfo:
    """Information about a subtype constraint."""

    base_type: AdaType
    constraint_low: Optional[int] = None  # For range constraint
    constraint_high: Optional[int] = None


# =============================================================================
# Predefined Types for Z80 Target
# =============================================================================


def create_predefined_types() -> dict[str, AdaType]:
    """Create the predefined Ada types for Z80 target."""
    types: dict[str, AdaType] = {}

    # Boolean type
    types["Boolean"] = EnumerationType(
        name="Boolean",
        kind=TypeKind.ENUMERATION,
        size_bits=8,  # Use full byte for simplicity
        literals=["False", "True"],
        positions={"False": 0, "True": 1},
    )

    # Character type (8-bit ASCII)
    char_literals = [chr(i) for i in range(256)]
    char_positions = {chr(i): i for i in range(256)}
    types["Character"] = EnumerationType(
        name="Character",
        kind=TypeKind.ENUMERATION,
        size_bits=8,
        literals=char_literals,
        positions=char_positions,
    )

    # Integer type (16-bit signed for Z80)
    types["Integer"] = IntegerType(
        name="Integer",
        kind=TypeKind.INTEGER,
        size_bits=16,
        low=-32768,
        high=32767,
    )

    # Natural subtype (0 .. Integer'Last)
    types["Natural"] = IntegerType(
        name="Natural",
        kind=TypeKind.INTEGER,
        size_bits=16,
        low=0,
        high=32767,
        base_type=types["Integer"],  # type: ignore
    )

    # Positive subtype (1 .. Integer'Last)
    types["Positive"] = IntegerType(
        name="Positive",
        kind=TypeKind.INTEGER,
        size_bits=16,
        low=1,
        high=32767,
        base_type=types["Integer"],  # type: ignore
    )

    # Short_Integer (8-bit signed)
    types["Short_Integer"] = IntegerType(
        name="Short_Integer",
        kind=TypeKind.INTEGER,
        size_bits=8,
        low=-128,
        high=127,
    )

    # Long_Integer (32-bit signed) - supported but slower on Z80
    types["Long_Integer"] = IntegerType(
        name="Long_Integer",
        kind=TypeKind.INTEGER,
        size_bits=32,
        low=-2147483648,
        high=2147483647,
    )

    # Unsigned_8 (modular byte)
    types["Unsigned_8"] = ModularType(
        name="Unsigned_8",
        kind=TypeKind.MODULAR,
        size_bits=8,
        modulus=256,
    )

    # Unsigned_16 (modular word)
    types["Unsigned_16"] = ModularType(
        name="Unsigned_16",
        kind=TypeKind.MODULAR,
        size_bits=16,
        modulus=65536,
    )

    # String type (unconstrained array of Character)
    types["String"] = ArrayType(
        name="String",
        kind=TypeKind.ARRAY,
        size_bits=0,  # Unconstrained
        index_types=[types["Positive"]],  # type: ignore
        component_type=types["Character"],
        is_constrained=False,
    )

    # Universal_Integer (compile-time integer, unlimited precision)
    types["Universal_Integer"] = AdaType(
        name="Universal_Integer",
        kind=TypeKind.UNIVERSAL_INTEGER,
        size_bits=0,  # Conceptual, not stored
    )

    return types


# Global predefined types instance
PREDEFINED_TYPES = create_predefined_types()


# =============================================================================
# Type Compatibility and Conversion
# =============================================================================


def same_type(t1: AdaType, t2: AdaType) -> bool:
    """
    Check if two types are the same type.

    Ada has name equivalence, not structural equivalence.
    Two types are the same only if they have the same declaration.
    """
    # For now, compare by name (proper implementation would use unique IDs)
    return t1.name == t2.name


def is_subtype_of(subtype: AdaType, parent: AdaType) -> bool:
    """Check if subtype is a subtype of parent."""
    if same_type(subtype, parent):
        return True

    # Check base type chain for integer subtypes
    if isinstance(subtype, IntegerType) and subtype.base_type:
        return is_subtype_of(subtype.base_type, parent)

    return False


def types_compatible(t1: AdaType, t2: AdaType) -> bool:
    """
    Check if two types are compatible for assignment/comparison.

    In Ada, types must be the same or one must be a subtype of the other.
    Universal types are compatible with their corresponding types.
    """
    if same_type(t1, t2):
        return True

    # Universal_Integer is compatible with integer/modular types only
    if t1.kind == TypeKind.UNIVERSAL_INTEGER:
        if t2.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.UNIVERSAL_INTEGER):
            return True
    if t2.kind == TypeKind.UNIVERSAL_INTEGER:
        if t1.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.UNIVERSAL_INTEGER):
            return True

    # Check subtype relationship
    if is_subtype_of(t1, t2) or is_subtype_of(t2, t1):
        return True

    return False


def can_convert(from_type: AdaType, to_type: AdaType) -> bool:
    """
    Check if explicit type conversion is allowed.

    Ada allows conversion between:
    - Numeric types
    - Related types (derived from same ancestor)
    - Array types with same component type and convertible index types
    """
    # Same type - always ok
    if same_type(from_type, to_type):
        return True

    # Numeric to numeric
    if from_type.is_numeric() and to_type.is_numeric():
        return True

    # Universal integer to discrete
    if from_type.kind == TypeKind.UNIVERSAL_INTEGER and to_type.is_discrete():
        return True

    # Enumeration types are not convertible to each other (except via Pos/Val)
    if from_type.kind == TypeKind.ENUMERATION and to_type.kind == TypeKind.ENUMERATION:
        return False

    return False


def common_type(t1: AdaType, t2: AdaType) -> Optional[AdaType]:
    """
    Find the common type for a binary operation.

    Returns None if no common type exists.
    """
    if same_type(t1, t2):
        return t1

    # Universal_Integer with discrete -> the discrete type
    if t1.kind == TypeKind.UNIVERSAL_INTEGER and t2.is_discrete():
        return t2
    if t2.kind == TypeKind.UNIVERSAL_INTEGER and t1.is_discrete():
        return t1

    # Subtype and base type -> base type
    if is_subtype_of(t1, t2):
        return t2
    if is_subtype_of(t2, t1):
        return t1

    return None


# =============================================================================
# Type Attributes
# =============================================================================


def get_attribute(type_obj: AdaType, attr: str) -> Optional[any]:
    """
    Get a type attribute value.

    Common attributes:
    - 'First: first value in range
    - 'Last: last value in range
    - 'Range: the range First..Last
    - 'Size: size in bits
    - 'Length: for arrays, number of elements
    - 'Pos: position of enumeration literal
    - 'Val: enumeration literal at position
    """
    attr_lower = attr.lower()

    if attr_lower == "size":
        return type_obj.size_bits

    if isinstance(type_obj, (IntegerType, ModularType)):
        if attr_lower == "first":
            return type_obj.low
        elif attr_lower == "last":
            return type_obj.high

    if isinstance(type_obj, EnumerationType):
        if attr_lower == "first":
            return type_obj.val(type_obj.low)
        elif attr_lower == "last":
            return type_obj.val(type_obj.high)

    if isinstance(type_obj, ArrayType):
        if attr_lower == "length":
            return type_obj.length(0)
        elif attr_lower == "first":
            if type_obj.bounds:
                return type_obj.bounds[0][0]
        elif attr_lower == "last":
            if type_obj.bounds:
                return type_obj.bounds[0][1]

    return None
