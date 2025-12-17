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

    # Concurrent types
    TASK = auto()  # Task type
    PROTECTED = auto()  # Protected type


@dataclass
class AdaType:
    """Base class for all Ada types."""

    name: str
    kind: TypeKind = TypeKind.INTEGER  # Default, overridden by subclasses
    size_bits: int = 0  # Size in bits
    alignment: int = 1  # Alignment in bytes
    is_packed: bool = False  # pragma Pack applied

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
        return self.kind in (
            TypeKind.INTEGER,
            TypeKind.MODULAR,
            TypeKind.ENUMERATION,
            TypeKind.UNIVERSAL_INTEGER,
        )

    def is_numeric(self) -> bool:
        """Check if this is a numeric type."""
        return self.kind in (
            TypeKind.INTEGER,
            TypeKind.MODULAR,
            TypeKind.FLOAT,
            TypeKind.FIXED,
            TypeKind.UNIVERSAL_INTEGER,
            TypeKind.UNIVERSAL_REAL,
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
    base_type: Optional["EnumerationType"] = None  # For derived enumeration types

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
class FloatType(AdaType):
    """Floating point type."""

    # Number of decimal digits of precision
    digits: int = 6
    # Range bounds (if specified)
    range_first: Optional[float] = None
    range_last: Optional[float] = None
    base_type: Optional["FloatType"] = None  # For subtypes

    def __post_init__(self) -> None:
        self.kind = TypeKind.FLOAT
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute size based on required digits."""
        # On Z80, we use software floating point
        # Single precision (32-bit) gives ~7 decimal digits
        # Double precision (64-bit) gives ~15 decimal digits
        if self.digits <= 7:
            return 32  # Single precision
        else:
            return 64  # Double precision (very slow on Z80)


@dataclass
class FixedType(AdaType):
    """Fixed point type."""

    # Delta (smallest increment)
    delta: float = 0.0
    # Range bounds
    range_first: float = 0.0
    range_last: float = 0.0
    # For ordinary fixed point: digits specifies decimal precision
    # For decimal fixed point: digits specifies decimal digits
    digits: Optional[int] = None
    # Small (actual delta after representation)
    small: Optional[float] = None
    base_type: Optional["FixedType"] = None  # For subtypes

    def __post_init__(self) -> None:
        self.kind = TypeKind.FIXED
        if self.small is None:
            self.small = self.delta
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _compute_size(self) -> int:
        """Compute size based on range and delta."""
        if self.delta == 0:
            return 16  # Default
        # Calculate number of values needed
        range_size = abs(self.range_last - self.range_first)
        num_values = int(range_size / self.delta) + 1
        if num_values <= 256:
            return 8
        elif num_values <= 65536:
            return 16
        else:
            return 32


@dataclass
class ArrayType(AdaType):
    """Array type."""

    index_types: list[AdaType] = field(default_factory=list)  # Index type(s)
    component_type: Optional[AdaType] = None
    is_constrained: bool = True
    # For constrained arrays, the bounds
    bounds: list[tuple[int, int]] = field(default_factory=list)
    base_type: Optional["ArrayType"] = None  # For derived array types

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
class DiscriminantConstraint:
    """A constraint on a discriminant value."""

    discriminant_name: str
    constraint_value: Optional[int] = None  # Static value constraint
    constraint_low: Optional[int] = None  # Range constraint lower bound
    constraint_high: Optional[int] = None  # Range constraint upper bound


@dataclass
class RecordComponent:
    """A component (field) of a record type."""

    name: str
    component_type: AdaType
    offset_bits: int = 0  # Offset from start of record
    default_value: Optional[any] = None
    size_bits: Optional[int] = None  # Representation-specified size (overrides component_type.size_bits)
    # For discriminants: constraint information
    is_discriminant: bool = False
    discriminant_constraint: Optional[DiscriminantConstraint] = None
    # Atomic/volatile component attributes (pragma Atomic_Components, etc.)
    is_atomic: bool = False
    is_volatile: bool = False


@dataclass
class PrimitiveOperation:
    """A primitive operation of a tagged type."""

    name: str
    is_function: bool = False
    parameter_types: list[AdaType] = field(default_factory=list)
    return_type: Optional[AdaType] = None
    slot_index: int = 0  # Index in vtable


@dataclass
class InterfaceType(AdaType):
    """Ada interface type (Ada 2005+).

    Interfaces define a set of abstract operations that tagged types can implement.
    Unlike tagged records, interfaces have no data components.
    """

    # List of abstract primitive operations
    primitive_ops: list[PrimitiveOperation] = field(default_factory=list)
    # Interface properties
    is_limited: bool = False  # limited interface
    is_synchronized: bool = False  # synchronized interface (for tasking)
    is_task: bool = False  # task interface
    is_protected: bool = False  # protected interface
    # Parent interfaces (for interface inheritance)
    parent_interfaces: list["InterfaceType"] = field(default_factory=list)

    def __post_init__(self) -> None:
        self.kind = TypeKind.RECORD  # Interfaces are treated as record-like
        # Interface itself has no size (only pointers to vtables)
        self.size_bits = 0

    def all_primitives(self) -> list[PrimitiveOperation]:
        """Get all primitives including those from parent interfaces."""
        result = []
        # Collect from parent interfaces first
        for parent in self.parent_interfaces:
            for op in parent.all_primitives():
                # Check if already in result
                if not any(p.name.lower() == op.name.lower() for p in result):
                    result.append(op)
        # Add own primitives
        for op in self.primitive_ops:
            if not any(p.name.lower() == op.name.lower() for p in result):
                result.append(op)
        return result

    def add_primitive(self, op: PrimitiveOperation) -> None:
        """Add an abstract primitive to this interface."""
        op.slot_index = len(self.all_primitives())
        self.primitive_ops.append(op)


@dataclass
class VariantInfo:
    """Information about a variant in a discriminated record."""
    choices: list  # List of discriminant values that select this variant
    components: list[RecordComponent] = field(default_factory=list)


@dataclass
class VariantPartInfo:
    """Information about the variant part of a discriminated record."""
    discriminant_name: str
    variants: list[VariantInfo] = field(default_factory=list)


@dataclass
class RecordType(AdaType):
    """Record type."""

    components: list[RecordComponent] = field(default_factory=list)
    discriminants: list[RecordComponent] = field(default_factory=list)
    is_tagged: bool = False
    parent_type: Optional["RecordType"] = None  # For derived types
    # For tagged types: list of primitive operations (for vtable)
    primitive_ops: list[PrimitiveOperation] = field(default_factory=list)
    # For class-wide types: reference to the specific tagged type
    is_class_wide: bool = False
    specific_type: Optional["RecordType"] = None
    # Implemented interfaces (Ada 2005+)
    interfaces: list[InterfaceType] = field(default_factory=list)
    # Controlled type support (Ada.Finalization)
    is_controlled: bool = False  # Derives from Controlled
    is_limited_controlled: bool = False  # Derives from Limited_Controlled
    # Limited types - cannot be assigned or copied
    is_limited: bool = False  # Explicitly declared as limited
    # Variant part for discriminated records
    variant_part: Optional[VariantPartInfo] = None

    def __post_init__(self) -> None:
        self.kind = TypeKind.RECORD
        if self.size_bits == 0:
            self.size_bits = self._compute_size()

    def _packed_size(self, comp_type: AdaType) -> int:
        """Get the minimum number of bits needed to store a component when packed.

        For packed records:
        - Boolean needs only 1 bit
        - Small enumerations need log2(count) bits
        - Other types keep their natural size
        """
        if isinstance(comp_type, EnumerationType):
            count = len(comp_type.literals)
            if count == 2:  # Boolean or two-literal enum
                return 1
            elif count <= 4:
                return 2
            elif count <= 8:
                return 3
            elif count <= 16:
                return 4
            elif count <= 32:
                return 5
            elif count <= 64:
                return 6
            elif count <= 128:
                return 7
            # Larger enums stay at their natural size
        return comp_type.size_bits

    def _compute_size(self) -> int:
        """Compute record size based on components.

        For variant records, size is computed as discriminants + common components +
        maximum variant size (all variants occupy the same space).

        When is_packed is True:
        - Boolean fields are packed to 1 bit
        - Small enumerations are packed to minimum bits
        - Larger fields (8+ bits) are aligned to byte boundaries for efficiency
        - This avoids expensive unaligned 16-bit access on Z80
        """
        total_bits = 0
        # Tagged types have a hidden tag field (pointer to vtable)
        if self.is_tagged and not self.is_class_wide:
            total_bits = 16  # Tag is 16-bit pointer on Z80

        # Add discriminants (always byte-aligned)
        for disc in self.discriminants:
            if total_bits % 8 != 0:
                total_bits = ((total_bits + 7) // 8) * 8
            disc.offset_bits = total_bits
            total_bits += disc.component_type.size_bits

        # Add common components
        for comp in self.components:
            if self.is_packed:
                packed_size = self._packed_size(comp.component_type)
                if packed_size < 8:
                    # Pack small fields at bit level
                    comp.offset_bits = total_bits
                    # Override component size for packed access
                    comp.size_bits = packed_size
                    total_bits += packed_size
                else:
                    # Align to byte boundary for larger fields
                    if total_bits % 8 != 0:
                        total_bits = ((total_bits + 7) // 8) * 8
                    comp.offset_bits = total_bits
                    total_bits += comp.component_type.size_bits
            else:
                # Non-packed: align to byte boundary
                if total_bits % 8 != 0:
                    total_bits = ((total_bits + 7) // 8) * 8
                comp.offset_bits = total_bits
                total_bits += comp.component_type.size_bits

        # Add variant part (size = max of all variants)
        if self.variant_part:
            # Align variant start to byte boundary
            if total_bits % 8 != 0:
                total_bits = ((total_bits + 7) // 8) * 8
            variant_start = total_bits
            max_variant_size = 0
            for variant in self.variant_part.variants:
                variant_size = 0
                for comp in variant.components:
                    if self.is_packed:
                        packed_size = self._packed_size(comp.component_type)
                        if packed_size < 8:
                            comp.offset_bits = variant_start + variant_size
                            comp.size_bits = packed_size
                            variant_size += packed_size
                        else:
                            if variant_size % 8 != 0:
                                variant_size = ((variant_size + 7) // 8) * 8
                            comp.offset_bits = variant_start + variant_size
                            variant_size += comp.component_type.size_bits
                    else:
                        if variant_size % 8 != 0:
                            variant_size = ((variant_size + 7) // 8) * 8
                        comp.offset_bits = variant_start + variant_size
                        variant_size += comp.component_type.size_bits
                if variant_size > max_variant_size:
                    max_variant_size = variant_size
            total_bits += max_variant_size

        return total_bits

    def get_component(self, name: str) -> Optional[RecordComponent]:
        """Look up component by name (including variant components)."""
        # Check discriminants first
        for disc in self.discriminants:
            if disc.name.lower() == name.lower():
                return disc
        # Check common components
        for comp in self.components:
            if comp.name.lower() == name.lower():
                return comp
        # Check variant components
        if self.variant_part:
            for variant in self.variant_part.variants:
                for comp in variant.components:
                    if comp.name.lower() == name.lower():
                        return comp
        # Check parent type for derived records
        if self.parent_type:
            return self.parent_type.get_component(name)
        return None

    def get_variant_for_discriminant(self, disc_value) -> Optional[VariantInfo]:
        """Find the variant that matches the given discriminant value."""
        if not self.variant_part:
            return None
        for variant in self.variant_part.variants:
            for choice in variant.choices:
                # Handle different choice types (simple value, range, others)
                if choice == disc_value:
                    return variant
                # Range check would go here
        return None

    def has_variant_part(self) -> bool:
        """Check if this record has a variant part."""
        return self.variant_part is not None

    def get_discriminant(self, name: str) -> Optional[RecordComponent]:
        """Look up discriminant by name."""
        for disc in self.discriminants:
            if disc.name.lower() == name.lower():
                return disc
        return None

    def has_discriminants(self) -> bool:
        """Check if this record type has discriminants."""
        return len(self.discriminants) > 0

    def get_class_wide_type(self) -> "RecordType":
        """Get the class-wide type T'Class for this tagged type."""
        if not self.is_tagged:
            raise ValueError(f"'{self.name}' is not a tagged type")
        if self.is_class_wide:
            return self  # Already class-wide
        return RecordType(
            name=f"{self.name}'Class",
            is_tagged=True,
            is_class_wide=True,
            specific_type=self,
            parent_type=self.parent_type,
            components=self.components,
            discriminants=self.discriminants,
            primitive_ops=self.primitive_ops,
        )

    def add_primitive(self, op: PrimitiveOperation) -> None:
        """Add a primitive operation to this tagged type."""
        # Check if this overrides a parent's operation
        if self.parent_type and self.parent_type.is_tagged:
            for i, parent_op in enumerate(self.parent_type.primitive_ops):
                if parent_op.name.lower() == op.name.lower():
                    # Override: use same slot
                    op.slot_index = parent_op.slot_index
                    self.primitive_ops.append(op)
                    return
        # New primitive: assign next slot
        op.slot_index = len(self.all_primitives())
        self.primitive_ops.append(op)

    def all_primitives(self) -> list[PrimitiveOperation]:
        """Get all primitives including inherited ones and interface primitives."""
        result: list[PrimitiveOperation] = []

        # First, get parent primitives
        if self.parent_type and self.parent_type.is_tagged:
            result = list(self.parent_type.all_primitives())

        # Add interface primitives (abstract placeholders)
        for iface in self.interfaces:
            for iface_op in iface.all_primitives():
                # Check if already have this operation (from parent or another interface)
                if not any(p.name.lower() == iface_op.name.lower() for p in result):
                    result.append(iface_op)

        # Add own primitives, replacing overridden ones
        for op in self.primitive_ops:
            replaced = False
            for i, p in enumerate(result):
                if p.name.lower() == op.name.lower():
                    result[i] = op
                    replaced = True
                    break
            if not replaced:
                result.append(op)

        return result

    def get_primitive(self, name: str) -> Optional[PrimitiveOperation]:
        """Look up a primitive operation by name."""
        for op in self.all_primitives():
            if op.name.lower() == name.lower():
                return op
        return None

    def implements_interface(self, iface: "InterfaceType") -> bool:
        """Check if this record type implements the given interface."""
        # Check direct implementation
        for impl_iface in self.interfaces:
            if impl_iface.name.lower() == iface.name.lower():
                return True
            # Check parent interfaces
            for parent_iface in impl_iface.parent_interfaces:
                if parent_iface.name.lower() == iface.name.lower():
                    return True
        # Check parent type
        if self.parent_type and isinstance(self.parent_type, RecordType):
            return self.parent_type.implements_interface(iface)
        return False

    def needs_finalization(self) -> bool:
        """Check if this type needs finalization (has Finalize)."""
        return self.is_controlled or self.is_limited_controlled or (
            self.parent_type is not None and
            isinstance(self.parent_type, RecordType) and
            self.parent_type.needs_finalization()
        )

    def needs_adjustment(self) -> bool:
        """Check if this type needs adjustment after assignment (Controlled only)."""
        return self.is_controlled or (
            self.parent_type is not None and
            isinstance(self.parent_type, RecordType) and
            self.parent_type.needs_adjustment()
        )

    def is_limited_type(self) -> bool:
        """Check if this type is limited (cannot be assigned or copied).

        A type is limited if:
        - It is explicitly declared as 'limited'
        - It derives from Limited_Controlled
        - It has limited components
        - Its parent type is limited
        """
        if self.is_limited or self.is_limited_controlled:
            return True
        # Check parent
        if self.parent_type and isinstance(self.parent_type, RecordType):
            if self.parent_type.is_limited_type():
                return True
        # Check if any component is limited
        for comp in self.components:
            if comp.component_type and hasattr(comp.component_type, 'is_limited_type'):
                if comp.component_type.is_limited_type():
                    return True
            elif isinstance(comp.component_type, RecordType):
                if comp.component_type.is_limited_type():
                    return True
        return False


@dataclass
class AccessType(AdaType):
    """Access (pointer) type."""

    designated_type: Optional[AdaType] = None
    is_access_all: bool = False  # 'access all' can point to aliased objects
    is_access_constant: bool = False  # 'access constant' for read-only
    is_not_null: bool = False  # 'not null access' cannot be null
    storage_pool: Optional[str] = None  # Name of storage pool (None = default pool)

    def __post_init__(self) -> None:
        self.kind = TypeKind.ACCESS
        # Z80 has 16-bit addresses
        self.size_bits = 16


@dataclass
class AccessSubprogramType(AdaType):
    """Access-to-subprogram type (function pointers)."""

    is_function: bool = False  # True for function, False for procedure
    parameter_types: list[AdaType] = field(default_factory=list)
    return_type: Optional[AdaType] = None  # Only for functions
    is_not_null: bool = False
    is_access_protected: bool = False  # For protected subprograms

    def __post_init__(self) -> None:
        self.kind = TypeKind.ACCESS
        # Z80 has 16-bit addresses - function pointer is just an address
        self.size_bits = 16


@dataclass
class TaskType(AdaType):
    """Task type for concurrent programming."""

    entries: list["EntryInfo"] = field(default_factory=list)
    discriminants: list[RecordComponent] = field(default_factory=list)
    is_single_task: bool = False  # task T is vs task type T is

    def __post_init__(self) -> None:
        self.kind = TypeKind.TASK
        # Task control block size: task ID (2) + state (1) + priority (1) + stack ptr (2)
        self.size_bits = 48  # 6 bytes for Z80


@dataclass
class EntryInfo:
    """Information about a task/protected entry."""

    name: str
    parameter_types: list[AdaType] = field(default_factory=list)
    family_index_type: Optional[AdaType] = None  # For entry families


@dataclass
class ProtectedType(AdaType):
    """Protected type for protected objects."""

    entries: list[EntryInfo] = field(default_factory=list)
    operations: list["ProtectedOperation"] = field(default_factory=list)
    components: list[RecordComponent] = field(default_factory=list)
    is_single_protected: bool = False  # protected P is vs protected type P is

    def __post_init__(self) -> None:
        self.kind = TypeKind.PROTECTED
        # Protected type size: lock byte (1) + components
        total_bits = 8  # Lock byte
        for comp in self.components:
            if total_bits % 8 != 0:
                total_bits = ((total_bits + 7) // 8) * 8
            total_bits += comp.component_type.size_bits
        self.size_bits = total_bits


@dataclass
class ProtectedOperation:
    """Protected procedure, function, or entry."""

    name: str
    kind: str  # "procedure", "function", or "entry"
    parameter_types: list[AdaType] = field(default_factory=list)
    return_type: Optional[AdaType] = None  # For functions


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

    # Wide_Character type (16-bit Unicode BMP)
    wide_char_literals = [chr(i) for i in range(65536)]
    wide_char_positions = {chr(i): i for i in range(65536)}
    types["Wide_Character"] = EnumerationType(
        name="Wide_Character",
        kind=TypeKind.ENUMERATION,
        size_bits=16,
        literals=wide_char_literals,
        positions=wide_char_positions,
    )

    # Wide_String type (unconstrained array of Wide_Character)
    types["Wide_String"] = ArrayType(
        name="Wide_String",
        kind=TypeKind.ARRAY,
        size_bits=0,  # Unconstrained
        index_types=[types["Positive"]],  # type: ignore
        component_type=types["Wide_Character"],
        is_constrained=False,
    )

    # Universal_Integer (compile-time integer, unlimited precision)
    types["Universal_Integer"] = AdaType(
        name="Universal_Integer",
        kind=TypeKind.UNIVERSAL_INTEGER,
        size_bits=0,  # Conceptual, not stored
    )

    # Universal_Real (compile-time real, unlimited precision)
    types["Universal_Real"] = AdaType(
        name="Universal_Real",
        kind=TypeKind.UNIVERSAL_REAL,
        size_bits=0,  # Conceptual, not stored
    )

    # Float type (single precision floating point - 32-bit IEEE on Z80)
    types["Float"] = FloatType(
        name="Float",
        kind=TypeKind.FLOAT,
        size_bits=32,
        digits=6,  # Standard single precision
    )

    # Long_Float type (double precision floating point - 64-bit IEEE 754)
    types["Long_Float"] = FloatType(
        name="Long_Float",
        kind=TypeKind.FLOAT,
        size_bits=64,
        digits=15,  # Standard double precision
    )

    # Long_Long_Float type (extended precision - same as Long_Float on Z80)
    types["Long_Long_Float"] = FloatType(
        name="Long_Long_Float",
        kind=TypeKind.FLOAT,
        size_bits=64,
        digits=15,  # Same as Long_Float on Z80
    )

    # Duration type (fixed point for time intervals)
    types["Duration"] = FixedType(
        name="Duration",
        kind=TypeKind.FIXED,
        size_bits=32,
        delta=0.000001,  # 1 microsecond resolution
        range_first=-86400.0,  # -1 day
        range_last=86400.0,  # +1 day
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


def get_root_type(t: AdaType) -> AdaType:
    """Get the root (base) type of a subtype chain."""
    if isinstance(t, IntegerType) and t.base_type:
        return get_root_type(t.base_type)
    if isinstance(t, EnumerationType) and t.base_type:
        return get_root_type(t.base_type)
    if isinstance(t, ArrayType) and t.base_type:
        return get_root_type(t.base_type)
    return t


def is_subtype_of(subtype: AdaType, parent: AdaType) -> bool:
    """Check if subtype is a subtype of parent."""
    if same_type(subtype, parent):
        return True

    # Check base type chain for integer subtypes
    if isinstance(subtype, IntegerType) and subtype.base_type:
        return is_subtype_of(subtype.base_type, parent)

    # Check base type chain for enumeration subtypes
    if isinstance(subtype, EnumerationType) and subtype.base_type:
        return is_subtype_of(subtype.base_type, parent)

    # Check base type chain for array subtypes
    if isinstance(subtype, ArrayType) and subtype.base_type:
        return is_subtype_of(subtype.base_type, parent)

    return False


def same_base_type(t1: AdaType, t2: AdaType) -> bool:
    """Check if two types share the same base type.

    In Ada, subtypes of the same type are compatible with each other.
    For example, Positive and Natural are both subtypes of Integer.
    """
    if t1.kind != t2.kind:
        return False
    root1 = get_root_type(t1)
    root2 = get_root_type(t2)
    return same_type(root1, root2)


def types_compatible(t1: AdaType, t2: AdaType) -> bool:
    """
    Check if two types are compatible for assignment/comparison.

    In Ada, types must be the same or one must be a subtype of the other.
    Universal types are compatible with their corresponding types.
    """
    if same_type(t1, t2):
        return True

    # Check subtype relationship (e.g., Small is subtype of Integer)
    if is_subtype_of(t1, t2) or is_subtype_of(t2, t1):
        return True

    # Subtypes of the same base type are compatible (e.g., Positive and Natural)
    if same_base_type(t1, t2):
        return True

    # Universal_Integer is compatible with numeric types
    # In Ada, integer literals can initialize/assign to any numeric type (including Float)
    if t1.kind == TypeKind.UNIVERSAL_INTEGER:
        if t2.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.UNIVERSAL_INTEGER,
                       TypeKind.FLOAT, TypeKind.FIXED, TypeKind.UNIVERSAL_REAL):
            return True
    if t2.kind == TypeKind.UNIVERSAL_INTEGER:
        if t1.kind in (TypeKind.INTEGER, TypeKind.MODULAR, TypeKind.UNIVERSAL_INTEGER,
                       TypeKind.FLOAT, TypeKind.FIXED, TypeKind.UNIVERSAL_REAL):
            return True

    # Universal_Real is compatible with float/fixed types
    if t1.kind == TypeKind.UNIVERSAL_REAL:
        if t2.kind in (TypeKind.FLOAT, TypeKind.FIXED, TypeKind.UNIVERSAL_REAL):
            return True
    if t2.kind == TypeKind.UNIVERSAL_REAL:
        if t1.kind in (TypeKind.FLOAT, TypeKind.FIXED, TypeKind.UNIVERSAL_REAL):
            return True

    # Interface compatibility: a tagged type is compatible with interfaces it implements
    if isinstance(t2, InterfaceType) and isinstance(t1, RecordType):
        if t1.is_tagged and t1.implements_interface(t2):
            return True
    if isinstance(t1, InterfaceType) and isinstance(t2, RecordType):
        if t2.is_tagged and t2.implements_interface(t1):
            return True

    # Access type compatibility: two access types are compatible if they
    # have the same designated type. This handles:
    # - Named access types with same designated type
    # - Anonymous access types (from allocators) with named access types
    if t1.kind == TypeKind.ACCESS and t2.kind == TypeKind.ACCESS:
        if isinstance(t1, AccessType) and isinstance(t2, AccessType):
            if t1.designated_type and t2.designated_type:
                # Compare designated types
                if same_type(t1.designated_type, t2.designated_type):
                    return True
                # Also check if designated types are compatible
                if types_compatible(t1.designated_type, t2.designated_type):
                    return True

    # Check subtype relationship
    if is_subtype_of(t1, t2) or is_subtype_of(t2, t1):
        return True

    # String literal compatibility: String is compatible with array-of-character types
    # In Ada, a string literal can be assigned to any array whose component type
    # is Character or a type derived from Character.
    if isinstance(t1, ArrayType) and isinstance(t2, ArrayType):
        if t1.name == "String" or t2.name == "String":
            # Check if the other type's component is Character or derived from Character
            other = t2 if t1.name == "String" else t1
            comp = other.component_type
            if comp:
                # Direct Character type
                if getattr(comp, 'name', None) == 'Character':
                    return True
                # Derived from Character (check base_type chain)
                while hasattr(comp, 'base_type') and comp.base_type:
                    if getattr(comp.base_type, 'name', None) == 'Character':
                        return True
                    comp = comp.base_type

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

    # Universal integer to float (e.g., Float(5))
    if from_type.kind == TypeKind.UNIVERSAL_INTEGER and to_type.kind == TypeKind.FLOAT:
        return True

    # Universal real to numeric
    if from_type.kind == TypeKind.UNIVERSAL_REAL and to_type.is_numeric():
        return True

    # Derived types: conversion between a type and its parent/ancestor is allowed
    # Check if from_type is derived from to_type or vice versa
    if hasattr(from_type, 'base_type') and from_type.base_type:
        if same_type(from_type.base_type, to_type):
            return True
        # Check ancestor chain
        ancestor = from_type.base_type
        while hasattr(ancestor, 'base_type') and ancestor.base_type:
            if same_type(ancestor, to_type) or same_type(ancestor.base_type, to_type):
                return True
            ancestor = ancestor.base_type

    if hasattr(to_type, 'base_type') and to_type.base_type:
        if same_type(to_type.base_type, from_type):
            return True
        # Check ancestor chain
        ancestor = to_type.base_type
        while hasattr(ancestor, 'base_type') and ancestor.base_type:
            if same_type(ancestor, from_type) or same_type(ancestor.base_type, from_type):
                return True
            ancestor = ancestor.base_type

    # Enumeration types that aren't related via derivation are not convertible
    if from_type.kind == TypeKind.ENUMERATION and to_type.kind == TypeKind.ENUMERATION:
        return False

    # Array types: conversion is allowed if they have same component type
    # and convertible index types (Ada RM 4.6)
    if from_type.kind == TypeKind.ARRAY and to_type.kind == TypeKind.ARRAY:
        if isinstance(from_type, ArrayType) and isinstance(to_type, ArrayType):
            # Check component types are the same
            if from_type.component_type and to_type.component_type:
                if same_type(from_type.component_type, to_type.component_type):
                    # Check index types are convertible (same number of dimensions)
                    if len(from_type.index_types) == len(to_type.index_types):
                        return True

    # Access types: conversion between related access types is allowed
    if from_type.kind == TypeKind.ACCESS and to_type.kind == TypeKind.ACCESS:
        if isinstance(from_type, AccessType) and isinstance(to_type, AccessType):
            # Same designated type allows conversion
            if from_type.designated_type and to_type.designated_type:
                if same_type(from_type.designated_type, to_type.designated_type):
                    return True
                # Also check if designated types are convertible
                if can_convert(from_type.designated_type, to_type.designated_type):
                    return True

    return False


def common_type(t1: AdaType, t2: AdaType) -> Optional[AdaType]:
    """
    Find the common type for a binary operation.

    Returns None if no common type exists.
    """
    if same_type(t1, t2):
        return t1

    # Universal_Integer with numeric types -> the numeric type
    # Integer literals can be used with any numeric type in Ada
    if t1.kind == TypeKind.UNIVERSAL_INTEGER:
        if t2.is_discrete():
            return t2
        if t2.kind in (TypeKind.FLOAT, TypeKind.FIXED):
            return t2
    if t2.kind == TypeKind.UNIVERSAL_INTEGER:
        if t1.is_discrete():
            return t1
        if t1.kind in (TypeKind.FLOAT, TypeKind.FIXED):
            return t1

    # Universal_Real with float/fixed -> the float/fixed type
    if t1.kind == TypeKind.UNIVERSAL_REAL:
        if t2.kind in (TypeKind.FLOAT, TypeKind.FIXED):
            return t2
    if t2.kind == TypeKind.UNIVERSAL_REAL:
        if t1.kind in (TypeKind.FLOAT, TypeKind.FIXED):
            return t1

    # Integer * Universal_Real -> Universal_Real (for fixed-point context)
    # This allows expressions like N * 0.0078125 where N is Integer
    # Ada RM allows this when the result is used in a fixed-point context
    if t1.kind == TypeKind.UNIVERSAL_REAL and t2.kind == TypeKind.INTEGER:
        return t1  # Universal_Real
    if t2.kind == TypeKind.UNIVERSAL_REAL and t1.kind == TypeKind.INTEGER:
        return t2  # Universal_Real

    # Universal_Integer * Universal_Real -> Universal_Real
    # This allows expressions like 8 * 0.0078125 (both literals)
    if t1.kind == TypeKind.UNIVERSAL_REAL and t2.kind == TypeKind.UNIVERSAL_INTEGER:
        return t1  # Universal_Real
    if t2.kind == TypeKind.UNIVERSAL_REAL and t1.kind == TypeKind.UNIVERSAL_INTEGER:
        return t2  # Universal_Real

    # Subtype and base type -> base type
    if is_subtype_of(t1, t2):
        return t2
    if is_subtype_of(t2, t1):
        return t1

    # Subtypes with same base type -> return the common base type
    if same_base_type(t1, t2):
        return get_root_type(t1)

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
