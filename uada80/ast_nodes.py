"""
Abstract Syntax Tree node definitions for Ada.

Defines all AST node types for the full Ada 2012 language.
Based on Ada Reference Manual structure.
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import List, Optional


@dataclass
class SourceSpan:
    """Source code location span."""

    filename: str
    start_line: int
    start_column: int
    end_line: int
    end_column: int

    def __str__(self) -> str:
        if self.start_line == self.end_line:
            return f"{self.filename}:{self.start_line}:{self.start_column}-{self.end_column}"
        return f"{self.filename}:{self.start_line}:{self.start_column}-{self.end_line}:{self.end_column}"


# ============================================================================
# Base Classes
# ============================================================================


@dataclass
class ASTNode:
    """Base class for all AST nodes."""

    span: Optional[SourceSpan] = field(default=None, kw_only=True)


@dataclass
class Expr(ASTNode):
    """Base class for expressions."""

    pass


@dataclass
class Stmt(ASTNode):
    """Base class for statements."""

    pass


@dataclass
class Decl(ASTNode):
    """Base class for declarations."""

    pass


@dataclass
class TypeDef(ASTNode):
    """Base class for type definitions."""

    pass


# ============================================================================
# Identifiers and Names
# ============================================================================


@dataclass
class Identifier(Expr):
    """Simple identifier."""

    name: str


@dataclass
class SelectedName(Expr):
    """Selected component (Package.Name)."""

    prefix: Expr
    selector: str


@dataclass
class AttributeReference(Expr):
    """Attribute reference (X'First, Array'Length)."""

    prefix: Expr
    attribute: str
    args: list[Expr] = field(default_factory=list)  # For attributes like Image


@dataclass
class IndexedComponent(Expr):
    """Array indexing (A(I), Matrix(1, 2))."""

    prefix: Expr
    indices: list[Expr]
    # For procedure/function calls with named parameters, preserve full ActualParameter list
    actual_params: list = field(default_factory=list)


@dataclass
class Slice(Expr):
    """Array slice (A(1 .. 10))."""

    prefix: Expr
    range_expr: "RangeExpr"


@dataclass
class FunctionCall(Expr):
    """Function call expression."""

    name: Expr  # Can be Identifier or SelectedName
    args: list["ActualParameter"]


@dataclass
class ActualParameter(ASTNode):
    """Actual parameter in call (positional or named)."""

    name: Optional[str] = None  # None for positional
    value: Optional[Expr] = None


# ============================================================================
# Literals
# ============================================================================


@dataclass
class IntegerLiteral(Expr):
    """Integer literal."""

    value: int
    text: str  # Original text (for based literals, etc.)


@dataclass
class RealLiteral(Expr):
    """Real (floating-point) literal."""

    value: float
    text: str


@dataclass
class StringLiteral(Expr):
    """String literal."""

    value: str


@dataclass
class CharacterLiteral(Expr):
    """Character literal."""

    value: str  # Single character


@dataclass
class NullLiteral(Expr):
    """The null literal for access types."""

    pass


@dataclass
class TargetName(Expr):
    """Target name '@' (Ada 2022).

    Refers to the target of an assignment in the expression.
    Example: X := @ + 1;  -- equivalent to X := X + 1;
    """

    pass


# ============================================================================
# Operators and Expressions
# ============================================================================


class BinaryOp(Enum):
    """Binary operators."""

    # Logical
    AND = auto()
    OR = auto()
    XOR = auto()
    AND_THEN = auto()  # Short-circuit and
    OR_ELSE = auto()  # Short-circuit or

    # Relational
    EQ = auto()  # =
    NE = auto()  # /=
    LT = auto()  # <
    LE = auto()  # <=
    GT = auto()  # >
    GE = auto()  # >=

    # Membership
    IN = auto()
    NOT_IN = auto()

    # Arithmetic
    ADD = auto()  # +
    SUB = auto()  # -
    MUL = auto()  # *
    DIV = auto()  # /
    MOD = auto()
    REM = auto()
    EXP = auto()  # **

    # String
    CONCAT = auto()  # &


class UnaryOp(Enum):
    """Unary operators."""

    PLUS = auto()  # +
    MINUS = auto()  # -
    NOT = auto()
    ABS = auto()


@dataclass
class BinaryExpr(Expr):
    """Binary expression."""

    op: BinaryOp
    left: Expr
    right: Expr


@dataclass
class UnaryExpr(Expr):
    """Unary expression."""

    op: UnaryOp
    operand: Expr


@dataclass
class Parenthesized(Expr):
    """Parenthesized expression."""

    expr: Expr


@dataclass
class Aggregate(Expr):
    """Aggregate expression (array, record, or extension aggregate)."""

    components: list["ComponentAssociation | IteratedComponentAssociation"]


@dataclass
class DeltaAggregate(Expr):
    """Delta aggregate expression (Ada 2022).

    Syntax: (base_expression with delta component_associations)
    Example: (Rec with delta Field1 => Value1, Field2 => Value2)
    Creates a copy of base_expression with specified components modified.
    """

    base_expression: Expr
    components: list["ComponentAssociation"]


@dataclass
class ExtensionAggregate(Expr):
    """Extension aggregate expression (Ada 95+).

    Syntax: (ancestor_part with component_associations)
    Example: (Parent_Type with Field => Value)
    Used to create values of derived record types with extensions.
    """

    ancestor_part: Expr  # Parent type or expression
    components: list["ComponentAssociation"]


@dataclass
class ContainerAggregate(Expr):
    """Container aggregate expression (Ada 2022).

    Syntax: [component_associations]
    Example: [for I in 1 .. 10 => I * 2]
    Uses square brackets for container types (vectors, maps, etc.).
    """

    components: list["ComponentAssociation | IteratedComponentAssociation"]


@dataclass
class ComponentAssociation(ASTNode):
    """Component association in aggregate."""

    choices: list["Choice"]  # Empty for positional
    value: Expr


@dataclass
class IteratedComponentAssociation(ASTNode):
    """Iterated component association (Ada 2012).

    Syntax: for Name in discrete_range => expression
            for Name of iterable => expression

    Example: (for I in 1 .. 10 => I * 2)
    """

    loop_parameter: str
    iterator_spec: Expr  # Discrete range or iterable
    is_of_form: bool  # True for "of", False for "in"
    value: Expr


@dataclass
class Choice(ASTNode):
    """Choice in aggregate or case statement."""

    pass


@dataclass
class ExprChoice(Choice):
    """Expression choice."""

    expr: Expr


@dataclass
class RangeChoice(Choice):
    """Range choice."""

    range_expr: "RangeExpr"


@dataclass
class OthersChoice(Choice):
    """'others' choice."""

    pass


@dataclass
class RangeExpr(Expr):
    """Range expression (1 .. 10)."""

    low: Expr
    high: Expr


@dataclass
class QualifiedExpr(Expr):
    """Qualified expression (Type'(expr))."""

    type_mark: Expr  # Type name
    expr: Expr


@dataclass
class Allocator(Expr):
    """Allocator (new Type, new Type'(expr))."""

    type_mark: Expr
    initial_value: Optional[Expr] = None


@dataclass
class Dereference(Expr):
    """Dereference expression (P.all)."""

    prefix: Expr


@dataclass
class TypeConversion(Expr):
    """Type conversion (Type(expr))."""

    type_mark: Expr
    operand: Expr


@dataclass
class AccessTypeIndication(Expr):
    """Anonymous access type indication.

    Used for:
    - Return types: function F return access Integer
    - Parameter types: procedure P (X : access Integer)
    - Object declarations: X : access Integer
    """

    subtype: Expr  # The designated type
    is_constant: bool = False  # access constant
    not_null: bool = False  # not null access
    is_all: bool = False  # access all
    is_protected: bool = False  # access protected (for subprograms)


@dataclass
class AccessSubprogramTypeIndication(Expr):
    """Anonymous access-to-subprogram type indication.

    Used for return types and parameters:
    - function F return access function (X : Integer) return Integer
    - procedure P (Callback : access procedure)
    """

    is_function: bool  # True for function, False for procedure
    parameters: list["ParameterSpec"] = field(default_factory=list)
    return_type: Optional[Expr] = None  # Only for functions
    not_null: bool = False
    is_protected: bool = False  # access protected procedure/function


@dataclass
class MembershipTest(Expr):
    """Membership test (X in Type, X not in 1..10)."""

    expr: Expr
    is_not: bool
    choices: list[Choice]


@dataclass
class ConditionalExpr(Expr):
    """Conditional expression (if...then...else for expressions, Ada 2012)."""

    condition: Expr
    then_expr: Expr
    elsif_parts: list[tuple[Expr, Expr]] = field(default_factory=list)  # (condition, expr) pairs
    else_expr: Optional[Expr] = None


@dataclass
class QuantifiedExpr(Expr):
    """Quantified expression (for all/some, Ada 2012)."""

    is_for_all: bool  # True for 'for all', False for 'for some'
    iterator: "IteratorSpec"
    predicate: Expr


@dataclass
class DeclareExpr(Expr):
    """Declare expression (Ada 2022).

    Syntax: (declare declarations begin expression)
    Example: (declare X : Integer := 5; begin X + 1)

    Allows local declarations within an expression.
    """

    declarations: list["Decl"]
    result_expr: Expr


@dataclass
class CaseExprAlternative(ASTNode):
    """A single alternative in a case expression."""

    choices: list["Choice"]  # List of choices (values, ranges, or 'others')
    result_expr: Expr


@dataclass
class CaseExpr(Expr):
    """Case expression (Ada 2012).

    Syntax: (case Selector is
               when Choice1 => Expr1,
               when Choice2 => Expr2,
               when others => DefaultExpr)
    """

    selector: Expr
    alternatives: list[CaseExprAlternative] = field(default_factory=list)


# ============================================================================
# Type Definitions
# ============================================================================


@dataclass
class IntegerTypeDef(TypeDef):
    """Integer type definition."""

    range_constraint: Optional[RangeExpr] = None


@dataclass
class ModularTypeDef(TypeDef):
    """Modular (unsigned wraparound) type definition: type X is mod N."""

    modulus: Expr  # The modulus expression (must be static)


@dataclass
class RealTypeDef(TypeDef):
    """Real type definition (floating or fixed point)."""

    is_floating: bool  # True for floating, False for fixed
    digits_expr: Optional[Expr] = None  # For floating
    delta_expr: Optional[Expr] = None  # For fixed
    range_constraint: Optional[RangeExpr] = None


@dataclass
class EnumerationTypeDef(TypeDef):
    """Enumeration type definition."""

    literals: list[str]  # Enumeration literals


@dataclass
class ArrayTypeDef(TypeDef):
    """Array type definition."""

    index_subtypes: list[Expr]  # Can be discrete ranges or type marks
    component_type: Expr
    is_constrained: bool = True
    is_aliased: bool = False  # For "array ... of aliased Type"


@dataclass
class RecordTypeDef(TypeDef):
    """Record type definition."""

    components: list["ComponentDecl"]
    variant_part: Optional["VariantPart"] = None


@dataclass
class ComponentDecl(Decl):
    """Record component declaration."""

    names: list[str]
    type_mark: Expr
    default_value: Optional[Expr] = None


@dataclass
class VariantPart(ASTNode):
    """Variant part of record (discriminated union)."""

    discriminant: str
    variants: list["Variant"]


@dataclass
class Variant(ASTNode):
    """Single variant in variant part."""

    choices: list[Choice]
    components: list[ComponentDecl]


@dataclass
class AccessTypeDef(TypeDef):
    """Access (pointer) type definition."""

    is_access_all: bool  # access all vs. plain access
    is_access_constant: bool
    designated_type: Expr
    is_not_null: bool = False  # not null access
    constraint: Optional[Expr] = None  # range constraint for designated subtype


@dataclass
class AccessSubprogramTypeDef(TypeDef):
    """Access-to-subprogram type definition (function pointers).

    Examples:
        type Unary_Func is access function (X : Integer) return Integer;
        type Callback is access procedure (Status : Integer);
    """

    is_function: bool  # True for function, False for procedure
    parameters: list["ParameterSpec"] = field(default_factory=list)
    return_type: Optional[Expr] = None  # Only for functions
    is_not_null: bool = False
    is_access_protected: bool = False  # access protected procedure/function


@dataclass
class DerivedTypeDef(TypeDef):
    """Derived type definition."""

    parent_type: Expr
    is_abstract: bool = False
    is_limited: bool = False
    record_extension: Optional[RecordTypeDef] = None  # For tagged types
    interfaces: list[Expr] = field(default_factory=list)  # Implemented interfaces (Ada 2005)
    constraint: Optional[Expr] = None  # Range constraint for scalar derived types
    digits_constraint: Optional[Expr] = None  # Precision constraint for derived floating-point
    delta_constraint: Optional[Expr] = None  # Delta constraint for derived fixed-point


@dataclass
class PrivateTypeDef(TypeDef):
    """Private type definition."""

    is_limited: bool = False
    is_tagged: bool = False
    is_abstract: bool = False


@dataclass
class InterfaceTypeDef(TypeDef):
    """Interface type definition (Ada 2005)."""

    is_limited: bool = False
    is_task: bool = False
    is_protected: bool = False
    is_synchronized: bool = False
    parent_interfaces: list[Expr] = field(default_factory=list)


@dataclass
class SubtypeIndication(Expr):
    """Subtype indication with optional constraints.

    This is an expression because it can appear in type contexts
    where an expression is expected (e.g., component declarations).
    """

    type_mark: Expr
    constraint: Optional["Constraint"] = None


# ============================================================================
# Constraints
# ============================================================================


@dataclass
class Constraint(ASTNode):
    """Base class for constraints."""

    pass


@dataclass
class RangeConstraint(Constraint):
    """Range constraint."""

    range_expr: RangeExpr


@dataclass
class IndexConstraint(Constraint):
    """Index constraint for arrays."""

    ranges: list[RangeExpr]


@dataclass
class DiscriminantConstraint(Constraint):
    """Discriminant constraint."""

    discriminants: list[tuple[Optional[str], Expr]]  # (name, value) pairs


@dataclass
class BoxConstraint(Constraint):
    """Box constraint (<>) for unconstrained ranges.

    Used in array index subtype indications like:
        type T is array (Positive range <>) of Element;

    The type_mark specifies the index type (e.g., Positive).
    """

    type_mark: Optional[Expr] = None  # The index type (may be implicit)


@dataclass
class DigitsConstraint(Constraint):
    """Digits constraint for floating-point subtypes.

    Example: subtype My_Float is Float digits 6 range 0.0 .. 1.0;
    """

    digits: Expr  # Number of decimal digits of precision
    range_constraint: Optional["RangeExpr"] = None


@dataclass
class DeltaConstraint(Constraint):
    """Delta constraint for fixed-point subtypes.

    Example: subtype My_Fixed is Fixed delta 0.01 range 0.0 .. 1.0;
    """

    delta: Expr  # The delta value (smallest representable step)
    range_constraint: Optional["RangeExpr"] = None


# ============================================================================
# Declarations
# ============================================================================


@dataclass
class AspectSpecification(ASTNode):
    """Aspect specification (Ada 2012).

    Syntax: with Aspect_Name [=> Expression] [, ...]
    Examples:
        with Inline
        with Pre => X > 0
        with Size => 32
    """

    name: str
    value: Optional[Expr] = None  # None for boolean aspects like Inline


@dataclass
class ObjectDecl(Decl):
    """Object (variable/constant) declaration."""

    names: list[str]
    type_mark: Optional[SubtypeIndication] = None  # Can be omitted if initializer present
    is_constant: bool = False
    is_aliased: bool = False
    init_expr: Optional[Expr] = None
    renames: Optional[Expr] = None  # For renaming declarations
    aspects: list[AspectSpecification] = field(default_factory=list)
    is_atomic: bool = False  # Set by pragma Atomic - wrap accesses in DI/EI
    is_volatile: bool = False  # Set by pragma Volatile - no caching


@dataclass
class NumberDecl(Decl):
    """Named number declaration."""

    names: list[str]
    value: Expr


@dataclass
class TypeDecl(Decl):
    """Type declaration."""

    name: str
    type_def: Optional[TypeDef] = None  # None for incomplete types
    discriminants: list["DiscriminantSpec"] = field(default_factory=list)
    is_abstract: bool = False
    is_tagged: bool = False
    is_limited: bool = False
    aspects: list[AspectSpecification] = field(default_factory=list)
    # Analyzed type from semantic analysis (preserves is_packed, etc.)
    ada_type: Optional[any] = None  # AdaType (avoid circular import)


@dataclass
class SubtypeDecl(Decl):
    """Subtype declaration."""

    name: str
    subtype_indication: SubtypeIndication
    aspects: list["AspectSpecification"] = field(default_factory=list)


@dataclass
class DiscriminantSpec(ASTNode):
    """Discriminant specification."""

    names: list[str]
    type_mark: Expr
    default_value: Optional[Expr] = None
    is_access: bool = False  # For access discriminants


@dataclass
class SubprogramDecl(Decl):
    """Subprogram (procedure/function) declaration."""

    name: str
    is_function: bool
    parameters: list["ParameterSpec"] = field(default_factory=list)
    return_type: Optional[Expr] = None  # For functions
    is_abstract: bool = False
    is_null: bool = False  # Ada 2005 null procedure: procedure P is null;
    is_overriding: bool = False
    is_not_overriding: bool = False
    renames: Optional[Expr] = None  # Subprogram renaming declaration
    aspects: list[AspectSpecification] = field(default_factory=list)


@dataclass
class SubprogramBody(Decl):
    """Subprogram body (implementation)."""

    spec: SubprogramDecl
    declarations: list[Decl] = field(default_factory=list)
    statements: list[Stmt] = field(default_factory=list)
    handled_exception_handlers: list["ExceptionHandler"] = field(default_factory=list)


@dataclass
class ParameterSpec(ASTNode):
    """Parameter specification."""

    names: list[str]
    mode: str  # "in", "out", "in out", "access"
    type_mark: Expr
    default_value: Optional[Expr] = None
    is_aliased: bool = False


@dataclass
class PackageDecl(Decl):
    """Package declaration."""

    name: str
    generic_formals: list["GenericFormal"] = field(default_factory=list)  # For generic packages
    declarations: list[Decl] = field(default_factory=list)
    private_declarations: list[Decl] = field(default_factory=list)
    renames: Optional[Expr] = None  # Package renaming declaration
    is_generic: bool = False  # True if declared with GENERIC keyword


@dataclass
class PackageBody(Decl):
    """Package body."""

    name: str
    declarations: list[Decl] = field(default_factory=list)
    statements: list[Stmt] = field(default_factory=list)  # Optional initialization
    handled_exception_handlers: list["ExceptionHandler"] = field(default_factory=list)


@dataclass
class ExceptionDecl(Decl):
    """Exception declaration."""

    names: list[str]


@dataclass
class UseClause(Decl):
    """Use clause (use Package; use type Type; use all type Type;)."""

    names: list[Expr]
    is_type: bool = False  # use type vs. use package
    is_all: bool = False  # use all type (Ada 2012)


@dataclass
class RenamingDecl(Decl):
    """Renaming declaration."""

    names: list[str]
    renames: Expr
    subtype_mark: Optional[Expr] = None


# ============================================================================
# Generic Declarations
# ============================================================================


@dataclass
class GenericFormal(ASTNode):
    """Generic formal parameter."""

    pass


@dataclass
class GenericTypeDecl(GenericFormal):
    """Generic type formal.

    Supports Ada generic formal type declarations:
    - type T is private
    - type T is tagged private
    - type T is (<>)  -- discrete type
    - type T is range <> -- signed integer
    - type T is mod <> -- modular integer
    - type T is digits <> -- floating point
    - type T is delta <> -- fixed point
    - type T is new Parent -- derived type
    - type T is new Parent with private -- extension
    - type T is array ... -- array type
    - type T is access ... -- access type
    """

    name: str
    definition: Optional[TypeDef] = None  # Full type definition (array, access, etc.)
    constraint: Optional[str] = None  # "private", "discrete", "range", "mod", "digits", "delta", "delta_digits", "derived"
    is_tagged: bool = False
    is_abstract: bool = False
    is_limited: bool = False
    parent_type: Optional[Expr] = None  # For derived types
    with_private: bool = False  # For "new Parent with private"


@dataclass
class GenericSubprogramDecl(GenericFormal):
    """Generic subprogram formal."""

    name: str
    kind: str  # "procedure" or "function"
    params: list["ParameterSpec"] = field(default_factory=list)
    return_type: Optional[Expr] = None  # For functions
    is_box: bool = False  # 'is <>' means match visible subprogram
    default_subprogram: Optional[Expr] = None  # 'is Name' specific default


@dataclass
class GenericPackageDecl(GenericFormal):
    """Generic package formal."""

    name: str
    generic_ref: Expr  # The generic package reference
    is_box: bool = False
    actuals: List = field(default_factory=list)  # Actual parameters for signature


@dataclass
class GenericObjectDecl(GenericFormal):
    """Generic object formal."""

    name: str  # Primary name (first in list)
    mode: str  # "in", "out", or "in out"
    type_ref: Expr
    default_value: Optional[Expr] = None
    names: list[str] = field(default_factory=list)  # All names (F, L : E)


@dataclass
class GenericSubprogramUnit(Decl):
    """Generic subprogram declaration (generic procedure or function).

    Wraps a SubprogramDecl/SubprogramBody with generic formals.
    """

    formals: list["GenericFormal"]
    subprogram: "SubprogramDecl | SubprogramBody"

    @property
    def name(self) -> str:
        if isinstance(self.subprogram, SubprogramBody):
            return self.subprogram.spec.name
        return self.subprogram.name

    @property
    def is_function(self) -> bool:
        if isinstance(self.subprogram, SubprogramBody):
            return self.subprogram.spec.is_function
        return self.subprogram.is_function


@dataclass
class GenericInstantiation(Decl):
    """Generic instantiation."""

    kind: str  # "package", "procedure", or "function"
    name: str
    generic_name: Expr
    actual_parameters: list[ActualParameter] = field(default_factory=list)


# ============================================================================
# Task and Protected Types
# ============================================================================


@dataclass
class TaskTypeDecl(Decl):
    """Task type declaration."""

    name: str
    discriminants: list[DiscriminantSpec] = field(default_factory=list)
    entries: list["EntryDecl"] = field(default_factory=list)
    declarations: list[Decl] = field(default_factory=list)
    interfaces: list["Expr"] = field(default_factory=list)  # Interface inheritance


@dataclass
class TaskBody(Decl):
    """Task body."""

    name: str
    declarations: list[Decl] = field(default_factory=list)
    statements: list[Stmt] = field(default_factory=list)
    handled_exception_handlers: list["ExceptionHandler"] = field(default_factory=list)


@dataclass
class Subunit(Decl):
    """Separate subunit (SEPARATE (parent) body).

    Ada allows subprogram bodies, package bodies, task bodies, and protected
    bodies to be compiled separately using the SEPARATE syntax.
    """

    parent_unit: Expr  # The parent unit name (can be qualified)
    body: Decl  # The actual body (SubprogramBody, PackageBody, TaskBody, etc.)


@dataclass
class BodyStub(Decl):
    """Body stub declaration (IS SEPARATE).

    A body stub declares that the actual body is in a separate compilation unit.
    Syntax: procedure/function Name is separate; or package/task body Name is separate;
    """

    name: str
    kind: str  # "procedure", "function", "package", "task", "protected"


@dataclass
class EntryDecl(Decl):
    """Entry declaration (for tasks/protected types)."""

    name: str
    parameters: list[ParameterSpec] = field(default_factory=list)
    family_index: Optional[Expr] = None  # For entry families


@dataclass
class EntryBody(Decl):
    """Entry body in a protected type (entry E when Barrier is ...).

    Protected entries have a barrier condition that must be true for
    callers to proceed. If the barrier is false, callers are queued.
    """

    name: str
    parameters: list[ParameterSpec] = field(default_factory=list)
    barrier: Optional[Expr] = None  # The "when Condition" barrier expression
    family_index: Optional[str] = None  # For entry family: "for I in Range"
    decls: list["Decl"] = field(default_factory=list)
    stmts: list["Stmt"] = field(default_factory=list)


@dataclass
class ProtectedTypeDecl(Decl):
    """Protected type declaration."""

    name: str
    discriminants: list[DiscriminantSpec] = field(default_factory=list)
    items: list[Decl] = field(default_factory=list)  # Entries, procedures, functions
    interfaces: list["Expr"] = field(default_factory=list)  # Interface inheritance


@dataclass
class ProtectedBody(Decl):
    """Protected body."""

    name: str
    items: list[Decl] = field(default_factory=list)  # Subprogram bodies, entry bodies


# ============================================================================
# Statements
# ============================================================================


@dataclass
class NullStmt(Stmt):
    """Null statement."""

    pass


@dataclass
class AssignmentStmt(Stmt):
    """Assignment statement."""

    target: Expr
    value: Expr


@dataclass
class ProcedureCallStmt(Stmt):
    """Procedure call statement."""

    name: Expr
    args: list[ActualParameter]


@dataclass
class IfStmt(Stmt):
    """If statement."""

    condition: Expr
    then_stmts: list[Stmt]
    elsif_parts: list[tuple[Expr, list[Stmt]]] = field(default_factory=list)
    else_stmts: list[Stmt] = field(default_factory=list)


@dataclass
class CaseStmt(Stmt):
    """Case statement."""

    expr: Expr
    alternatives: list["CaseAlternative"]


@dataclass
class CaseAlternative(ASTNode):
    """Case statement alternative."""

    choices: list[Choice]
    statements: list[Stmt]


@dataclass
class LoopStmt(Stmt):
    """Loop statement."""

    iteration_scheme: Optional["IterationScheme"] = None
    statements: list[Stmt] = field(default_factory=list)
    label: Optional[str] = None
    is_parallel: bool = False  # Ada 2022 parallel loop


@dataclass
class ParallelBlockStmt(Stmt):
    """Parallel block statement (Ada 2022).

    Syntax: parallel do seq1; and do seq2; end parallel;
    Executes multiple statement sequences concurrently.
    """

    sequences: list[list[Stmt]]  # List of statement sequences


@dataclass
class IterationScheme(ASTNode):
    """Iteration scheme for loops."""

    pass


@dataclass
class WhileScheme(IterationScheme):
    """While iteration scheme."""

    condition: Expr


@dataclass
class ForScheme(IterationScheme):
    """For iteration scheme."""

    iterator: "IteratorSpec"


@dataclass
class IteratorSpec(ASTNode):
    """Iterator specification."""

    name: str
    is_reverse: bool = False
    iterable: Expr = None  # Can be range or iterable
    is_of_iterator: bool = False  # True for "for X of Array", False for "for X in Range"


@dataclass
class BlockStmt(Stmt):
    """Block statement."""

    label: Optional[str] = None
    declarations: list[Decl] = field(default_factory=list)
    statements: list[Stmt] = field(default_factory=list)
    handled_exception_handlers: list["ExceptionHandler"] = field(default_factory=list)


@dataclass
class ExitStmt(Stmt):
    """Exit statement."""

    loop_label: Optional[str] = None
    condition: Optional[Expr] = None  # For 'exit when'


@dataclass
class ReturnStmt(Stmt):
    """Return statement."""

    value: Optional[Expr] = None


@dataclass
class ExtendedReturnStmt(Stmt):
    """Extended return statement (Ada 2005).

    return Object_Name : Type [:= Init] do
        Statements;
    end return;
    """

    object_name: str
    type_mark: Optional[Expr] = None
    init_expr: Optional[Expr] = None
    statements: list["Stmt"] = field(default_factory=list)


@dataclass
class LabeledStmt(Stmt):
    """A statement with a label prefix (<<Label>> Statement)."""

    label: str
    statement: Stmt


@dataclass
class GotoStmt(Stmt):
    """Goto statement."""

    label: str


@dataclass
class RaiseStmt(Stmt):
    """Raise statement."""

    exception_name: Optional[Expr] = None
    message: Optional[Expr] = None  # For 'raise E with "message"'


@dataclass
class RaiseExpr(Expr):
    """Raise expression (Ada 2012).

    Syntax: raise Exception_Name [with String_Expression]
    Example: (if X > 0 then X else raise Constraint_Error with "X must be positive")
    """

    exception_name: Expr
    message: Optional[Expr] = None


@dataclass
class DelayStmt(Stmt):
    """Delay statement."""

    is_until: bool  # delay until vs. delay
    expression: Expr


@dataclass
class AbortStmt(Stmt):
    """Abort statement."""

    task_names: list[Expr]


@dataclass
class AcceptStmt(Stmt):
    """Accept statement (for tasks)."""

    entry_name: str
    parameters: list[ParameterSpec] = field(default_factory=list)
    statements: list[Stmt] = field(default_factory=list)


@dataclass
class SelectStmt(Stmt):
    """Select statement (for tasks).

    Supports:
    - Selective accept: alternatives with accept/delay/terminate
    - Conditional entry call: first alternative + else_statements
    - Timed entry call: first alternative + delay alternative
    - Asynchronous select: triggering + then_abort_statements
    """

    alternatives: list["SelectAlternative"]
    else_statements: Optional[list[Stmt]] = None  # For conditional entry call
    then_abort_statements: Optional[list[Stmt]] = None  # For asynchronous select


@dataclass
class SelectAlternative(ASTNode):
    """Select alternative."""

    guard: Optional[Expr] = None
    statements: list[Stmt] = field(default_factory=list)
    is_terminate: bool = False  # For terminate alternative


@dataclass
class RequeueStmt(Stmt):
    """Requeue statement (for entries)."""

    entry_name: Expr
    is_with_abort: bool = False


@dataclass
class PragmaStmt(Stmt):
    """Pragma statement/directive."""

    name: str
    args: list[Expr] = field(default_factory=list)


# ============================================================================
# Exception Handling
# ============================================================================


@dataclass
class ExceptionHandler(ASTNode):
    """Exception handler."""

    exception_names: list[Expr]  # Empty or 'others' for catch-all
    statements: list[Stmt]
    occurrence_name: Optional[str] = None  # Exception occurrence identifier


# ============================================================================
# Compilation Units
# ============================================================================


@dataclass
class WithClause(ASTNode):
    """With clause."""

    names: list[Expr]
    is_private: bool = False
    is_limited: bool = False


@dataclass
class CompilationUnit(ASTNode):
    """Top-level compilation unit."""

    unit: Decl  # Package, subprogram, or generic
    context_clauses: list[WithClause | UseClause] = field(default_factory=list)
    is_separate: bool = False


@dataclass
class Program(ASTNode):
    """Entire program (collection of compilation units)."""

    units: list[CompilationUnit]


# ============================================================================
# Representation Clauses
# ============================================================================


@dataclass
class RepresentationClause(Decl):
    """Base class for representation clauses."""

    pass


@dataclass
class AttributeDefinitionClause(RepresentationClause):
    """Attribute definition clause (for Type'Size use 16;)."""

    name: Expr
    attribute: str
    value: Expr


@dataclass
class RecordRepresentationClause(RepresentationClause):
    """Record representation clause."""

    type_name: Expr
    component_clauses: list["ComponentClause"]


@dataclass
class ComponentClause(ASTNode):
    """Component clause in record representation."""

    name: str
    position: Expr
    first_bit: Expr
    last_bit: Expr


@dataclass
class EnumerationRepresentationClause(RepresentationClause):
    """Enumeration representation clause."""

    type_name: Expr
    values: list[tuple[str, Expr]]  # (literal, code) pairs


# ============================================================================
# Helper Functions
# ============================================================================


def is_simple_expression(node: ASTNode) -> bool:
    """Check if node is a simple expression (identifier or literal)."""
    return isinstance(
        node, (Identifier, IntegerLiteral, RealLiteral, StringLiteral, CharacterLiteral)
    )


def get_node_name(node: Decl) -> Optional[str]:
    """Extract name from a declaration node."""
    if hasattr(node, "name"):
        return node.name
    if hasattr(node, "names") and node.names:
        return node.names[0]
    return None
