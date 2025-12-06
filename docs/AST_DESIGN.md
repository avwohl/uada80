# AST Design for uada80

## Overview

The Abstract Syntax Tree (AST) for uada80 covers the complete Ada 2012 language specification. The design is hierarchical and type-safe, using Python dataclasses for clean, immutable node definitions.

## Design Principles

1. **Complete Coverage**: All Ada 2012 language constructs
2. **Type Safety**: Strong typing using Python type hints
3. **Immutability**: Dataclasses for value-based nodes
4. **Source Tracking**: Optional source span on every node
5. **Visitor-Friendly**: Clean hierarchy for pattern matching

## Node Hierarchy

```
ASTNode (base)
├── Expr (expressions)
├── Stmt (statements)
├── Decl (declarations)
├── TypeDef (type definitions)
├── Constraint
├── GenericFormal
└── Other specialized nodes
```

## Complete Node Coverage

### Expressions (35+ types)

**Names and References:**
- `Identifier` - Simple names
- `SelectedName` - Qualified names (Package.Entity)
- `AttributeReference` - Attributes (X'First, Type'Size)
- `IndexedComponent` - Array indexing
- `Slice` - Array slices
- `FunctionCall` - Function calls

**Literals:**
- `IntegerLiteral` - Integer constants
- `RealLiteral` - Floating-point constants
- `StringLiteral` - String constants
- `CharacterLiteral` - Character constants
- `NullLiteral` - Null for access types

**Operators:**
- `BinaryExpr` - Binary operations (30+ operators)
  - Arithmetic: +, -, *, /, mod, rem, **
  - Logical: and, or, xor, and then, or else
  - Relational: =, /=, <, <=, >, >=
  - Membership: in, not in
  - String: & (concatenation)
- `UnaryExpr` - Unary operations (+, -, not, abs)

**Complex Expressions:**
- `Aggregate` - Array/record aggregates
- `ComponentAssociation` - Aggregate components
- `RangeExpr` - Range expressions (1..10)
- `QualifiedExpr` - Qualified expressions Type'(value)
- `Allocator` - Dynamic allocation (new Type)
- `TypeConversion` - Type conversions
- `MembershipTest` - Membership tests
- `ConditionalExpr` - If expressions (Ada 2012)
- `QuantifiedExpr` - Quantified expressions (Ada 2012)

### Statements (20+ types)

**Basic:**
- `NullStmt` - Null statement
- `AssignmentStmt` - Assignment
- `ProcedureCallStmt` - Procedure calls

**Control Flow:**
- `IfStmt` - If/elsif/else
- `CaseStmt` - Case statements
- `LoopStmt` - Loops (while, for, infinite)
- `ExitStmt` - Loop exit
- `ReturnStmt` - Return from subprogram
- `GotoStmt` - Goto statement
- `BlockStmt` - Block statements

**Exception Handling:**
- `RaiseStmt` - Raise exception

**Concurrency:**
- `DelayStmt` - Delay/delay until
- `AcceptStmt` - Accept entry call
- `SelectStmt` - Select statement
- `RequeueStmt` - Requeue
- `AbortStmt` - Abort tasks

**Other:**
- `PragmaStmt` - Pragma directives

### Declarations (30+ types)

**Objects:**
- `ObjectDecl` - Variables and constants
- `NumberDecl` - Named numbers

**Types:**
- `TypeDecl` - Type declarations
- `SubtypeDecl` - Subtype declarations

**Subprograms:**
- `SubprogramDecl` - Procedure/function declarations
- `SubprogramBody` - Procedure/function bodies
- `ParameterSpec` - Parameter specifications

**Packages:**
- `PackageDecl` - Package specifications
- `PackageBody` - Package bodies

**Generics:**
- `GenericTypeDecl` - Generic type parameters
- `GenericSubprogramDecl` - Generic subprogram parameters
- `GenericPackageDecl` - Generic package parameters
- `GenericObjectDecl` - Generic object parameters
- `GenericInstantiation` - Generic instantiations

**Tasks and Protected Types:**
- `TaskTypeDecl` - Task type declarations
- `TaskBody` - Task bodies
- `EntryDecl` - Entry declarations
- `ProtectedTypeDecl` - Protected type declarations
- `ProtectedBody` - Protected bodies

**Other:**
- `ExceptionDecl` - Exception declarations
- `UseClause` - Use clauses
- `RenamingDecl` - Renaming declarations

### Type Definitions (10+ types)

- `IntegerTypeDef` - Integer types
- `RealTypeDef` - Floating/fixed point types
- `EnumerationTypeDef` - Enumeration types
- `ArrayTypeDef` - Array types
- `RecordTypeDef` - Record types
  - `ComponentDecl` - Record components
  - `VariantPart` - Discriminated unions
- `AccessTypeDef` - Access (pointer) types
- `DerivedTypeDef` - Derived types (with extensions)
- `PrivateTypeDef` - Private types
- `InterfaceTypeDef` - Interface types (Ada 2005)

### Constraints

- `RangeConstraint` - Range constraints
- `IndexConstraint` - Array index constraints
- `DiscriminantConstraint` - Discriminant constraints

### Representation Clauses

- `AttributeDefinitionClause` - Attribute definitions
- `RecordRepresentationClause` - Record layout
- `EnumerationRepresentationClause` - Enumeration codes

### Compilation Units

- `WithClause` - Context clauses (with/use)
- `CompilationUnit` - Top-level unit
- `Program` - Complete program

## Usage Example

```python
from ast_nodes import *

# Simple variable declaration: X : Integer := 42;
var_decl = ObjectDecl(
    names=["X"],
    type_mark=SubtypeIndication(
        type_mark=Identifier("Integer")
    ),
    is_constant=False,
    init_expr=IntegerLiteral(value=42, text="42")
)

# Function call: Max(A, B)
func_call = FunctionCall(
    name=Identifier("Max"),
    args=[
        ActualParameter(value=Identifier("A")),
        ActualParameter(value=Identifier("B"))
    ]
)

# If statement
if_stmt = IfStmt(
    condition=BinaryExpr(
        op=BinaryOp.GT,
        left=Identifier("X"),
        right=IntegerLiteral(value=0, text="0")
    ),
    then_stmts=[
        AssignmentStmt(
            target=Identifier("Y"),
            value=IntegerLiteral(value=1, text="1")
        )
    ],
    else_stmts=[
        AssignmentStmt(
            target=Identifier("Y"),
            value=IntegerLiteral(value=0, text="0")
        )
    ]
)
```

## Ada Features Covered

### Core Language (Ada 83/95)
- ✅ Basic types (integer, real, enumeration)
- ✅ Arrays and records
- ✅ Subprograms (procedures, functions)
- ✅ Packages
- ✅ Generics
- ✅ Exceptions
- ✅ Access types
- ✅ Derived types
- ✅ Private types

### Object-Oriented Features (Ada 95)
- ✅ Tagged types
- ✅ Type extensions
- ✅ Dispatching operations
- ✅ Abstract types and operations

### Ada 2005 Features
- ✅ Interfaces
- ✅ Null procedures
- ✅ Overriding indicators

### Ada 2012 Features
- ✅ Conditional expressions (if expressions)
- ✅ Quantified expressions (for all/some)
- ✅ Expression functions
- ✅ Some keyword

### Concurrent Features
- ✅ Tasks
- ✅ Protected types
- ✅ Entries and rendezvous
- ✅ Select statements
- ✅ Delay statements

### Low-Level Features
- ✅ Representation clauses
- ✅ Address clauses
- ✅ Unchecked conversion
- ✅ Pragmas

## Parser Integration

The parser will construct these AST nodes during parsing. Key patterns:

### Top-Down Construction
```python
def parse_if_statement(self) -> IfStmt:
    self.expect(TokenType.IF)
    condition = self.parse_expression()
    self.expect(TokenType.THEN)
    then_stmts = self.parse_statement_list()
    # ... handle elsif and else
    return IfStmt(condition=condition, then_stmts=then_stmts, ...)
```

### Expression Precedence
Binary operators are handled with precedence climbing or recursive descent with proper precedence levels.

### Error Recovery
Parser can create partial AST nodes for error recovery and better diagnostics.

## Semantic Analysis Integration

The AST nodes are annotated during semantic analysis:

- **Type Information**: Attached to Expr nodes
- **Symbol References**: Identifiers linked to declarations
- **Scope Information**: Tracked through symbol tables
- **Visibility Rules**: Resolved during analysis

## Code Generation Integration

Code generator walks the AST:

- **Visitor Pattern**: Process each node type
- **Context Tracking**: Maintain state (current function, etc.)
- **Type-Driven**: Use type annotations for correct code gen

## Testing Strategy

1. **Unit Tests**: Test individual node creation
2. **Parser Tests**: Ensure parser creates correct AST
3. **Semantic Tests**: Verify type checking on AST
4. **Transformation Tests**: Test AST optimizations
5. **Code Gen Tests**: Verify correct code from AST

## Future Extensions

- **Source Formatting**: Pretty-printer from AST
- **AST Queries**: Query API for analysis tools
- **Transformations**: AST-to-AST optimizations
- **Metrics**: Complexity metrics from AST

## Statistics

- **Total Node Types**: 80+
- **Expression Types**: 35+
- **Statement Types**: 20+
- **Declaration Types**: 30+
- **Type Definition Types**: 10+
- **Binary Operators**: 30+
- **Unary Operators**: 4

This AST provides a solid foundation for the full Ada compiler implementation.
