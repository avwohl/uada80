# uada80 Architecture Design

## Project Goal
Create a full Ada compiler targeting the Z80 processor, capable of passing the ACATS (Ada Conformity Assessment Test Suite).

## Overall Architecture

```
┌─────────────────┐
│  Ada Source     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Lexer/Parser   │  (ANTLR4 or hand-written recursive descent)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   AST Builder   │  (Ada-specific AST nodes)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Semantic        │  (Symbol tables, type checking, resolution)
│ Analysis        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ AST Optimizer   │  (Constant folding, CSE, dead code elimination)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Code Generator  │  (Z80 assembly generation)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Peephole        │  (Assembly pattern optimization)
│ Optimizer       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Z80 Assembler   │  (Or emit for external assembler)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Z80 Binary     │
└─────────────────┘
```

## Component Design

### 1. Lexer/Parser

**Technology Options:**
- **ANTLR4**: Powerful parser generator with Ada grammar available
- **Hand-written**: Maximum control, easier debugging, better error messages

**Recommendation**: Start with hand-written recursive descent parser
- Ada's syntax is well-suited for recursive descent
- Better error recovery and diagnostics
- Easier to maintain and debug
- Can optimize for compilation speed

**Output**: Parse tree or direct AST construction

### 2. AST (Abstract Syntax Tree)

**Design Principles:**
- Immutable nodes for safety and optimization
- Type information attached to nodes
- Source location tracking for error reporting
- Support for incremental compilation

**Key Node Categories:**
```python
# Declarations
- PackageDecl
- SubprogramDecl (procedure, function)
- TypeDecl
- ObjectDecl (variables, constants)
- GenericDecl

# Statements
- AssignmentStmt
- IfStmt
- CaseStmt
- LoopStmt (for, while, plain loop)
- BlockStmt
- CallStmt
- ReturnStmt
- RaiseStmt (exceptions)
- ExitStmt
- GotoStmt (limited use)

# Expressions
- Literal (integer, real, string, char, enumeration)
- Identifier
- BinaryOp (arithmetic, logical, relational)
- UnaryOp (not, -, +, abs)
- FunctionCall
- ArrayAccess
- RecordAccess
- TypeConversion
- Qualification
- Aggregate
- AttributeReference ('First, 'Last, 'Length, etc.)

# Types
- ScalarType (integer, enumeration, real)
- ArrayType
- RecordType
- AccessType (pointers)
- SubrangeType
- DerivedType
```

### 3. Semantic Analysis

**Phase 1: Symbol Table Construction**
- Scope management (package, subprogram, block)
- Declaration tracking
- Visibility rules
- Use clauses and renaming

**Phase 2: Name Resolution**
- Overload resolution
- Dotted name resolution (package.entity)
- Attribute resolution

**Phase 3: Type Checking**
- Type compatibility checking
- Implicit conversions
- Constraint checking
- Generic instantiation

**Data Structures:**
```python
class SymbolTable:
    scopes: Stack[Scope]

class Scope:
    parent: Scope | None
    declarations: dict[str, Symbol]
    types: dict[str, TypeInfo]

class Symbol:
    name: str
    kind: SymbolKind  # variable, constant, type, subprogram
    type_info: TypeInfo
    visibility: Visibility

class TypeInfo:
    kind: TypeKind
    base_type: TypeInfo | None
    constraints: list[Constraint]
    size: int  # For code generation
```

### 4. Intermediate Representation (IR)

**Option A: Use Ada AST directly**
- Pros: Simpler, fewer transformations
- Cons: Harder to optimize, target-dependent optimizations difficult

**Option B: Three-Address Code (TAC) / SSA**
- Pros: Easier to optimize, well-studied
- Cons: More complex, additional translation step

**Option C: Hybrid Approach** (Recommended)
- Keep high-level AST for front-end
- Lower to simplified IR for optimization and code generation
- IR closer to Ada semantics than generic TAC

**Proposed IR:**
```python
# Simplified Ada IR - between AST and assembly
class IR_Node:
    pass

class IR_Assign:
    target: IR_Value
    source: IR_Expr

class IR_BinOp:
    op: Operator
    left: IR_Value
    right: IR_Value
    result: IR_Temp

class IR_Call:
    target: str
    args: list[IR_Value]
    result: IR_Temp | None

class IR_If:
    condition: IR_Value
    true_label: str
    false_label: str

class IR_Jump:
    target: str

class IR_Label:
    name: str
```

### 5. AST Optimizer

**Reuse from uplm80** (see OPTIMIZATION_ANALYSIS.md):
- Multi-level optimization (0-3)
- Constant folding and propagation
- CSE (Common Subexpression Elimination)
- Dead code elimination
- Strength reduction
- Loop optimizations

**Ada-specific additions:**
- Attribute evaluation ('First, 'Last, 'Length, 'Size, 'Pos, 'Val)
- Range check elimination (when provably safe)
- Constraint folding
- Discriminant evaluation
- Generic instantiation optimization

### 6. Code Generator

**Target**: Z80 assembly

**Register Allocation:**
Z80 registers:
- A (accumulator): Primary computation
- B, C, D, E: General purpose
- H, L: Address calculation (HL pair)
- IX, IY: Index registers
- SP: Stack pointer

**Calling Convention:**
- Arguments: Pass in registers when possible, else stack
- Return value: A (byte), HL (word), or stack for larger
- Caller/callee saved registers: Define convention
- Stack frame: Use IX or IY as frame pointer

**Memory Layout:**
```
┌─────────────────┐ 0000h
│   ROM/Code      │
├─────────────────┤
│   Constants     │
├─────────────────┤
│   Static Data   │
├─────────────────┤
│   Heap          │ (if dynamic allocation needed)
│   ↓             │
│                 │
│   ↑             │
│   Stack         │
└─────────────────┘ FFFFh
```

**Code Generation Strategy:**
1. **Template-based**: Pattern matching on IR/AST nodes
2. **Expression evaluation**: Stack-based or register allocation
3. **Control flow**: Jump tables for case, structured for loops
4. **Type conversions**: Inline code for simple cases
5. **Runtime library**: For complex operations (multiply, divide for some cases)

### 7. Peephole Optimizer

**Reuse from uplm80**:
- Pattern-based optimization engine
- Multi-pass framework
- Statistics tracking

**Z80-specific patterns:**
- Redundant load elimination
- Jump optimization (convert to relative jumps where possible)
- Stack operation combining
- Register allocation cleanup

### 8. Runtime Library

**Required Components:**
- **Startup code**: Initialize stack, data sections, call main
- **Arithmetic**: 16-bit multiply, divide, modulo
- **I/O**: Basic character I/O for debugging
- **Memory**: Allocation (if heap is supported)
- **Exception handling**: Basic support (may be limited for embedded)
- **Ada runtime**: Minimal runtime for language features

## Compiler Driver

```python
class AdaCompiler:
    def compile(self, source_files: list[str], options: CompileOptions) -> bool:
        # 1. Lex and parse
        ast_units = []
        for file in source_files:
            tokens = lexer.lex(file)
            ast = parser.parse(tokens)
            ast_units.append(ast)

        # 2. Semantic analysis
        symbol_table = SymbolTable()
        for ast in ast_units:
            semantic_analyzer.analyze(ast, symbol_table)

        # 3. Optimization (if enabled)
        if options.opt_level > 0:
            for ast in ast_units:
                ast_optimizer.optimize(ast, options.opt_level)

        # 4. Code generation
        asm_code = []
        for ast in ast_units:
            ir = ir_generator.generate(ast)
            asm = code_generator.generate(ir)
            asm_code.append(asm)

        # 5. Peephole optimization
        if options.opt_level >= 2:
            asm_code = peephole_optimizer.optimize(asm_code)

        # 6. Assembly
        for asm in asm_code:
            assembler.assemble(asm, options.output)

        return True
```

## Language Subset for Initial Implementation

See LANGUAGE_SUBSET.md (to be created)

**Phase 1 (Minimal viable compiler):**
- Procedures and functions
- Integer types (8-bit, 16-bit)
- Boolean type
- Basic control flow (if, loop, for)
- Arrays (constrained)
- Records (simple)
- Assignment and expressions

**Phase 2 (Expanded):**
- Packages
- Access types (pointers)
- Enumeration types
- Subranges
- Type derivation
- More comprehensive runtime

**Phase 3 (ACATS compliance):**
- Generics
- Tasking (if feasible for Z80)
- Exception handling
- Full Ada standard library (adapted for Z80)

## Testing Strategy

1. **Unit tests**: For each compiler phase
2. **Integration tests**: End-to-end compilation tests
3. **ACATS**: Run conformance tests progressively
4. **Regression tests**: Ensure fixes don't break existing code
5. **Z80 emulation**: Test generated code in emulator

## Build System

**Tool**: Python-based build system (similar to uplm80's pyproject.toml)

**Structure:**
```
pyproject.toml
uada80/
  __init__.py
  lexer.py
  parser.py
  ast_nodes.py
  semantic.py
  symbol_table.py
  type_system.py
  ir.py
  optimizer/
    __init__.py
    ast_optimizer.py
    peephole.py
  codegen/
    __init__.py
    z80_codegen.py
    register_alloc.py
  runtime/
    startup.asm
    runtime.asm
  main.py
tests/
  test_lexer.py
  test_parser.py
  test_semantic.py
  test_codegen.py
  integration/
    simple_program.ada
    expected_output.txt
docs/
  ARCHITECTURE.md
  OPTIMIZATION_ANALYSIS.md
  LANGUAGE_SUBSET.md
specs/
  RM-2012.pdf
acats/
  (ACATS test suite)
examples/
  hello.ada
  fibonacci.ada
```

## Development Phases

### Phase 1: Foundation (Weeks 1-4)
- [ ] Set up project structure
- [ ] Implement lexer
- [ ] Implement parser (subset)
- [ ] Define AST nodes
- [ ] Basic semantic analysis

### Phase 2: Code Generation (Weeks 5-8)
- [ ] Symbol table and type system
- [ ] Simple code generator
- [ ] Runtime library basics
- [ ] Test with simple programs

### Phase 3: Optimization (Weeks 9-12)
- [ ] Port AST optimizer from uplm80
- [ ] Port peephole optimizer from uplm80
- [ ] Tune for Ada and Z80

### Phase 4: Language Expansion (Weeks 13-20)
- [ ] Add more language features
- [ ] Expand type system
- [ ] Add package support
- [ ] Improve runtime library

### Phase 5: ACATS Compliance (Weeks 21+)
- [ ] Run ACATS tests
- [ ] Fix failing tests
- [ ] Add missing features
- [ ] Performance tuning

## Success Criteria

1. **Compilation**: Successfully compile Ada subset to Z80
2. **Correctness**: Generated code produces correct results
3. **ACATS**: Pass core ACATS tests
4. **Performance**: Reasonable code size and execution speed
5. **Usability**: Clear error messages, reasonable compile times
