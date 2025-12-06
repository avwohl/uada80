# Optimization Code Analysis from uplm80

## Overview
The uplm80 project contains two main optimization components that can be factored out for reuse in uada80:

1. **AST Optimizer** (`ast_optimizer.py`) - High-level language-agnostic optimizations
2. **Peephole Optimizer** (`peephole.py`) - Low-level assembly pattern-based optimizations

## AST Optimizer (ast_optimizer.py)

### Architecture-Independent Features
The following optimizations are language-agnostic and can be reused:

#### Level 1 Optimizations (Basic)
- **Constant folding**: Evaluate compile-time constant expressions
- **Algebraic simplifications**: x+0=x, x*1=x, x*0=0, etc.
- **Built-in function optimization**: Fold pure functions with constant args

#### Level 2 Optimizations (Standard)
- **Strength reduction**: Convert expensive operations to cheaper ones
  - Multiply by power-of-2 → shift left
  - Divide by power-of-2 → shift right
  - Modulo by power-of-2 → bitwise AND
- **Dead code elimination**: Remove unreachable code after RETURN/GOTO
- **Boolean simplifications**: x=x→true, x<>x→false, etc.
- **Constant condition elimination**: Remove dead branches

#### Level 3 Optimizations (Aggressive)
- **Common Subexpression Elimination (CSE)**: Cache repeated expressions
- **Copy propagation**: Track x=y assignments
- **Dead store elimination**: Remove writes immediately overwritten
- **Loop optimizations**:
  - Loop-invariant code motion
  - Loop unrolling (configurable for size vs speed)
- **Procedure inlining**: Inline small procedures

### Optimization Statistics
The optimizer tracks metrics:
- Constants folded
- Strength reductions
- Dead code eliminated
- Algebraic simplifications
- CSE eliminations
- Loop invariants moved
- Boolean simplifications
- Copies propagated
- Dead stores eliminated
- Loops unrolled
- Procedures inlined

### Optimization Targets
Supports optimization for:
- **SPEED**: Prefer faster code (may increase size)
- **SIZE**: Prefer smaller code (may be slower)
- **BALANCED**: Balance between size and speed

### Key Reusable Components

#### Expression Analysis
```python
_get_expr_vars(expr) - Extract all variables from an expression
_expr_key(expr) - Generate hashable key for CSE
_is_loop_invariant(expr, modified_vars) - Check if expr is loop-invariant
```

#### Constant Evaluation
```python
_eval_binary_const(op, left, right) - Evaluate binary operations
_eval_unary_const(op, value) - Evaluate unary operations
```

#### Pattern Matching
```python
_strength_reduce(op, left, right) - Apply strength reduction
_algebraic_simplify(op, left, right) - Apply algebraic rules
_boolean_simplify(op, left, right) - Apply boolean logic rules
_normalize_commutative(op, left, right) - Canonicalize for CSE
```

## Peephole Optimizer (peephole.py)

### Target-Specific Optimizations
The peephole optimizer is ~2877 lines and performs assembly-level optimizations.

#### Features
1. **Pattern-based optimization**: Define before/after patterns
2. **Multi-pass optimization**: Iterate until no changes
3. **Target-specific rules**:
   - 8080-specific patterns
   - Z80-specific patterns (JR relative jumps, etc.)
   - Mnemonic translation (8080→Z80)

#### Pattern Categories
- Register allocation cleanup
- Redundant load/store elimination
- Jump optimization
- Stack operation optimization
- Arithmetic sequence optimization

### Architecture
```python
@dataclass
class PeepholePattern:
    name: str
    before: list[str]  # Pattern to match (regex)
    after: list[str]   # Replacement
    condition: Callable[[dict], bool] | None  # Optional guard
```

### Key Reusable Components

#### Pattern Engine
- Regex-based pattern matching on instruction sequences
- Variable binding and substitution
- Conditional application
- Multi-pattern application in single pass

#### Target Abstraction
- Support for multiple target architectures
- Mnemonic translation tables
- Target-specific pattern sets

## Factoring Strategy for uada80

### Shared Optimization Library

Create a new module structure:

```
uada80/
  optimizer/
    __init__.py
    ast_optimizer.py      # Adapted from uplm80
    peephole.py          # Adapted from uplm80
    config.py            # Optimization configuration
```

### Adaptations Needed

#### 1. AST Optimizer Adaptations
- Replace PL/M AST node types with Ada AST node types
- Add Ada-specific optimizations:
  - Range check elimination
  - Type constraint folding
  - Discriminant evaluation
  - Generic instantiation optimization
- Preserve the optimization infrastructure:
  - Multi-level optimization (0-3)
  - Statistics tracking
  - Size vs speed tradeoffs

#### 2. Peephole Optimizer Adaptations
- Keep the pattern-matching engine unchanged
- Create Ada/Z80-specific pattern library
- Reuse the multi-pass framework
- Reuse the statistics tracking

### Implementation Plan

1. **Copy optimization infrastructure**
   - Copy `ast_optimizer.py` framework
   - Copy `peephole.py` pattern engine
   - Copy optimization statistics and config

2. **Create abstract base classes**
   ```python
   class ASTOptimizer(ABC):
       # Common optimization logic

   class AdaASTOptimizer(ASTOptimizer):
       # Ada-specific implementations
   ```

3. **Adapt expression handling**
   - Map Ada expression types to optimizer
   - Implement Ada-specific constant evaluation
   - Add Ada type system awareness

4. **Preserve proven patterns**
   - Keep all mathematical simplifications
   - Keep CSE and copy propagation
   - Keep dead code elimination
   - Keep strength reduction

5. **Add Ada-specific optimizations**
   - Constrained type folding
   - Discriminant evaluation
   - Attribute evaluation (e.g., 'First, 'Last, 'Length)
   - Access type dereference optimization

## Benefits of Factoring

1. **Proven code**: uplm80 optimizations are tested and working
2. **Comprehensive**: Covers both high-level and low-level optimizations
3. **Configurable**: Supports different optimization levels and targets
4. **Measurable**: Built-in statistics for optimization effectiveness
5. **Maintainable**: Clear separation of concerns

## Code Quality Notes

The uplm80 optimizer code is:
- Well-documented with clear comments
- Properly typed with Python type hints
- Modular with clear separation of concerns
- Tested (based on the tests/ directory presence)

This makes it an excellent foundation for the Ada compiler optimizer.
