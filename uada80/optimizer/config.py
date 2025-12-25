"""
Optimizer configuration for uada80.

Defines optimization levels and targets following uplm80's proven architecture.
"""

from dataclasses import dataclass
from enum import Enum, auto


class OptimizationTarget(Enum):
    """Optimization goal."""

    SPEED = auto()  # Prefer faster code (may increase size)
    SIZE = auto()  # Prefer smaller code (may be slower)
    BALANCED = auto()  # Balance between size and speed


class OptimizationLevel(Enum):
    """Optimization level (0-3)."""

    NONE = 0  # No optimizations
    BASIC = 1  # Constant folding, algebraic simplifications
    STANDARD = 2  # + Strength reduction, dead code elimination
    AGGRESSIVE = 3  # + CSE, copy propagation, loop optimizations


@dataclass
class OptimizationStats:
    """Statistics about applied optimizations."""

    constants_folded: int = 0
    algebraic_simplifications: int = 0
    strength_reductions: int = 0
    dead_code_eliminated: int = 0
    boolean_simplifications: int = 0
    cse_eliminations: int = 0
    copies_propagated: int = 0
    dead_stores_eliminated: int = 0
    loop_invariants_moved: int = 0
    attribute_evaluations: int = 0
    range_checks_eliminated: int = 0
    cases_eliminated: int = 0

    def total(self) -> int:
        """Total optimizations applied."""
        return (
            self.constants_folded
            + self.algebraic_simplifications
            + self.strength_reductions
            + self.dead_code_eliminated
            + self.boolean_simplifications
            + self.cse_eliminations
            + self.copies_propagated
            + self.dead_stores_eliminated
            + self.loop_invariants_moved
            + self.attribute_evaluations
            + self.range_checks_eliminated
            + self.cases_eliminated
        )

    def __str__(self) -> str:
        lines = ["Optimization Statistics:"]
        if self.constants_folded:
            lines.append(f"  Constants folded: {self.constants_folded}")
        if self.algebraic_simplifications:
            lines.append(f"  Algebraic simplifications: {self.algebraic_simplifications}")
        if self.strength_reductions:
            lines.append(f"  Strength reductions: {self.strength_reductions}")
        if self.dead_code_eliminated:
            lines.append(f"  Dead code eliminated: {self.dead_code_eliminated}")
        if self.boolean_simplifications:
            lines.append(f"  Boolean simplifications: {self.boolean_simplifications}")
        if self.cse_eliminations:
            lines.append(f"  CSE eliminations: {self.cse_eliminations}")
        if self.copies_propagated:
            lines.append(f"  Copies propagated: {self.copies_propagated}")
        if self.dead_stores_eliminated:
            lines.append(f"  Dead stores eliminated: {self.dead_stores_eliminated}")
        if self.loop_invariants_moved:
            lines.append(f"  Loop invariants moved: {self.loop_invariants_moved}")
        if self.attribute_evaluations:
            lines.append(f"  Attribute evaluations: {self.attribute_evaluations}")
        if self.range_checks_eliminated:
            lines.append(f"  Range checks eliminated: {self.range_checks_eliminated}")
        if self.cases_eliminated:
            lines.append(f"  Cases eliminated: {self.cases_eliminated}")
        lines.append(f"  Total: {self.total()}")
        return "\n".join(lines)


@dataclass
class OptimizerConfig:
    """Configuration for the optimizer."""

    level: OptimizationLevel = OptimizationLevel.STANDARD
    target: OptimizationTarget = OptimizationTarget.BALANCED

    # Fine-grained control
    fold_constants: bool = True
    algebraic_simplify: bool = True
    strength_reduce: bool = True
    eliminate_dead_code: bool = True
    boolean_simplify: bool = True
    eliminate_cse: bool = True
    propagate_copies: bool = True
    eliminate_dead_stores: bool = True
    move_loop_invariants: bool = True
    evaluate_attributes: bool = True
    eliminate_range_checks: bool = True

    # Loop unrolling threshold (for aggressive optimization)
    max_unroll_iterations: int = 4
    max_unroll_body_size: int = 10

    # Inlining thresholds
    max_inline_statements: int = 5
    max_inline_calls: int = 3

    @classmethod
    def for_level(cls, level: int, target: OptimizationTarget = OptimizationTarget.BALANCED) -> "OptimizerConfig":
        """Create config for a specific optimization level."""
        opt_level = OptimizationLevel(min(level, 3))

        config = cls(level=opt_level, target=target)

        if opt_level == OptimizationLevel.NONE:
            # Disable all optimizations
            config.fold_constants = False
            config.algebraic_simplify = False
            config.strength_reduce = False
            config.eliminate_dead_code = False
            config.boolean_simplify = False
            config.eliminate_cse = False
            config.propagate_copies = False
            config.eliminate_dead_stores = False
            config.move_loop_invariants = False
            config.evaluate_attributes = False
            config.eliminate_range_checks = False

        elif opt_level == OptimizationLevel.BASIC:
            # Level 1: Basic optimizations only
            config.strength_reduce = False
            config.eliminate_dead_code = False
            config.eliminate_cse = False
            config.propagate_copies = False
            config.eliminate_dead_stores = False
            config.move_loop_invariants = False
            config.eliminate_range_checks = False

        elif opt_level == OptimizationLevel.STANDARD:
            # Level 2: Standard optimizations (no aggressive ones)
            config.eliminate_cse = False
            config.propagate_copies = False
            config.eliminate_dead_stores = False
            config.move_loop_invariants = False

        # Level 3 (AGGRESSIVE) enables everything

        return config
