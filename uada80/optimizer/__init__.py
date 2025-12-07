"""
Optimizer module for uada80.

Provides AST-level optimizations for Ada programs.
"""

from uada80.optimizer.config import (
    OptimizationLevel,
    OptimizationTarget,
    OptimizationStats,
    OptimizerConfig,
)
from uada80.optimizer.ast_optimizer import (
    ASTOptimizer,
    optimize,
)

__all__ = [
    "OptimizationLevel",
    "OptimizationTarget",
    "OptimizationStats",
    "OptimizerConfig",
    "ASTOptimizer",
    "optimize",
]
