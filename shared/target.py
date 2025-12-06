"""
Target processor definitions for Z80/8080 code generation.
"""

from enum import Enum, auto


class Target(Enum):
    """Target processor."""

    I8080 = auto()
    Z80 = auto()
