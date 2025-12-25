"""
Runtime Library Manager for uada80.

Tracks which runtime routines are needed during compilation and generates
the appropriate EXTRN declarations. This replaces inline code emission
with linking against libada.lib.
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Set


class RuntimeCategory(Enum):
    """Categories of runtime routines."""

    HEAP = auto()       # Memory allocation
    MATH = auto()       # Integer math operations
    FIXED = auto()      # Fixed-point (16.16) operations
    FLOAT48 = auto()    # 48-bit floating point
    STRING = auto()     # String operations
    IO = auto()         # Text I/O
    EXCEPTION = auto()  # Exception handling
    TASKING = auto()    # Multitasking runtime
    CONTAINER = auto()  # Container operations
    DISPATCH = auto()   # OOP dispatch
    CONTROLLED = auto() # Controlled type finalization
    STREAM = auto()     # Stream I/O
    BOUNDED = auto()    # Bounded strings
    C_INTERFACE = auto() # C interoperability


@dataclass
class RuntimeRoutine:
    """Description of a runtime routine."""

    name: str           # Symbol name (e.g., "_mul16")
    category: RuntimeCategory
    description: str = ""
    dependencies: Set[str] = field(default_factory=set)  # Other routines this depends on


# Master catalog of all runtime routines from libada.lib
RUNTIME_CATALOG = {
    # =========================================================================
    # Heap operations (heap.asm)
    # =========================================================================
    "_heap_init": RuntimeRoutine("_heap_init", RuntimeCategory.HEAP, "Initialize heap"),
    "_heap_alloc": RuntimeRoutine("_heap_alloc", RuntimeCategory.HEAP, "Allocate memory"),
    "_heap_free": RuntimeRoutine("_heap_free", RuntimeCategory.HEAP, "Free memory"),

    # =========================================================================
    # Integer math operations (math.asm)
    # =========================================================================
    "_mul16": RuntimeRoutine("_mul16", RuntimeCategory.MATH, "16-bit multiply"),
    "_mul16_32": RuntimeRoutine("_mul16_32", RuntimeCategory.MATH, "16x16->32 multiply"),
    "_div16": RuntimeRoutine("_div16", RuntimeCategory.MATH, "16-bit unsigned divide"),
    "_div16_signed": RuntimeRoutine("_div16_signed", RuntimeCategory.MATH, "16-bit signed divide"),
    "_mod16": RuntimeRoutine("_mod16", RuntimeCategory.MATH, "16-bit modulo"),
    "_abs16": RuntimeRoutine("_abs16", RuntimeCategory.MATH, "16-bit absolute value"),
    "_neg16": RuntimeRoutine("_neg16", RuntimeCategory.MATH, "16-bit negate"),
    "_min16": RuntimeRoutine("_min16", RuntimeCategory.MATH, "16-bit minimum"),
    "_max16": RuntimeRoutine("_max16", RuntimeCategory.MATH, "16-bit maximum"),

    # =========================================================================
    # 48-bit floating point operations (z88dk math48 library)
    # Library: libmath48.lib
    # Uses BCDEHL for primary accumulator (L = exponent)
    # Uses B'C'D'E'H'L' for secondary accumulator
    # =========================================================================
    "mm48_fpadd": RuntimeRoutine("mm48_fpadd", RuntimeCategory.FLOAT48, "Float add: AC' = AC' + AC"),
    "mm48_fpsub": RuntimeRoutine("mm48_fpsub", RuntimeCategory.FLOAT48, "Float subtract: AC' = AC' - AC"),
    "mm48_fpmul": RuntimeRoutine("mm48_fpmul", RuntimeCategory.FLOAT48, "Float multiply: AC' = AC' * AC"),
    "mm48_fpdiv": RuntimeRoutine("mm48_fpdiv", RuntimeCategory.FLOAT48, "Float divide: AC' = AC' / AC"),
    "mm48_negate": RuntimeRoutine("mm48_negate", RuntimeCategory.FLOAT48, "Float negate: AC' = -AC'"),
    "mm48_cmp": RuntimeRoutine("mm48_cmp", RuntimeCategory.FLOAT48, "Float compare: AC' <=> AC"),
    "mm48_equal": RuntimeRoutine("mm48_equal", RuntimeCategory.FLOAT48, "Float equality test"),
    "mm48_int": RuntimeRoutine("mm48_int", RuntimeCategory.FLOAT48, "Float to integer"),
    "mm48_frac": RuntimeRoutine("mm48_frac", RuntimeCategory.FLOAT48, "Fractional part"),
    "mm48_mod": RuntimeRoutine("mm48_mod", RuntimeCategory.FLOAT48, "Float modulo"),
    "mm48_sqr": RuntimeRoutine("mm48_sqr", RuntimeCategory.FLOAT48, "Square root"),
    "mm48_sin": RuntimeRoutine("mm48_sin", RuntimeCategory.FLOAT48, "Sine"),
    "mm48_cos": RuntimeRoutine("mm48_cos", RuntimeCategory.FLOAT48, "Cosine"),
    "mm48_tan": RuntimeRoutine("mm48_tan", RuntimeCategory.FLOAT48, "Tangent"),
    "mm48_atn": RuntimeRoutine("mm48_atn", RuntimeCategory.FLOAT48, "Arctangent"),
    "mm48_exp": RuntimeRoutine("mm48_exp", RuntimeCategory.FLOAT48, "Exponential"),
    "mm48_ln": RuntimeRoutine("mm48_ln", RuntimeCategory.FLOAT48, "Natural logarithm"),
    "mm48_log": RuntimeRoutine("mm48_log", RuntimeCategory.FLOAT48, "Base-10 logarithm"),
    "mm48_pwr": RuntimeRoutine("mm48_pwr", RuntimeCategory.FLOAT48, "Power: AC'^AC"),
    "mm48_mul10": RuntimeRoutine("mm48_mul10", RuntimeCategory.FLOAT48, "Multiply by 10"),
    "mm48_tenf": RuntimeRoutine("mm48_tenf", RuntimeCategory.FLOAT48, "Power of 10"),
    # Support routines
    "am48_dfix16": RuntimeRoutine("am48_dfix16", RuntimeCategory.FLOAT48, "Float to 16-bit int in HL"),
    "am48_double16": RuntimeRoutine("am48_double16", RuntimeCategory.FLOAT48, "16-bit int to float"),
    "am48_dneg": RuntimeRoutine("am48_dneg", RuntimeCategory.FLOAT48, "Negate AC'"),

    # =========================================================================
    # Fixed-point operations (math.asm)
    # =========================================================================
    "_fix_add": RuntimeRoutine("_fix_add", RuntimeCategory.FIXED, "Fixed-point add"),
    "_fix_sub": RuntimeRoutine("_fix_sub", RuntimeCategory.FIXED, "Fixed-point subtract"),
    "_fix_neg": RuntimeRoutine("_fix_neg", RuntimeCategory.FIXED, "Fixed-point negate"),
    "_fix_abs": RuntimeRoutine("_fix_abs", RuntimeCategory.FIXED, "Fixed-point absolute"),
    "_fix_to_int": RuntimeRoutine("_fix_to_int", RuntimeCategory.FIXED, "Fixed to integer"),
    "_fix_from_int": RuntimeRoutine("_fix_from_int", RuntimeCategory.FIXED, "Integer to fixed"),
    "_fix_cmp": RuntimeRoutine("_fix_cmp", RuntimeCategory.FIXED, "Fixed-point compare"),

    # =========================================================================
    # String operations (strings.asm)
    # =========================================================================
    "_str_move": RuntimeRoutine("_str_move", RuntimeCategory.STRING, "Move string"),
    "_str_index": RuntimeRoutine("_str_index", RuntimeCategory.STRING, "Find pattern"),
    "_str_index_char": RuntimeRoutine("_str_index_char", RuntimeCategory.STRING, "Find char"),
    "_str_count": RuntimeRoutine("_str_count", RuntimeCategory.STRING, "Count occurrences"),
    "_str_replace": RuntimeRoutine("_str_replace", RuntimeCategory.STRING, "Replace slice"),
    "_str_delete": RuntimeRoutine("_str_delete", RuntimeCategory.STRING, "Delete slice"),
    "_str_insert": RuntimeRoutine("_str_insert", RuntimeCategory.STRING, "Insert string"),
    "_str_overwrite": RuntimeRoutine("_str_overwrite", RuntimeCategory.STRING, "Overwrite slice"),
    "_str_head": RuntimeRoutine("_str_head", RuntimeCategory.STRING, "Get head"),
    "_str_tail": RuntimeRoutine("_str_tail", RuntimeCategory.STRING, "Get tail"),
    "_str_trim": RuntimeRoutine("_str_trim", RuntimeCategory.STRING, "Trim whitespace"),
    "_str_len": RuntimeRoutine("_str_len", RuntimeCategory.STRING, "String length"),
    "_str_cmp": RuntimeRoutine("_str_cmp", RuntimeCategory.STRING, "Compare strings"),
    "_str_copy": RuntimeRoutine("_str_copy", RuntimeCategory.STRING, "Copy string"),
    "_strcat": RuntimeRoutine("_strcat", RuntimeCategory.STRING, "Concatenate strings"),

    # =========================================================================
    # I/O operations (io.asm)
    # =========================================================================
    "_put_char": RuntimeRoutine("_put_char", RuntimeCategory.IO, "Output character"),
    "_get_char": RuntimeRoutine("_get_char", RuntimeCategory.IO, "Input character"),
    "_put_string": RuntimeRoutine("_put_string", RuntimeCategory.IO, "Output string"),
    "_put_line": RuntimeRoutine("_put_line", RuntimeCategory.IO, "Output string with newline"),
    "_put_integer": RuntimeRoutine("_put_integer", RuntimeCategory.IO, "Output integer"),
    "_get_line": RuntimeRoutine("_get_line", RuntimeCategory.IO, "Input line"),
    "_new_line": RuntimeRoutine("_new_line", RuntimeCategory.IO, "Output newline"),
    "_int_to_str": RuntimeRoutine("_int_to_str", RuntimeCategory.IO, "Integer to string"),
    "_str_to_int": RuntimeRoutine("_str_to_int", RuntimeCategory.IO, "String to integer"),

    # =========================================================================
    # Bounded string operations (bounded.asm)
    # =========================================================================
    "_bnd_append": RuntimeRoutine("_bnd_append", RuntimeCategory.BOUNDED, "Append to bounded string"),
    "_bnd_element": RuntimeRoutine("_bnd_element", RuntimeCategory.BOUNDED, "Get element"),
    "_bnd_replace_element": RuntimeRoutine("_bnd_replace_element", RuntimeCategory.BOUNDED, "Replace element"),
    "_bnd_slice": RuntimeRoutine("_bnd_slice", RuntimeCategory.BOUNDED, "Get slice"),
    "_bnd_index": RuntimeRoutine("_bnd_index", RuntimeCategory.BOUNDED, "Find pattern"),
    "_bnd_replace_slice": RuntimeRoutine("_bnd_replace_slice", RuntimeCategory.BOUNDED, "Replace slice"),
    "_bnd_insert": RuntimeRoutine("_bnd_insert", RuntimeCategory.BOUNDED, "Insert string"),
    "_bnd_delete": RuntimeRoutine("_bnd_delete", RuntimeCategory.BOUNDED, "Delete slice"),
    "_bnd_trim": RuntimeRoutine("_bnd_trim", RuntimeCategory.BOUNDED, "Trim string"),
    "_bnd_head": RuntimeRoutine("_bnd_head", RuntimeCategory.BOUNDED, "Get head"),
    "_bnd_tail": RuntimeRoutine("_bnd_tail", RuntimeCategory.BOUNDED, "Get tail"),
    "_bnd_to_string": RuntimeRoutine("_bnd_to_string", RuntimeCategory.BOUNDED, "To fixed string"),
    "_bnd_set_bounded": RuntimeRoutine("_bnd_set_bounded", RuntimeCategory.BOUNDED, "Set bounded string"),
    "_bnd_length": RuntimeRoutine("_bnd_length", RuntimeCategory.BOUNDED, "Get length"),
    "_bnd_max_length": RuntimeRoutine("_bnd_max_length", RuntimeCategory.BOUNDED, "Get max length"),

    # =========================================================================
    # Container operations (containers.asm)
    # =========================================================================
    "_vec_init": RuntimeRoutine("_vec_init", RuntimeCategory.CONTAINER, "Initialize vector"),
    "_vec_append": RuntimeRoutine("_vec_append", RuntimeCategory.CONTAINER, "Append to vector"),
    "_vec_get": RuntimeRoutine("_vec_get", RuntimeCategory.CONTAINER, "Get element"),
    "_vec_set": RuntimeRoutine("_vec_set", RuntimeCategory.CONTAINER, "Set element"),
    "_vec_length": RuntimeRoutine("_vec_length", RuntimeCategory.CONTAINER, "Get length"),
    "_vec_clear": RuntimeRoutine("_vec_clear", RuntimeCategory.CONTAINER, "Clear vector"),
    "_vec_delete": RuntimeRoutine("_vec_delete", RuntimeCategory.CONTAINER, "Delete element"),
    "_vec_first": RuntimeRoutine("_vec_first", RuntimeCategory.CONTAINER, "Get first cursor"),
    "_vec_last": RuntimeRoutine("_vec_last", RuntimeCategory.CONTAINER, "Get last cursor"),
    "_vec_next": RuntimeRoutine("_vec_next", RuntimeCategory.CONTAINER, "Next cursor"),
    "_vec_previous": RuntimeRoutine("_vec_previous", RuntimeCategory.CONTAINER, "Previous cursor"),
    "_vec_has_element": RuntimeRoutine("_vec_has_element", RuntimeCategory.CONTAINER, "Has element at cursor"),
    "_vec_element": RuntimeRoutine("_vec_element", RuntimeCategory.CONTAINER, "Get element at cursor"),

    # =========================================================================
    # Exception handling (to be added to runtime)
    # =========================================================================
    "_exc_push_handler": RuntimeRoutine("_exc_push_handler", RuntimeCategory.EXCEPTION, "Push exception handler"),
    "_exc_pop_handler": RuntimeRoutine("_exc_pop_handler", RuntimeCategory.EXCEPTION, "Pop exception handler"),
    "_exc_raise": RuntimeRoutine("_exc_raise", RuntimeCategory.EXCEPTION, "Raise exception"),
    "_exc_reraise": RuntimeRoutine("_exc_reraise", RuntimeCategory.EXCEPTION, "Reraise current exception"),
    "_exc_get_message": RuntimeRoutine("_exc_get_message", RuntimeCategory.EXCEPTION, "Get exception message"),

    # =========================================================================
    # Tasking (to be completed in runtime)
    # =========================================================================
    "_task_init": RuntimeRoutine("_task_init", RuntimeCategory.TASKING, "Initialize tasking"),
    "_task_create": RuntimeRoutine("_task_create", RuntimeCategory.TASKING, "Create task"),
    "_task_yield": RuntimeRoutine("_task_yield", RuntimeCategory.TASKING, "Yield to scheduler"),
    "_task_terminate": RuntimeRoutine("_task_terminate", RuntimeCategory.TASKING, "Terminate task"),
    "_task_delay": RuntimeRoutine("_task_delay", RuntimeCategory.TASKING, "Delay task"),
    "_entry_call": RuntimeRoutine("_entry_call", RuntimeCategory.TASKING, "Call entry"),
    "_entry_accept": RuntimeRoutine("_entry_accept", RuntimeCategory.TASKING, "Accept entry"),
    "_protected_enter": RuntimeRoutine("_protected_enter", RuntimeCategory.TASKING, "Enter protected"),
    "_protected_leave": RuntimeRoutine("_protected_leave", RuntimeCategory.TASKING, "Leave protected"),

    # =========================================================================
    # OOP Dispatch
    # =========================================================================
    "_dispatch_call": RuntimeRoutine("_dispatch_call", RuntimeCategory.DISPATCH, "Dynamic dispatch call"),
    "_get_tag": RuntimeRoutine("_get_tag", RuntimeCategory.DISPATCH, "Get object tag"),

    # =========================================================================
    # Controlled types
    # =========================================================================
    "_ctrl_initialize": RuntimeRoutine("_ctrl_initialize", RuntimeCategory.CONTROLLED, "Initialize controlled"),
    "_ctrl_adjust": RuntimeRoutine("_ctrl_adjust", RuntimeCategory.CONTROLLED, "Adjust controlled"),
    "_ctrl_finalize": RuntimeRoutine("_ctrl_finalize", RuntimeCategory.CONTROLLED, "Finalize controlled"),
}


class RuntimeManager:
    """
    Manages runtime library dependencies during compilation.

    During lowering and code generation, routines are marked as needed.
    At the end, the manager generates EXTRN declarations for all needed
    routines, which will be resolved by linking with libada.lib.
    """

    def __init__(self):
        self._needed: Set[str] = set()
        self._categories_needed: Set[RuntimeCategory] = set()

    def need(self, routine_name: str) -> None:
        """Mark a runtime routine as needed."""
        if routine_name in RUNTIME_CATALOG:
            self._needed.add(routine_name)
            routine = RUNTIME_CATALOG[routine_name]
            self._categories_needed.add(routine.category)
            # Also add dependencies
            for dep in routine.dependencies:
                self.need(dep)
        else:
            # Unknown routine - still mark it for EXTRN
            self._needed.add(routine_name)

    def need_category(self, category: RuntimeCategory) -> None:
        """Mark all routines in a category as needed."""
        self._categories_needed.add(category)
        for name, routine in RUNTIME_CATALOG.items():
            if routine.category == category:
                self._needed.add(name)

    def is_needed(self, routine_name: str) -> bool:
        """Check if a routine is needed."""
        return routine_name in self._needed

    def get_needed_routines(self) -> Set[str]:
        """Get all needed routine names."""
        return self._needed.copy()

    def get_needed_categories(self) -> Set[RuntimeCategory]:
        """Get categories of needed routines."""
        return self._categories_needed.copy()

    def generate_externs(self) -> list[str]:
        """Generate EXTRN declarations for all needed routines."""
        lines = []

        if not self._needed:
            return lines

        lines.append("")
        lines.append("; =========================================")
        lines.append("; External runtime library references")
        lines.append("; Link with: ul80 program.rel -l libada.lib")
        if RuntimeCategory.FLOAT48 in self._categories_needed:
            lines.append(";            ul80 program.rel -l libmath48.lib")
        lines.append("; =========================================")
        lines.append("")

        # Group by category for readability
        by_category: dict[RuntimeCategory, list[str]] = {}
        uncategorized: list[str] = []

        for name in sorted(self._needed):
            if name in RUNTIME_CATALOG:
                routine = RUNTIME_CATALOG[name]
                if routine.category not in by_category:
                    by_category[routine.category] = []
                by_category[routine.category].append(name)
            else:
                uncategorized.append(name)

        # Emit by category
        for category in RuntimeCategory:
            if category in by_category:
                lines.append(f"; {category.name} routines")
                for name in by_category[category]:
                    routine = RUNTIME_CATALOG.get(name)
                    desc = routine.description if routine else ""
                    if desc:
                        lines.append(f"    EXTRN {name:<20} ; {desc}")
                    else:
                        lines.append(f"    EXTRN {name}")
                lines.append("")

        # Emit uncategorized
        if uncategorized:
            lines.append("; Other external routines")
            for name in uncategorized:
                lines.append(f"    EXTRN {name}")
            lines.append("")

        return lines

    def clear(self) -> None:
        """Clear all tracked dependencies."""
        self._needed.clear()
        self._categories_needed.clear()


# Global runtime manager instance
_runtime_manager: RuntimeManager | None = None


def get_runtime_manager() -> RuntimeManager:
    """Get the global runtime manager instance."""
    global _runtime_manager
    if _runtime_manager is None:
        _runtime_manager = RuntimeManager()
    return _runtime_manager


def reset_runtime_manager() -> None:
    """Reset the global runtime manager (for testing)."""
    global _runtime_manager
    _runtime_manager = RuntimeManager()
