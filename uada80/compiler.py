"""
Compiler driver for uada80.

Coordinates all compilation phases: parsing, semantic analysis,
lowering, and code generation.
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Optional

from uada80.ast_nodes import Program
from uada80.parser import parse, ParseError
from uada80.semantic import analyze
from uada80.lowering import lower_to_ir
from uada80.codegen import generate_z80
from uada80.ir import IRModule
from uada80.optimizer import ASTOptimizer, OptimizerConfig, OptimizationStats

try:
    from upeepz80 import PeepholeOptimizer as Z80PeepholeOptimizer
    HAS_PEEPHOLE = True
except ImportError:
    HAS_PEEPHOLE = False
    Z80PeepholeOptimizer = None


class OutputFormat(Enum):
    """Output format options."""

    ASM = auto()  # Z80 assembly
    IR = auto()  # IR dump (for debugging)
    AST = auto()  # AST dump (for debugging)


@dataclass
class CompilerError:
    """A compilation error."""

    phase: str
    message: str
    filename: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None

    def __str__(self) -> str:
        location = ""
        if self.filename:
            location = f"{self.filename}"
            if self.line:
                location += f":{self.line}"
                if self.column:
                    location += f":{self.column}"
            location += ": "
        return f"{location}{self.phase}: {self.message}"


@dataclass
class CompilationResult:
    """Result of compilation."""

    success: bool
    errors: list[CompilerError] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    output: str = ""
    ast: Optional[Program] = None
    ir: Optional[IRModule] = None
    optimization_stats: Optional[OptimizationStats] = None
    peephole_stats: Optional[dict[str, int]] = None

    @property
    def has_errors(self) -> bool:
        return len(self.errors) > 0


class Compiler:
    """
    Main compiler driver.

    Usage:
        compiler = Compiler()
        result = compiler.compile("procedure Main is begin null; end Main;")
        if result.success:
            print(result.output)
    """

    def __init__(
        self,
        output_format: OutputFormat = OutputFormat.ASM,
        debug: bool = False,
        optimize: bool = True,
        optimization_level: int = 2,
        emit_inline_runtime: bool = False,
    ):
        self.output_format = output_format
        self.debug = debug
        self.optimize = optimize
        self.optimization_level = optimization_level
        self.peephole_optimize = optimize and HAS_PEEPHOLE
        self.emit_inline_runtime = emit_inline_runtime  # False = use libada.lib

    def compile(
        self,
        source: str,
        filename: str = "<input>",
    ) -> CompilationResult:
        """
        Compile source code to Z80 assembly.

        Args:
            source: Ada source code
            filename: Name of the source file (for error messages)

        Returns:
            CompilationResult with output or errors
        """
        result = CompilationResult(success=False)

        # Phase 1: Parse
        try:
            ast = parse(source, filename)
            result.ast = ast
        except ParseError as e:
            result.errors.append(
                CompilerError(
                    phase="parse",
                    message=str(e),
                    filename=filename,
                )
            )
            return result

        if self.output_format == OutputFormat.AST:
            result.success = True
            result.output = self._dump_ast(ast)
            return result

        # Phase 2: Semantic analysis
        try:
            semantic_result = analyze(ast)
        except Exception as e:
            result.errors.append(
                CompilerError(
                    phase="semantic",
                    message=str(e),
                    filename=filename,
                )
            )
            return result

        if semantic_result.has_errors:
            for err in semantic_result.errors:
                result.errors.append(
                    CompilerError(
                        phase="semantic",
                        message=err.message,
                        filename=filename,
                        line=err.line,
                        column=err.column,
                    )
                )
            return result

        # Phase 3: AST Optimization
        if self.optimize and self.optimization_level > 0:
            try:
                config = OptimizerConfig.for_level(self.optimization_level)
                optimizer = ASTOptimizer(config)
                ast = optimizer.optimize(ast)
                result.ast = ast
                result.optimization_stats = optimizer.stats
            except Exception as e:
                # Optimization failure is non-fatal
                if self.debug:
                    result.warnings.append(f"AST optimization failed: {e}")

        # Phase 4: Lower to IR
        try:
            ir = lower_to_ir(ast, semantic_result)
            result.ir = ir
        except Exception as e:
            result.errors.append(
                CompilerError(
                    phase="lowering",
                    message=str(e),
                    filename=filename,
                )
            )
            return result

        if self.output_format == OutputFormat.IR:
            result.success = True
            result.output = self._dump_ir(ir)
            return result

        # Phase 5: Code generation
        try:
            asm = generate_z80(ir, emit_inline_runtime=self.emit_inline_runtime)
        except Exception as e:
            result.errors.append(
                CompilerError(
                    phase="codegen",
                    message=str(e),
                    filename=filename,
                )
            )
            return result

        # Phase 6: Peephole optimization (optional, uses upeepz80)
        if self.peephole_optimize and Z80PeepholeOptimizer is not None:
            try:
                peephole = Z80PeepholeOptimizer()
                asm = peephole.optimize(asm)
                result.peephole_stats = peephole.stats
            except Exception as e:
                # Optimization failure is non-fatal, use unoptimized code
                if self.debug:
                    result.warnings.append(f"Peephole optimization failed: {e}")

        result.output = asm
        result.success = True
        return result

    def compile_file(self, path: Path | str) -> CompilationResult:
        """
        Compile a source file.

        Args:
            path: Path to the Ada source file

        Returns:
            CompilationResult with output or errors
        """
        path = Path(path)

        if not path.exists():
            result = CompilationResult(success=False)
            result.errors.append(
                CompilerError(
                    phase="io",
                    message=f"File not found: {path}",
                )
            )
            return result

        try:
            source = path.read_text()
        except Exception as e:
            result = CompilationResult(success=False)
            result.errors.append(
                CompilerError(
                    phase="io",
                    message=f"Cannot read file: {e}",
                )
            )
            return result

        return self.compile(source, str(path))

    def compile_files(self, paths: list[Path | str]) -> CompilationResult:
        """
        Compile multiple source files together.

        Parses all files, combines them into a single AST, and compiles.
        Like PL/M-80, all source is compiled together without separate compilation.

        Args:
            paths: List of paths to Ada source files

        Returns:
            CompilationResult with output or errors
        """
        result = CompilationResult(success=False)
        combined_ast = Program(units=[])

        # Phase 1: Parse all files and combine into single AST
        for path in paths:
            path = Path(path)

            if not path.exists():
                result.errors.append(
                    CompilerError(
                        phase="io",
                        message=f"File not found: {path}",
                    )
                )
                return result

            try:
                source = path.read_text()
            except Exception as e:
                result.errors.append(
                    CompilerError(
                        phase="io",
                        message=f"Cannot read file: {e}",
                        filename=str(path),
                    )
                )
                return result

            try:
                ast = parse(source, str(path))
                # Combine compilation units
                combined_ast.units.extend(ast.units)
            except ParseError as e:
                result.errors.append(
                    CompilerError(
                        phase="parse",
                        message=str(e),
                        filename=str(path),
                    )
                )
                return result

        result.ast = combined_ast

        if self.output_format == OutputFormat.AST:
            result.success = True
            result.output = self._dump_ast(combined_ast)
            return result

        # Phase 2: Semantic analysis on combined AST
        try:
            semantic_result = analyze(combined_ast)
        except Exception as e:
            result.errors.append(
                CompilerError(
                    phase="semantic",
                    message=str(e),
                )
            )
            return result

        if semantic_result.has_errors:
            for err in semantic_result.errors:
                result.errors.append(
                    CompilerError(
                        phase="semantic",
                        message=err.message,
                        line=err.line,
                        column=err.column,
                    )
                )
            return result

        # Phase 3: AST Optimization
        ast = combined_ast
        if self.optimize and self.optimization_level > 0:
            try:
                config = OptimizerConfig.for_level(self.optimization_level)
                optimizer = ASTOptimizer(config)
                ast = optimizer.optimize(ast)
                result.ast = ast
                result.optimization_stats = optimizer.stats
            except Exception as e:
                if self.debug:
                    result.warnings.append(f"AST optimization failed: {e}")

        # Phase 4: Lower to IR
        try:
            ir = lower_to_ir(ast, semantic_result)
            result.ir = ir
        except Exception as e:
            result.errors.append(
                CompilerError(
                    phase="lowering",
                    message=str(e),
                )
            )
            return result

        if self.output_format == OutputFormat.IR:
            result.success = True
            result.output = self._dump_ir(ir)
            return result

        # Phase 5: Code generation
        try:
            asm = generate_z80(ir, emit_inline_runtime=self.emit_inline_runtime)
        except Exception as e:
            result.errors.append(
                CompilerError(
                    phase="codegen",
                    message=str(e),
                )
            )
            return result

        # Phase 6: Peephole optimization (uses upeepz80)
        if self.peephole_optimize and Z80PeepholeOptimizer is not None:
            try:
                peephole = Z80PeepholeOptimizer()
                asm = peephole.optimize(asm)
                result.peephole_stats = peephole.stats
            except Exception as e:
                if self.debug:
                    result.warnings.append(f"Peephole optimization failed: {e}")

        result.output = asm
        result.success = True
        return result

    def _dump_ast(self, ast: Program) -> str:
        """Generate a string representation of the AST."""
        lines = ["AST Dump:", "========="]
        for unit in ast.units:
            lines.append(f"CompilationUnit: {type(unit.unit).__name__}")
            lines.append(f"  {unit.unit}")
        return "\n".join(lines)

    def _dump_ir(self, ir: IRModule) -> str:
        """Generate a string representation of the IR."""
        lines = [f"IR Module: {ir.name}", "=" * 40]

        if ir.globals:
            lines.append("\nGlobals:")
            for name, (irtype, size) in ir.globals.items():
                lines.append(f"  {name}: {irtype.name} ({size} bytes)")

        if ir.string_literals:
            lines.append("\nStrings:")
            for name, value in ir.string_literals.items():
                lines.append(f"  {name}: {repr(value)}")

        for func in ir.functions:
            lines.append(f"\nFunction: {func.name} -> {func.return_type.name}")
            if func.params:
                params = ", ".join(
                    f"{p.name or f'v{p.id}'}: {p.ir_type.name}" for p in func.params
                )
                lines.append(f"  Params: {params}")
            lines.append(f"  Locals: {func.locals_size} bytes")

            for block in func.blocks:
                lines.append(f"\n  {block.label}:")
                for instr in block.instructions:
                    lines.append(f"    {instr}")

        return "\n".join(lines)


def compile_source(source: str, filename: str = "<input>") -> CompilationResult:
    """
    Convenience function to compile source code.

    Args:
        source: Ada source code
        filename: Name of the source file (for error messages)

    Returns:
        CompilationResult with output or errors
    """
    compiler = Compiler()
    return compiler.compile(source, filename)


def compile_file(path: Path | str) -> CompilationResult:
    """
    Convenience function to compile a file.

    Args:
        path: Path to the Ada source file

    Returns:
        CompilationResult with output or errors
    """
    compiler = Compiler()
    return compiler.compile_file(path)


def compile_files(paths: list[Path | str]) -> CompilationResult:
    """
    Compile multiple Ada source files together.

    All files are parsed, combined into a single AST, and compiled together.
    This enables cross-file references without separate compilation.
    File order matters: packages must be declared before they're used.

    Args:
        paths: List of paths to Ada source files

    Returns:
        CompilationResult with output or errors
    """
    compiler = Compiler()
    return compiler.compile_files(paths)
