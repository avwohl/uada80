"""
Command-line interface for uada80 Ada compiler.
"""

import argparse
import sys
from pathlib import Path

from uada80 import __version__
from uada80.compiler import Compiler, OutputFormat


def main():
    """Main entry point for the uada80 compiler."""
    parser = argparse.ArgumentParser(
        prog="uada80",
        description="Ada compiler for Z80/8080 processors",
    )

    parser.add_argument(
        "source",
        nargs="*",
        help="Ada source file(s) to compile (multiple files compiled together)",
    )

    parser.add_argument(
        "-o", "--output",
        metavar="FILE",
        help="Output file (default: stdout, uses .mac extension for Z80 assembly)",
    )

    parser.add_argument(
        "--dump-ast",
        action="store_true",
        help="Dump AST instead of generating code",
    )

    parser.add_argument(
        "--dump-ir",
        action="store_true",
        help="Dump IR instead of generating code",
    )

    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug output",
    )

    parser.add_argument(
        "-O0", "--no-optimize",
        action="store_true",
        help="Disable peephole optimization",
    )

    parser.add_argument(
        "-v", "--version",
        action="version",
        version=f"uada80 {__version__}",
    )

    args = parser.parse_args()

    if not args.source:
        parser.print_help()
        return 0

    # Determine output format
    output_format = OutputFormat.ASM
    if args.dump_ast:
        output_format = OutputFormat.AST
    elif args.dump_ir:
        output_format = OutputFormat.IR

    # Create compiler
    optimize = not args.no_optimize
    compiler = Compiler(output_format=output_format, debug=args.debug, optimize=optimize)

    # Compile (single or multiple files)
    source_paths = [Path(s) for s in args.source]
    if len(source_paths) == 1:
        result = compiler.compile_file(source_paths[0])
    else:
        result = compiler.compile_files(source_paths)

    # Handle errors
    if result.has_errors:
        for error in result.errors:
            print(f"error: {error}", file=sys.stderr)
        return 1

    # Handle warnings
    for warning in result.warnings:
        print(f"warning: {warning}", file=sys.stderr)

    # Output result
    if args.output:
        output_path = Path(args.output)
        # Default to .mac extension for Z80 assembly if no extension given
        if output_format == OutputFormat.ASM and not output_path.suffix:
            output_path = output_path.with_suffix(".mac")
        output_path.write_text(result.output)
        print(f"Output written to {output_path}", file=sys.stderr)
    else:
        print(result.output)

    return 0


if __name__ == "__main__":
    sys.exit(main())
