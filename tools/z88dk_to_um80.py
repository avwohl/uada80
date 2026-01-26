#!/usr/bin/env python3
"""
z88dk to um80 (MACRO-80) assembler syntax converter.

Converts z88dk's z80asm source files to um80/MACRO-80 compatible syntax.

Differences handled:
- SECTION -> CSEG/DSEG (or removed)
- EXTERN -> EXTRN
- lowercase mnemonics -> UPPERCASE
- $xx hex -> 0xxH or xxH
- 0xXXXX hex -> 0XXXXH
- %binary -> 0nnnnnnnnB or nnnnnnnnB
- defw/defb/defs -> DW/DB/DS
- DEFQ (32-bit) -> 4x DB
- DEFC name = value -> name EQU value
- Local labels (.name) -> global$name
- Labels: z88dk uses label: or label, um80 uses label: at start of line

Usage:
    python z88dk_to_um80.py input.asm [output.asm]
    python z88dk_to_um80.py --dir /path/to/z88dk/src /path/to/output
"""

import re
import sys
import argparse
from pathlib import Path


# Z80 mnemonics (including z88dk extensions)
Z80_MNEMONICS = {
    'adc', 'add', 'and', 'bit', 'call', 'ccf', 'cp', 'cpd', 'cpdr', 'cpi',
    'cpir', 'cpl', 'daa', 'dec', 'di', 'djnz', 'ei', 'ex', 'exx', 'halt',
    'im', 'in', 'inc', 'ind', 'indr', 'ini', 'inir', 'jp', 'jr', 'ld',
    'ldd', 'lddr', 'ldi', 'ldir', 'neg', 'nop', 'or', 'otdr', 'otir',
    'out', 'outd', 'outi', 'pop', 'push', 'res', 'ret', 'reti', 'retn',
    'rl', 'rla', 'rlc', 'rlca', 'rld', 'rr', 'rra', 'rrc', 'rrca', 'rrd',
    'rst', 'sbc', 'scf', 'set', 'sla', 'sra', 'srl', 'sub', 'xor',
    # pseudo-ops that might appear
    'defb', 'defw', 'defs', 'db', 'dw', 'ds', 'equ', 'org',
}

# Register names to uppercase
REGISTERS = {'a', 'b', 'c', 'd', 'e', 'h', 'l', 'af', 'bc', 'de', 'hl',
             'sp', 'ix', 'iy', 'i', 'r', 'ixh', 'ixl', 'iyh', 'iyl',
             'nc', 'nz', 'z', 'p', 'm', 'pe', 'po'}


def convert_hex(match: re.Match) -> str:
    """Convert $xx hex notation to MACRO-80 format (0xxH or xxH)."""
    hex_val = match.group(1)
    # If starts with a-f, prefix with 0
    if hex_val[0].lower() in 'abcdef':
        return f"0{hex_val.upper()}H"
    return f"{hex_val.upper()}H"


def convert_hex_0x(match: re.Match) -> str:
    """Convert 0xXXXX hex notation to MACRO-80 format (0XXXXH or XXXXH)."""
    hex_val = match.group(1)
    # If starts with a-f, prefix with 0
    if hex_val[0].lower() in 'abcdef':
        return f"0{hex_val.upper()}H"
    return f"{hex_val.upper()}H"


def convert_binary(match: re.Match) -> str:
    """Convert %nnnn binary notation to MACRO-80 format (0nnnnB or nnnnB)."""
    bin_val = match.group(1)
    # Binary always starts with 0 or 1, so no need for leading 0
    return f"{bin_val}B"


# Track current global label for local label expansion
current_global_label = ""


def expand_local_label(name: str) -> str:
    """Expand .local label to global$local format."""
    global current_global_label
    if name.startswith('.'):
        if current_global_label:
            return f"{current_global_label}${name[1:]}"
        else:
            # No global context, just remove the dot
            return name[1:]
    return name


def convert_defq_value(value_str: str) -> list:
    """Convert a 32-bit value to 4 DB bytes (little-endian).

    Returns list of 4 byte values, or None if cannot parse.
    """
    value_str = value_str.strip()

    # Try to evaluate the expression
    try:
        # Handle hex formats
        if value_str.startswith('$'):
            val = int(value_str[1:], 16)
        elif value_str.lower().startswith('0x'):
            val = int(value_str[2:], 16)
        elif value_str.upper().endswith('H'):
            hex_part = value_str[:-1]
            if hex_part.startswith('0') and len(hex_part) > 1:
                val = int(hex_part, 16)
            else:
                val = int(hex_part, 16)
        elif value_str.startswith('%'):
            val = int(value_str[1:], 2)
        elif value_str.upper().endswith('B'):
            val = int(value_str[:-1], 2)
        elif value_str.upper().endswith('O') or value_str.upper().endswith('Q'):
            val = int(value_str[:-1], 8)
        else:
            val = int(value_str)

        # Split into 4 bytes (little-endian)
        return [
            val & 0xFF,
            (val >> 8) & 0xFF,
            (val >> 16) & 0xFF,
            (val >> 24) & 0xFF
        ]
    except ValueError:
        # Cannot parse - return None, caller will handle as expression
        return None


def convert_line(line: str) -> str:
    """Convert a single line from z88dk to um80 syntax."""
    global current_global_label
    original = line

    # Preserve empty lines
    if not line.strip():
        return line

    # Handle comments - preserve the comment part
    # NOTE: um80 has a bug where AF' (alternate register) triggers string mode
    # causing comments after AF' to be misinterpreted. We strip comments from
    # lines containing AF' as a workaround.
    comment = ""
    if ';' in line:
        pos = line.find(';')
        code_part = line[:pos]
        comment = ';' + line[pos+1:]
        # Workaround for um80 bug: if line has AF', strip the comment entirely
        if "af'" in code_part.lower():
            comment = ""
    else:
        code_part = line

    # Skip SECTION directives entirely (we'll add CSEG at module level)
    if re.match(r'^\s*SECTION\s+', code_part, re.IGNORECASE):
        return ""  # Just remove SECTION lines

    # Handle IF conditional compilation - for now, assume Z80 target
    # Skip lines like: IF __CPU_Z80__ | __CPU_KC160__
    # We'll include all code and ignore the conditionals
    if re.match(r'^\s*IF\s+', code_part, re.IGNORECASE):
        return "; SKIPPED: " + original.strip()  # Comment out IF lines

    # Handle ENDIF
    if re.match(r'^\s*ENDIF\s*', code_part, re.IGNORECASE):
        return "; SKIPPED: " + original.strip()  # Comment out ENDIF lines

    # Convert EXTERN to EXTRN
    code_part = re.sub(r'\bEXTERN\b', 'EXTRN', code_part, flags=re.IGNORECASE)

    # Convert PUBLIC (keep as-is but uppercase)
    code_part = re.sub(r'\bpublic\b', 'PUBLIC', code_part, flags=re.IGNORECASE)

    # Convert hex notation: $xx -> xxH
    code_part = re.sub(r'\$([0-9a-fA-F]+)', convert_hex, code_part)

    # Convert 0xXXXX hex notation to XXXXH
    code_part = re.sub(r'0x([0-9a-fA-F]+)', convert_hex_0x, code_part, flags=re.IGNORECASE)

    # Convert %binary notation to nnnB
    code_part = re.sub(r'%([01]+)', convert_binary, code_part)

    # Convert pseudo-ops
    code_part = re.sub(r'\bdefw\b', 'DW', code_part, flags=re.IGNORECASE)
    code_part = re.sub(r'\bdefb\b', 'DB', code_part, flags=re.IGNORECASE)
    code_part = re.sub(r'\bdefs\b', 'DS', code_part, flags=re.IGNORECASE)

    # Extract leading whitespace
    indent_match = re.match(r'^(\s*)', code_part)
    indent = indent_match.group(1) if indent_match else ''
    code_part = code_part[len(indent):]

    if not code_part.strip():
        return indent + comment

    # Check if line starts with a label (identifier followed by colon)
    # Don't treat PUBLIC/EXTRN/etc as labels
    DIRECTIVES = {'public', 'extrn', 'extern', 'equ', 'org', 'end', 'name', 'cseg', 'dseg',
                  'dw', 'db', 'ds', 'defw', 'defb', 'defs', 'defq', 'defc', 'include', 'if', 'endif', 'else'}

    label = ""
    # Check for local label (starts with dot)
    local_label_match = re.match(r'^(\.([a-zA-Z_][a-zA-Z0-9_]*)):?\s*', code_part)
    if local_label_match:
        # This is a local label
        local_name = local_label_match.group(2)
        expanded = expand_local_label('.' + local_name)
        label = expanded + ':'
        code_part = code_part[local_label_match.end():]
    else:
        # Check for global label
        label_match = re.match(r'^([a-zA-Z_][a-zA-Z0-9_]*):?\s*', code_part)
        if label_match:
            potential_label = label_match.group(1)
            has_colon = code_part[len(potential_label):len(potential_label)+1] == ':'
            rest_after = code_part[label_match.end():]

            # It's a label if: has explicit colon AND not a directive
            if has_colon and potential_label.lower() not in DIRECTIVES:
                label = potential_label + ':'
                code_part = rest_after
                # Track this as the current global label for local label expansion
                current_global_label = potential_label

    # Now process the instruction part
    code_part = code_part.strip()
    if not code_part:
        return (label if label else indent) + comment

    # Split into mnemonic and operands
    parts = code_part.split(None, 1)
    mnemonic = parts[0]
    operands = parts[1] if len(parts) > 1 else ""

    # Handle DEFQ (32-bit data) - convert to 4 DB bytes
    if mnemonic.upper() == 'DEFQ':
        # Split operands by comma, convert each to 4 DB bytes
        ops = [o.strip() for o in operands.split(',')]
        db_values = []
        for op in ops:
            bytes_list = convert_defq_value(op)
            if bytes_list:
                for b in bytes_list:
                    db_values.append(f"{b}")
            else:
                # Cannot parse as constant - output LOW/HIGH operations
                # For expressions, we need to emit them using LOW/HIGH
                db_values.append(f"LOW({op})")
                db_values.append(f"HIGH({op})")
                db_values.append(f"LOW(({op}) SHR 16)")
                db_values.append(f"HIGH(({op}) SHR 16)")

        if label:
            return f"{label}\tDB\t{','.join(db_values)}{comment}"
        else:
            return f"\tDB\t{','.join(db_values)}{comment}"

    # Handle DEFC (name = value)
    # If value looks like a constant, use EQU
    # If value looks like a symbol (likely external), use JP to create a jump alias
    if mnemonic.upper() == 'DEFC':
        # operands should be "name = value"
        if '=' in operands:
            parts = operands.split('=', 1)
            name = parts[0].strip()
            value = parts[1].strip()
            # Expand any local label references in the value
            value = re.sub(r'\.([\w]+)', lambda m: expand_local_label('.' + m.group(1)), value)

            # Check if value looks like a constant (number) vs symbol
            # Constants: 0x..., 0...H, %..., decimal, etc.
            is_constant = bool(re.match(
                r'^[+-]?('
                r'0x[0-9a-fA-F]+|'       # 0xHEX
                r'[0-9a-fA-F]+H|'         # HEXH
                r'%[01]+|'                # %binary
                r'[01]+B|'                # binaryB
                r'\$[0-9a-fA-F]+|'        # $HEX
                r'[0-9]+D?|'              # decimal
                r'[0-9]+[OQ]'             # octal
                r')$', value, re.IGNORECASE
            ))

            if is_constant:
                return f"{name}\tEQU\t{value}{comment}"
            else:
                # Symbol (likely external) - create a jump instruction alias
                return f"{name}:\tJP\t{value}{comment}"

    # Uppercase mnemonic if it's a Z80 instruction or pseudo-op
    if mnemonic.lower() in Z80_MNEMONICS:
        mnemonic = mnemonic.upper()
    elif mnemonic.upper() in ('PUBLIC', 'EXTRN', 'EQU', 'ORG', 'END', 'NAME', 'CSEG', 'DSEG', 'DW', 'DB', 'DS'):
        mnemonic = mnemonic.upper()

    # Expand local label references in operands (e.g., JP .loop -> JP global$loop)
    if operands:
        operands = re.sub(r'\.([\w]+)', lambda m: expand_local_label('.' + m.group(1)), operands)

    # Handle special instruction cases before uppercasing registers
    expanded_instructions = []

    # Handle EX AF,AF -> EX AF,AF'
    if mnemonic.upper() == 'EX' and operands and re.match(r'^\s*AF\s*,\s*AF\s*$', operands, re.IGNORECASE):
        operands = "AF,AF'"

    # Handle LD r,(HL+) and LD r,(HL-) - not supported in MACRO-80
    # LD A,(HL+) -> LD A,(HL) \ INC HL
    # LD A,(HL-) -> LD A,(HL) \ DEC HL
    if mnemonic.upper() == 'LD' and operands:
        # Match LD reg,(HL+) or LD reg,(HL-)
        inc_match = re.match(r'^\s*([A-Z]+)\s*,\s*\(HL\+\)\s*$', operands, re.IGNORECASE)
        dec_match = re.match(r'^\s*([A-Z]+)\s*,\s*\(HL-\)\s*$', operands, re.IGNORECASE)

        if inc_match:
            reg = inc_match.group(1).upper()
            if label:
                expanded_instructions.append(f"{label}\tLD\t{reg},(HL){comment}")
                label = ""
                comment = ""
            else:
                expanded_instructions.append(f"\tLD\t{reg},(HL){comment}")
                comment = ""
            expanded_instructions.append(f"\tINC\tHL")
            return '\n'.join(expanded_instructions)

        if dec_match:
            reg = dec_match.group(1).upper()
            if label:
                expanded_instructions.append(f"{label}\tLD\t{reg},(HL){comment}")
                label = ""
                comment = ""
            else:
                expanded_instructions.append(f"\tLD\t{reg},(HL){comment}")
                comment = ""
            expanded_instructions.append(f"\tDEC\tHL")
            return '\n'.join(expanded_instructions)

    # Handle 16-bit rotate/shift instructions (not supported by MACRO-80)
    # These need to be expanded to operate on individual bytes
    # For right operations (RR, SRL, SRA): high byte first, then low byte
    # For left operations (RL, SLA): low byte first, then high byte
    if mnemonic.upper() in ('RR', 'RL', 'SRL', 'SRA', 'SLA', 'RLC', 'RRC'):
        operand_stripped = operands.strip().upper() if operands else ""
        if operand_stripped in ('DE', 'HL', 'BC'):
            high_reg = operand_stripped[0]
            low_reg = operand_stripped[1]

            # Determine order based on instruction type
            # For right shifts/rotates: process high byte first so carry propagates down
            # For left shifts/rotates: process low byte first so carry propagates up
            if mnemonic.upper() in ('RR', 'SRL', 'SRA', 'RRC'):
                # Right operations: RR D, then RR E (carry flows from D to E)
                first_reg = high_reg
                second_reg = low_reg
            else:
                # Left operations: RL E, then RL D (carry flows from E to D)
                first_reg = low_reg
                second_reg = high_reg

            # Expand to two instructions
            if label:
                expanded_instructions.append(f"{label}\t{mnemonic}\t{first_reg}{comment}")
                label = ""
                comment = ""
            else:
                expanded_instructions.append(f"\t{mnemonic}\t{first_reg}{comment}")
                comment = ""
            expanded_instructions.append(f"\t{mnemonic}\t{second_reg}")
            # Return early with expanded instructions joined by newlines
            return '\n'.join(expanded_instructions)

    # Uppercase register names in operands
    if operands:
        for reg in REGISTERS:
            operands = re.sub(rf'\b{reg}\b', reg.upper(), operands, flags=re.IGNORECASE)

    # Build result - ensure space before comment if there's one
    if comment and not comment.startswith(' ') and not comment.startswith('\t'):
        comment = '\t' + comment

    if label:
        if operands:
            return f"{label}\t{mnemonic}\t{operands}{comment}"
        elif mnemonic:
            return f"{label}\t{mnemonic}{comment}"
        else:
            return f"{label}{comment}"
    else:
        if operands:
            return f"\t{mnemonic}\t{operands}{comment}"
        else:
            return f"\t{mnemonic}{comment}"


def convert_file(input_path: Path, output_path: Path = None, module_name: str = None) -> str:
    """Convert a z88dk assembly file to um80 format."""
    global current_global_label

    # Reset global label tracking for each file
    current_global_label = ""

    with open(input_path, 'r') as f:
        lines = f.readlines()

    # Determine module name from filename if not provided
    if module_name is None:
        module_name = input_path.stem.upper()

    result = []

    # Add header
    result.append(f"; Converted from z88dk: {input_path.name}")
    result.append(f"; Auto-generated by z88dk_to_um80.py")
    result.append("")
    result.append("\t.Z80")
    result.append(f"\tNAME {module_name}")
    result.append("")

    # Track if we've seen any code yet (to know when to insert CSEG)
    seen_code = False
    seen_public = False
    public_lines = []
    extrn_lines = []

    # First pass: collect PUBLIC and EXTRN declarations
    for line in lines:
        stripped = line.strip()
        if stripped.upper().startswith('PUBLIC'):
            public_lines.append(convert_line(line).strip())
        elif stripped.upper().startswith('EXTERN'):
            extrn_lines.append(convert_line(line).strip())

    # Output PUBLIC declarations (ensure they have tab prefix)
    for pub in public_lines:
        pub = pub.strip()
        if not pub.startswith('\t'):
            pub = '\t' + pub
        result.append(pub)
    if public_lines:
        result.append("")

    # Output EXTRN declarations (ensure they have tab prefix)
    for ext in extrn_lines:
        ext = ext.strip()
        if not ext.startswith('\t'):
            ext = '\t' + ext
        result.append(ext)
    if extrn_lines:
        result.append("")

    # Add CSEG
    result.append("\tCSEG")
    result.append("")

    # Reset global label before second pass
    current_global_label = ""

    # Second pass: convert the rest
    for line in lines:
        stripped = line.strip().upper()
        # Skip PUBLIC/EXTERN/SECTION - already handled
        if (stripped.startswith('PUBLIC') or
            stripped.startswith('EXTERN') or
            stripped.startswith('SECTION')):
            continue

        converted = convert_line(line)
        result.append(converted.rstrip())

    # Add END
    result.append("")
    result.append("\tEND")
    result.append("")

    output = '\n'.join(result)

    if output_path:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, 'w') as f:
            f.write(output)

    return output


def convert_directory(input_dir: Path, output_dir: Path, pattern: str = "*.asm"):
    """Convert all assembly files in a directory."""
    input_dir = Path(input_dir)
    output_dir = Path(output_dir)

    for asm_file in input_dir.glob(pattern):
        output_file = output_dir / asm_file.name
        print(f"Converting {asm_file} -> {output_file}")
        convert_file(asm_file, output_file)


def main():
    parser = argparse.ArgumentParser(
        description="Convert z88dk assembly to um80/MACRO-80 syntax"
    )
    parser.add_argument('input', help='Input file or directory')
    parser.add_argument('output', nargs='?', help='Output file or directory')
    parser.add_argument('--dir', action='store_true',
                        help='Process entire directory')
    parser.add_argument('--pattern', default='*.asm',
                        help='File pattern when processing directory')

    args = parser.parse_args()

    input_path = Path(args.input)

    if args.dir or input_path.is_dir():
        if not args.output:
            print("Error: output directory required for directory mode")
            sys.exit(1)
        convert_directory(input_path, Path(args.output), args.pattern)
    else:
        output_path = Path(args.output) if args.output else None
        result = convert_file(input_path, output_path)
        if not output_path:
            print(result)


if __name__ == '__main__':
    main()
