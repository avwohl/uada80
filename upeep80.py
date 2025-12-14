"""
Z80 Peephole Optimizer for uada80.

Performs pattern-based optimizations on Z80 assembly output.
Inspired by optimizations from z88dk and other Z80 compilers.

Optimization patterns:
1. Redundant load elimination
2. Push/pop optimization
3. Jump chain optimization
4. Strength reduction for arithmetic
5. Register pairing optimizations
"""

import re
from typing import Optional


def optimize_peephole(asm: str) -> str:
    """
    Apply peephole optimizations to Z80 assembly.

    Args:
        asm: Z80 assembly source code

    Returns:
        Optimized assembly source code
    """
    lines = asm.split('\n')
    changed = True
    passes = 0
    max_passes = 10

    while changed and passes < max_passes:
        changed = False
        passes += 1
        new_lines = []
        i = 0

        while i < len(lines):
            # Try each optimization pattern
            result = None

            # Pattern 1: ld A, X / ld A, X -> ld A, X (redundant load)
            if i + 1 < len(lines):
                result = _opt_redundant_load(lines[i], lines[i + 1])
                if result:
                    new_lines.append(result)
                    i += 2
                    changed = True
                    continue

            # Pattern 2: push HL / pop HL -> (nothing)
            if i + 1 < len(lines):
                result = _opt_push_pop_same(lines[i], lines[i + 1])
                if result is not None:  # Empty string means remove both
                    if result:
                        new_lines.append(result)
                    i += 2
                    changed = True
                    continue

            # Pattern 3: push HL / pop DE -> ld D, H / ld E, L
            if i + 1 < len(lines):
                result = _opt_push_pop_transfer(lines[i], lines[i + 1])
                if result:
                    new_lines.extend(result)
                    i += 2
                    changed = True
                    continue

            # Pattern 4: ld A, 0 -> xor A
            result = _opt_load_zero(lines[i])
            if result:
                new_lines.append(result)
                i += 1
                changed = True
                continue

            # Pattern 5: jp Z, L / jp L2 / L: -> jp NZ, L2 / L:
            if i + 2 < len(lines):
                result = _opt_jump_chain(lines[i], lines[i + 1], lines[i + 2])
                if result:
                    new_lines.extend(result)
                    i += 3
                    changed = True
                    continue

            # Pattern 6: add HL, BC where BC = 0 -> (nothing)
            # (Would need data flow analysis)

            # Pattern 7: ld HL, N / add HL, DE -> ld HL, N+DE (if DE known)
            # (Would need constant propagation)

            # Pattern 8: inc HL / dec HL -> (nothing)
            if i + 1 < len(lines):
                result = _opt_inc_dec_cancel(lines[i], lines[i + 1])
                if result is not None:
                    if result:
                        new_lines.append(result)
                    i += 2
                    changed = True
                    continue

            # Pattern 9: ld A, (HL) / ld (HL), A -> ld A, (HL)
            if i + 1 < len(lines):
                result = _opt_load_store_same(lines[i], lines[i + 1])
                if result:
                    new_lines.append(result)
                    i += 2
                    changed = True
                    continue

            # Pattern 10: jp L / L: -> L:
            if i + 1 < len(lines):
                result = _opt_jump_to_next(lines[i], lines[i + 1])
                if result:
                    new_lines.append(result)
                    i += 2
                    changed = True
                    continue

            # Pattern 11: ld HL, N / ld DE, N -> ld HL, N / ex DE, HL / push HL / pop DE
            # Actually simpler: ld HL, N / ld D, H / ld E, L
            # (Would need to track what N is)

            # Pattern 12: Multiply by power of 2 using shifts
            # add HL, HL is 2x, sla L / rl H is also 2x but slower

            # Pattern 13: or A / jp Z -> jp Z (or A doesn't change Z after most ops)
            # (Needs careful analysis of what sets flags)

            # Pattern 14: cp 0 / jp Z -> or A / jp Z
            result = _opt_compare_zero(lines[i])
            if result:
                new_lines.append(result)
                i += 1
                changed = True
                continue

            # Pattern 15: ld A, B / or A -> ld A, B / or B (same effect, sometimes faster)
            # Actually ld A, B already sets some flags, so we could skip or A in some cases

            # No optimization found - keep the line
            new_lines.append(lines[i])
            i += 1

        lines = new_lines

    return '\n'.join(lines)


def _parse_instr(line: str) -> Optional[tuple[str, list[str]]]:
    """Parse an assembly instruction into mnemonic and operands."""
    line = line.strip()
    if not line or line.startswith(';') or line.endswith(':'):
        return None

    # Remove comments
    if ';' in line:
        line = line[:line.index(';')].strip()

    # Split into mnemonic and operands
    parts = line.split(None, 1)
    if not parts:
        return None

    mnemonic = parts[0].lower()
    operands = []
    if len(parts) > 1:
        # Split operands by comma, handling parentheses
        ops_str = parts[1]
        operands = [op.strip() for op in ops_str.split(',')]

    return (mnemonic, operands)


def _opt_redundant_load(line1: str, line2: str) -> Optional[str]:
    """Remove redundant identical loads."""
    instr1 = _parse_instr(line1)
    instr2 = _parse_instr(line2)

    if instr1 and instr2:
        if (instr1[0] == 'ld' and instr2[0] == 'ld' and
            instr1[1] == instr2[1] and len(instr1[1]) == 2):
            # Two identical ld instructions - keep only first
            return line1

    return None


def _opt_push_pop_same(line1: str, line2: str) -> Optional[str]:
    """Remove push X / pop X sequences."""
    instr1 = _parse_instr(line1)
    instr2 = _parse_instr(line2)

    if instr1 and instr2:
        if (instr1[0] == 'push' and instr2[0] == 'pop' and
            len(instr1[1]) == 1 and len(instr2[1]) == 1 and
            instr1[1][0].upper() == instr2[1][0].upper()):
            # push X / pop X -> nothing
            return ""  # Empty string signals removal

    return None


def _opt_push_pop_transfer(line1: str, line2: str) -> Optional[list[str]]:
    """Convert push X / pop Y to direct register transfer."""
    instr1 = _parse_instr(line1)
    instr2 = _parse_instr(line2)

    if instr1 and instr2:
        if (instr1[0] == 'push' and instr2[0] == 'pop' and
            len(instr1[1]) == 1 and len(instr2[1]) == 1):
            src = instr1[1][0].upper()
            dst = instr2[1][0].upper()
            if src != dst:
                # Map register pairs to their halves
                # Note: IX/IY half-registers (IXH, IXL, IYH, IYL) are undocumented
                # and not supported by um80 assembler, so we exclude them
                pair_map = {'HL': ('H', 'L'), 'DE': ('D', 'E'), 'BC': ('B', 'C'),
                           'AF': ('A', 'F')}
                if src in pair_map and dst in pair_map:
                    src_h, src_l = pair_map[src]
                    dst_h, dst_l = pair_map[dst]
                    # push HL / pop DE -> ld D, H / ld E, L
                    indent = '    '
                    return [f"{indent}ld {dst_h}, {src_h}", f"{indent}ld {dst_l}, {src_l}"]

    return None


def _opt_load_zero(line: str) -> Optional[str]:
    """Convert ld A, 0 to xor A."""
    instr = _parse_instr(line)
    if instr and instr[0] == 'ld':
        if len(instr[1]) == 2 and instr[1][0].upper() == 'A' and instr[1][1] == '0':
            # ld A, 0 -> xor A (smaller and faster)
            indent = line[:len(line) - len(line.lstrip())]
            return f"{indent}xor A"
    return None


def _opt_jump_chain(line1: str, line2: str, line3: str) -> Optional[list[str]]:
    """Optimize jump chains: jp Z, L1 / jp L2 / L1: -> jp NZ, L2 / L1:"""
    instr1 = _parse_instr(line1)
    instr2 = _parse_instr(line2)

    if instr1 and instr2 and line3.strip().endswith(':'):
        label = line3.strip()[:-1]  # Remove ':'
        if (instr1[0] in ('jp', 'jr') and len(instr1[1]) == 2 and
            instr2[0] in ('jp', 'jr') and len(instr2[1]) == 1):
            cond = instr1[1][0].upper()
            target1 = instr1[1][1]
            target2 = instr2[1][0]
            if target1 == label:
                # Invert condition and jump directly
                inv_cond = {'Z': 'NZ', 'NZ': 'Z', 'C': 'NC', 'NC': 'C',
                           'PE': 'PO', 'PO': 'PE', 'M': 'P', 'P': 'M'}
                if cond in inv_cond:
                    indent = line1[:len(line1) - len(line1.lstrip())]
                    return [f"{indent}jp {inv_cond[cond]}, {target2}", line3]

    return None


def _opt_inc_dec_cancel(line1: str, line2: str) -> Optional[str]:
    """Remove inc X / dec X sequences."""
    instr1 = _parse_instr(line1)
    instr2 = _parse_instr(line2)

    if instr1 and instr2:
        if (instr1[0] == 'inc' and instr2[0] == 'dec' and
            instr1[1] == instr2[1]):
            return ""
        if (instr1[0] == 'dec' and instr2[0] == 'inc' and
            instr1[1] == instr2[1]):
            return ""

    return None


def _opt_load_store_same(line1: str, line2: str) -> Optional[str]:
    """Remove redundant ld A, (HL) / ld (HL), A."""
    instr1 = _parse_instr(line1)
    instr2 = _parse_instr(line2)

    if instr1 and instr2:
        if (instr1[0] == 'ld' and instr2[0] == 'ld' and
            len(instr1[1]) == 2 and len(instr2[1]) == 2):
            # Check for ld A, (X) / ld (X), A
            if (instr1[1][1] == instr2[1][0] and
                instr1[1][0] == instr2[1][1]):
                # Keep only the load
                return line1

    return None


def _opt_jump_to_next(line1: str, line2: str) -> Optional[str]:
    """Remove jump to immediately following label."""
    instr = _parse_instr(line1)

    if instr and line2.strip().endswith(':'):
        label = line2.strip()[:-1]
        if instr[0] in ('jp', 'jr') and len(instr[1]) == 1:
            if instr[1][0] == label:
                # Jump to next line - remove jump
                return line2

    return None


def _opt_compare_zero(line: str) -> Optional[str]:
    """Convert cp 0 to or A."""
    instr = _parse_instr(line)
    if instr and instr[0] == 'cp':
        if len(instr[1]) == 1 and instr[1][0] == '0':
            # cp 0 -> or A (sets Z flag the same way)
            indent = line[:len(line) - len(line.lstrip())]
            return f"{indent}or A"
    return None


# Additional optimization functions can be added here

def _opt_multiply_power_of_two(lines: list[str], start: int) -> Optional[tuple[list[str], int]]:
    """
    Optimize multiplication by power of 2.

    ld DE, 2 / call _mul16 -> add HL, HL
    ld DE, 4 / call _mul16 -> add HL, HL / add HL, HL
    etc.
    """
    # Would need to look for the pattern and replace
    # This is more complex and would need context
    return None


def _opt_divide_power_of_two(lines: list[str], start: int) -> Optional[tuple[list[str], int]]:
    """
    Optimize division by power of 2.

    ld DE, 2 / call _div16 -> srl H / rr L
    ld DE, 4 / call _div16 -> srl H / rr L / srl H / rr L
    """
    return None
