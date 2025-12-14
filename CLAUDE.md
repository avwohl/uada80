# Claude Code Notes for UADA80

## Running Z80 Executables

### Toolchain Overview

The full toolchain for compiling and running Ada programs on Z80/CP/M:

```
Ada Source (.ada/.adb/.ads)
    ↓ uada80 (this compiler)
Z80 Assembly (.asm)
    ↓ um80 (assembler)
Relocatable Object (.rel)
    ↓ ul80 (linker) + libada.lib
CP/M Executable (.com)
    ↓ cpmemu (emulator)
Output
```

### Tool Locations

- **uada80**: This project (`python -m uada80`)
- **um80**: `~/src/um80_and_friends/` or `~/.local/bin/um80`
- **ul80**: `~/src/um80_and_friends/` or `~/.local/bin/ul80`
- **ulib80**: Library manager at same location
- **cpmemu**: `~/src/cpmemu/src/cpmemu`

### Compiling and Running an Ada Program

```bash
# 1. Compile Ada to Z80 assembly
python -m uada80 program.ada -o program.asm

# 2. Assemble to relocatable object
um80 program.asm -o program.rel

# 3. Link with runtime library
ul80 program.rel -L runtime/ -l libada.lib -o program.com

# 4. Run on CP/M emulator (Z80 mode for full instruction set)
~/src/cpmemu/src/cpmemu --z80 program.com
```

### Quick Test Command

For a simple test without the runtime library:

```bash
# Compile, assemble, and run
python -m uada80 test.ada -o test.asm && \
um80 test.asm -o test.rel && \
ul80 test.rel -o test.com && \
~/src/cpmemu/src/cpmemu --z80 test.com
```

### cpmemu Options

```bash
# Basic execution
~/src/cpmemu/src/cpmemu program.com

# Z80 mode (required for Z80-specific instructions)
~/src/cpmemu/src/cpmemu --z80 program.com

# With command line arguments
~/src/cpmemu/src/cpmemu --z80 program.com arg1 arg2

# Progress reporting (useful for long-running tests)
~/src/cpmemu/src/cpmemu --z80 --progress=100 program.com
```

### Environment Variables for cpmemu

```bash
export CPM_PROGRESS=100        # Report every 100M instructions
export CPM_PRINTER=/tmp/lst    # Redirect printer output
export CPM_DEBUG_BDOS=9,20,21  # Debug specific BDOS calls
```

### Runtime Library

The Ada runtime library is in `runtime/`:

- `runtime/libada.lib` - Prebuilt library
- `runtime/*.mac` - Source files (macro assembler format)
- `runtime/Makefile` - Build instructions

To rebuild the runtime:

```bash
cd runtime/
make clean && make
```

### Testing Compiled Programs

When adding execution tests, use this pattern:

```python
import subprocess
import tempfile
import os

def run_ada_program(source_code: str) -> tuple[int, str, str]:
    """Compile and run Ada source, return (exit_code, stdout, stderr)."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Write source
        ada_file = os.path.join(tmpdir, "test.ada")
        with open(ada_file, "w") as f:
            f.write(source_code)

        # Compile
        asm_file = os.path.join(tmpdir, "test.asm")
        subprocess.run(["python", "-m", "uada80", ada_file, "-o", asm_file], check=True)

        # Assemble
        rel_file = os.path.join(tmpdir, "test.rel")
        subprocess.run(["um80", asm_file, "-o", rel_file], check=True)

        # Link
        com_file = os.path.join(tmpdir, "test.com")
        subprocess.run(["ul80", rel_file, "-o", com_file], check=True)

        # Run
        result = subprocess.run(
            [os.path.expanduser("~/src/cpmemu/src/cpmemu"), "--z80", com_file],
            capture_output=True,
            text=True,
            timeout=30
        )
        return result.returncode, result.stdout, result.stderr
```

### CP/M Exit Codes

- Exit code 0: Success (program called BDOS function 0 or returned normally)
- Exit code 1: Error or assertion failure
- The emulator returns the value passed to BDOS warm boot

### Debugging Tips

1. **Check assembly output**: The generated .asm file shows the Z80 code
2. **Use progress reporting**: `--progress` shows instruction count
3. **Debug BDOS calls**: `CPM_DEBUG_BDOS=2` shows console output calls
4. **Ctrl+C**: Press 5 times rapidly to exit cpmemu if program hangs

### Memory Layout

```
0x0000-0x00FF  System area (BDOS vectors, FCBs, DMA)
0x0100-0xFBFF  TPA - where .com programs load and run
0xFC00+        BDOS/BIOS (emulated)
```

Maximum program size is approximately 57KB.

### Common Issues

1. **"um80 not found"**: Add `~/.local/bin` to PATH or use full path
2. **Link errors**: Ensure libada.lib is built and -L path is correct
3. **Timeout in tests**: Z80 is slow; increase timeout for complex programs
4. **Missing runtime**: Some Ada features need runtime support in libada.lib

### um80 Z80 Mode

**IMPORTANT**: um80 defaults to 8080 mode. The code generator emits a `.Z80` directive at the start of each assembly file to enable Z80 instructions.

Key points:
- The `.Z80` directive must appear before any Z80-specific instructions
- Instruction mnemonics must be UPPERCASE for um80 (lowercase won't work)
- um80 does NOT support undocumented Z80 instructions (IXH, IXL, IYH, IYL)
- Use `PUSH IX / POP HL` instead of `LD H, IXH / LD L, IXL`

### Execution Tests

End-to-end execution tests are in `tests/test_execution.py`. They:
1. Compile Ada source to Z80 assembly
2. Assemble with um80
3. Link with ul80
4. Run with cpmemu
5. Verify correct execution

Some tests are marked `xfail` because they need runtime library functions that aren't yet built for um80 format.
