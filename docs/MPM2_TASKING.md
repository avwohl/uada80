# Ada tasking on MP/M II

Ada tasking (tasks, entries, rendezvous, protected types) runs on [MP/M II](https://github.com/avwohl/mpm2) using OS-native primitives for preemptive multitasking. On CP/M 2.2 (single-user), the runtime provides cooperative tasking stubs.

## How It Works

Ada tasks become MP/M II subprocesses sharing one memory bank. The OS handles preemptive scheduling, and all inter-task communication uses BDOS queue operations:

| Ada Construct | MP/M II Primitive |
|---|---|
| Task creation | P_CREATE (BDOS 144) |
| Entry call | Q_WRITE (BDOS 139) - blocking send to queue |
| Accept statement | Q_READ (BDOS 137) - blocking receive from queue |
| Select/else | Q_CREAD (BDOS 138) - conditional (non-blocking) read |
| Protected object | MX queue (mutual exclusion) |
| Delay | P_DELAY (BDOS 141) |
| Abort | P_ABORT (BDOS 157) |

## Building for MP/M II

```bash
# Compile Ada to assembly
python -m uada80 program.ada -o program.asm

# Assemble
um80 -o program.rel program.asm

# Link as PRL (relocatable for MP/M II)
ul80 --prl -p 0 -o program.prl program.rel -L runtime/ -l libada_mpm.lib

# Or link as COM (also works under MP/M)
ul80 -o program.com program.rel -L runtime/ -l libada_mpm.lib
```

## Running on the MP/M II Emulator

The [mpm2](https://github.com/avwohl/mpm2) emulator provides a full MP/M II environment with SSH terminal access:

```bash
# Start the emulator (up to 4 concurrent consoles)
mpm2_emu --no-auth -p 127.0.0.1:2222 -d A:disks/mpm2_system_work.img

# Connect via SSH
ssh -p 2222 user@localhost

# Upload and run
sftp -P 2222 user@localhost
put program.prl /A.0/PROGRAM.PRL
```

## Runtime Libraries

Two runtime libraries are provided:

- **`libada.lib`** - CP/M 2.2 runtime with cooperative tasking stubs
- **`libada_mpm.lib`** - MP/M II runtime with OS-native preemptive tasking

Rebuild after changes:
```bash
cd runtime && make clean && make
```
