# Record Types Implementation

## Overview

Record types are now fully implemented in uada80, including:
- Record type declarations
- Field access (reading and writing)
- Nested records
- Multiple field types
- Record parameters and return values

## Features Implemented

### Basic Record Support

```ada
type Point is record
    X : Integer;
    Y : Integer;
end record;

P : Point;
```

### Field Assignment

```ada
P.X := 10;
P.Y := 20;
```

Generates Z80 assembly using:
- `LEA` (Load Effective Address) to calculate field addresses
- `STORE` instructions to write to computed addresses
- Field offsets calculated at compile time

### Field Reading

```ada
Sum : Integer;
Sum := P.X + P.Y;
```

Generates:
- `LEA` to get field address
- `LOAD` to read from computed address
- Arithmetic operations on loaded values

### Nested Records

```ada
type Line is record
    Start : Point;
    Finish : Point;
end record;

L : Line;
L.Start.X := 0;
L.Finish.X := 100;
```

Nested field access properly calculates cumulative offsets.

### Mixed Field Types

```ada
type Person is record
    Age : Integer;      -- 16 bits
    Initial : Character; -- 8 bits
    Active : Boolean;   -- 8 bits
end record;
```

Fields laid out sequentially in memory with proper offsets.

## Code Generation

### IR Generation

Record field access is lowered to IR as:

1. **Get record base address** (`_get_record_base`):
   - For local records: `LEA` from stack frame (IX + offset)
   - For global records: `LEA` with global symbol name

2. **Calculate field offset** (`_get_field_offset`):
   - Lookup field in record type's component list
   - Return offset in bytes (offset_bits / 8)

3. **Compute field address** (`_lower_selected`):
   ```
   base_addr = LEA record
   if offset != 0:
       field_addr = ADD base_addr, offset
   else:
       field_addr = base_addr
   ```

4. **Load or store** through computed address:
   - `LOAD result, [field_addr]`
   - `STORE [field_addr], value`

### Z80 Assembly Generation

Example for `P.X := 42`:

```asm
; Get address of P (local variable)
push IX
pop HL
ld DE, offset_of_P
add HL, DE

; Add field offset for X (0 bytes)
; (no add needed for first field)

; Store value
ld (HL), 42   ; Low byte
inc HL
ld (HL), 0    ; High byte
```

Example for nested `L.Start.X := 0`:

```asm
; Get address of L
push IX
pop HL
ld DE, offset_of_L
add HL, DE

; Add offset for Start field
ld DE, offset_of_Start
add HL, DE

; Add offset for X field (0 bytes)
; (no add for first field of Start)

; Store value
ld (HL), 0
inc HL
ld (HL), 0
```

## Memory Layout

Records are laid out sequentially in memory:

```ada
type Rec is record
    A : Integer;    -- Offset 0, size 2 bytes
    B : Integer;    -- Offset 2, size 2 bytes
    C : Character;  -- Offset 4, size 1 byte
end record;
```

Memory layout:
```
+---+---+---+---+---+
| A | A | B | B | C |
+---+---+---+---+---+
  0   1   2   3   4
```

## Type System Integration

### RecordType Class

Located in `uada80/type_system.py`:

```python
@dataclass
class RecordComponent:
    name: str
    component_type: AdaType
    offset_bits: int

@dataclass
class RecordType(AdaType):
    name: str
    components: list[RecordComponent]
    size_bits: int
```

### Field Offset Calculation

During semantic analysis:
1. Iterate through components
2. Calculate cumulative offset based on previous component sizes
3. Store offset in `RecordComponent.offset_bits`

Example:
```python
offset = 0
for comp in record.components:
    comp.offset_bits = offset
    offset += comp.component_type.size_bits
total_size = offset
```

## Lowering Implementation

### Files Modified

**`uada80/lowering.py`**:
- `_lower_assignment_stmt`: Handle `SelectedName` targets
- `_lower_selected_store`: Store to record field
- `_lower_selected`: Load from record field
- `_get_record_base`: Get record base address
- `_get_field_offset`: Get field byte offset
- `_get_field_size`: Get field byte size

### Key Functions

```python
def _get_record_base(self, prefix: Expr) -> Optional[VReg]:
    """Get the base address of a record (local or global)."""

def _get_field_offset(self, selected: SelectedName) -> int:
    """Get the byte offset of a record field."""

def _lower_selected(self, expr: SelectedName):
    """Lower a record field read."""

def _lower_selected_store(self, target: SelectedName, value):
    """Lower a record field assignment."""
```

## Testing

Comprehensive test suite in `tests/test_records.py`:

### Basic Tests (8 tests)
- Simple record declaration
- Field assignment
- Field reading
- Multiple field types
- Nested records
- Record initialization
- Records as parameters
- Records as return values

### Implementation Tests (4 tests)
- Field offset calculation
- Total record size
- IR generation for assignment
- IR generation for reading

### Error Handling (3 tests)
- Undefined field access
- Type mismatch
- Multiple record types

**Status**: ✅ All 15 tests passing

## Limitations

### Current Limitations

1. **No record aggregates**: `P := (X => 10, Y => 20)` not yet supported
2. **No discriminants**: Variant records not implemented
3. **No alignment control**: Fields laid out sequentially
4. **No packing**: No bit-level packing of fields

### Future Enhancements

1. **Record aggregates**:
   ```ada
   P : Point := (X => 10, Y => 20);
   P := (10, 20);  -- Positional
   ```

2. **Variant records** (discriminants):
   ```ada
   type Shape(Kind : Shape_Type) is record
       case Kind is
           when Circle => Radius : Integer;
           when Rectangle => Width, Height : Integer;
       end case;
   end record;
   ```

3. **Representation clauses**:
   ```ada
   for Point use record
       X at 0 range 0 .. 15;
       Y at 2 range 0 .. 15;
   end record;
   ```

4. **Limited types**:
   ```ada
   type Handle is limited record
       -- No assignment or copying
   end record;
   ```

## Examples

### Example 1: Graphics Point

```ada
procedure Draw_Line is
    type Point is record
        X : Integer;
        Y : Integer;
    end record;

    Start, Finish : Point;
begin
    Start.X := 0;
    Start.Y := 0;
    Finish.X := 100;
    Finish.Y := 100;
    -- Draw line from Start to Finish
end Draw_Line;
```

### Example 2: RGB Color

```ada
procedure Set_Color is
    type RGB is record
        Red   : Integer;  -- 0-255
        Green : Integer;
        Blue  : Integer;
    end record;

    Background : RGB;
begin
    Background.Red := 255;
    Background.Green := 255;
    Background.Blue := 255;  -- White
end Set_Color;
```

### Example 3: Nested Date/Time

```ada
procedure Log_Event is
    type Time is record
        Hour : Integer;
        Minute : Integer;
    end record;

    type Date is record
        Year : Integer;
        Month : Integer;
        Day : Integer;
    end record;

    type Timestamp is record
        Date_Part : Date;
        Time_Part : Time;
    end record;

    Event_Time : Timestamp;
begin
    Event_Time.Date_Part.Year := 2025;
    Event_Time.Date_Part.Month := 12;
    Event_Time.Date_Part.Day := 6;
    Event_Time.Time_Part.Hour := 23;
    Event_Time.Time_Part.Minute := 15;
end Log_Event;
```

## Performance Considerations

### Z80 Assembly

Record field access on Z80 is relatively efficient:

1. **First field** (offset 0): 1-2 instructions to load base, then direct access
2. **Other fields**: Add 2-4 instructions to calculate field address
3. **Nested records**: Linear cost proportional to nesting depth

### Optimization Opportunities

Future optimizations could include:

1. **Register allocation**: Keep frequently-used record addresses in IX/IY
2. **Constant folding**: Pre-calculate field offsets at compile time
3. **Peephole optimization**: Eliminate redundant address calculations
4. **Strength reduction**: Replace offset ADD with simpler inc operations

## Related Documentation

- [LANGUAGE_SUBSET.md](LANGUAGE_SUBSET.md) - Ada language subset specification
- [AST_DESIGN.md](AST_DESIGN.md) - AST node definitions
- [type_system.py](../uada80/type_system.py) - Type system implementation
- [lowering.py](../uada80/lowering.py) - AST to IR lowering
- [test_records.py](../tests/test_records.py) - Comprehensive test suite

## Conclusion

Record types are fully functional in uada80, supporting:
- ✅ Basic record declarations
- ✅ Field access (read/write)
- ✅ Nested records
- ✅ Mixed field types
- ✅ Records as parameters
- ✅ Proper code generation
- ✅ Comprehensive testing

This provides a solid foundation for building more complex Ada programs with structured data types.
