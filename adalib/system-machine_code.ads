-- System.Machine_Code for Z80
-- Inline assembly code support
--
-- Allows embedding Z80 assembly instructions in Ada code

package System.Machine_Code is
   pragma Preelaborate;

   -- Asm_Input_Operand specifies an input to an assembly statement
   type Asm_Input_Operand is private;

   -- Asm_Output_Operand specifies an output from an assembly statement
   type Asm_Output_Operand is private;

   -- Asm_Insn is the assembly instruction type
   -- In practice, the compiler handles Asm statements specially
   type Asm_Insn is private;

   -- No_Input_Operands and No_Output_Operands for Asm statements
   -- that don't need inputs/outputs
   No_Input_Operands  : constant Asm_Input_Operand;
   No_Output_Operands : constant Asm_Output_Operand;

   -- Volatile controls whether the compiler can reorder the Asm statement
   -- True = cannot be moved, False = can be optimized

   -- Common Z80 instructions as string constants
   NOP   : constant String := "NOP";
   HALT  : constant String := "HALT";
   DI    : constant String := "DI";
   EI    : constant String := "EI";
   RET   : constant String := "RET";
   RETI  : constant String := "RETI";
   RETN  : constant String := "RETN";

   -- Note: Actual Asm procedure calls are intrinsic and handled
   -- by the compiler. This package provides supporting types.

private

   type Asm_Input_Operand is null record;
   type Asm_Output_Operand is null record;
   type Asm_Insn is null record;

   No_Input_Operands  : constant Asm_Input_Operand := (null record);
   No_Output_Operands : constant Asm_Output_Operand := (null record);

end System.Machine_Code;
