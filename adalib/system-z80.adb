-- System.Z80 body for Z80
-- Z80-specific implementation

with System.Machine_Code;
with System.Storage_Elements;

package body System.Z80 is

   use System.Storage_Elements;

   --------------
   -- Out_Port --
   --------------

   procedure Out_Port (Port : Byte; Value : Byte) is
   begin
      System.Machine_Code.Asm (
         "out (%0), %1",
         Inputs => (Byte'Asm_Input ("d", Port),
                    Byte'Asm_Input ("a", Value)),
         Volatile => True);
   end Out_Port;

   -------------
   -- In_Port --
   -------------

   function In_Port (Port : Byte) return Byte is
      Result : Byte;
   begin
      System.Machine_Code.Asm (
         "in %0, (%1)",
         Outputs => Byte'Asm_Output ("=a", Result),
         Inputs => Byte'Asm_Input ("d", Port),
         Volatile => True);
      return Result;
   end In_Port;

   ----------
   -- Peek --
   ----------

   function Peek (Address : Word) return Byte is
      Mem : Byte;
      for Mem'Address use System.Address (Integer_Address (Address));
   begin
      return Mem;
   end Peek;

   ----------
   -- Poke --
   ----------

   procedure Poke (Address : Word; Value : Byte) is
      Mem : Byte;
      for Mem'Address use System.Address (Integer_Address (Address));
   begin
      Mem := Value;
   end Poke;

   ---------------
   -- Peek_Word --
   ---------------

   function Peek_Word (Address : Word) return Word is
   begin
      return Word (Peek (Address)) + Word (Peek (Address + 1)) * 256;
   end Peek_Word;

   ---------------
   -- Poke_Word --
   ---------------

   procedure Poke_Word (Address : Word; Value : Word) is
   begin
      Poke (Address, Byte (Value mod 256));
      Poke (Address + 1, Byte (Value / 256));
   end Poke_Word;

   ----------
   -- Halt --
   ----------

   procedure Halt is
   begin
      System.Machine_Code.Asm ("halt", Volatile => True);
   end Halt;

   ---------
   -- Nop --
   ---------

   procedure Nop is
   begin
      System.Machine_Code.Asm ("nop", Volatile => True);
   end Nop;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Enable_Interrupts;

   ------------------------
   -- Set_Interrupt_Mode --
   ------------------------

   procedure Set_Interrupt_Mode (Mode : Interrupt_Mode) is
   begin
      case Mode is
         when Mode_0 =>
            System.Machine_Code.Asm ("im 0", Volatile => True);
         when Mode_1 =>
            System.Machine_Code.Asm ("im 1", Volatile => True);
         when Mode_2 =>
            System.Machine_Code.Asm ("im 2", Volatile => True);
      end case;
   end Set_Interrupt_Mode;

   ------------
   -- Get_SP --
   ------------

   function Get_SP return Word is
      Result : Word;
   begin
      System.Machine_Code.Asm (
         "ld (%0), sp",
         Outputs => Word'Asm_Output ("=m", Result),
         Volatile => True);
      return Result;
   end Get_SP;

   ------------
   -- Set_SP --
   ------------

   procedure Set_SP (Value : Word) is
   begin
      System.Machine_Code.Asm (
         "ld sp, %0",
         Inputs => Word'Asm_Input ("g", Value),
         Volatile => True);
   end Set_SP;

end System.Z80;
