-- System.Z80 for Z80
-- Z80-specific definitions and utilities

package System.Z80 is
   pragma Preelaborate;

   -- Z80 register types
   type Byte is mod 2**8;
   type Word is mod 2**16;

   -- Register pair
   type Register_Pair is record
      Low  : Byte;
      High : Byte;
   end record;
   for Register_Pair'Size use 16;

   -- CPU flags
   type Flags_Register is record
      C  : Boolean;  -- Carry
      N  : Boolean;  -- Add/Subtract
      PV : Boolean;  -- Parity/Overflow
      H  : Boolean;  -- Half Carry
      Z  : Boolean;  -- Zero
      S  : Boolean;  -- Sign
   end record;

   -- Interrupt modes
   type Interrupt_Mode is (Mode_0, Mode_1, Mode_2);

   -- Port I/O
   procedure Out_Port (Port : Byte; Value : Byte);
   function In_Port (Port : Byte) return Byte;

   -- Memory access
   function Peek (Address : Word) return Byte;
   procedure Poke (Address : Word; Value : Byte);
   function Peek_Word (Address : Word) return Word;
   procedure Poke_Word (Address : Word; Value : Word);

   -- CPU control
   procedure Halt;
   procedure Nop;
   procedure Disable_Interrupts;
   procedure Enable_Interrupts;
   procedure Set_Interrupt_Mode (Mode : Interrupt_Mode);

   -- Stack pointer access
   function Get_SP return Word;
   procedure Set_SP (Value : Word);

end System.Z80;
