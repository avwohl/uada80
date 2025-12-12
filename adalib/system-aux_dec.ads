-- System.Aux_DEC for Z80
-- DEC Ada compatibility support

package System.Aux_DEC is
   pragma Pure;

   -- Type definitions for DEC Ada compatibility
   type Bit_Array is array (Natural range <>) of Boolean;
   pragma Pack (Bit_Array);

   type Bit_Array_8 is new Bit_Array (0 .. 7);
   type Bit_Array_16 is new Bit_Array (0 .. 15);
   type Bit_Array_32 is new Bit_Array (0 .. 31);

   type Unsigned_Byte is mod 2 ** 8;
   type Unsigned_Word is mod 2 ** 16;
   type Unsigned_Longword is mod 2 ** 32;

   -- AST handler type (unused on Z80)
   type AST_Handler is access procedure (Param : System.Address);
   No_AST_Handler : constant AST_Handler := null;

   -- Import/Export for machine code
   type External_Name is new String;

   -- Short Address (for Z80's 16-bit addressing)
   type Short_Address is new System.Address;

   -- Type conversions
   function To_Address (Value : Integer) return System.Address;
   function To_Integer (Value : System.Address) return Integer;

end System.Aux_DEC;
