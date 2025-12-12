-- GNAT.Byte_Swapping body for Z80
-- Byte swapping implementation

package body GNAT.Byte_Swapping is

   use Interfaces;

   --------------
   -- Swapped2 --
   --------------

   function Swapped2 (U : Unsigned_16) return Unsigned_16 is
   begin
      return Unsigned_16 (Shift_Left (U and 16#00FF#, 8) or
                          Shift_Right (U and 16#FF00#, 8));
   end Swapped2;

   --------------
   -- Swapped4 --
   --------------

   function Swapped4 (U : Unsigned_32) return Unsigned_32 is
   begin
      return Unsigned_32 (
         Shift_Left (U and 16#000000FF#, 24) or
         Shift_Left (U and 16#0000FF00#, 8) or
         Shift_Right (U and 16#00FF0000#, 8) or
         Shift_Right (U and 16#FF000000#, 24));
   end Swapped4;

   --------------
   -- Swapped8 --
   --------------

   function Swapped8 (U : Unsigned_64) return Unsigned_64 is
   begin
      return Unsigned_64 (
         Shift_Left (U and 16#00000000000000FF#, 56) or
         Shift_Left (U and 16#000000000000FF00#, 40) or
         Shift_Left (U and 16#0000000000FF0000#, 24) or
         Shift_Left (U and 16#00000000FF000000#, 8) or
         Shift_Right (U and 16#000000FF00000000#, 8) or
         Shift_Right (U and 16#0000FF0000000000#, 24) or
         Shift_Right (U and 16#00FF000000000000#, 40) or
         Shift_Right (U and 16#FF00000000000000#, 56));
   end Swapped8;

   -----------
   -- Swap2 --
   -----------

   procedure Swap2 (U : in Out Unsigned_16) is
   begin
      U := Swapped2 (U);
   end Swap2;

   -----------
   -- Swap4 --
   -----------

   procedure Swap4 (U : in Out Unsigned_32) is
   begin
      U := Swapped4 (U);
   end Swap4;

   -----------
   -- Swap8 --
   -----------

   procedure Swap8 (U : in Out Unsigned_64) is
   begin
      U := Swapped8 (U);
   end Swap8;

end GNAT.Byte_Swapping;
