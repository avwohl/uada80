-- GNAT.Debug_Utilities body for Z80
-- Debug output utilities implementation

with System.Storage_Elements;

package body GNAT.Debug_Utilities is

   Hex_Digits : constant String := "0123456789ABCDEF";

   -----------
   -- Image --
   -----------

   function Image (A : System.Address) return String is
      use System.Storage_Elements;
      Val : Integer_Address := To_Integer (A);
      Result : String (1 .. 6) := "0x0000";
   begin
      for I in reverse 3 .. 6 loop
         Result (I) := Hex_Digits (Natural (Val mod 16) + 1);
         Val := Val / 16;
      end loop;
      return Result;
   end Image;

   function Image (N : Integer) return String is
      Abs_N : Natural;
      Temp  : String (1 .. 11);  -- Max: -2147483648
      Pos   : Natural := Temp'Last;
      Neg   : constant Boolean := N < 0;
   begin
      if N = Integer'First then
         return "-2147483648";
      end if;

      Abs_N := (if Neg then -N else N);

      loop
         Temp (Pos) := Character'Val (Character'Pos ('0') + (Abs_N mod 10));
         Pos := Pos - 1;
         Abs_N := Abs_N / 10;
         exit when Abs_N = 0;
      end loop;

      if Neg then
         Temp (Pos) := '-';
         Pos := Pos - 1;
      end if;

      return Temp (Pos + 1 .. Temp'Last);
   end Image;

   function Image (N : Long_Integer) return String is
   begin
      return Image (Integer (N));  -- Z80 limitation
   end Image;

end GNAT.Debug_Utilities;
