-- System.Bit_Ops body for Z80
-- Bit manipulation operations implementation

package body System.Bit_Ops is

   use Interfaces;

   ---------------
   -- Bit_Count --
   ---------------

   function Bit_Count (Value : Unsigned_8) return Natural is
      V : Unsigned_8 := Value;
      Count : Natural := 0;
   begin
      while V /= 0 loop
         Count := Count + Natural (V and 1);
         V := V / 2;
      end loop;
      return Count;
   end Bit_Count;

   function Bit_Count (Value : Unsigned_16) return Natural is
      V : Unsigned_16 := Value;
      Count : Natural := 0;
   begin
      while V /= 0 loop
         Count := Count + Natural (V and 1);
         V := V / 2;
      end loop;
      return Count;
   end Bit_Count;

   function Bit_Count (Value : Unsigned_32) return Natural is
      V : Unsigned_32 := Value;
      Count : Natural := 0;
   begin
      while V /= 0 loop
         Count := Count + Natural (V and 1);
         V := V / 2;
      end loop;
      return Count;
   end Bit_Count;

   -------------------
   -- Leading_Zeros --
   -------------------

   function Leading_Zeros (Value : Unsigned_8) return Natural is
      V : Unsigned_8 := Value;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 8;
      end if;
      while V < 16#80# loop
         Count := Count + 1;
         V := V * 2;
      end loop;
      return Count;
   end Leading_Zeros;

   function Leading_Zeros (Value : Unsigned_16) return Natural is
      V : Unsigned_16 := Value;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 16;
      end if;
      while V < 16#8000# loop
         Count := Count + 1;
         V := V * 2;
      end loop;
      return Count;
   end Leading_Zeros;

   function Leading_Zeros (Value : Unsigned_32) return Natural is
      V : Unsigned_32 := Value;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 32;
      end if;
      while V < 16#80000000# loop
         Count := Count + 1;
         V := V * 2;
      end loop;
      return Count;
   end Leading_Zeros;

   --------------------
   -- Trailing_Zeros --
   --------------------

   function Trailing_Zeros (Value : Unsigned_8) return Natural is
      V : Unsigned_8 := Value;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 8;
      end if;
      while (V and 1) = 0 loop
         Count := Count + 1;
         V := V / 2;
      end loop;
      return Count;
   end Trailing_Zeros;

   function Trailing_Zeros (Value : Unsigned_16) return Natural is
      V : Unsigned_16 := Value;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 16;
      end if;
      while (V and 1) = 0 loop
         Count := Count + 1;
         V := V / 2;
      end loop;
      return Count;
   end Trailing_Zeros;

   function Trailing_Zeros (Value : Unsigned_32) return Natural is
      V : Unsigned_32 := Value;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 32;
      end if;
      while (V and 1) = 0 loop
         Count := Count + 1;
         V := V / 2;
      end loop;
      return Count;
   end Trailing_Zeros;

   -----------------
   -- Bit_Reverse --
   -----------------

   function Bit_Reverse (Value : Unsigned_8) return Unsigned_8 is
      V : Unsigned_8 := Value;
      R : Unsigned_8 := 0;
   begin
      for I in 1 .. 8 loop
         R := R * 2 + (V and 1);
         V := V / 2;
      end loop;
      return R;
   end Bit_Reverse;

   function Bit_Reverse (Value : Unsigned_16) return Unsigned_16 is
      V : Unsigned_16 := Value;
      R : Unsigned_16 := 0;
   begin
      for I in 1 .. 16 loop
         R := R * 2 + (V and 1);
         V := V / 2;
      end loop;
      return R;
   end Bit_Reverse;

   ---------------
   -- Byte_Swap --
   ---------------

   function Byte_Swap (Value : Unsigned_16) return Unsigned_16 is
   begin
      return (Value / 256) + (Value mod 256) * 256;
   end Byte_Swap;

   function Byte_Swap (Value : Unsigned_32) return Unsigned_32 is
      B0 : constant Unsigned_32 := Value mod 256;
      B1 : constant Unsigned_32 := (Value / 256) mod 256;
      B2 : constant Unsigned_32 := (Value / 65536) mod 256;
      B3 : constant Unsigned_32 := Value / 16777216;
   begin
      return B0 * 16777216 + B1 * 65536 + B2 * 256 + B3;
   end Byte_Swap;

   -----------------
   -- Rotate_Left --
   -----------------

   function Rotate_Left
     (Value : Unsigned_8;
      Amount : Natural) return Unsigned_8
   is
      A : constant Natural := Amount mod 8;
   begin
      return (Value * (2 ** A)) or (Value / (2 ** (8 - A)));
   end Rotate_Left;

   function Rotate_Left
     (Value : Unsigned_16;
      Amount : Natural) return Unsigned_16
   is
      A : constant Natural := Amount mod 16;
   begin
      return (Value * (2 ** A)) or (Value / (2 ** (16 - A)));
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   function Rotate_Right
     (Value : Unsigned_8;
      Amount : Natural) return Unsigned_8
   is
      A : constant Natural := Amount mod 8;
   begin
      return (Value / (2 ** A)) or (Value * (2 ** (8 - A)));
   end Rotate_Right;

   function Rotate_Right
     (Value : Unsigned_16;
      Amount : Natural) return Unsigned_16
   is
      A : constant Natural := Amount mod 16;
   begin
      return (Value / (2 ** A)) or (Value * (2 ** (16 - A)));
   end Rotate_Right;

end System.Bit_Ops;
