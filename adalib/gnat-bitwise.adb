-- GNAT.Bitwise body for Z80
-- Bitwise operations implementation

package body GNAT.Bitwise is

   use System.Unsigned_Types;

   ----------------
   -- Count_Ones --
   ----------------

   function Count_Ones (Value : Unsigned) return Natural is
      V      : Unsigned := Value;
      Result : Natural := 0;
   begin
      while V /= 0 loop
         if (V and 1) /= 0 then
            Result := Result + 1;
         end if;
         V := V / 2;
      end loop;
      return Result;
   end Count_Ones;

   -----------------
   -- Count_Zeros --
   -----------------

   function Count_Zeros (Value : Unsigned) return Natural is
   begin
      return 16 - Count_Ones (Value);  -- 16 bits for Z80
   end Count_Zeros;

   -------------------
   -- Leading_Zeros --
   -------------------

   function Leading_Zeros (Value : Unsigned) return Natural is
      V      : Unsigned := Value;
      Result : Natural := 0;
   begin
      if V = 0 then
         return 16;
      end if;

      if (V and 16#FF00#) = 0 then
         Result := Result + 8;
         V := V * 256;
      end if;
      if (V and 16#F000#) = 0 then
         Result := Result + 4;
         V := V * 16;
      end if;
      if (V and 16#C000#) = 0 then
         Result := Result + 2;
         V := V * 4;
      end if;
      if (V and 16#8000#) = 0 then
         Result := Result + 1;
      end if;

      return Result;
   end Leading_Zeros;

   --------------------
   -- Trailing_Zeros --
   --------------------

   function Trailing_Zeros (Value : Unsigned) return Natural is
      V      : Unsigned := Value;
      Result : Natural := 0;
   begin
      if V = 0 then
         return 16;
      end if;

      if (V and 16#00FF#) = 0 then
         Result := Result + 8;
         V := V / 256;
      end if;
      if (V and 16#000F#) = 0 then
         Result := Result + 4;
         V := V / 16;
      end if;
      if (V and 16#0003#) = 0 then
         Result := Result + 2;
         V := V / 4;
      end if;
      if (V and 16#0001#) = 0 then
         Result := Result + 1;
      end if;

      return Result;
   end Trailing_Zeros;

   ------------------
   -- Leading_Ones --
   ------------------

   function Leading_Ones (Value : Unsigned) return Natural is
   begin
      return Leading_Zeros (not Value);
   end Leading_Ones;

   -------------------
   -- Trailing_Ones --
   -------------------

   function Trailing_Ones (Value : Unsigned) return Natural is
   begin
      return Trailing_Zeros (not Value);
   end Trailing_Ones;

   -----------------
   -- Rotate_Left --
   -----------------

   function Rotate_Left (Value : Unsigned; Amount : Natural) return Unsigned is
      A : constant Natural := Amount mod 16;
   begin
      return (Value * (2 ** A)) or (Value / (2 ** (16 - A)));
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   function Rotate_Right (Value : Unsigned; Amount : Natural) return Unsigned is
      A : constant Natural := Amount mod 16;
   begin
      return (Value / (2 ** A)) or (Value * (2 ** (16 - A)));
   end Rotate_Right;

   ------------------
   -- Reverse_Bits --
   ------------------

   function Reverse_Bits (Value : Unsigned) return Unsigned is
      V      : Unsigned := Value;
      Result : Unsigned := 0;
   begin
      for I in 1 .. 16 loop
         Result := Result * 2;
         if (V and 1) /= 0 then
            Result := Result or 1;
         end if;
         V := V / 2;
      end loop;
      return Result;
   end Reverse_Bits;

   ---------------------
   -- Is_Power_Of_Two --
   ---------------------

   function Is_Power_Of_Two (Value : Unsigned) return Boolean is
   begin
      return Value /= 0 and then (Value and (Value - 1)) = 0;
   end Is_Power_Of_Two;

   -----------------------
   -- Next_Power_Of_Two --
   -----------------------

   function Next_Power_Of_Two (Value : Unsigned) return Unsigned is
      V : Unsigned := Value;
   begin
      if V = 0 then
         return 1;
      end if;
      V := V - 1;
      V := V or (V / 2);
      V := V or (V / 4);
      V := V or (V / 16);
      V := V or (V / 256);
      return V + 1;
   end Next_Power_Of_Two;

end GNAT.Bitwise;
