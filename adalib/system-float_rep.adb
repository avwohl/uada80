-- System.Float_Rep body for Z80
-- Floating-point representation utilities

package body System.Float_Rep is

   ---------------
   -- Decompose --
   ---------------

   function Decompose (F : Float) return Float_Components is
      Result : Float_Components;
      V      : Float := abs F;
      E      : Integer := 0;
   begin
      Result.Sign := F < 0.0;

      if V = 0.0 then
         Result.Exponent := 0;
         Result.Mantissa := 0;
         return Result;
      end if;

      -- Normalize
      while V >= 2.0 loop
         V := V / 2.0;
         E := E + 1;
      end loop;

      while V < 1.0 loop
         V := V * 2.0;
         E := E - 1;
      end loop;

      Result.Exponent := E;
      -- Convert mantissa to integer (24-bit precision)
      Result.Mantissa := Unsigned_32 (V * 16777216.0);

      return Result;
   end Decompose;

   -------------
   -- Compose --
   -------------

   function Compose (C : Float_Components) return Float is
      Result : Float;
   begin
      if C.Mantissa = 0 then
         return 0.0;
      end if;

      -- Convert mantissa back to float
      Result := Float (C.Mantissa) / 16777216.0;

      -- Apply exponent
      if C.Exponent > 0 then
         for I in 1 .. C.Exponent loop
            Result := Result * 2.0;
         end loop;
      elsif C.Exponent < 0 then
         for I in 1 .. (-C.Exponent) loop
            Result := Result / 2.0;
         end loop;
      end if;

      if C.Sign then
         return -Result;
      else
         return Result;
      end if;
   end Compose;

   --------------
   -- Sign_Bit --
   --------------

   function Sign_Bit (F : Float) return Boolean is
   begin
      return F < 0.0;
   end Sign_Bit;

   ------------------
   -- Get_Exponent --
   ------------------

   function Get_Exponent (F : Float) return Integer is
      C : constant Float_Components := Decompose (F);
   begin
      return C.Exponent;
   end Get_Exponent;

   ------------------
   -- Get_Mantissa --
   ------------------

   function Get_Mantissa (F : Float) return Unsigned_32 is
      C : constant Float_Components := Decompose (F);
   begin
      return C.Mantissa;
   end Get_Mantissa;

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero (F : Float) return Boolean is
   begin
      return F = 0.0;
   end Is_Zero;

   -----------------
   -- Is_Infinite --
   -----------------

   function Is_Infinite (F : Float) return Boolean is
      pragma Unreferenced (F);
   begin
      -- Software float on Z80 doesn't support infinity
      return False;
   end Is_Infinite;

   ------------
   -- Is_NaN --
   ------------

   function Is_NaN (F : Float) return Boolean is
      pragma Unreferenced (F);
   begin
      -- Software float on Z80 doesn't support NaN
      return False;
   end Is_NaN;

end System.Float_Rep;
