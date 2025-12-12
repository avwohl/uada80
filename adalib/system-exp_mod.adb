-- System.Exp_Mod body for Z80
-- Modular exponentiation implementation

package body System.Exp_Mod is

   use Interfaces;

   -----------------
   -- Exp_Modular --
   -----------------

   function Exp_Modular
     (Left    : Unsigned_32;
      Right   : Natural;
      Modulus : Unsigned_32) return Unsigned_32
   is
      Result : Unsigned_32 := 1;
      Base   : Unsigned_32 := Left mod Modulus;
      Exp    : Natural := Right;
   begin
      if Modulus = 0 then
         raise Constraint_Error;
      end if;

      if Modulus = 1 then
         return 0;
      end if;

      -- Binary exponentiation with modular reduction
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            -- Use 64-bit intermediate to avoid overflow
            Result := Unsigned_32 (
              (Unsigned_64 (Result) * Unsigned_64 (Base)) mod Unsigned_64 (Modulus));
         end if;
         Base := Unsigned_32 (
           (Unsigned_64 (Base) * Unsigned_64 (Base)) mod Unsigned_64 (Modulus));
         Exp := Exp / 2;
      end loop;

      return Result;
   end Exp_Modular;

   --------------------
   -- Exp_Modular_16 --
   --------------------

   function Exp_Modular_16
     (Left    : Unsigned_16;
      Right   : Natural;
      Modulus : Unsigned_16) return Unsigned_16
   is
      Result : Unsigned_16 := 1;
      Base   : Unsigned_16 := Left mod Modulus;
      Exp    : Natural := Right;
   begin
      if Modulus = 0 then
         raise Constraint_Error;
      end if;

      if Modulus = 1 then
         return 0;
      end if;

      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Unsigned_16 (
              (Unsigned_32 (Result) * Unsigned_32 (Base)) mod Unsigned_32 (Modulus));
         end if;
         Base := Unsigned_16 (
           (Unsigned_32 (Base) * Unsigned_32 (Base)) mod Unsigned_32 (Modulus));
         Exp := Exp / 2;
      end loop;

      return Result;
   end Exp_Modular_16;

   -------------------
   -- Exp_Modular_8 --
   -------------------

   function Exp_Modular_8
     (Left    : Unsigned_8;
      Right   : Natural;
      Modulus : Unsigned_8) return Unsigned_8
   is
      Result : Unsigned_8 := 1;
      Base   : Unsigned_8 := Left mod Modulus;
      Exp    : Natural := Right;
   begin
      if Modulus = 0 then
         raise Constraint_Error;
      end if;

      if Modulus = 1 then
         return 0;
      end if;

      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Unsigned_8 (
              (Unsigned_16 (Result) * Unsigned_16 (Base)) mod Unsigned_16 (Modulus));
         end if;
         Base := Unsigned_8 (
           (Unsigned_16 (Base) * Unsigned_16 (Base)) mod Unsigned_16 (Modulus));
         Exp := Exp / 2;
      end loop;

      return Result;
   end Exp_Modular_8;

end System.Exp_Mod;
