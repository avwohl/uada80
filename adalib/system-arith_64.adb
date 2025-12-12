-- System.Arith_64 body for Z80
-- 64-bit arithmetic operations implementation

package body System.Arith_64 is

   ----------------------------
   -- Add_With_Ovflo_Check --
   ----------------------------

   function Add_With_Ovflo_Check (X, Y : Int64) return Int64 is
      Result : Int64;
   begin
      -- Check for overflow
      if Y > 0 and X > Int64'Last - Y then
         raise Constraint_Error;
      elsif Y < 0 and X < Int64'First - Y then
         raise Constraint_Error;
      end if;
      return X + Y;
   end Add_With_Ovflo_Check;

   ---------------------------------
   -- Subtract_With_Ovflo_Check --
   ---------------------------------

   function Subtract_With_Ovflo_Check (X, Y : Int64) return Int64 is
   begin
      -- Check for overflow
      if Y < 0 and X > Int64'Last + Y then
         raise Constraint_Error;
      elsif Y > 0 and X < Int64'First + Y then
         raise Constraint_Error;
      end if;
      return X - Y;
   end Subtract_With_Ovflo_Check;

   ---------------------------------
   -- Multiply_With_Ovflo_Check --
   ---------------------------------

   function Multiply_With_Ovflo_Check (X, Y : Int64) return Int64 is
   begin
      -- Simplified overflow check
      if X /= 0 and Y /= 0 then
         if abs X > Int64'Last / abs Y then
            raise Constraint_Error;
         end if;
      end if;
      return X * Y;
   end Multiply_With_Ovflo_Check;

   --------------------
   -- Double_Divide --
   --------------------

   procedure Double_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64)
   is
      -- Simplified: compute (X * Y) / Z and mod
      Product : Int64;
   begin
      -- Note: This may overflow for large values
      -- A full implementation would use double-precision
      Product := X * Y;
      Q := Product / Z;
      R := Product mod Z;
   end Double_Divide;

   -------------------
   -- Scaled_Divide --
   -------------------

   procedure Scaled_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean)
   is
      Product : Int64;
      Quot    : Int64;
      Rem     : Int64;
   begin
      Product := X * Y;
      Quot := Product / Z;
      Rem := Product mod Z;

      if Round then
         -- Round to nearest
         if abs Rem >= abs (Z / 2) then
            if (Product >= 0) = (Z >= 0) then
               Quot := Quot + 1;
            else
               Quot := Quot - 1;
            end if;
            Rem := Product - Quot * Z;
         end if;
      end if;

      Q := Quot;
      R := Rem;
   end Scaled_Divide;

end System.Arith_64;
