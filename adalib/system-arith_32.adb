-- System.Arith_32 body for Z80
-- 32-bit arithmetic operations implementation

package body System.Arith_32 is

   ----------------------------
   -- Add_With_Ovflo_Check --
   ----------------------------

   function Add_With_Ovflo_Check (X, Y : Int32) return Int32 is
   begin
      if Y > 0 and X > Int32'Last - Y then
         raise Constraint_Error;
      elsif Y < 0 and X < Int32'First - Y then
         raise Constraint_Error;
      end if;
      return X + Y;
   end Add_With_Ovflo_Check;

   ---------------------------------
   -- Subtract_With_Ovflo_Check --
   ---------------------------------

   function Subtract_With_Ovflo_Check (X, Y : Int32) return Int32 is
   begin
      if Y < 0 and X > Int32'Last + Y then
         raise Constraint_Error;
      elsif Y > 0 and X < Int32'First + Y then
         raise Constraint_Error;
      end if;
      return X - Y;
   end Subtract_With_Ovflo_Check;

   ---------------------------------
   -- Multiply_With_Ovflo_Check --
   ---------------------------------

   function Multiply_With_Ovflo_Check (X, Y : Int32) return Int32 is
   begin
      if X /= 0 and Y /= 0 then
         if abs X > Int32'Last / abs Y then
            raise Constraint_Error;
         end if;
      end if;
      return X * Y;
   end Multiply_With_Ovflo_Check;

   ------------
   -- Divide --
   ------------

   procedure Divide
     (X, Y : Int32;
      Q, R : out Int32)
   is
   begin
      if Y = 0 then
         raise Constraint_Error;
      end if;
      Q := X / Y;
      R := X rem Y;
   end Divide;

   -------------------
   -- Scaled_Divide --
   -------------------

   procedure Scaled_Divide
     (X, Y, Z : Int32;
      Q, R    : out Int32;
      Round   : Boolean)
   is
      Product : Long_Long_Integer;
      Quot    : Long_Long_Integer;
      Rem     : Long_Long_Integer;
   begin
      if Z = 0 then
         raise Constraint_Error;
      end if;

      -- Use 64-bit for intermediate calculation
      Product := Long_Long_Integer (X) * Long_Long_Integer (Y);
      Quot := Product / Long_Long_Integer (Z);
      Rem := Product rem Long_Long_Integer (Z);

      if Round then
         if abs Rem >= abs (Long_Long_Integer (Z) / 2) then
            if (Product >= 0) = (Z >= 0) then
               Quot := Quot + 1;
            else
               Quot := Quot - 1;
            end if;
            Rem := Product - Quot * Long_Long_Integer (Z);
         end if;
      end if;

      -- Check for overflow
      if Quot > Long_Long_Integer (Int32'Last) or
         Quot < Long_Long_Integer (Int32'First)
      then
         raise Constraint_Error;
      end if;

      Q := Int32 (Quot);
      R := Int32 (Rem);
   end Scaled_Divide;

   -------------------
   -- Double_Divide --
   -------------------

   procedure Double_Divide
     (X, Y, Z : Int32;
      Q, R    : out Int32)
   is
      Product : Long_Long_Integer;
   begin
      if Z = 0 then
         raise Constraint_Error;
      end if;

      Product := Long_Long_Integer (X) * Long_Long_Integer (Y);
      Q := Int32 (Product / Long_Long_Integer (Z));
      R := Int32 (Product rem Long_Long_Integer (Z));
   end Double_Divide;

end System.Arith_32;
