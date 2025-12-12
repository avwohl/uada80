-- System.Bignums body for Z80
-- Arbitrary precision integer arithmetic

package body System.Bignums is

   ----------------
   -- To_Bignum --
   ----------------

   function To_Bignum (Value : Integer) return Bignum is
      Result : Bignum (2);
      V      : Integer := Value;
   begin
      if Value < 0 then
         Result.Sign := True;
         V := -Value;
      else
         Result.Sign := False;
      end if;

      Result.Data (0) := Bignum_Word (V mod 65536);
      Result.Data (1) := Bignum_Word (V / 65536);
      return Result;
   end To_Bignum;

   function To_Bignum (Value : Long_Long_Integer) return Bignum is
      Result : Bignum (4);
      V      : Long_Long_Integer := Value;
   begin
      if Value < 0 then
         Result.Sign := True;
         V := -Value;
      else
         Result.Sign := False;
      end if;

      for I in Result.Data'Range loop
         Result.Data (I) := Bignum_Word (V mod 65536);
         V := V / 65536;
      end loop;
      return Result;
   end To_Bignum;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Bignum) return Integer is
      Result : Integer := 0;
   begin
      if Value.Length >= 1 then
         Result := Integer (Value.Data (0));
      end if;
      if Value.Length >= 2 then
         Result := Result + Integer (Value.Data (1)) * 65536;
      end if;

      if Value.Sign then
         Result := -Result;
      end if;
      return Result;
   end To_Integer;

   ---------------------------
   -- To_Long_Long_Integer --
   ---------------------------

   function To_Long_Long_Integer (Value : Bignum) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
      Factor : Long_Long_Integer := 1;
   begin
      for I in Value.Data'Range loop
         Result := Result + Long_Long_Integer (Value.Data (I)) * Factor;
         Factor := Factor * 65536;
      end loop;

      if Value.Sign then
         Result := -Result;
      end if;
      return Result;
   end To_Long_Long_Integer;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Bignum) return Bignum is
      Max_Len : constant Natural := Natural'Max (Left.Length, Right.Length);
      Result  : Bignum (Max_Len + 1);
      Carry   : Bignum_Word := 0;
      L, R    : Bignum_Word;
      Sum     : Natural;
   begin
      if Left.Sign /= Right.Sign then
         -- Different signs: subtract
         if Left.Sign then
            return Right - Abs_Value (Left);
         else
            return Left - Abs_Value (Right);
         end if;
      end if;

      Result.Sign := Left.Sign;

      for I in 0 .. Max_Len - 1 loop
         if I < Left.Length then
            L := Left.Data (I);
         else
            L := 0;
         end if;

         if I < Right.Length then
            R := Right.Data (I);
         else
            R := 0;
         end if;

         Sum := Natural (L) + Natural (R) + Natural (Carry);
         Result.Data (I) := Bignum_Word (Sum mod 65536);
         Carry := Bignum_Word (Sum / 65536);
      end loop;

      Result.Data (Max_Len) := Carry;
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Bignum) return Bignum is
      Result : Bignum := Right;
   begin
      Result.Sign := not Right.Sign;
      return Result;
   end "-";

   function "-" (Left, Right : Bignum) return Bignum is
   begin
      return Left + (-Right);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Bignum) return Bignum is
      Result : Bignum (Left.Length + Right.Length);
      Prod   : Natural;
      Carry  : Bignum_Word;
   begin
      Result.Sign := Left.Sign xor Right.Sign;

      for I in Result.Data'Range loop
         Result.Data (I) := 0;
      end loop;

      for I in Left.Data'Range loop
         Carry := 0;
         for J in Right.Data'Range loop
            Prod := Natural (Left.Data (I)) * Natural (Right.Data (J)) +
                    Natural (Result.Data (I + J)) + Natural (Carry);
            Result.Data (I + J) := Bignum_Word (Prod mod 65536);
            Carry := Bignum_Word (Prod / 65536);
         end loop;
         if I + Right.Length < Result.Length then
            Result.Data (I + Right.Length) := Carry;
         end if;
      end loop;

      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Bignum) return Bignum is
      -- Simplified: only handles single-word divisor
      Result : Bignum (Left.Length);
      Rem    : Natural := 0;
      Div    : constant Natural := Natural (Right.Data (0));
      Temp   : Natural;
   begin
      if Is_Zero (Right) then
         raise Constraint_Error;
      end if;

      Result.Sign := Left.Sign xor Right.Sign;

      for I in reverse Left.Data'Range loop
         Temp := Rem * 65536 + Natural (Left.Data (I));
         Result.Data (I) := Bignum_Word (Temp / Div);
         Rem := Temp mod Div;
      end loop;

      return Result;
   end "/";

   -----------
   -- "mod" --
   -----------

   function "mod" (Left, Right : Bignum) return Bignum is
   begin
      return Left - (Left / Right) * Right;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem" (Left, Right : Bignum) return Bignum is
   begin
      return Left - (Left / Right) * Right;
   end "rem";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Bignum) return Boolean is
   begin
      if Left.Sign /= Right.Sign then
         return False;
      end if;

      if Left.Length /= Right.Length then
         return False;  -- Simplified comparison
      end if;

      for I in Left.Data'Range loop
         if Left.Data (I) /= Right.Data (I) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Bignum) return Boolean is
   begin
      if Left.Sign and not Right.Sign then
         return True;
      elsif not Left.Sign and Right.Sign then
         return False;
      end if;

      -- Same sign
      for I in reverse Left.Data'Range loop
         if Left.Data (I) < Right.Data (I) then
            return not Left.Sign;
         elsif Left.Data (I) > Right.Data (I) then
            return Left.Sign;
         end if;
      end loop;

      return False;  -- Equal
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Bignum) return Boolean is
   begin
      return Left < Right or Left = Right;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Bignum) return Boolean is
   begin
      return Right < Left;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Bignum) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero (Value : Bignum) return Boolean is
   begin
      for I in Value.Data'Range loop
         if Value.Data (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero;

   ---------------
   -- Abs_Value --
   ---------------

   function Abs_Value (Value : Bignum) return Bignum is
      Result : Bignum := Value;
   begin
      Result.Sign := False;
      return Result;
   end Abs_Value;

end System.Bignums;
