-- System.Packed_Arrays body for Z80
-- Support for packed boolean arrays and bit manipulation

package body System.Packed_Arrays is

   ------------
   -- Create --
   ------------

   function Create (Size : Positive) return Bit_Array is
      A : Bit_Array;
   begin
      A.Bits := Natural'Min (Size, Max_Bits);
      A.Data := (others => 0);
      return A;
   end Create;

   ----------
   -- Size --
   ----------

   function Size (A : Bit_Array) return Natural is
   begin
      return A.Bits;
   end Size;

   ---------
   -- Get --
   ---------

   function Get (A : Bit_Array; Index : Natural) return Boolean is
      Byte_Idx : Positive;
      Bit_Pos  : Natural;
   begin
      if Index >= A.Bits then
         return False;
      end if;

      Byte_Idx := Index / 8 + 1;
      Bit_Pos := Index mod 8;

      return (A.Data (Byte_Idx) / (2 ** Bit_Pos)) mod 2 = 1;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (A : in Out Bit_Array; Index : Natural; Value : Boolean) is
      Byte_Idx : Positive;
      Bit_Pos  : Natural;
      Mask     : Natural;
   begin
      if Index >= A.Bits then
         return;
      end if;

      Byte_Idx := Index / 8 + 1;
      Bit_Pos := Index mod 8;
      Mask := 2 ** Bit_Pos;

      if Value then
         A.Data (Byte_Idx) := A.Data (Byte_Idx) or Mask;
      else
         A.Data (Byte_Idx) := A.Data (Byte_Idx) and (255 - Mask);
      end if;
   end Set;

   -------------
   -- Set_All --
   -------------

   procedure Set_All (A : out Bit_Array; Value : Boolean) is
      Full_Bytes : constant Natural := A.Bits / 8;
      Extra_Bits : constant Natural := A.Bits mod 8;
   begin
      if Value then
         for I in 1 .. Full_Bytes loop
            A.Data (I) := 255;
         end loop;
         if Extra_Bits > 0 then
            A.Data (Full_Bytes + 1) := (2 ** Extra_Bits) - 1;
         end if;
      else
         A.Data := (others => 0);
      end if;
   end Set_All;

   -----------
   -- Clear --
   -----------

   procedure Clear (A : out Bit_Array) is
   begin
      A.Data := (others => 0);
   end Clear;

   ----------------
   -- Count_True --
   ----------------

   function Count_True (A : Bit_Array) return Natural is
      Count : Natural := 0;
      B     : Natural;
   begin
      for I in 1 .. (A.Bits + 7) / 8 loop
         B := A.Data (I);
         while B > 0 loop
            Count := Count + (B mod 2);
            B := B / 2;
         end loop;
      end loop;
      return Count;
   end Count_True;

   -----------------
   -- Count_False --
   -----------------

   function Count_False (A : Bit_Array) return Natural is
   begin
      return A.Bits - Count_True (A);
   end Count_False;

   ----------------
   -- First_True --
   ----------------

   function First_True (A : Bit_Array) return Integer is
   begin
      for I in 0 .. A.Bits - 1 loop
         if Get (A, I) then
            return I;
         end if;
      end loop;
      return -1;
   end First_True;

   -----------------
   -- First_False --
   -----------------

   function First_False (A : Bit_Array) return Integer is
   begin
      for I in 0 .. A.Bits - 1 loop
         if not Get (A, I) then
            return I;
         end if;
      end loop;
      return -1;
   end First_False;

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Bit_Array) return Bit_Array is
      Result : Bit_Array := Left;
   begin
      for I in 1 .. Max_Bytes loop
         Result.Data (I) := Left.Data (I) and Right.Data (I);
      end loop;
      return Result;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Bit_Array) return Bit_Array is
      Result : Bit_Array := Left;
   begin
      for I in 1 .. Max_Bytes loop
         Result.Data (I) := Left.Data (I) or Right.Data (I);
      end loop;
      return Result;
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Bit_Array) return Bit_Array is
      Result : Bit_Array := Left;
   begin
      for I in 1 .. Max_Bytes loop
         Result.Data (I) := Left.Data (I) xor Right.Data (I);
      end loop;
      return Result;
   end "xor";

   -----------
   -- "not" --
   -----------

   function "not" (A : Bit_Array) return Bit_Array is
      Result     : Bit_Array := A;
      Full_Bytes : constant Natural := A.Bits / 8;
      Extra_Bits : constant Natural := A.Bits mod 8;
   begin
      for I in 1 .. Full_Bytes loop
         Result.Data (I) := 255 - A.Data (I);
      end loop;
      if Extra_Bits > 0 then
         Result.Data (Full_Bytes + 1) :=
           ((2 ** Extra_Bits) - 1) - (A.Data (Full_Bytes + 1) mod (2 ** Extra_Bits));
      end if;
      return Result;
   end "not";

   ----------------
   -- Shift_Left --
   ----------------

   function Shift_Left (A : Bit_Array; Count : Natural) return Bit_Array is
      Result : Bit_Array := Create (A.Bits);
   begin
      if Count >= A.Bits then
         return Result;  -- All zeros
      end if;

      for I in 0 .. A.Bits - Count - 1 loop
         Set (Result, I + Count, Get (A, I));
      end loop;
      return Result;
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   function Shift_Right (A : Bit_Array; Count : Natural) return Bit_Array is
      Result : Bit_Array := Create (A.Bits);
   begin
      if Count >= A.Bits then
         return Result;  -- All zeros
      end if;

      for I in Count .. A.Bits - 1 loop
         Set (Result, I - Count, Get (A, I));
      end loop;
      return Result;
   end Shift_Right;

   ----------
   -- Pack --
   ----------

   procedure Pack (A : Bit_Array; Data : out Byte_Array; Last : out Natural) is
      Bytes_Needed : constant Natural := (A.Bits + 7) / 8;
   begin
      Last := Data'First - 1;
      for I in 1 .. Natural'Min (Bytes_Needed, Data'Length) loop
         Last := Data'First + I - 1;
         Data (Last) := A.Data (I);
      end loop;
   end Pack;

   ------------
   -- Unpack --
   ------------

   procedure Unpack (Data : Byte_Array; A : out Bit_Array) is
   begin
      A.Bits := Data'Length * 8;
      if A.Bits > Max_Bits then
         A.Bits := Max_Bits;
      end if;
      A.Data := (others => 0);

      for I in Data'Range loop
         exit when I - Data'First + 1 > Max_Bytes;
         A.Data (I - Data'First + 1) := Data (I);
      end loop;
   end Unpack;

end System.Packed_Arrays;
