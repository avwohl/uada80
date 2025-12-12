-- GNAT.Bit_Array body for Z80
-- Efficient bit array operations implementation

package body GNAT.Bit_Array is

   package body Fixed is

      -----------
      -- Clear --
      -----------

      procedure Clear (B : out Bit_Array) is
      begin
         B.Data := (others => 0);
      end Clear;

      -------------
      -- Set_All --
      -------------

      procedure Set_All (B : out Bit_Array) is
      begin
         B.Data := (others => 255);
      end Set_All;

      ---------
      -- Set --
      ---------

      procedure Set (B : in Out Bit_Array; Index : Positive) is
         Byte_Idx : constant Positive := (Index - 1) / 8 + 1;
         Bit_Idx  : constant Natural := (Index - 1) mod 8;
      begin
         if Index > Size then
            raise Constraint_Error;
         end if;
         B.Data (Byte_Idx) := B.Data (Byte_Idx) or (2 ** Bit_Idx);
      end Set;

      ---------------
      -- Clear_Bit --
      ---------------

      procedure Clear_Bit (B : in Out Bit_Array; Index : Positive) is
         Byte_Idx : constant Positive := (Index - 1) / 8 + 1;
         Bit_Idx  : constant Natural := (Index - 1) mod 8;
      begin
         if Index > Size then
            raise Constraint_Error;
         end if;
         B.Data (Byte_Idx) := B.Data (Byte_Idx) and not (2 ** Bit_Idx);
      end Clear_Bit;

      ------------
      -- Toggle --
      ------------

      procedure Toggle (B : in Out Bit_Array; Index : Positive) is
         Byte_Idx : constant Positive := (Index - 1) / 8 + 1;
         Bit_Idx  : constant Natural := (Index - 1) mod 8;
      begin
         if Index > Size then
            raise Constraint_Error;
         end if;
         B.Data (Byte_Idx) := B.Data (Byte_Idx) xor (2 ** Bit_Idx);
      end Toggle;

      ----------
      -- Test --
      ----------

      function Test (B : Bit_Array; Index : Positive) return Boolean is
         Byte_Idx : constant Positive := (Index - 1) / 8 + 1;
         Bit_Idx  : constant Natural := (Index - 1) mod 8;
      begin
         if Index > Size then
            raise Constraint_Error;
         end if;
         return (B.Data (Byte_Idx) and (2 ** Bit_Idx)) /= 0;
      end Test;

      ---------------
      -- Count_Set --
      ---------------

      function Count_Set (B : Bit_Array) return Natural is
         Result : Natural := 0;
         V      : Natural;
      begin
         for I in B.Data'Range loop
            V := B.Data (I);
            while V /= 0 loop
               Result := Result + (V and 1);
               V := V / 2;
            end loop;
         end loop;
         return Result;
      end Count_Set;

      -----------------
      -- Count_Clear --
      -----------------

      function Count_Clear (B : Bit_Array) return Natural is
      begin
         return Size - Count_Set (B);
      end Count_Clear;

      --------------------
      -- Find_First_Set --
      --------------------

      function Find_First_Set (B : Bit_Array) return Natural is
      begin
         for I in 1 .. Size loop
            if Test (B, I) then
               return I;
            end if;
         end loop;
         return 0;
      end Find_First_Set;

      ----------------------
      -- Find_First_Clear --
      ----------------------

      function Find_First_Clear (B : Bit_Array) return Natural is
      begin
         for I in 1 .. Size loop
            if not Test (B, I) then
               return I;
            end if;
         end loop;
         return 0;
      end Find_First_Clear;

      -----------
      -- "and" --
      -----------

      function "and" (Left, Right : Bit_Array) return Bit_Array is
         Result : Bit_Array;
      begin
         for I in Result.Data'Range loop
            Result.Data (I) := Left.Data (I) and Right.Data (I);
         end loop;
         return Result;
      end "and";

      ----------
      -- "or" --
      ----------

      function "or" (Left, Right : Bit_Array) return Bit_Array is
         Result : Bit_Array;
      begin
         for I in Result.Data'Range loop
            Result.Data (I) := Left.Data (I) or Right.Data (I);
         end loop;
         return Result;
      end "or";

      -----------
      -- "xor" --
      -----------

      function "xor" (Left, Right : Bit_Array) return Bit_Array is
         Result : Bit_Array;
      begin
         for I in Result.Data'Range loop
            Result.Data (I) := Left.Data (I) xor Right.Data (I);
         end loop;
         return Result;
      end "xor";

      -----------
      -- "not" --
      -----------

      function "not" (B : Bit_Array) return Bit_Array is
         Result : Bit_Array;
      begin
         for I in Result.Data'Range loop
            Result.Data (I) := 255 - B.Data (I);
         end loop;
         return Result;
      end "not";

   end Fixed;

end GNAT.Bit_Array;
