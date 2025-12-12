-- GNAT.Bit_Vector body for Z80
-- Dynamic bit vector implementation

package body GNAT.Bit_Vector is

   ------------
   -- Create --
   ------------

   function Create (Size : Positive) return Bit_Vector is
      V : Bit_Vector;
   begin
      V.Bits := Natural'Min (Size, Max_Bits);
      V.Data := (others => 0);
      return V;
   end Create;

   ----------
   -- Size --
   ----------

   function Size (V : Bit_Vector) return Natural is
   begin
      return V.Bits;
   end Size;

   ---------
   -- Get --
   ---------

   function Get (V : Bit_Vector; Index : Natural) return Boolean is
   begin
      if Index >= V.Bits then
         return False;
      end if;
      return (V.Data (Index / 8 + 1) / (2 ** (Index mod 8))) mod 2 = 1;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (V : in Out Bit_Vector; Index : Natural; Value : Boolean := True) is
      Byte_Idx : constant Positive := Index / 8 + 1;
      Mask     : constant Natural := 2 ** (Index mod 8);
   begin
      if Index >= V.Bits then
         return;
      end if;
      if Value then
         V.Data (Byte_Idx) := V.Data (Byte_Idx) or Mask;
      else
         V.Data (Byte_Idx) := V.Data (Byte_Idx) and (255 - Mask);
      end if;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (V : in Out Bit_Vector; Index : Natural) is
   begin
      Set (V, Index, False);
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (V : in Out Bit_Vector; Index : Natural) is
   begin
      Set (V, Index, not Get (V, Index));
   end Toggle;

   -------------
   -- Set_All --
   -------------

   procedure Set_All (V : out Bit_Vector) is
      Full_Bytes : constant Natural := V.Bits / 8;
      Extra_Bits : constant Natural := V.Bits mod 8;
   begin
      for I in 1 .. Full_Bytes loop
         V.Data (I) := 255;
      end loop;
      if Extra_Bits > 0 then
         V.Data (Full_Bytes + 1) := (2 ** Extra_Bits) - 1;
      end if;
   end Set_All;

   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All (V : out Bit_Vector) is
   begin
      V.Data := (others => 0);
   end Clear_All;

   ----------
   -- Fill --
   ----------

   procedure Fill (V : out Bit_Vector; Value : Boolean) is
   begin
      if Value then
         Set_All (V);
      else
         Clear_All (V);
      end if;
   end Fill;

   -----------
   -- Count --
   -----------

   function Count (V : Bit_Vector) return Natural is
      Result : Natural := 0;
      B      : Natural;
   begin
      for I in 1 .. (V.Bits + 7) / 8 loop
         B := V.Data (I);
         while B > 0 loop
            Result := Result + (B mod 2);
            B := B / 2;
         end loop;
      end loop;
      return Result;
   end Count;

   ---------
   -- Any --
   ---------

   function Any (V : Bit_Vector) return Boolean is
   begin
      for I in 1 .. (V.Bits + 7) / 8 loop
         if V.Data (I) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Any;

   -------------
   -- All_Set --
   -------------

   function All_Set (V : Bit_Vector) return Boolean is
   begin
      return Count (V) = V.Bits;
   end All_Set;

   ----------
   -- None --
   ----------

   function None (V : Bit_Vector) return Boolean is
   begin
      return not Any (V);
   end None;

   ---------------
   -- First_Set --
   ---------------

   function First_Set (V : Bit_Vector) return Integer is
   begin
      for I in 0 .. V.Bits - 1 loop
         if Get (V, I) then
            return I;
         end if;
      end loop;
      return -1;
   end First_Set;

   --------------
   -- Last_Set --
   --------------

   function Last_Set (V : Bit_Vector) return Integer is
   begin
      for I in reverse 0 .. V.Bits - 1 loop
         if Get (V, I) then
            return I;
         end if;
      end loop;
      return -1;
   end Last_Set;

   -----------------
   -- First_Clear --
   -----------------

   function First_Clear (V : Bit_Vector) return Integer is
   begin
      for I in 0 .. V.Bits - 1 loop
         if not Get (V, I) then
            return I;
         end if;
      end loop;
      return -1;
   end First_Clear;

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Bit_Vector) return Bit_Vector is
      Result : Bit_Vector := Left;
   begin
      for I in 1 .. Max_Bytes loop
         Result.Data (I) := Left.Data (I) and Right.Data (I);
      end loop;
      return Result;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Bit_Vector) return Bit_Vector is
      Result : Bit_Vector := Left;
   begin
      for I in 1 .. Max_Bytes loop
         Result.Data (I) := Left.Data (I) or Right.Data (I);
      end loop;
      return Result;
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Bit_Vector) return Bit_Vector is
      Result : Bit_Vector := Left;
   begin
      for I in 1 .. Max_Bytes loop
         Result.Data (I) := Left.Data (I) xor Right.Data (I);
      end loop;
      return Result;
   end "xor";

   -----------
   -- "not" --
   -----------

   function "not" (V : Bit_Vector) return Bit_Vector is
      Result     : Bit_Vector := V;
      Full_Bytes : constant Natural := V.Bits / 8;
      Extra_Bits : constant Natural := V.Bits mod 8;
   begin
      for I in 1 .. Full_Bytes loop
         Result.Data (I) := 255 - V.Data (I);
      end loop;
      if Extra_Bits > 0 then
         Result.Data (Full_Bytes + 1) :=
           ((2 ** Extra_Bits) - 1) - (V.Data (Full_Bytes + 1) mod (2 ** Extra_Bits));
      end if;
      return Result;
   end "not";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Bit_Vector) return Boolean is
   begin
      if Left.Bits /= Right.Bits then
         return False;
      end if;
      for I in 1 .. (Left.Bits + 7) / 8 loop
         if Left.Data (I) /= Right.Data (I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (V, Of_Set : Bit_Vector) return Boolean is
   begin
      return (V and Of_Set) = V;
   end Is_Subset;

   -----------------
   -- Is_Superset --
   -----------------

   function Is_Superset (V, Of_Set : Bit_Vector) return Boolean is
   begin
      return Is_Subset (Of_Set, V);
   end Is_Superset;

   ----------------
   -- Intersects --
   ----------------

   function Intersects (Left, Right : Bit_Vector) return Boolean is
   begin
      return Any (Left and Right);
   end Intersects;

   ----------------
   -- Shift_Left --
   ----------------

   procedure Shift_Left (V : in Out Bit_Vector; Count : Natural) is
      Temp : Bit_Vector := Create (V.Bits);
   begin
      if Count >= V.Bits then
         Clear_All (V);
         return;
      end if;
      for I in 0 .. V.Bits - Count - 1 loop
         Set (Temp, I + Count, Get (V, I));
      end loop;
      V := Temp;
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   procedure Shift_Right (V : in Out Bit_Vector; Count : Natural) is
      Temp : Bit_Vector := Create (V.Bits);
   begin
      if Count >= V.Bits then
         Clear_All (V);
         return;
      end if;
      for I in Count .. V.Bits - 1 loop
         Set (Temp, I - Count, Get (V, I));
      end loop;
      V := Temp;
   end Shift_Right;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left (V : in Out Bit_Vector; Count : Natural) is
      Actual : constant Natural := Count mod V.Bits;
      Temp   : Bit_Vector := Create (V.Bits);
   begin
      if V.Bits = 0 or Actual = 0 then
         return;
      end if;
      for I in 0 .. V.Bits - 1 loop
         Set (Temp, (I + Actual) mod V.Bits, Get (V, I));
      end loop;
      V := Temp;
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   procedure Rotate_Right (V : in Out Bit_Vector; Count : Natural) is
      Actual : constant Natural := Count mod V.Bits;
   begin
      if V.Bits = 0 or Actual = 0 then
         return;
      end if;
      Rotate_Left (V, V.Bits - Actual);
   end Rotate_Right;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range (V : in Out Bit_Vector; Low, High : Natural) is
   begin
      for I in Low .. Natural'Min (High, V.Bits - 1) loop
         Set (V, I, True);
      end loop;
   end Set_Range;

   -----------------
   -- Clear_Range --
   -----------------

   procedure Clear_Range (V : in Out Bit_Vector; Low, High : Natural) is
   begin
      for I in Low .. Natural'Min (High, V.Bits - 1) loop
         Set (V, I, False);
      end loop;
   end Clear_Range;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (V : Bit_Vector) return Natural is
      Result : Natural := 0;
   begin
      -- Convert up to 16 bits for Z80
      for I in 0 .. Natural'Min (V.Bits, 16) - 1 loop
         if Get (V, I) then
            Result := Result + (2 ** I);
         end if;
      end loop;
      return Result;
   end To_Natural;

   ------------------
   -- From_Natural --
   ------------------

   function From_Natural (Value : Natural; Size : Positive) return Bit_Vector is
      V : Bit_Vector := Create (Size);
      N : Natural := Value;
      I : Natural := 0;
   begin
      while N > 0 and I < V.Bits loop
         if N mod 2 = 1 then
            Set (V, I, True);
         end if;
         N := N / 2;
         I := I + 1;
      end loop;
      return V;
   end From_Natural;

end GNAT.Bit_Vector;
