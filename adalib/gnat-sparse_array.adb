-- GNAT.Sparse_Array body for Z80
-- Sparse array implementation

package body GNAT.Sparse_Array is

   function Find_Entry (A : Sparse_Array; Index : Integer) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid and then A.Entries (I).Index = Index then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Entry;

   function Allocate_Entry (A : in Out Sparse_Array) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if not A.Entries (I).Valid then
            return I;
         end if;
      end loop;
      return 0;
   end Allocate_Entry;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (A : out Sparse_Array) is
   begin
      A.Entries := (others => (Index => 0, Value => 0, Valid => False));
      A.Count := 0;
   end Initialize;

   -----------
   -- Count --
   -----------

   function Count (A : Sparse_Array) return Natural is
   begin
      return A.Count;
   end Count;

   ---------
   -- Get --
   ---------

   function Get (A : Sparse_Array; Index : Integer) return Integer is
      Idx : constant Natural := Find_Entry (A, Index);
   begin
      if Idx = 0 then
         return 0;
      end if;
      return A.Entries (Idx).Value;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (A : in Out Sparse_Array; Index : Integer; Value : Integer) is
      Idx : Natural := Find_Entry (A, Index);
   begin
      if Value = 0 then
         -- Remove entry
         if Idx > 0 then
            A.Entries (Idx).Valid := False;
            A.Count := A.Count - 1;
         end if;
         return;
      end if;

      if Idx > 0 then
         -- Update existing
         A.Entries (Idx).Value := Value;
      else
         -- Create new
         Idx := Allocate_Entry (A);
         if Idx > 0 then
            A.Entries (Idx) := (Index => Index, Value => Value, Valid => True);
            A.Count := A.Count + 1;
         end if;
      end if;
   end Set;

   --------------
   -- Contains --
   --------------

   function Contains (A : Sparse_Array; Index : Integer) return Boolean is
   begin
      return Find_Entry (A, Index) > 0;
   end Contains;

   ------------
   -- Remove --
   ------------

   procedure Remove (A : in Out Sparse_Array; Index : Integer) is
      Idx : constant Natural := Find_Entry (A, Index);
   begin
      if Idx > 0 then
         A.Entries (Idx).Valid := False;
         A.Count := A.Count - 1;
      end if;
   end Remove;

   -----------
   -- Clear --
   -----------

   procedure Clear (A : out Sparse_Array) is
   begin
      Initialize (A);
   end Clear;

   ---------------
   -- Min_Index --
   ---------------

   function Min_Index (A : Sparse_Array) return Integer is
      Min : Integer := Integer'Last;
      Found : Boolean := False;
   begin
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            if not Found or A.Entries (I).Index < Min then
               Min := A.Entries (I).Index;
               Found := True;
            end if;
         end if;
      end loop;

      if Found then
         return Min;
      else
         return 0;
      end if;
   end Min_Index;

   ---------------
   -- Max_Index --
   ---------------

   function Max_Index (A : Sparse_Array) return Integer is
      Max : Integer := Integer'First;
      Found : Boolean := False;
   begin
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            if not Found or A.Entries (I).Index > Max then
               Max := A.Entries (I).Index;
               Found := True;
            end if;
         end if;
      end loop;

      if Found then
         return Max;
      else
         return 0;
      end if;
   end Max_Index;

   ---------
   -- Add --
   ---------

   function Add (A, B : Sparse_Array) return Sparse_Array is
      Result : Sparse_Array;
   begin
      Initialize (Result);

      -- Add all from A
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            Set (Result, A.Entries (I).Index, A.Entries (I).Value);
         end if;
      end loop;

      -- Add all from B
      for I in 1 .. Max_Entries loop
         if B.Entries (I).Valid then
            Set (Result, B.Entries (I).Index,
                 Get (Result, B.Entries (I).Index) + B.Entries (I).Value);
         end if;
      end loop;

      return Result;
   end Add;

   --------------
   -- Subtract --
   --------------

   function Subtract (A, B : Sparse_Array) return Sparse_Array is
      Result : Sparse_Array;
   begin
      Initialize (Result);

      -- Add all from A
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            Set (Result, A.Entries (I).Index, A.Entries (I).Value);
         end if;
      end loop;

      -- Subtract all from B
      for I in 1 .. Max_Entries loop
         if B.Entries (I).Valid then
            Set (Result, B.Entries (I).Index,
                 Get (Result, B.Entries (I).Index) - B.Entries (I).Value);
         end if;
      end loop;

      return Result;
   end Subtract;

   -----------
   -- Scale --
   -----------

   function Scale (A : Sparse_Array; S : Integer) return Sparse_Array is
      Result : Sparse_Array;
   begin
      Initialize (Result);

      if S = 0 then
         return Result;
      end if;

      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            Set (Result, A.Entries (I).Index, A.Entries (I).Value * S);
         end if;
      end loop;

      return Result;
   end Scale;

   -----------------
   -- Dot_Product --
   -----------------

   function Dot_Product (A, B : Sparse_Array) return Integer is
      Result : Integer := 0;
      B_Val  : Integer;
   begin
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            B_Val := Get (B, A.Entries (I).Index);
            if B_Val /= 0 then
               Result := Result + A.Entries (I).Value * B_Val;
            end if;
         end if;
      end loop;
      return Result;
   end Dot_Product;

   ---------
   -- Sum --
   ---------

   function Sum (A : Sparse_Array) return Integer is
      Result : Integer := 0;
   begin
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            Result := Result + A.Entries (I).Value;
         end if;
      end loop;
      return Result;
   end Sum;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (A : Sparse_Array; N : Positive) return Entry_Info is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Entries loop
         if A.Entries (I).Valid then
            Count := Count + 1;
            if Count = N then
               return (Index => A.Entries (I).Index,
                       Value => A.Entries (I).Value,
                       Valid => True);
            end if;
         end if;
      end loop;
      return (Index => 0, Value => 0, Valid => False);
   end Get_Entry;

end GNAT.Sparse_Array;
