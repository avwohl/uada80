-- GNAT.Max_Heap body for Z80
-- Binary max-heap implementation

package body GNAT.Max_Heap is

   procedure Sift_Up (H : in Out Heap; Idx : Positive) is
      I      : Positive := Idx;
      Parent : Positive;
      Temp   : Integer;
   begin
      while I > 1 loop
         Parent := I / 2;
         if H.Data (I) > H.Data (Parent) then
            Temp := H.Data (I);
            H.Data (I) := H.Data (Parent);
            H.Data (Parent) := Temp;
            I := Parent;
         else
            exit;
         end if;
      end loop;
   end Sift_Up;

   procedure Sift_Down (H : in Out Heap; Idx : Positive) is
      I       : Positive := Idx;
      Left    : Positive;
      Right   : Positive;
      Largest : Positive;
      Temp    : Integer;
   begin
      loop
         Left := I * 2;
         Right := I * 2 + 1;
         Largest := I;

         if Left <= H.Count and then H.Data (Left) > H.Data (Largest) then
            Largest := Left;
         end if;

         if Right <= H.Count and then H.Data (Right) > H.Data (Largest) then
            Largest := Right;
         end if;

         if Largest /= I then
            Temp := H.Data (I);
            H.Data (I) := H.Data (Largest);
            H.Data (Largest) := Temp;
            I := Largest;
         else
            exit;
         end if;
      end loop;
   end Sift_Down;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (H : out Heap) is
   begin
      H.Data := (others => 0);
      H.Count := 0;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (H : Heap) return Boolean is
   begin
      return H.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (H : Heap) return Boolean is
   begin
      return H.Count = Max_Size;
   end Is_Full;

   ----------
   -- Size --
   ----------

   function Size (H : Heap) return Natural is
   begin
      return H.Count;
   end Size;

   ----------
   -- Peek --
   ----------

   function Peek (H : Heap) return Integer is
   begin
      if H.Count = 0 then
         return 0;
      end if;
      return H.Data (1);
   end Peek;

   ----------
   -- Push --
   ----------

   procedure Push (H : in Out Heap; Value : Integer) is
   begin
      if H.Count >= Max_Size then
         return;
      end if;

      H.Count := H.Count + 1;
      H.Data (H.Count) := Value;
      Sift_Up (H, H.Count);
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop (H : in Out Heap) return Integer is
      Result : Integer;
   begin
      if H.Count = 0 then
         return 0;
      end if;

      Result := H.Data (1);
      H.Data (1) := H.Data (H.Count);
      H.Count := H.Count - 1;

      if H.Count > 0 then
         Sift_Down (H, 1);
      end if;

      return Result;
   end Pop;

   -----------
   -- Clear --
   -----------

   procedure Clear (H : out Heap) is
   begin
      Initialize (H);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (H : Heap; Value : Integer) return Boolean is
   begin
      for I in 1 .. H.Count loop
         if H.Data (I) = Value then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ----------------
   -- Build_Heap --
   ----------------

   procedure Build_Heap (H : out Heap; Values : Integer_Array) is
   begin
      Initialize (H);

      for V of Values loop
         exit when H.Count >= Max_Size;
         H.Count := H.Count + 1;
         H.Data (H.Count) := V;
      end loop;

      -- Heapify from bottom up
      for I in reverse 1 .. H.Count / 2 loop
         Sift_Down (H, I);
      end loop;
   end Build_Heap;

   --------------------------
   -- Heap_Sort_Descending --
   --------------------------

   procedure Heap_Sort_Descending (Values : in Out Integer_Array) is
      H : Heap;
   begin
      -- Build heap from input
      Initialize (H);
      for V of Values loop
         Push (H, V);
      end loop;

      -- Extract in descending order
      for I in Values'Range loop
         Values (I) := Pop (H);
      end loop;
   end Heap_Sort_Descending;

end GNAT.Max_Heap;
