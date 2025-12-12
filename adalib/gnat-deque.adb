-- GNAT.Deque body for Z80
-- Double-ended queue implementation

package body GNAT.Deque is

   function Prev_Index (I : Positive) return Positive is
   begin
      if I = 1 then
         return Max_Size;
      else
         return I - 1;
      end if;
   end Prev_Index;

   function Next_Index (I : Positive) return Positive is
   begin
      if I = Max_Size then
         return 1;
      else
         return I + 1;
      end if;
   end Next_Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (D : out Deque) is
   begin
      D.Data := (others => 0);
      D.Head := 1;
      D.Tail := 0;
      D.Count := 0;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (D : Deque) return Boolean is
   begin
      return D.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (D : Deque) return Boolean is
   begin
      return D.Count = Max_Size;
   end Is_Full;

   ----------
   -- Size --
   ----------

   function Size (D : Deque) return Natural is
   begin
      return D.Count;
   end Size;

   -----------
   -- Front --
   -----------

   function Front (D : Deque) return Integer is
   begin
      if D.Count = 0 then
         return 0;
      end if;
      return D.Data (D.Head);
   end Front;

   ----------
   -- Back --
   ----------

   function Back (D : Deque) return Integer is
   begin
      if D.Count = 0 then
         return 0;
      end if;
      return D.Data (D.Tail);
   end Back;

   ----------------
   -- Push_Front --
   ----------------

   procedure Push_Front (D : in Out Deque; Value : Integer) is
   begin
      if D.Count >= Max_Size then
         return;
      end if;

      if D.Count = 0 then
         D.Head := 1;
         D.Tail := 1;
      else
         D.Head := Prev_Index (D.Head);
      end if;

      D.Data (D.Head) := Value;
      D.Count := D.Count + 1;
   end Push_Front;

   ---------------
   -- Push_Back --
   ---------------

   procedure Push_Back (D : in Out Deque; Value : Integer) is
   begin
      if D.Count >= Max_Size then
         return;
      end if;

      if D.Count = 0 then
         D.Head := 1;
         D.Tail := 1;
      else
         D.Tail := Next_Index (D.Tail);
      end if;

      D.Data (D.Tail) := Value;
      D.Count := D.Count + 1;
   end Push_Back;

   ---------------
   -- Pop_Front --
   ---------------

   function Pop_Front (D : in Out Deque) return Integer is
      Result : Integer;
   begin
      if D.Count = 0 then
         return 0;
      end if;

      Result := D.Data (D.Head);
      D.Head := Next_Index (D.Head);
      D.Count := D.Count - 1;

      if D.Count = 0 then
         D.Head := 1;
         D.Tail := 0;
      end if;

      return Result;
   end Pop_Front;

   --------------
   -- Pop_Back --
   --------------

   function Pop_Back (D : in Out Deque) return Integer is
      Result : Integer;
   begin
      if D.Count = 0 then
         return 0;
      end if;

      Result := D.Data (D.Tail);
      D.Tail := Prev_Index (D.Tail);
      D.Count := D.Count - 1;

      if D.Count = 0 then
         D.Head := 1;
         D.Tail := 0;
      end if;

      return Result;
   end Pop_Back;

   -----------
   -- Clear --
   -----------

   procedure Clear (D : out Deque) is
   begin
      Initialize (D);
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (D : Deque; Index : Positive) return Integer is
      Actual_Idx : Positive;
   begin
      if Index > D.Count then
         return 0;
      end if;

      Actual_Idx := D.Head + Index - 1;
      if Actual_Idx > Max_Size then
         Actual_Idx := Actual_Idx - Max_Size;
      end if;

      return D.Data (Actual_Idx);
   end Element;

   --------------
   -- Contains --
   --------------

   function Contains (D : Deque; Value : Integer) return Boolean is
   begin
      for I in 1 .. D.Count loop
         if Element (D, I) = Value then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   -------------------
   -- Reverse_Deque --
   -------------------

   procedure Reverse_Deque (D : in Out Deque) is
      Temp : Deque;
   begin
      Initialize (Temp);

      -- Pop from back and push to back of new deque
      while not Is_Empty (D) loop
         Push_Back (Temp, Pop_Back (D));
      end loop;

      D := Temp;
   end Reverse_Deque;

end GNAT.Deque;
