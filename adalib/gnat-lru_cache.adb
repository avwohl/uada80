-- GNAT.LRU_Cache body for Z80
-- Least Recently Used cache implementation

package body GNAT.LRU_Cache is

   function Keys_Equal (K1 : Key_String; L1 : Natural;
                        K2 : String) return Boolean
   is
   begin
      if L1 /= K2'Length then
         return False;
      end if;
      for I in 1 .. L1 loop
         if K1 (I) /= K2 (K2'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Keys_Equal;

   function Find_Entry (C : Cache; Key : String) return Natural is
   begin
      for I in 1 .. C.Cap loop
         if C.Entries (I).Valid and then
           Keys_Equal (C.Entries (I).Key, C.Entries (I).Key_Len, Key)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Entry;

   function Find_LRU (C : Cache) return Positive is
      Min_Seq : Natural := Natural'Last;
      LRU_Idx : Positive := 1;
   begin
      for I in 1 .. C.Cap loop
         if C.Entries (I).Valid then
            if C.Entries (I).Access_Seq < Min_Seq then
               Min_Seq := C.Entries (I).Access_Seq;
               LRU_Idx := I;
            end if;
         else
            -- Empty slot found
            return I;
         end if;
      end loop;
      return LRU_Idx;
   end Find_LRU;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : out Cache; Capacity : Positive := Max_Cache_Size) is
   begin
      C.Entries := (others => (Key        => (others => ASCII.NUL),
                               Key_Len    => 0,
                               Value      => (others => ASCII.NUL),
                               Value_Len  => 0,
                               Access_Seq => 0,
                               Valid      => False));
      C.Count := 0;
      C.Cap := Positive'Min (Capacity, Max_Cache_Size);
      C.Seq := 0;
      C.Hit_Count := 0;
      C.Miss_Count := 0;
   end Initialize;

   --------------
   -- Contains --
   --------------

   function Contains (C : Cache; Key : String) return Boolean is
   begin
      return Find_Entry (C, Key) > 0;
   end Contains;

   ---------
   -- Get --
   ---------

   procedure Get
     (C       : in Out Cache;
      Key     : String;
      Value   : out String;
      Last    : out Natural;
      Found   : out Boolean)
   is
      Idx : constant Natural := Find_Entry (C, Key);
   begin
      Last := Value'First - 1;

      if Idx = 0 then
         Found := False;
         C.Miss_Count := C.Miss_Count + 1;
         return;
      end if;

      Found := True;
      C.Hit_Count := C.Hit_Count + 1;

      -- Update access sequence
      C.Seq := C.Seq + 1;
      C.Entries (Idx).Access_Seq := C.Seq;

      -- Copy value
      declare
         E : Cache_Entry renames C.Entries (Idx);
         Copy_Len : constant Natural := Natural'Min (E.Value_Len, Value'Length);
      begin
         Value (Value'First .. Value'First + Copy_Len - 1) :=
           String (E.Value (1 .. Copy_Len));
         Last := Value'First + Copy_Len - 1;
      end;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (C     : in Out Cache;
      Key   : String;
      Value : String)
   is
      Idx     : Natural := Find_Entry (C, Key);
      Key_Len : constant Natural := Natural'Min (Key'Length, Max_Key_Length);
      Val_Len : constant Natural := Natural'Min (Value'Length, Max_Value_Length);
   begin
      C.Seq := C.Seq + 1;

      if Idx = 0 then
         -- New entry - find slot
         Idx := Find_LRU (C);
         if not C.Entries (Idx).Valid then
            C.Count := C.Count + 1;
         end if;
      end if;

      declare
         E : Cache_Entry renames C.Entries (Idx);
      begin
         E.Key := (others => ASCII.NUL);
         for I in 1 .. Key_Len loop
            E.Key (I) := Key (Key'First + I - 1);
         end loop;
         E.Key_Len := Key_Len;

         E.Value := (others => ASCII.NUL);
         for I in 1 .. Val_Len loop
            E.Value (I) := Value (Value'First + I - 1);
         end loop;
         E.Value_Len := Val_Len;

         E.Access_Seq := C.Seq;
         E.Valid := True;
      end;
   end Put;

   ------------
   -- Remove --
   ------------

   procedure Remove (C : in Out Cache; Key : String) is
      Idx : constant Natural := Find_Entry (C, Key);
   begin
      if Idx > 0 then
         C.Entries (Idx).Valid := False;
         C.Count := C.Count - 1;
      end if;
   end Remove;

   -----------
   -- Clear --
   -----------

   procedure Clear (C : out Cache) is
   begin
      for I in 1 .. C.Cap loop
         C.Entries (I).Valid := False;
      end loop;
      C.Count := 0;
      C.Seq := 0;
   end Clear;

   ----------
   -- Size --
   ----------

   function Size (C : Cache) return Natural is
   begin
      return C.Count;
   end Size;

   --------------
   -- Capacity --
   --------------

   function Capacity (C : Cache) return Positive is
   begin
      return C.Cap;
   end Capacity;

   ----------
   -- Hits --
   ----------

   function Hits (C : Cache) return Natural is
   begin
      return C.Hit_Count;
   end Hits;

   ------------
   -- Misses --
   ------------

   function Misses (C : Cache) return Natural is
   begin
      return C.Miss_Count;
   end Misses;

   -----------------
   -- Reset_Stats --
   -----------------

   procedure Reset_Stats (C : in Out Cache) is
   begin
      C.Hit_Count := 0;
      C.Miss_Count := 0;
   end Reset_Stats;

end GNAT.LRU_Cache;
