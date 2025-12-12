-- GNAT.Hash_Map body for Z80
-- Simple hash map implementation

package body GNAT.Hash_Map is

   function Hash (Key : String) return Positive is
      H : Natural := 0;
   begin
      for C of Key loop
         H := (H * 31 + Character'Pos (C)) mod 65536;
      end loop;
      return (H mod Bucket_Count) + 1;
   end Hash;

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

   function Find_Entry (M : Hash_Map; Key : String) return Natural is
      Bucket : constant Positive := Hash (Key);
      Idx    : Natural := M.Buckets (Bucket);
   begin
      while Idx > 0 loop
         if M.Entries (Idx).Valid and then
           Keys_Equal (M.Entries (Idx).Key, M.Entries (Idx).Key_Len, Key)
         then
            return Idx;
         end if;
         Idx := M.Entries (Idx).Next;
      end loop;
      return 0;
   end Find_Entry;

   function Allocate_Entry (M : in Out Hash_Map) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if not M.Entries (I).Valid then
            return I;
         end if;
      end loop;
      return 0;
   end Allocate_Entry;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (M : out Hash_Map) is
   begin
      M.Entries := (others => (Key => (others => ASCII.NUL),
                               Key_Len => 0,
                               Value => 0,
                               Valid => False,
                               Next => 0));
      M.Buckets := (others => 0);
      M.Count := 0;
      M.Collisions := 0;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (M : Hash_Map) return Boolean is
   begin
      return M.Count = 0;
   end Is_Empty;

   ----------
   -- Size --
   ----------

   function Size (M : Hash_Map) return Natural is
   begin
      return M.Count;
   end Size;

   --------------
   -- Contains --
   --------------

   function Contains (M : Hash_Map; Key : String) return Boolean is
   begin
      return Find_Entry (M, Key) > 0;
   end Contains;

   ---------
   -- Get --
   ---------

   function Get (M : Hash_Map; Key : String) return Integer is
      Idx : constant Natural := Find_Entry (M, Key);
   begin
      if Idx = 0 then
         return 0;
      end if;
      return M.Entries (Idx).Value;
   end Get;

   function Get (M : Hash_Map; Key : String; Default : Integer) return Integer is
      Idx : constant Natural := Find_Entry (M, Key);
   begin
      if Idx = 0 then
         return Default;
      end if;
      return M.Entries (Idx).Value;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (M : in Out Hash_Map; Key : String; Value : Integer) is
      Bucket  : constant Positive := Hash (Key);
      Idx     : Natural := Find_Entry (M, Key);
      Key_Len : Natural;
   begin
      if Idx > 0 then
         -- Update existing
         M.Entries (Idx).Value := Value;
         return;
      end if;

      -- Allocate new entry
      Idx := Allocate_Entry (M);
      if Idx = 0 then
         return;  -- Map full
      end if;

      Key_Len := Natural'Min (Key'Length, Max_Key_Length);

      M.Entries (Idx).Key := (others => ASCII.NUL);
      for I in 1 .. Key_Len loop
         M.Entries (Idx).Key (I) := Key (Key'First + I - 1);
      end loop;
      M.Entries (Idx).Key_Len := Key_Len;
      M.Entries (Idx).Value := Value;
      M.Entries (Idx).Valid := True;

      -- Add to bucket chain
      if M.Buckets (Bucket) > 0 then
         M.Collisions := M.Collisions + 1;
         M.Entries (Idx).Next := M.Buckets (Bucket);
      else
         M.Entries (Idx).Next := 0;
      end if;
      M.Buckets (Bucket) := Idx;

      M.Count := M.Count + 1;
   end Put;

   ------------
   -- Remove --
   ------------

   procedure Remove (M : in Out Hash_Map; Key : String) is
      Bucket : constant Positive := Hash (Key);
      Idx    : Natural := M.Buckets (Bucket);
      Prev   : Natural := 0;
   begin
      while Idx > 0 loop
         if M.Entries (Idx).Valid and then
           Keys_Equal (M.Entries (Idx).Key, M.Entries (Idx).Key_Len, Key)
         then
            -- Found - remove from chain
            if Prev = 0 then
               M.Buckets (Bucket) := M.Entries (Idx).Next;
            else
               M.Entries (Prev).Next := M.Entries (Idx).Next;
            end if;

            M.Entries (Idx).Valid := False;
            M.Entries (Idx).Next := 0;
            M.Count := M.Count - 1;
            return;
         end if;
         Prev := Idx;
         Idx := M.Entries (Idx).Next;
      end loop;
   end Remove;

   -----------
   -- Clear --
   -----------

   procedure Clear (M : out Hash_Map) is
   begin
      Initialize (M);
   end Clear;

   -----------------
   -- Entry_Count --
   -----------------

   function Entry_Count (M : Hash_Map) return Natural is
   begin
      return M.Count;
   end Entry_Count;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (M : Hash_Map; Index : Positive) return Entry_Info is
      Count : Natural := 0;
      Info  : Entry_Info := (Key => (others => ' '),
                             Key_Len => 0,
                             Value => 0,
                             Valid => False);
   begin
      for I in 1 .. Max_Entries loop
         if M.Entries (I).Valid then
            Count := Count + 1;
            if Count = Index then
               Info.Key := String (M.Entries (I).Key);
               Info.Key_Len := M.Entries (I).Key_Len;
               Info.Value := M.Entries (I).Value;
               Info.Valid := True;
               return Info;
            end if;
         end if;
      end loop;
      return Info;
   end Get_Entry;

   ---------------------
   -- Collision_Count --
   ---------------------

   function Collision_Count (M : Hash_Map) return Natural is
   begin
      return M.Collisions;
   end Collision_Count;

   -----------------
   -- Load_Factor --
   -----------------

   function Load_Factor (M : Hash_Map) return Natural is
   begin
      return (M.Count * 100) / Bucket_Count;
   end Load_Factor;

end GNAT.Hash_Map;
