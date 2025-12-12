-- GNAT.Dynamic_HTables body for Z80
-- Dynamic hash tables implementation

package body GNAT.Dynamic_HTables is

   function Find_Slot (T : Instance; Key : Key_Type) return Natural is
      H     : constant Ada.Containers.Hash_Type := Hash (Key);
      Start : constant Natural := Natural (H mod Ada.Containers.Hash_Type (T.Capacity)) + 1;
      I     : Natural := Start;
   begin
      loop
         case T.Entries (I).State is
            when Empty =>
               return I;
            when Used =>
               if T.Entries (I).Key = Key then
                  return I;
               end if;
            when Deleted =>
               null;  -- Continue probing
         end case;

         I := I + 1;
         if I > T.Capacity then
            I := 1;
         end if;

         exit when I = Start;
      end loop;
      return 0;  -- Table full
   end Find_Slot;

   ------------
   -- Create --
   ------------

   function Create (Initial_Size : Positive := 37) return Instance is
      Result : Instance;
   begin
      Result.Capacity := Natural'Min (Initial_Size, Max_Size);
      Result.Count := 0;
      return Result;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (T : in Out Instance) is
   begin
      T.Count := 0;
      for I in 1 .. T.Capacity loop
         T.Entries (I).State := Empty;
      end loop;
   end Destroy;

   ---------
   -- Put --
   ---------

   procedure Put (T : in Out Instance; Key : Key_Type; Value : Value_Type) is
      Slot : Natural;
   begin
      Slot := Find_Slot (T, Key);
      if Slot = 0 then
         -- Table full - ignore on Z80
         return;
      end if;

      if T.Entries (Slot).State /= Used then
         T.Count := T.Count + 1;
      end if;

      T.Entries (Slot) := (State => Used, Key => Key, Value => Value);
   end Put;

   ---------
   -- Get --
   ---------

   function Get (T : Instance; Key : Key_Type) return Value_Type is
      Slot : constant Natural := Find_Slot (T, Key);
   begin
      if Slot /= 0 and then T.Entries (Slot).State = Used
         and then T.Entries (Slot).Key = Key
      then
         return T.Entries (Slot).Value;
      else
         return No_Value;
      end if;
   end Get;

   --------------
   -- Contains --
   --------------

   function Contains (T : Instance; Key : Key_Type) return Boolean is
      Slot : constant Natural := Find_Slot (T, Key);
   begin
      return Slot /= 0 and then T.Entries (Slot).State = Used
             and then T.Entries (Slot).Key = Key;
   end Contains;

   ------------
   -- Remove --
   ------------

   procedure Remove (T : in Out Instance; Key : Key_Type) is
      Slot : constant Natural := Find_Slot (T, Key);
   begin
      if Slot /= 0 and then T.Entries (Slot).State = Used
         and then T.Entries (Slot).Key = Key
      then
         T.Entries (Slot).State := Deleted;
         T.Count := T.Count - 1;
      end if;
   end Remove;

   ----------
   -- Size --
   ----------

   function Size (T : Instance) return Natural is
   begin
      return T.Count;
   end Size;

   -----------
   -- First --
   -----------

   function First (T : Instance) return Cursor is
      C : Cursor;
   begin
      C.Index := 0;
      for I in 1 .. T.Capacity loop
         if T.Entries (I).State = Used then
            C.Index := I;
            exit;
         end if;
      end loop;
      return C;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
      C : Cursor := Position;
   begin
      if C.Table = null or else C.Index = 0 then
         C.Index := 0;
         return C;
      end if;

      for I in C.Index + 1 .. C.Table.Capacity loop
         if C.Table.Entries (I).State = Used then
            C.Index := I;
            return C;
         end if;
      end loop;
      C.Index := 0;
      return C;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index > 0;
   end Has_Element;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Table.Entries (Position.Index).Key;
   end Key;

   -----------
   -- Value --
   -----------

   function Value (Position : Cursor) return Value_Type is
   begin
      return Position.Table.Entries (Position.Index).Value;
   end Value;

end GNAT.Dynamic_HTables;
