-- GNAT.Spitbol.Table_Integer body for Z80
-- SPITBOL integer table implementation

package body GNAT.Spitbol.Table_Integer is

   function Find (T : Table; Key : String) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if T.Entries (I).Used and then
            T.Entries (I).Key_Len = Key'Length and then
            T.Entries (I).Key (1 .. Key'Length) = Key
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Find_Free (T : Table) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if not T.Entries (I).Used then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   ---------
   -- Get --
   ---------

   function Get (T : Table; Key : String) return Integer is
      Idx : constant Natural := Find (T, Key);
   begin
      if Idx > 0 then
         return T.Entries (Idx).Value;
      else
         return 0;
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (T : in Out Table; Key : String; Value : Integer) is
      Idx : Natural := Find (T, Key);
   begin
      if Idx = 0 then
         Idx := Find_Free (T);
         if Idx = 0 then
            return;
         end if;
         T.Entries (Idx).Key (1 .. Key'Length) := Key;
         T.Entries (Idx).Key_Len := Key'Length;
         T.Entries (Idx).Used := True;
         T.Count := T.Count + 1;
      end if;
      T.Entries (Idx).Value := Value;
   end Set;

   -------------
   -- Present --
   -------------

   function Present (T : Table; Key : String) return Boolean is
   begin
      return Find (T, Key) > 0;
   end Present;

   ------------
   -- Delete --
   ------------

   procedure Delete (T : in Out Table; Key : String) is
      Idx : constant Natural := Find (T, Key);
   begin
      if Idx > 0 then
         T.Entries (Idx).Used := False;
         T.Count := T.Count - 1;
      end if;
   end Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (T : in Out Table) is
   begin
      for I in 1 .. Max_Entries loop
         T.Entries (I).Used := False;
      end loop;
      T.Count := 0;
   end Clear;

   ----------
   -- Size --
   ----------

   function Size (T : Table) return Natural is
   begin
      return T.Count;
   end Size;

end GNAT.Spitbol.Table_Integer;
