-- GNAT.String_Maps body for Z80
-- String mapping implementation

package body GNAT.String_Maps is

   function Keys_Equal (K1 : String; K1_Len : Natural;
                        K2 : String) return Boolean is
   begin
      if K1_Len /= K2'Length then
         return False;
      end if;
      for I in 1 .. K1_Len loop
         if K1 (I) /= K2 (K2'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Keys_Equal;

   function Find_Entry (M : String_Map; Key : String) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used and then
            Keys_Equal (M.Entries (I).Key, M.Entries (I).Key_Len, Key) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Entry;

   function Find_Free (M : String_Map) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if not M.Entries (I).Used then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (M : out String_Map) is
   begin
      M.Count := 0;
      for I in 1 .. Max_Entries loop
         M.Entries (I).Used := False;
         M.Entries (I).Key_Len := 0;
         M.Entries (I).Val_Len := 0;
      end loop;
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set (M : in Out String_Map; Key, Value : String) is
      Idx : Natural;
      KL  : Natural;
      VL  : Natural;
   begin
      -- Check if key already exists
      Idx := Find_Entry (M, Key);

      if Idx = 0 then
         -- Find free slot
         Idx := Find_Free (M);
         if Idx = 0 then
            return;  -- Map is full
         end if;
         M.Count := M.Count + 1;
      end if;

      -- Store key
      M.Entries (Idx).Key := (others => ' ');
      KL := Key'Length;
      if KL > Max_Key_Length then
         KL := Max_Key_Length;
      end if;
      M.Entries (Idx).Key_Len := KL;
      for I in 1 .. KL loop
         M.Entries (Idx).Key (I) := Key (Key'First + I - 1);
      end loop;

      -- Store value
      M.Entries (Idx).Value := (others => ' ');
      VL := Value'Length;
      if VL > Max_Val_Length then
         VL := Max_Val_Length;
      end if;
      M.Entries (Idx).Val_Len := VL;
      for I in 1 .. VL loop
         M.Entries (Idx).Value (I) := Value (Value'First + I - 1);
      end loop;

      M.Entries (Idx).Used := True;
   end Set;

   ---------
   -- Get --
   ---------

   function Get (M : String_Map; Key : String) return String is
      Idx : constant Natural := Find_Entry (M, Key);
   begin
      if Idx = 0 then
         return "";
      end if;
      return M.Entries (Idx).Value (1 .. M.Entries (Idx).Val_Len);
   end Get;

   --------------
   -- Contains --
   --------------

   function Contains (M : String_Map; Key : String) return Boolean is
   begin
      return Find_Entry (M, Key) /= 0;
   end Contains;

   ------------
   -- Remove --
   ------------

   procedure Remove (M : in Out String_Map; Key : String) is
      Idx : constant Natural := Find_Entry (M, Key);
   begin
      if Idx /= 0 then
         M.Entries (Idx).Used := False;
         M.Count := M.Count - 1;
      end if;
   end Remove;

   --------------------
   -- Get_Or_Default --
   --------------------

   function Get_Or_Default (M : String_Map; Key : String;
                            Default : String) return String is
      Idx : constant Natural := Find_Entry (M, Key);
   begin
      if Idx = 0 then
         return Default;
      end if;
      return M.Entries (Idx).Value (1 .. M.Entries (Idx).Val_Len);
   end Get_Or_Default;

   -----------
   -- Clear --
   -----------

   procedure Clear (M : out String_Map) is
   begin
      Initialize (M);
   end Clear;

   ----------
   -- Size --
   ----------

   function Size (M : String_Map) return Natural is
   begin
      return M.Count;
   end Size;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (M : String_Map) return Boolean is
   begin
      return M.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (M : String_Map) return Boolean is
   begin
      return M.Count = Max_Entries;
   end Is_Full;

   ------------
   -- Key_At --
   ------------

   function Key_At (M : String_Map; Index : Positive) return String is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used then
            Count := Count + 1;
            if Count = Index then
               return M.Entries (I).Key (1 .. M.Entries (I).Key_Len);
            end if;
         end if;
      end loop;
      return "";
   end Key_At;

   --------------
   -- Value_At --
   --------------

   function Value_At (M : String_Map; Index : Positive) return String is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used then
            Count := Count + 1;
            if Count = Index then
               return M.Entries (I).Value (1 .. M.Entries (I).Val_Len);
            end if;
         end if;
      end loop;
      return "";
   end Value_At;

   --------------
   -- Find_Key --
   --------------

   function Find_Key (M : String_Map; Value : String) return String is
   begin
      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used then
            declare
               V : constant String := M.Entries (I).Value (1 .. M.Entries (I).Val_Len);
            begin
               if V = Value then
                  return M.Entries (I).Key (1 .. M.Entries (I).Key_Len);
               end if;
            end;
         end if;
      end loop;
      return "";
   end Find_Key;

   -----------
   -- First --
   -----------

   function First (M : String_Map) return Cursor is
      C : Cursor;
   begin
      C.Index := 0;
      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used then
            C.Index := I;
            exit;
         end if;
      end loop;
      return C;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (C : Cursor) return Boolean is
   begin
      return C.Index > 0;
   end Has_Element;

   ----------
   -- Next --
   ----------

   procedure Next (M : String_Map; C : in Out Cursor) is
   begin
      if C.Index = 0 then
         return;
      end if;

      for I in C.Index + 1 .. Max_Entries loop
         if M.Entries (I).Used then
            C.Index := I;
            return;
         end if;
      end loop;

      C.Index := 0;
   end Next;

   ---------
   -- Key --
   ---------

   function Key (M : String_Map; C : Cursor) return String is
   begin
      if C.Index = 0 or else not M.Entries (C.Index).Used then
         return "";
      end if;
      return M.Entries (C.Index).Key (1 .. M.Entries (C.Index).Key_Len);
   end Key;

   -----------
   -- Value --
   -----------

   function Value (M : String_Map; C : Cursor) return String is
   begin
      if C.Index = 0 or else not M.Entries (C.Index).Used then
         return "";
      end if;
      return M.Entries (C.Index).Value (1 .. M.Entries (C.Index).Val_Len);
   end Value;

   -----------
   -- Merge --
   -----------

   procedure Merge (Target : in Out String_Map; Source : String_Map) is
   begin
      for I in 1 .. Max_Entries loop
         if Source.Entries (I).Used then
            Set (Target,
                 Source.Entries (I).Key (1 .. Source.Entries (I).Key_Len),
                 Source.Entries (I).Value (1 .. Source.Entries (I).Val_Len));
         end if;
      end loop;
   end Merge;

   --------------
   -- All_Keys --
   --------------

   function All_Keys (M : String_Map; Sep : String := ",") return String is
      Result : String (1 .. Max_Entries * (Max_Key_Length + Sep'Length));
      Pos    : Natural := 1;
      First  : Boolean := True;
   begin
      Result := (others => ' ');

      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used then
            if not First then
               for C of Sep loop
                  Result (Pos) := C;
                  Pos := Pos + 1;
               end loop;
            end if;
            First := False;

            for J in 1 .. M.Entries (I).Key_Len loop
               Result (Pos) := M.Entries (I).Key (J);
               Pos := Pos + 1;
            end loop;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end All_Keys;

   ----------------
   -- All_Values --
   ----------------

   function All_Values (M : String_Map; Sep : String := ",") return String is
      Result : String (1 .. Max_Entries * (Max_Val_Length + Sep'Length));
      Pos    : Natural := 1;
      First  : Boolean := True;
   begin
      Result := (others => ' ');

      for I in 1 .. Max_Entries loop
         if M.Entries (I).Used then
            if not First then
               for C of Sep loop
                  Result (Pos) := C;
                  Pos := Pos + 1;
               end loop;
            end if;
            First := False;

            for J in 1 .. M.Entries (I).Val_Len loop
               Result (Pos) := M.Entries (I).Value (J);
               Pos := Pos + 1;
            end loop;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end All_Values;

end GNAT.String_Maps;
