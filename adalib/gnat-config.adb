-- GNAT.Config body for Z80
-- Configuration management implementation

package body GNAT.Config is

   function Str_Equal (S1 : String; L1 : Natural; S2 : String) return Boolean is
   begin
      if L1 /= S2'Length then
         return False;
      end if;
      for I in 1 .. L1 loop
         if S1 (I) /= S2 (S2'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Str_Equal;

   function Find_Entry (C : Config_Store; Section, Key : String) return Natural is
   begin
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and then
            Str_Equal (C.Entries (I).Section, C.Entries (I).Section_Len, Section) and then
            Str_Equal (C.Entries (I).Key, C.Entries (I).Key_Len, Key) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Entry;

   function Find_Free (C : Config_Store) return Natural is
   begin
      for I in 1 .. Max_Config_Entries loop
         if not C.Entries (I).Used then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   procedure Store_String (Dest : out String; Dest_Len : out Natural;
                           Source : String; Max_Len : Natural) is
      Len : Natural := Source'Length;
   begin
      if Len > Max_Len then
         Len := Max_Len;
      end if;
      Dest := (others => ' ');
      Dest_Len := Len;
      for I in 1 .. Len loop
         Dest (I) := Source (Source'First + I - 1);
      end loop;
   end Store_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : out Config_Store) is
   begin
      C.Count := 0;
      for I in 1 .. Max_Config_Entries loop
         C.Entries (I).Used := False;
         C.Entries (I).Section_Len := 0;
         C.Entries (I).Key_Len := 0;
         C.Entries (I).Value_Len := 0;
         C.Entries (I).Is_Default := False;
      end loop;
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set (C : in Out Config_Store; Key, Value : String) is
   begin
      Set (C, "", Key, Value);
   end Set;

   procedure Set (C : in Out Config_Store; Section, Key, Value : String) is
      Idx : Natural;
   begin
      Idx := Find_Entry (C, Section, Key);

      if Idx = 0 then
         Idx := Find_Free (C);
         if Idx = 0 then
            return;  -- Full
         end if;
         C.Count := C.Count + 1;
      end if;

      Store_String (C.Entries (Idx).Section, C.Entries (Idx).Section_Len,
                    Section, Max_Section_Length);
      Store_String (C.Entries (Idx).Key, C.Entries (Idx).Key_Len,
                    Key, Max_Key_Length);
      Store_String (C.Entries (Idx).Value, C.Entries (Idx).Value_Len,
                    Value, Max_Value_Length);
      C.Entries (Idx).Used := True;
      C.Entries (Idx).Is_Default := False;
   end Set;

   ---------
   -- Get --
   ---------

   function Get (C : Config_Store; Key : String) return String is
   begin
      return Get (C, "", Key);
   end Get;

   function Get (C : Config_Store; Section, Key : String) return String is
      Idx : constant Natural := Find_Entry (C, Section, Key);
   begin
      if Idx = 0 then
         return "";
      end if;
      return C.Entries (Idx).Value (1 .. C.Entries (Idx).Value_Len);
   end Get;

   --------------------
   -- Get_Or_Default --
   --------------------

   function Get_Or_Default (C : Config_Store; Key : String;
                            Default : String) return String is
   begin
      return Get_Or_Default (C, "", Key, Default);
   end Get_Or_Default;

   function Get_Or_Default (C : Config_Store; Section, Key : String;
                            Default : String) return String is
      Idx : constant Natural := Find_Entry (C, Section, Key);
   begin
      if Idx = 0 then
         return Default;
      end if;
      return C.Entries (Idx).Value (1 .. C.Entries (Idx).Value_Len);
   end Get_Or_Default;

   --------------
   -- Contains --
   --------------

   function Contains (C : Config_Store; Key : String) return Boolean is
   begin
      return Contains (C, "", Key);
   end Contains;

   function Contains (C : Config_Store; Section, Key : String) return Boolean is
   begin
      return Find_Entry (C, Section, Key) /= 0;
   end Contains;

   ------------
   -- Remove --
   ------------

   procedure Remove (C : in Out Config_Store; Key : String) is
   begin
      Remove (C, "", Key);
   end Remove;

   procedure Remove (C : in Out Config_Store; Section, Key : String) is
      Idx : constant Natural := Find_Entry (C, Section, Key);
   begin
      if Idx /= 0 then
         C.Entries (Idx).Used := False;
         C.Count := C.Count - 1;
      end if;
   end Remove;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (C : in Out Config_Store; Key : String; Value : Integer) is
   begin
      Set_Int (C, "", Key, Value);
   end Set_Int;

   procedure Set_Int (C : in Out Config_Store; Section, Key : String; Value : Integer) is
      Buf : String (1 .. 12);
      Pos : Natural := 12;
      V   : Integer := abs Value;
   begin
      if V = 0 then
         Set (C, Section, Key, "0");
         return;
      end if;

      while V > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + (V mod 10));
         V := V / 10;
         Pos := Pos - 1;
      end loop;

      if Value < 0 then
         Buf (Pos) := '-';
         Pos := Pos - 1;
      end if;

      Set (C, Section, Key, Buf (Pos + 1 .. 12));
   end Set_Int;

   -------------
   -- Get_Int --
   -------------

   function Get_Int (C : Config_Store; Key : String) return Integer is
   begin
      return Get_Int (C, "", Key);
   end Get_Int;

   function Get_Int (C : Config_Store; Section, Key : String) return Integer is
      Val : constant String := Get (C, Section, Key);
      Result : Integer := 0;
      Negative : Boolean := False;
      Start : Natural := Val'First;
   begin
      if Val'Length = 0 then
         return 0;
      end if;

      if Val (Start) = '-' then
         Negative := True;
         Start := Start + 1;
      elsif Val (Start) = '+' then
         Start := Start + 1;
      end if;

      for I in Start .. Val'Last loop
         exit when Val (I) < '0' or Val (I) > '9';
         Result := Result * 10 + (Character'Pos (Val (I)) - Character'Pos ('0'));
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Get_Int;

   ------------------------
   -- Get_Int_Or_Default --
   ------------------------

   function Get_Int_Or_Default (C : Config_Store; Key : String;
                                Default : Integer) return Integer is
   begin
      if Contains (C, Key) then
         return Get_Int (C, Key);
      else
         return Default;
      end if;
   end Get_Int_Or_Default;

   --------------
   -- Set_Bool --
   --------------

   procedure Set_Bool (C : in Out Config_Store; Key : String; Value : Boolean) is
   begin
      if Value then
         Set (C, Key, "true");
      else
         Set (C, Key, "false");
      end if;
   end Set_Bool;

   --------------
   -- Get_Bool --
   --------------

   function Get_Bool (C : Config_Store; Key : String) return Boolean is
      Val : constant String := Get (C, Key);
   begin
      if Val'Length > 0 then
         return Val (Val'First) = '1' or
                Val (Val'First) = 't' or Val (Val'First) = 'T' or
                Val (Val'First) = 'y' or Val (Val'First) = 'Y';
      else
         return False;
      end if;
   end Get_Bool;

   -------------------------
   -- Get_Bool_Or_Default --
   -------------------------

   function Get_Bool_Or_Default (C : Config_Store; Key : String;
                                 Default : Boolean) return Boolean is
   begin
      if Contains (C, Key) then
         return Get_Bool (C, Key);
      else
         return Default;
      end if;
   end Get_Bool_Or_Default;

   -----------
   -- Clear --
   -----------

   procedure Clear (C : out Config_Store) is
   begin
      Initialize (C);
   end Clear;

   -------------------
   -- Clear_Section --
   -------------------

   procedure Clear_Section (C : in Out Config_Store; Section : String) is
   begin
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and then
            Str_Equal (C.Entries (I).Section, C.Entries (I).Section_Len, Section) then
            C.Entries (I).Used := False;
            C.Count := C.Count - 1;
         end if;
      end loop;
   end Clear_Section;

   -----------
   -- Count --
   -----------

   function Count (C : Config_Store) return Natural is
   begin
      return C.Count;
   end Count;

   ----------------------
   -- Count_In_Section --
   ----------------------

   function Count_In_Section (C : Config_Store; Section : String) return Natural is
      Result : Natural := 0;
   begin
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and then
            Str_Equal (C.Entries (I).Section, C.Entries (I).Section_Len, Section) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_In_Section;

   --------------
   -- All_Keys --
   --------------

   function All_Keys (C : Config_Store) return String is
      Result : String (1 .. Max_Config_Entries * (Max_Key_Length + 1));
      Pos    : Natural := 1;
      First  : Boolean := True;
   begin
      Result := (others => ' ');

      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used then
            if not First then
               Result (Pos) := ',';
               Pos := Pos + 1;
            end if;
            First := False;

            for J in 1 .. C.Entries (I).Key_Len loop
               Result (Pos) := C.Entries (I).Key (J);
               Pos := Pos + 1;
            end loop;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end All_Keys;

   ---------------------
   -- Keys_In_Section --
   ---------------------

   function Keys_In_Section (C : Config_Store; Section : String) return String is
      Result : String (1 .. Max_Config_Entries * (Max_Key_Length + 1));
      Pos    : Natural := 1;
      First  : Boolean := True;
   begin
      Result := (others => ' ');

      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and then
            Str_Equal (C.Entries (I).Section, C.Entries (I).Section_Len, Section) then
            if not First then
               Result (Pos) := ',';
               Pos := Pos + 1;
            end if;
            First := False;

            for J in 1 .. C.Entries (I).Key_Len loop
               Result (Pos) := C.Entries (I).Key (J);
               Pos := Pos + 1;
            end loop;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end Keys_In_Section;

   ------------------
   -- All_Sections --
   ------------------

   function All_Sections (C : Config_Store) return String is
      Result   : String (1 .. Max_Config_Entries * (Max_Section_Length + 1));
      Pos      : Natural := 1;
      First    : Boolean := True;
      Sections : array (1 .. Max_Config_Entries) of String (1 .. Max_Section_Length);
      Sec_Lens : array (1 .. Max_Config_Entries) of Natural := (others => 0);
      Sec_Count : Natural := 0;
      Found    : Boolean;
   begin
      Result := (others => ' ');

      -- Collect unique sections
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and C.Entries (I).Section_Len > 0 then
            Found := False;
            for J in 1 .. Sec_Count loop
               if Str_Equal (Sections (J), Sec_Lens (J),
                           C.Entries (I).Section (1 .. C.Entries (I).Section_Len)) then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found and Sec_Count < Max_Config_Entries then
               Sec_Count := Sec_Count + 1;
               Sections (Sec_Count) := C.Entries (I).Section;
               Sec_Lens (Sec_Count) := C.Entries (I).Section_Len;
            end if;
         end if;
      end loop;

      -- Build result
      for I in 1 .. Sec_Count loop
         if not First then
            Result (Pos) := ',';
            Pos := Pos + 1;
         end if;
         First := False;

         for J in 1 .. Sec_Lens (I) loop
            Result (Pos) := Sections (I) (J);
            Pos := Pos + 1;
         end loop;
      end loop;

      return Result (1 .. Pos - 1);
   end All_Sections;

   -----------
   -- First --
   -----------

   function First (C : Config_Store) return Cursor is
      Cur : Cursor;
   begin
      Cur.Index := 0;
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used then
            Cur.Index := I;
            exit;
         end if;
      end loop;
      return Cur;
   end First;

   ----------------------
   -- First_In_Section --
   ----------------------

   function First_In_Section (C : Config_Store; Section : String) return Cursor is
      Cur : Cursor;
   begin
      Cur.Index := 0;
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and then
            Str_Equal (C.Entries (I).Section, C.Entries (I).Section_Len, Section) then
            Cur.Index := I;
            exit;
         end if;
      end loop;
      return Cur;
   end First_In_Section;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Cur : Cursor) return Boolean is
   begin
      return Cur.Index > 0;
   end Has_Element;

   ----------
   -- Next --
   ----------

   procedure Next (C : Config_Store; Cur : in Out Cursor) is
   begin
      if Cur.Index = 0 then
         return;
      end if;

      for I in Cur.Index + 1 .. Max_Config_Entries loop
         if C.Entries (I).Used then
            Cur.Index := I;
            return;
         end if;
      end loop;

      Cur.Index := 0;
   end Next;

   ---------
   -- Key --
   ---------

   function Key (C : Config_Store; Cur : Cursor) return String is
   begin
      if Cur.Index = 0 or else not C.Entries (Cur.Index).Used then
         return "";
      end if;
      return C.Entries (Cur.Index).Key (1 .. C.Entries (Cur.Index).Key_Len);
   end Key;

   -----------
   -- Value --
   -----------

   function Value (C : Config_Store; Cur : Cursor) return String is
   begin
      if Cur.Index = 0 or else not C.Entries (Cur.Index).Used then
         return "";
      end if;
      return C.Entries (Cur.Index).Value (1 .. C.Entries (Cur.Index).Value_Len);
   end Value;

   -------------
   -- Section --
   -------------

   function Section (C : Config_Store; Cur : Cursor) return String is
   begin
      if Cur.Index = 0 or else not C.Entries (Cur.Index).Used then
         return "";
      end if;
      return C.Entries (Cur.Index).Section (1 .. C.Entries (Cur.Index).Section_Len);
   end Section;

   ---------------
   -- To_String --
   ---------------

   function To_String (C : Config_Store) return String is
      -- This returns a simple representation
      Result : String (1 .. 512);
      Pos    : Natural := 1;
   begin
      Result := (others => ' ');

      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used then
            -- Section prefix
            if C.Entries (I).Section_Len > 0 then
               for J in 1 .. C.Entries (I).Section_Len loop
                  if Pos < 510 then
                     Result (Pos) := C.Entries (I).Section (J);
                     Pos := Pos + 1;
                  end if;
               end loop;
               if Pos < 510 then
                  Result (Pos) := '.';
                  Pos := Pos + 1;
               end if;
            end if;

            -- Key
            for J in 1 .. C.Entries (I).Key_Len loop
               if Pos < 510 then
                  Result (Pos) := C.Entries (I).Key (J);
                  Pos := Pos + 1;
               end if;
            end loop;

            -- Equals
            if Pos < 510 then
               Result (Pos) := '=';
               Pos := Pos + 1;
            end if;

            -- Value
            for J in 1 .. C.Entries (I).Value_Len loop
               if Pos < 510 then
                  Result (Pos) := C.Entries (I).Value (J);
                  Pos := Pos + 1;
               end if;
            end loop;

            -- Newline (semicolon for simplicity)
            if Pos < 510 then
               Result (Pos) := ';';
               Pos := Pos + 1;
            end if;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end To_String;

   ---------------
   -- From_Line --
   ---------------

   procedure From_Line (C : in Out Config_Store; Line : String) is
      Eq_Pos  : Natural := 0;
      Dot_Pos : Natural := 0;
      Sect    : String (1 .. Max_Section_Length);
      Key     : String (1 .. Max_Key_Length);
      Val     : String (1 .. Max_Value_Length);
      Sect_Len, Key_Len, Val_Len : Natural;
   begin
      -- Find '=' position
      for I in Line'Range loop
         if Line (I) = '=' then
            Eq_Pos := I;
            exit;
         end if;
      end loop;

      if Eq_Pos = 0 then
         return;  -- No '=' found
      end if;

      -- Find '.' for section
      for I in Line'First .. Eq_Pos - 1 loop
         if Line (I) = '.' then
            Dot_Pos := I;
            exit;
         end if;
      end loop;

      -- Extract section, key, value
      Sect := (others => ' ');
      Key := (others => ' ');
      Val := (others => ' ');

      if Dot_Pos > 0 then
         Sect_Len := Dot_Pos - Line'First;
         if Sect_Len > Max_Section_Length then
            Sect_Len := Max_Section_Length;
         end if;
         for I in 1 .. Sect_Len loop
            Sect (I) := Line (Line'First + I - 1);
         end loop;

         Key_Len := Eq_Pos - Dot_Pos - 1;
         if Key_Len > Max_Key_Length then
            Key_Len := Max_Key_Length;
         end if;
         for I in 1 .. Key_Len loop
            Key (I) := Line (Dot_Pos + I);
         end loop;
      else
         Sect_Len := 0;
         Key_Len := Eq_Pos - Line'First;
         if Key_Len > Max_Key_Length then
            Key_Len := Max_Key_Length;
         end if;
         for I in 1 .. Key_Len loop
            Key (I) := Line (Line'First + I - 1);
         end loop;
      end if;

      Val_Len := Line'Last - Eq_Pos;
      if Val_Len > Max_Value_Length then
         Val_Len := Max_Value_Length;
      end if;
      for I in 1 .. Val_Len loop
         Val (I) := Line (Eq_Pos + I);
      end loop;

      Set (C, Sect (1 .. Sect_Len), Key (1 .. Key_Len), Val (1 .. Val_Len));
   end From_Line;

   -----------
   -- Merge --
   -----------

   procedure Merge (Target : in Out Config_Store; Source : Config_Store) is
   begin
      for I in 1 .. Max_Config_Entries loop
         if Source.Entries (I).Used then
            Set (Target,
                 Source.Entries (I).Section (1 .. Source.Entries (I).Section_Len),
                 Source.Entries (I).Key (1 .. Source.Entries (I).Key_Len),
                 Source.Entries (I).Value (1 .. Source.Entries (I).Value_Len));
         end if;
      end loop;
   end Merge;

   ----------------
   -- Copy_Value --
   ----------------

   procedure Copy_Value (C : in Out Config_Store; From_Key, To_Key : String) is
      Val : constant String := Get (C, From_Key);
   begin
      if Val'Length > 0 then
         Set (C, To_Key, Val);
      end if;
   end Copy_Value;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default (C : in Out Config_Store; Key, Value : String) is
      Idx : Natural;
   begin
      Idx := Find_Entry (C, "", Key);

      if Idx = 0 then
         Idx := Find_Free (C);
         if Idx = 0 then
            return;
         end if;
         C.Count := C.Count + 1;
      else
         -- Already exists, don't overwrite
         return;
      end if;

      Store_String (C.Entries (Idx).Section, C.Entries (Idx).Section_Len,
                    "", Max_Section_Length);
      Store_String (C.Entries (Idx).Key, C.Entries (Idx).Key_Len,
                    Key, Max_Key_Length);
      Store_String (C.Entries (Idx).Value, C.Entries (Idx).Value_Len,
                    Value, Max_Value_Length);
      C.Entries (Idx).Used := True;
      C.Entries (Idx).Is_Default := True;
   end Set_Default;

   -----------------
   -- Has_Default --
   -----------------

   function Has_Default (C : Config_Store; Key : String) return Boolean is
      Idx : constant Natural := Find_Entry (C, "", Key);
   begin
      if Idx = 0 then
         return False;
      end if;
      return C.Entries (Idx).Is_Default;
   end Has_Default;

   --------------------
   -- Apply_Defaults --
   --------------------

   procedure Apply_Defaults (C : in Out Config_Store) is
   begin
      -- Defaults are already in the store as Is_Default = True
      -- This could be used to reset values to defaults
      for I in 1 .. Max_Config_Entries loop
         if C.Entries (I).Used and C.Entries (I).Is_Default then
            -- Mark as applied (not default anymore)
            C.Entries (I).Is_Default := False;
         end if;
      end loop;
   end Apply_Defaults;

end GNAT.Config;
