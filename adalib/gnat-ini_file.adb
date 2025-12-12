-- GNAT.INI_File body for Z80
-- INI file format implementation

package body GNAT.INI_File is

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

   function Find_Section (D : INI_Data; Name : String) return Natural is
   begin
      for I in 1 .. Max_Sections loop
         if D.Sections (I).Used and then
            Str_Equal (D.Sections (I).Name, D.Sections (I).Name_Len, Name) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Section;

   function Find_Key (D : INI_Data; Section_Idx : Natural;
                      Key : String) return Natural is
   begin
      for I in 1 .. Max_Keys loop
         if D.Keys (I).Used and then
            D.Keys (I).Section = Section_Idx and then
            Str_Equal (D.Keys (I).Key, D.Keys (I).Key_Len, Key) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Key;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (D : out INI_Data) is
   begin
      D.Section_Count := 0;
      D.Key_Count := 0;

      for I in 1 .. Max_Sections loop
         D.Sections (I).Used := False;
         D.Sections (I).Name_Len := 0;
      end loop;

      for I in 1 .. Max_Keys loop
         D.Keys (I).Used := False;
         D.Keys (I).Key_Len := 0;
         D.Keys (I).Value_Len := 0;
         D.Keys (I).Section := 0;
      end loop;
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear (D : out INI_Data) is
   begin
      Initialize (D);
   end Clear;

   -----------------
   -- Add_Section --
   -----------------

   procedure Add_Section (D : in Out INI_Data; Name : String) is
      Len : Natural;
   begin
      -- Check if already exists
      if Find_Section (D, Name) /= 0 then
         return;
      end if;

      -- Find free slot
      for I in 1 .. Max_Sections loop
         if not D.Sections (I).Used then
            D.Sections (I).Name := (others => ' ');
            Len := Name'Length;
            if Len > Max_Name_Len then
               Len := Max_Name_Len;
            end if;
            D.Sections (I).Name_Len := Len;
            for J in 1 .. Len loop
               D.Sections (I).Name (J) := Name (Name'First + J - 1);
            end loop;
            D.Sections (I).Used := True;
            D.Section_Count := D.Section_Count + 1;
            return;
         end if;
      end loop;
   end Add_Section;

   -----------------
   -- Has_Section --
   -----------------

   function Has_Section (D : INI_Data; Name : String) return Boolean is
   begin
      return Find_Section (D, Name) /= 0;
   end Has_Section;

   -------------------
   -- Section_Count --
   -------------------

   function Section_Count (D : INI_Data) return Natural is
   begin
      return D.Section_Count;
   end Section_Count;

   ------------------
   -- Section_Name --
   ------------------

   function Section_Name (D : INI_Data; Index : Positive) return String is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Sections loop
         if D.Sections (I).Used then
            Count := Count + 1;
            if Count = Index then
               return D.Sections (I).Name (1 .. D.Sections (I).Name_Len);
            end if;
         end if;
      end loop;
      return "";
   end Section_Name;

   --------------------
   -- Remove_Section --
   --------------------

   procedure Remove_Section (D : in Out INI_Data; Name : String) is
      Idx : constant Natural := Find_Section (D, Name);
   begin
      if Idx /= 0 then
         D.Sections (Idx).Used := False;
         D.Section_Count := D.Section_Count - 1;

         -- Remove all keys in this section
         for I in 1 .. Max_Keys loop
            if D.Keys (I).Used and D.Keys (I).Section = Idx then
               D.Keys (I).Used := False;
               D.Key_Count := D.Key_Count - 1;
            end if;
         end loop;
      end if;
   end Remove_Section;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (D : in Out INI_Data;
                        Section, Key, Value : String) is
      Sec_Idx : Natural;
      Key_Idx : Natural;
      Len     : Natural;
   begin
      -- Ensure section exists
      Sec_Idx := Find_Section (D, Section);
      if Sec_Idx = 0 then
         Add_Section (D, Section);
         Sec_Idx := Find_Section (D, Section);
         if Sec_Idx = 0 then
            return;  -- Failed to add section
         end if;
      end if;

      -- Find or create key
      Key_Idx := Find_Key (D, Sec_Idx, Key);
      if Key_Idx = 0 then
         -- Find free key slot
         for I in 1 .. Max_Keys loop
            if not D.Keys (I).Used then
               Key_Idx := I;
               D.Keys (I).Section := Sec_Idx;
               D.Key_Count := D.Key_Count + 1;
               exit;
            end if;
         end loop;

         if Key_Idx = 0 then
            return;  -- No free slots
         end if;
      end if;

      -- Store key
      D.Keys (Key_Idx).Key := (others => ' ');
      Len := Key'Length;
      if Len > Max_Name_Len then
         Len := Max_Name_Len;
      end if;
      D.Keys (Key_Idx).Key_Len := Len;
      for I in 1 .. Len loop
         D.Keys (Key_Idx).Key (I) := Key (Key'First + I - 1);
      end loop;

      -- Store value
      D.Keys (Key_Idx).Value := (others => ' ');
      Len := Value'Length;
      if Len > Max_Value_Len then
         Len := Max_Value_Len;
      end if;
      D.Keys (Key_Idx).Value_Len := Len;
      for I in 1 .. Len loop
         D.Keys (Key_Idx).Value (I) := Value (Value'First + I - 1);
      end loop;

      D.Keys (Key_Idx).Used := True;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (D : INI_Data;
                       Section, Key : String) return String is
      Sec_Idx : constant Natural := Find_Section (D, Section);
      Key_Idx : Natural;
   begin
      if Sec_Idx = 0 then
         return "";
      end if;

      Key_Idx := Find_Key (D, Sec_Idx, Key);
      if Key_Idx = 0 then
         return "";
      end if;

      return D.Keys (Key_Idx).Value (1 .. D.Keys (Key_Idx).Value_Len);
   end Get_Value;

   --------------------------
   -- Get_Value_Or_Default --
   --------------------------

   function Get_Value_Or_Default (D : INI_Data;
                                  Section, Key : String;
                                  Default : String) return String is
      Val : constant String := Get_Value (D, Section, Key);
   begin
      if Val'Length = 0 then
         return Default;
      end if;
      return Val;
   end Get_Value_Or_Default;

   -------------
   -- Has_Key --
   -------------

   function Has_Key (D : INI_Data; Section, Key : String) return Boolean is
      Sec_Idx : constant Natural := Find_Section (D, Section);
   begin
      if Sec_Idx = 0 then
         return False;
      end if;
      return Find_Key (D, Sec_Idx, Key) /= 0;
   end Has_Key;

   ----------------
   -- Remove_Key --
   ----------------

   procedure Remove_Key (D : in Out INI_Data; Section, Key : String) is
      Sec_Idx : constant Natural := Find_Section (D, Section);
      Key_Idx : Natural;
   begin
      if Sec_Idx /= 0 then
         Key_Idx := Find_Key (D, Sec_Idx, Key);
         if Key_Idx /= 0 then
            D.Keys (Key_Idx).Used := False;
            D.Key_Count := D.Key_Count - 1;
         end if;
      end if;
   end Remove_Key;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (D : in Out INI_Data;
                      Section, Key : String; Value : Integer) is
      Buf : String (1 .. 12);
      Pos : Natural := 12;
      V   : Integer := abs Value;
   begin
      if V = 0 then
         Set_Value (D, Section, Key, "0");
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

      Set_Value (D, Section, Key, Buf (Pos + 1 .. 12));
   end Set_Int;

   -------------
   -- Get_Int --
   -------------

   function Get_Int (D : INI_Data;
                     Section, Key : String) return Integer is
      Val : constant String := Get_Value (D, Section, Key);
      Result : Integer := 0;
      Negative : Boolean := False;
      Start : Natural;
   begin
      if Val'Length = 0 then
         return 0;
      end if;

      Start := Val'First;
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

   function Get_Int_Or_Default (D : INI_Data;
                                Section, Key : String;
                                Default : Integer) return Integer is
   begin
      if Has_Key (D, Section, Key) then
         return Get_Int (D, Section, Key);
      else
         return Default;
      end if;
   end Get_Int_Or_Default;

   --------------
   -- Set_Bool --
   --------------

   procedure Set_Bool (D : in Out INI_Data;
                       Section, Key : String; Value : Boolean) is
   begin
      if Value then
         Set_Value (D, Section, Key, "true");
      else
         Set_Value (D, Section, Key, "false");
      end if;
   end Set_Bool;

   --------------
   -- Get_Bool --
   --------------

   function Get_Bool (D : INI_Data;
                      Section, Key : String) return Boolean is
      Val : constant String := Get_Value (D, Section, Key);
   begin
      if Val'Length > 0 then
         return Val (Val'First) = '1' or
                Val (Val'First) = 't' or Val (Val'First) = 'T' or
                Val (Val'First) = 'y' or Val (Val'First) = 'Y';
      else
         return False;
      end if;
   end Get_Bool;

   ---------------
   -- Key_Count --
   ---------------

   function Key_Count (D : INI_Data; Section : String) return Natural is
      Sec_Idx : constant Natural := Find_Section (D, Section);
      Count   : Natural := 0;
   begin
      if Sec_Idx = 0 then
         return 0;
      end if;

      for I in 1 .. Max_Keys loop
         if D.Keys (I).Used and D.Keys (I).Section = Sec_Idx then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Key_Count;

   --------------
   -- Key_Name --
   --------------

   function Key_Name (D : INI_Data;
                      Section : String; Index : Positive) return String is
      Sec_Idx : constant Natural := Find_Section (D, Section);
      Count   : Natural := 0;
   begin
      if Sec_Idx = 0 then
         return "";
      end if;

      for I in 1 .. Max_Keys loop
         if D.Keys (I).Used and D.Keys (I).Section = Sec_Idx then
            Count := Count + 1;
            if Count = Index then
               return D.Keys (I).Key (1 .. D.Keys (I).Key_Len);
            end if;
         end if;
      end loop;
      return "";
   end Key_Name;

   ----------------
   -- First_Line --
   ----------------

   function First_Line (D : INI_Data) return Line_Iterator is
      It : Line_Iterator;
   begin
      It.Phase := 0;
      It.Section_Idx := 0;
      It.Key_Idx := 0;
      It.Done := D.Section_Count = 0 and D.Key_Count = 0;

      -- Find first section
      if not It.Done then
         for I in 1 .. Max_Sections loop
            if D.Sections (I).Used then
               It.Section_Idx := I;
               It.Phase := 1;
               exit;
            end if;
         end loop;
      end if;

      return It;
   end First_Line;

   --------------
   -- Has_Line --
   --------------

   function Has_Line (It : Line_Iterator) return Boolean is
   begin
      return not It.Done;
   end Has_Line;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line (D : INI_Data; It : Line_Iterator) return String is
      Result : String (1 .. Max_Name_Len + Max_Value_Len + 4);
      Pos    : Natural := 1;
   begin
      Result := (others => ' ');

      if It.Phase = 1 then
         -- Section header [name]
         Result (Pos) := '[';
         Pos := Pos + 1;
         for I in 1 .. D.Sections (It.Section_Idx).Name_Len loop
            Result (Pos) := D.Sections (It.Section_Idx).Name (I);
            Pos := Pos + 1;
         end loop;
         Result (Pos) := ']';
         Pos := Pos + 1;
      elsif It.Phase = 2 and It.Key_Idx > 0 then
         -- Key=Value
         for I in 1 .. D.Keys (It.Key_Idx).Key_Len loop
            Result (Pos) := D.Keys (It.Key_Idx).Key (I);
            Pos := Pos + 1;
         end loop;
         Result (Pos) := '=';
         Pos := Pos + 1;
         for I in 1 .. D.Keys (It.Key_Idx).Value_Len loop
            Result (Pos) := D.Keys (It.Key_Idx).Value (I);
            Pos := Pos + 1;
         end loop;
      end if;

      return Result (1 .. Pos - 1);
   end Current_Line;

   ---------------
   -- Next_Line --
   ---------------

   procedure Next_Line (D : INI_Data; It : in Out Line_Iterator) is
   begin
      if It.Done then
         return;
      end if;

      if It.Phase = 1 then
         -- Just output section header, now output keys
         It.Phase := 2;
         It.Key_Idx := 0;

         -- Find first key in this section
         for I in 1 .. Max_Keys loop
            if D.Keys (I).Used and D.Keys (I).Section = It.Section_Idx then
               It.Key_Idx := I;
               return;
            end if;
         end loop;

         -- No keys in section, move to next section
         It.Phase := 1;
         for I in It.Section_Idx + 1 .. Max_Sections loop
            if D.Sections (I).Used then
               It.Section_Idx := I;
               return;
            end if;
         end loop;

         It.Done := True;
      elsif It.Phase = 2 then
         -- Find next key in this section
         for I in It.Key_Idx + 1 .. Max_Keys loop
            if D.Keys (I).Used and D.Keys (I).Section = It.Section_Idx then
               It.Key_Idx := I;
               return;
            end if;
         end loop;

         -- No more keys, move to next section
         It.Phase := 1;
         for I in It.Section_Idx + 1 .. Max_Sections loop
            if D.Sections (I).Used then
               It.Section_Idx := I;
               return;
            end if;
         end loop;

         It.Done := True;
      end if;
   end Next_Line;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line (D : in Out INI_Data;
                        Line : String;
                        Current_Section : in Out String) return Boolean is
      Trimmed : constant String := Trim (Line);
   begin
      if Trimmed'Length = 0 then
         return True;  -- Empty line is OK
      end if;

      if Is_Comment_Line (Trimmed) then
         return True;  -- Comment is OK
      end if;

      if Is_Section_Line (Trimmed) then
         declare
            Sec_Name : constant String := Extract_Section_Name (Trimmed);
         begin
            if Sec_Name'Length > 0 then
               Add_Section (D, Sec_Name);
               -- Update current section (caller's buffer)
               for I in Current_Section'Range loop
                  if I - Current_Section'First < Sec_Name'Length then
                     Current_Section (I) := Sec_Name (Sec_Name'First + I - Current_Section'First);
                  else
                     Current_Section (I) := ' ';
                  end if;
               end loop;
               return True;
            end if;
         end;
      end if;

      if Is_Key_Value_Line (Trimmed) then
         declare
            K : constant String := Extract_Key (Trimmed);
            V : constant String := Extract_Value (Trimmed);
            Sec : String (1 .. Max_Name_Len);
            Sec_Len : Natural := 0;
         begin
            -- Get current section name (trim spaces)
            for I in Current_Section'Range loop
               exit when Current_Section (I) = ' ';
               Sec_Len := Sec_Len + 1;
               Sec (Sec_Len) := Current_Section (I);
            end loop;

            if K'Length > 0 then
               Set_Value (D, Sec (1 .. Sec_Len), K, V);
               return True;
            end if;
         end;
      end if;

      return False;  -- Unrecognized line
   end Parse_Line;

   ---------------------
   -- Is_Section_Line --
   ---------------------

   function Is_Section_Line (Line : String) return Boolean is
   begin
      if Line'Length < 2 then
         return False;
      end if;
      return Line (Line'First) = '[' and Line (Line'Last) = ']';
   end Is_Section_Line;

   -----------------------
   -- Is_Key_Value_Line --
   -----------------------

   function Is_Key_Value_Line (Line : String) return Boolean is
   begin
      for I in Line'Range loop
         if Line (I) = '=' then
            return I > Line'First;  -- Must have something before '='
         end if;
      end loop;
      return False;
   end Is_Key_Value_Line;

   ---------------------
   -- Is_Comment_Line --
   ---------------------

   function Is_Comment_Line (Line : String) return Boolean is
   begin
      if Line'Length = 0 then
         return False;
      end if;
      return Line (Line'First) = ';' or Line (Line'First) = '#';
   end Is_Comment_Line;

   --------------------------
   -- Extract_Section_Name --
   --------------------------

   function Extract_Section_Name (Line : String) return String is
   begin
      if Line'Length < 3 then
         return "";
      end if;
      if Line (Line'First) = '[' and Line (Line'Last) = ']' then
         return Line (Line'First + 1 .. Line'Last - 1);
      end if;
      return "";
   end Extract_Section_Name;

   -----------------
   -- Extract_Key --
   -----------------

   function Extract_Key (Line : String) return String is
      Eq_Pos : Natural := 0;
   begin
      for I in Line'Range loop
         if Line (I) = '=' then
            Eq_Pos := I;
            exit;
         end if;
      end loop;

      if Eq_Pos = 0 or Eq_Pos = Line'First then
         return "";
      end if;

      return Trim (Line (Line'First .. Eq_Pos - 1));
   end Extract_Key;

   -------------------
   -- Extract_Value --
   -------------------

   function Extract_Value (Line : String) return String is
      Eq_Pos : Natural := 0;
   begin
      for I in Line'Range loop
         if Line (I) = '=' then
            Eq_Pos := I;
            exit;
         end if;
      end loop;

      if Eq_Pos = 0 or Eq_Pos = Line'Last then
         return "";
      end if;

      return Trim (Line (Eq_Pos + 1 .. Line'Last));
   end Extract_Value;

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String is
   begin
      return Trim_Right (Trim_Left (S));
   end Trim;

   ---------------
   -- Trim_Left --
   ---------------

   function Trim_Left (S : String) return String is
      Start : Natural := S'First;
   begin
      while Start <= S'Last and then
            (S (Start) = ' ' or S (Start) = Character'Val (9)) loop
         Start := Start + 1;
      end loop;
      if Start > S'Last then
         return "";
      end if;
      return S (Start .. S'Last);
   end Trim_Left;

   ----------------
   -- Trim_Right --
   ----------------

   function Trim_Right (S : String) return String is
      Finish : Natural := S'Last;
   begin
      while Finish >= S'First and then
            (S (Finish) = ' ' or S (Finish) = Character'Val (9)) loop
         Finish := Finish - 1;
      end loop;
      if Finish < S'First then
         return "";
      end if;
      return S (S'First .. Finish);
   end Trim_Right;

end GNAT.INI_File;
