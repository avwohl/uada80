-- GNAT.INI_Files body for Z80
-- Simple INI file parser implementation

package body GNAT.INI_Files is

   function Match_Name (N : Name_String; Len : Natural; S : String) return Boolean is
   begin
      if Len /= S'Length then
         return False;
      end if;
      for I in 1 .. Len loop
         -- Case insensitive comparison
         declare
            C1 : Character := N (I);
            C2 : Character := S (S'First + I - 1);
         begin
            if C1 in 'a' .. 'z' then
               C1 := Character'Val (Character'Pos (C1) - 32);
            end if;
            if C2 in 'a' .. 'z' then
               C2 := Character'Val (Character'Pos (C2) - 32);
            end if;
            if C1 /= C2 then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Match_Name;

   function Find_Section (F : INI_File; Name : String) return Natural is
   begin
      for I in 1 .. F.Sec_Count loop
         if F.Sections (I).Valid and then
           Match_Name (F.Sections (I).Name, F.Sections (I).Name_Len, Name)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Section;

   function Find_Key (F : INI_File; Sec_Idx : Natural; Key : String) return Natural is
   begin
      for I in 1 .. F.Key_Count loop
         if F.Keys (I).Valid and then F.Keys (I).Section = Sec_Idx and then
           Match_Name (F.Keys (I).Name, F.Keys (I).Name_Len, Key)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Key;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (F : out INI_File) is
   begin
      F.Sections := (others => (Name => (others => ASCII.NUL),
                                Name_Len => 0,
                                Valid => False));
      F.Sec_Count := 0;
      F.Keys := (others => (Name => (others => ASCII.NUL),
                            Name_Len => 0,
                            Value => (others => ASCII.NUL),
                            Value_Len => 0,
                            Section => 0,
                            Valid => False));
      F.Key_Count := 0;
      F.Has_Err := False;
      F.Err_Line := 0;
   end Initialize;

   -----------
   -- Parse --
   -----------

   procedure Parse (F : in Out INI_File; Content : String) is
      I            : Positive := Content'First;
      Line_Start   : Positive;
      Line_End     : Natural;
      Line_Num     : Natural := 0;
      Current_Sec  : Natural := 0;  -- 0 = global/no section
   begin
      Initialize (F);

      while I <= Content'Last loop
         Line_Num := Line_Num + 1;
         Line_Start := I;

         -- Find end of line
         Line_End := I - 1;
         while I <= Content'Last and then
           Content (I) /= ASCII.CR and then Content (I) /= ASCII.LF
         loop
            Line_End := I;
            I := I + 1;
         end loop;

         -- Skip line ending
         while I <= Content'Last and then
           (Content (I) = ASCII.CR or Content (I) = ASCII.LF)
         loop
            I := I + 1;
         end loop;

         -- Process line
         if Line_End >= Line_Start then
            declare
               Line : String renames Content (Line_Start .. Line_End);
               First_Char : Positive := Line'First;
            begin
               -- Skip leading whitespace
               while First_Char <= Line'Last and then
                 (Line (First_Char) = ' ' or Line (First_Char) = ASCII.HT)
               loop
                  First_Char := First_Char + 1;
               end loop;

               if First_Char <= Line'Last then
                  if Line (First_Char) = ';' or Line (First_Char) = '#' then
                     -- Comment - skip
                     null;

                  elsif Line (First_Char) = '[' then
                     -- Section header
                     declare
                        Bracket_End : Natural := First_Char + 1;
                     begin
                        while Bracket_End <= Line'Last and then
                          Line (Bracket_End) /= ']'
                        loop
                           Bracket_End := Bracket_End + 1;
                        end loop;

                        if Bracket_End <= Line'Last and F.Sec_Count < Max_Sections then
                           F.Sec_Count := F.Sec_Count + 1;
                           Current_Sec := F.Sec_Count;

                           declare
                              Name_Len : constant Natural :=
                                Natural'Min (Bracket_End - First_Char - 1, Max_Name_Length);
                           begin
                              F.Sections (Current_Sec).Name := (others => ASCII.NUL);
                              for J in 1 .. Name_Len loop
                                 F.Sections (Current_Sec).Name (J) :=
                                   Line (First_Char + J);
                              end loop;
                              F.Sections (Current_Sec).Name_Len := Name_Len;
                              F.Sections (Current_Sec).Valid := True;
                           end;
                        end if;
                     end;

                  else
                     -- Key=Value
                     declare
                        Eq_Pos : Natural := First_Char;
                     begin
                        while Eq_Pos <= Line'Last and then Line (Eq_Pos) /= '=' loop
                           Eq_Pos := Eq_Pos + 1;
                        end loop;

                        if Eq_Pos <= Line'Last and F.Key_Count < Max_Keys then
                           F.Key_Count := F.Key_Count + 1;

                           -- Trim key name
                           declare
                              Key_End : Natural := Eq_Pos - 1;
                           begin
                              while Key_End >= First_Char and then
                                (Line (Key_End) = ' ' or Line (Key_End) = ASCII.HT)
                              loop
                                 Key_End := Key_End - 1;
                              end loop;

                              declare
                                 Key_Len : constant Natural :=
                                   Natural'Min (Key_End - First_Char + 1, Max_Name_Length);
                              begin
                                 F.Keys (F.Key_Count).Name := (others => ASCII.NUL);
                                 for J in 1 .. Key_Len loop
                                    F.Keys (F.Key_Count).Name (J) :=
                                      Line (First_Char + J - 1);
                                 end loop;
                                 F.Keys (F.Key_Count).Name_Len := Key_Len;
                              end;
                           end;

                           -- Trim value
                           declare
                              Val_Start : Natural := Eq_Pos + 1;
                              Val_End   : Natural := Line'Last;
                           begin
                              while Val_Start <= Val_End and then
                                (Line (Val_Start) = ' ' or Line (Val_Start) = ASCII.HT)
                              loop
                                 Val_Start := Val_Start + 1;
                              end loop;

                              while Val_End >= Val_Start and then
                                (Line (Val_End) = ' ' or Line (Val_End) = ASCII.HT)
                              loop
                                 Val_End := Val_End - 1;
                              end loop;

                              declare
                                 Val_Len : constant Natural :=
                                   Natural'Min (Val_End - Val_Start + 1, Max_Value_Length);
                              begin
                                 F.Keys (F.Key_Count).Value := (others => ASCII.NUL);
                                 if Val_Start <= Val_End then
                                    for J in 1 .. Val_Len loop
                                       F.Keys (F.Key_Count).Value (J) :=
                                         Line (Val_Start + J - 1);
                                    end loop;
                                 end if;
                                 F.Keys (F.Key_Count).Value_Len := Val_Len;
                              end;
                           end;

                           F.Keys (F.Key_Count).Section := Current_Sec;
                           F.Keys (F.Key_Count).Valid := True;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Parse;

   -------------------
   -- Section_Count --
   -------------------

   function Section_Count (F : INI_File) return Natural is
   begin
      return F.Sec_Count;
   end Section_Count;

   ------------------
   -- Section_Name --
   ------------------

   function Section_Name (F : INI_File; Index : Positive) return String is
   begin
      if Index <= F.Sec_Count and then F.Sections (Index).Valid then
         return String (F.Sections (Index).Name (1 .. F.Sections (Index).Name_Len));
      else
         return "";
      end if;
   end Section_Name;

   -----------------
   -- Has_Section --
   -----------------

   function Has_Section (F : INI_File; Section : String) return Boolean is
   begin
      return Find_Section (F, Section) > 0;
   end Has_Section;

   ---------------
   -- Key_Count --
   ---------------

   function Key_Count (F : INI_File; Section : String) return Natural is
      Sec_Idx : constant Natural := Find_Section (F, Section);
      Count   : Natural := 0;
   begin
      for I in 1 .. F.Key_Count loop
         if F.Keys (I).Valid and then F.Keys (I).Section = Sec_Idx then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Key_Count;

   -------------
   -- Has_Key --
   -------------

   function Has_Key (F : INI_File; Section, Key : String) return Boolean is
      Sec_Idx : constant Natural := Find_Section (F, Section);
   begin
      return Find_Key (F, Sec_Idx, Key) > 0;
   end Has_Key;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (F : INI_File; Section, Key : String) return String is
      Sec_Idx : constant Natural := Find_Section (F, Section);
      Key_Idx : constant Natural := Find_Key (F, Sec_Idx, Key);
   begin
      if Key_Idx > 0 then
         return String (F.Keys (Key_Idx).Value (1 .. F.Keys (Key_Idx).Value_Len));
      else
         return "";
      end if;
   end Get_Value;

   function Get_Value (F : INI_File; Section, Key : String;
                       Default : String) return String
   is
      V : constant String := Get_Value (F, Section, Key);
   begin
      if V'Length > 0 then
         return V;
      else
         return Default;
      end if;
   end Get_Value;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (F : INI_File; Section, Key : String;
                         Default : Integer := 0) return Integer
   is
      V : constant String := Get_Value (F, Section, Key);
      Result : Integer := 0;
      Neg    : Boolean := False;
      Start  : Positive := V'First;
   begin
      if V'Length = 0 then
         return Default;
      end if;

      if V (Start) = '-' then
         Neg := True;
         Start := Start + 1;
      elsif V (Start) = '+' then
         Start := Start + 1;
      end if;

      for I in Start .. V'Last loop
         if V (I) in '0' .. '9' then
            Result := Result * 10 + Character'Pos (V (I)) - Character'Pos ('0');
         else
            exit;
         end if;
      end loop;

      if Neg then
         return -Result;
      else
         return Result;
      end if;
   end Get_Integer;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (F : INI_File; Section, Key : String;
                         Default : Boolean := False) return Boolean
   is
      V : constant String := Get_Value (F, Section, Key);
   begin
      if V'Length = 0 then
         return Default;
      end if;

      if V = "true" or V = "True" or V = "TRUE" or
        V = "yes" or V = "Yes" or V = "YES" or
        V = "1" or V = "on" or V = "On" or V = "ON"
      then
         return True;
      else
         return False;
      end if;
   end Get_Boolean;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (F : in Out INI_File; Section, Key, Value : String) is
      Sec_Idx : Natural := Find_Section (F, Section);
      Key_Idx : Natural;
   begin
      -- Create section if needed
      if Sec_Idx = 0 and F.Sec_Count < Max_Sections then
         F.Sec_Count := F.Sec_Count + 1;
         Sec_Idx := F.Sec_Count;
         F.Sections (Sec_Idx).Name := (others => ASCII.NUL);
         for I in 1 .. Natural'Min (Section'Length, Max_Name_Length) loop
            F.Sections (Sec_Idx).Name (I) := Section (Section'First + I - 1);
         end loop;
         F.Sections (Sec_Idx).Name_Len := Natural'Min (Section'Length, Max_Name_Length);
         F.Sections (Sec_Idx).Valid := True;
      end if;

      if Sec_Idx = 0 then
         return;
      end if;

      Key_Idx := Find_Key (F, Sec_Idx, Key);

      if Key_Idx = 0 and F.Key_Count < Max_Keys then
         F.Key_Count := F.Key_Count + 1;
         Key_Idx := F.Key_Count;
         F.Keys (Key_Idx).Name := (others => ASCII.NUL);
         for I in 1 .. Natural'Min (Key'Length, Max_Name_Length) loop
            F.Keys (Key_Idx).Name (I) := Key (Key'First + I - 1);
         end loop;
         F.Keys (Key_Idx).Name_Len := Natural'Min (Key'Length, Max_Name_Length);
         F.Keys (Key_Idx).Section := Sec_Idx;
         F.Keys (Key_Idx).Valid := True;
      end if;

      if Key_Idx > 0 then
         F.Keys (Key_Idx).Value := (others => ASCII.NUL);
         for I in 1 .. Natural'Min (Value'Length, Max_Value_Length) loop
            F.Keys (Key_Idx).Value (I) := Value (Value'First + I - 1);
         end loop;
         F.Keys (Key_Idx).Value_Len := Natural'Min (Value'Length, Max_Value_Length);
      end if;
   end Set_Value;

   ----------------
   -- Remove_Key --
   ----------------

   procedure Remove_Key (F : in Out INI_File; Section, Key : String) is
      Sec_Idx : constant Natural := Find_Section (F, Section);
      Key_Idx : constant Natural := Find_Key (F, Sec_Idx, Key);
   begin
      if Key_Idx > 0 then
         F.Keys (Key_Idx).Valid := False;
      end if;
   end Remove_Key;

   --------------------
   -- Remove_Section --
   --------------------

   procedure Remove_Section (F : in Out INI_File; Section : String) is
      Sec_Idx : constant Natural := Find_Section (F, Section);
   begin
      if Sec_Idx > 0 then
         F.Sections (Sec_Idx).Valid := False;
         -- Also invalidate all keys in this section
         for I in 1 .. F.Key_Count loop
            if F.Keys (I).Section = Sec_Idx then
               F.Keys (I).Valid := False;
            end if;
         end loop;
      end if;
   end Remove_Section;

   ---------------
   -- To_String --
   ---------------

   function To_String (F : INI_File) return String is
      Result : String (1 .. 2048);  -- Large enough buffer
      Pos    : Natural := 0;

      procedure Add (S : String) is
      begin
         for C of S loop
            if Pos < Result'Last then
               Pos := Pos + 1;
               Result (Pos) := C;
            end if;
         end loop;
      end Add;

      procedure Add_NL is
      begin
         Add (ASCII.CR & ASCII.LF);
      end Add_NL;

   begin
      -- Output global keys (section = 0)
      for K in 1 .. F.Key_Count loop
         if F.Keys (K).Valid and then F.Keys (K).Section = 0 then
            Add (String (F.Keys (K).Name (1 .. F.Keys (K).Name_Len)));
            Add ("=");
            Add (String (F.Keys (K).Value (1 .. F.Keys (K).Value_Len)));
            Add_NL;
         end if;
      end loop;

      -- Output each section
      for S in 1 .. F.Sec_Count loop
         if F.Sections (S).Valid then
            Add ("[");
            Add (String (F.Sections (S).Name (1 .. F.Sections (S).Name_Len)));
            Add ("]");
            Add_NL;

            for K in 1 .. F.Key_Count loop
               if F.Keys (K).Valid and then F.Keys (K).Section = S then
                  Add (String (F.Keys (K).Name (1 .. F.Keys (K).Name_Len)));
                  Add ("=");
                  Add (String (F.Keys (K).Value (1 .. F.Keys (K).Value_Len)));
                  Add_NL;
               end if;
            end loop;
         end if;
      end loop;

      return Result (1 .. Pos);
   end To_String;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (F : INI_File) return Boolean is
   begin
      return F.Has_Err;
   end Has_Error;

   ----------------
   -- Error_Line --
   ----------------

   function Error_Line (F : INI_File) return Natural is
   begin
      return F.Err_Line;
   end Error_Line;

end GNAT.INI_Files;
