-- GNAT.Menu body for Z80
-- Menu system implementation

package body GNAT.Menu is

   LF : constant Character := Character'Val (10);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (M : out Menu; Title : String := "") is
   begin
      M.Count := 0;
      M.Title := (others => ' ');
      M.Title_Len := Title'Length;
      if M.Title_Len > Max_Title_Len then
         M.Title_Len := Max_Title_Len;
      end if;
      for I in 1 .. M.Title_Len loop
         M.Title (I) := Title (Title'First + I - 1);
      end loop;
   end Initialize;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (M : in Out Menu; Title : String) is
   begin
      M.Title := (others => ' ');
      M.Title_Len := Title'Length;
      if M.Title_Len > Max_Title_Len then
         M.Title_Len := Max_Title_Len;
      end if;
      for I in 1 .. M.Title_Len loop
         M.Title (I) := Title (Title'First + I - 1);
      end loop;
   end Set_Title;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item (M : in Out Menu; Label : String; Key : Character := ' ') is
   begin
      Add_Item (M, Label, Key, True);
   end Add_Item;

   procedure Add_Item (M : in Out Menu; Label : String; Key : Character;
                       Enabled : Boolean) is
   begin
      if M.Count < Max_Items then
         M.Count := M.Count + 1;
         M.Items (M.Count).Label := (others => ' ');
         M.Items (M.Count).Len := Label'Length;
         if M.Items (M.Count).Len > Max_Item_Len then
            M.Items (M.Count).Len := Max_Item_Len;
         end if;
         for I in 1 .. M.Items (M.Count).Len loop
            M.Items (M.Count).Label (I) := Label (Label'First + I - 1);
         end loop;
         M.Items (M.Count).Key := Key;
         M.Items (M.Count).Enabled := Enabled;
      end if;
   end Add_Item;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items (M : out Menu) is
   begin
      M.Count := 0;
   end Clear_Items;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled (M : in Out Menu; Index : Positive; Enabled : Boolean) is
   begin
      if Index <= M.Count then
         M.Items (Index).Enabled := Enabled;
      end if;
   end Set_Enabled;

   ----------------
   -- Item_Count --
   ----------------

   function Item_Count (M : Menu) return Natural is
   begin
      return M.Count;
   end Item_Count;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (M : Menu) return String is
   begin
      return M.Title (1 .. M.Title_Len);
   end Get_Title;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (M : Menu; Index : Positive) return String is
   begin
      if Index <= M.Count then
         return M.Items (Index).Label (1 .. M.Items (Index).Len);
      else
         return "";
      end if;
   end Get_Label;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (M : Menu; Index : Positive) return Character is
   begin
      if Index <= M.Count then
         return M.Items (Index).Key;
      else
         return ' ';
      end if;
   end Get_Key;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (M : Menu; Index : Positive) return Boolean is
   begin
      if Index <= M.Count then
         return M.Items (Index).Enabled;
      else
         return False;
      end if;
   end Is_Enabled;

   -----------------
   -- Find_By_Key --
   -----------------

   function Find_By_Key (M : Menu; Key : Character) return Natural is
      UC : Character := Key;
   begin
      -- Convert to uppercase for comparison
      if Key >= 'a' and Key <= 'z' then
         UC := Character'Val (Character'Pos (Key) - 32);
      end if;

      for I in 1 .. M.Count loop
         declare
            Item_Key : Character := M.Items (I).Key;
         begin
            if Item_Key >= 'a' and Item_Key <= 'z' then
               Item_Key := Character'Val (Character'Pos (Item_Key) - 32);
            end if;
            if Item_Key = UC and M.Items (I).Enabled then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end Find_By_Key;

   ----------------
   -- Menu_Width --
   ----------------

   function Menu_Width (M : Menu) return Natural is
      Width : Natural := M.Title_Len + 4;  -- Title + borders
   begin
      for I in 1 .. M.Count loop
         declare
            Item_Width : Natural := M.Items (I).Len + 8;  -- Number + label + padding
         begin
            if M.Items (I).Key /= ' ' then
               Item_Width := Item_Width + 4;  -- [X] key display
            end if;
            if Item_Width > Width then
               Width := Item_Width;
            end if;
         end;
      end loop;

      if Width < 20 then
         Width := 20;
      end if;

      return Width;
   end Menu_Width;

   ----------------
   -- Top_Border --
   ----------------

   function Top_Border (M : Menu) return String is
      Width : constant Natural := Menu_Width (M);
      Result : String (1 .. Width);
   begin
      Result (1) := '+';
      for I in 2 .. Width - 1 loop
         Result (I) := '-';
      end loop;
      Result (Width) := '+';
      return Result;
   end Top_Border;

   -------------------
   -- Bottom_Border --
   -------------------

   function Bottom_Border (M : Menu) return String is
   begin
      return Top_Border (M);
   end Bottom_Border;

   ----------------
   -- Title_Line --
   ----------------

   function Title_Line (M : Menu) return String is
      Width  : constant Natural := Menu_Width (M);
      Result : String (1 .. Width);
      Start  : Natural;
   begin
      Result := (others => ' ');
      Result (1) := '|';
      Result (Width) := '|';

      if M.Title_Len > 0 then
         Start := (Width - M.Title_Len) / 2;
         for I in 1 .. M.Title_Len loop
            Result (Start + I) := M.Title (I);
         end loop;
      end if;

      return Result;
   end Title_Line;

   ---------------
   -- Separator --
   ---------------

   function Separator (M : Menu) return String is
      Width  : constant Natural := Menu_Width (M);
      Result : String (1 .. Width);
   begin
      Result (1) := '|';
      for I in 2 .. Width - 1 loop
         Result (I) := '-';
      end loop;
      Result (Width) := '|';
      return Result;
   end Separator;

   ---------------
   -- Item_Line --
   ---------------

   function Item_Line (M : Menu; Index : Positive) return String is
      Width  : constant Natural := Menu_Width (M);
      Result : String (1 .. Width);
      Pos    : Natural := 3;
      Num_Str : constant String := Natural'Image (Index);
   begin
      Result := (others => ' ');
      Result (1) := '|';
      Result (Width) := '|';

      if Index > M.Count then
         return Result;
      end if;

      -- Item number
      for C of Num_Str (Num_Str'First + 1 .. Num_Str'Last) loop
         Result (Pos) := C;
         Pos := Pos + 1;
      end loop;
      Result (Pos) := '.';
      Pos := Pos + 2;

      -- Show key shortcut if present
      if M.Items (Index).Key /= ' ' then
         Result (Pos) := '[';
         Result (Pos + 1) := M.Items (Index).Key;
         Result (Pos + 2) := ']';
         Pos := Pos + 4;
      end if;

      -- Label
      if M.Items (Index).Enabled then
         for I in 1 .. M.Items (Index).Len loop
            exit when Pos >= Width - 1;
            Result (Pos) := M.Items (Index).Label (I);
            Pos := Pos + 1;
         end loop;
      else
         -- Disabled items shown with brackets
         Result (Pos) := '(';
         Pos := Pos + 1;
         for I in 1 .. M.Items (Index).Len loop
            exit when Pos >= Width - 2;
            Result (Pos) := M.Items (Index).Label (I);
            Pos := Pos + 1;
         end loop;
         if Pos < Width - 1 then
            Result (Pos) := ')';
         end if;
      end if;

      return Result;
   end Item_Line;

   -----------------
   -- Prompt_Line --
   -----------------

   function Prompt_Line (M : Menu; Prompt : String := "Choice: ") return String is
      pragma Unreferenced (M);
   begin
      return Prompt;
   end Prompt_Line;

   ------------
   -- Render --
   ------------

   function Render (M : Menu) return String is
      Width     : constant Natural := Menu_Width (M);
      -- Calculate total size: borders + title + separator + items + bottom
      Total_Len : constant Natural :=
        (Width + 1) * (4 + M.Count);  -- +1 for LF each line
      Result    : String (1 .. Total_Len);
      Pos       : Natural := 1;

      procedure Add_Line (S : String) is
      begin
         for C of S loop
            exit when Pos > Total_Len;
            Result (Pos) := C;
            Pos := Pos + 1;
         end loop;
         if Pos <= Total_Len then
            Result (Pos) := LF;
            Pos := Pos + 1;
         end if;
      end Add_Line;

   begin
      Result := (others => ' ');

      Add_Line (Top_Border (M));
      if M.Title_Len > 0 then
         Add_Line (Title_Line (M));
         Add_Line (Separator (M));
      end if;

      for I in 1 .. M.Count loop
         Add_Line (Item_Line (M, I));
      end loop;

      Add_Line (Bottom_Border (M));

      return Result (1 .. Pos - 1);
   end Render;

   ---------------------
   -- Parse_Selection --
   ---------------------

   function Parse_Selection (M : Menu; Input : String) return Selection_Result is
      Result : Selection_Result := (Index => 0, Key => ' ', Valid => False);
      Value  : Natural := 0;
   begin
      if Input'Length = 0 then
         return Result;
      end if;

      -- Try parsing as number first
      for C of Input loop
         if C >= '0' and C <= '9' then
            Value := Value * 10 + (Character'Pos (C) - Character'Pos ('0'));
         elsif C /= ' ' then
            -- Not a number, try as key
            Result.Index := Find_By_Key (M, Input (Input'First));
            if Result.Index > 0 then
               Result.Key := Input (Input'First);
               Result.Valid := True;
            end if;
            return Result;
         end if;
      end loop;

      -- Valid number input
      if Value >= 1 and Value <= M.Count then
         if M.Items (Value).Enabled then
            Result.Index := Value;
            Result.Key := M.Items (Value).Key;
            Result.Valid := True;
         end if;
      end if;

      return Result;
   end Parse_Selection;

   ------------------------
   -- Is_Valid_Selection --
   ------------------------

   function Is_Valid_Selection (M : Menu; Input : String) return Boolean is
      Result : constant Selection_Result := Parse_Selection (M, Input);
   begin
      return Result.Valid;
   end Is_Valid_Selection;

end GNAT.Menu;
