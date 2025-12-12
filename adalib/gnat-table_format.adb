-- GNAT.Table_Format body for Z80
-- Table formatting implementation

package body GNAT.Table_Format is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : out Table; Columns : Positive) is
   begin
      T.Col_Count := Columns;
      if T.Col_Count > Max_Columns then
         T.Col_Count := Max_Columns;
      end if;

      for I in 1 .. T.Col_Count loop
         T.Columns (I).Width := 10;
         T.Columns (I).Align := Left;
         T.Columns (I).Title := (others => ' ');
         T.Columns (I).Title_Len := 0;
      end loop;
   end Initialize;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column (T : in Out Table;
                         Col : Positive;
                         Width : Positive;
                         Align : Alignment := Left;
                         Title : String := "") is
   begin
      if Col <= T.Col_Count then
         T.Columns (Col).Width := Width;
         if T.Columns (Col).Width > Max_Col_Width then
            T.Columns (Col).Width := Max_Col_Width;
         end if;
         T.Columns (Col).Align := Align;

         T.Columns (Col).Title := (others => ' ');
         T.Columns (Col).Title_Len := Title'Length;
         if T.Columns (Col).Title_Len > Max_Col_Width then
            T.Columns (Col).Title_Len := Max_Col_Width;
         end if;
         for I in 1 .. T.Columns (Col).Title_Len loop
            T.Columns (Col).Title (I) := Title (Title'First + I - 1);
         end loop;
      end if;
   end Set_Column;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (T : in Out Table; Col : Positive; Title : String) is
   begin
      if Col <= T.Col_Count then
         T.Columns (Col).Title := (others => ' ');
         T.Columns (Col).Title_Len := Title'Length;
         if T.Columns (Col).Title_Len > Max_Col_Width then
            T.Columns (Col).Title_Len := Max_Col_Width;
         end if;
         for I in 1 .. T.Columns (Col).Title_Len loop
            T.Columns (Col).Title (I) := Title (Title'First + I - 1);
         end loop;
      end if;
   end Set_Title;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width (T : in Out Table; Col : Positive; Width : Positive) is
   begin
      if Col <= T.Col_Count then
         T.Columns (Col).Width := Width;
         if T.Columns (Col).Width > Max_Col_Width then
            T.Columns (Col).Width := Max_Col_Width;
         end if;
      end if;
   end Set_Width;

   ---------------
   -- Set_Align --
   ---------------

   procedure Set_Align (T : in Out Table; Col : Positive; Align : Alignment) is
   begin
      if Col <= T.Col_Count then
         T.Columns (Col).Align := Align;
      end if;
   end Set_Align;

   ---------
   -- Pad --
   ---------

   function Pad (S : String; Width : Positive; Align : Alignment) return String is
      Result : String (1 .. Width) := (others => ' ');
      Len    : Natural := S'Length;
      Start  : Positive;
   begin
      if Len > Width then
         Len := Width;
      end if;

      case Align is
         when Left =>
            for I in 1 .. Len loop
               Result (I) := S (S'First + I - 1);
            end loop;

         when Right =>
            Start := Width - Len + 1;
            for I in 1 .. Len loop
               Result (Start + I - 1) := S (S'First + I - 1);
            end loop;

         when Center =>
            Start := (Width - Len) / 2 + 1;
            for I in 1 .. Len loop
               Result (Start + I - 1) := S (S'First + I - 1);
            end loop;
      end case;

      return Result;
   end Pad;

   -----------------
   -- Format_Cell --
   -----------------

   function Format_Cell (T : Table; Col : Positive; Value : String) return String is
   begin
      if Col <= T.Col_Count then
         return Pad (Value, T.Columns (Col).Width, T.Columns (Col).Align);
      else
         return "";
      end if;
   end Format_Cell;

   function Format_Cell (T : Table; Col : Positive; Value : Integer) return String is
      S : constant String := Integer'Image (Value);
   begin
      if Value >= 0 then
         return Format_Cell (T, Col, S (S'First + 1 .. S'Last));
      else
         return Format_Cell (T, Col, S);
      end if;
   end Format_Cell;

   ----------------
   -- Format_Row --
   ----------------

   function Format_Row (T : Table; Values : String) return String is
      Result : String (1 .. Max_Row_Width);
      Pos    : Natural := 1;
      Col    : Natural := 1;
      Start  : Natural := Values'First;
      Idx    : Natural;
   begin
      Result := (others => ' ');

      -- Add leading separator
      if Pos <= Max_Row_Width then
         Result (Pos) := '|';
         Pos := Pos + 1;
      end if;

      -- Parse values separated by '|'
      Idx := Values'First;
      while Idx <= Values'Last and Col <= T.Col_Count loop
         if Values (Idx) = '|' or Idx = Values'Last then
            declare
               End_Idx : constant Natural :=
                 (if Idx = Values'Last and Values (Idx) /= '|'
                  then Idx else Idx - 1);
               Cell_Value : constant String := Values (Start .. End_Idx);
               Formatted  : constant String :=
                 Pad (Cell_Value, T.Columns (Col).Width, T.Columns (Col).Align);
            begin
               for C of Formatted loop
                  exit when Pos > Max_Row_Width;
                  Result (Pos) := C;
                  Pos := Pos + 1;
               end loop;

               if Pos <= Max_Row_Width then
                  Result (Pos) := '|';
                  Pos := Pos + 1;
               end if;
            end;

            Col := Col + 1;
            Start := Idx + 1;
         end if;
         Idx := Idx + 1;
      end loop;

      -- Fill remaining columns if any
      while Col <= T.Col_Count loop
         declare
            Formatted : constant String :=
              Pad ("", T.Columns (Col).Width, T.Columns (Col).Align);
         begin
            for C of Formatted loop
               exit when Pos > Max_Row_Width;
               Result (Pos) := C;
               Pos := Pos + 1;
            end loop;

            if Pos <= Max_Row_Width then
               Result (Pos) := '|';
               Pos := Pos + 1;
            end if;
         end;
         Col := Col + 1;
      end loop;

      return Result (1 .. Pos - 1);
   end Format_Row;

   -----------------
   -- Header_Line --
   -----------------

   function Header_Line (T : Table) return String is
      Result : String (1 .. Max_Row_Width);
      Pos    : Natural := 1;
   begin
      Result := (others => ' ');

      if Pos <= Max_Row_Width then
         Result (Pos) := '|';
         Pos := Pos + 1;
      end if;

      for Col in 1 .. T.Col_Count loop
         declare
            Title : constant String :=
              T.Columns (Col).Title (1 .. T.Columns (Col).Title_Len);
            Formatted : constant String :=
              Pad (Title, T.Columns (Col).Width, Center);
         begin
            for C of Formatted loop
               exit when Pos > Max_Row_Width;
               Result (Pos) := C;
               Pos := Pos + 1;
            end loop;

            if Pos <= Max_Row_Width then
               Result (Pos) := '|';
               Pos := Pos + 1;
            end if;
         end;
      end loop;

      return Result (1 .. Pos - 1);
   end Header_Line;

   --------------------
   -- Separator_Line --
   --------------------

   function Separator_Line (T : Table) return String is
   begin
      return Separator_Line (T, '-');
   end Separator_Line;

   function Separator_Line (T : Table; C : Character) return String is
      Result : String (1 .. Max_Row_Width);
      Pos    : Natural := 1;
   begin
      Result := (others => ' ');

      if Pos <= Max_Row_Width then
         Result (Pos) := '+';
         Pos := Pos + 1;
      end if;

      for Col in 1 .. T.Col_Count loop
         for I in 1 .. T.Columns (Col).Width loop
            exit when Pos > Max_Row_Width;
            Result (Pos) := C;
            Pos := Pos + 1;
         end loop;

         if Pos <= Max_Row_Width then
            Result (Pos) := '+';
            Pos := Pos + 1;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end Separator_Line;

   -----------------
   -- Double_Line --
   -----------------

   function Double_Line (T : Table) return String is
   begin
      return Separator_Line (T, '=');
   end Double_Line;

   -----------------
   -- Total_Width --
   -----------------

   function Total_Width (T : Table) return Natural is
      Width : Natural := 1;  -- Leading '|'
   begin
      for Col in 1 .. T.Col_Count loop
         Width := Width + T.Columns (Col).Width + 1;  -- +1 for trailing '|'
      end loop;
      return Width;
   end Total_Width;

   -------------
   -- Box_Top --
   -------------

   function Box_Top (T : Table) return String is
      Result : String (1 .. Max_Row_Width);
      Pos    : Natural := 1;
   begin
      Result := (others => ' ');

      if Pos <= Max_Row_Width then
         Result (Pos) := '+';
         Pos := Pos + 1;
      end if;

      for Col in 1 .. T.Col_Count loop
         for I in 1 .. T.Columns (Col).Width loop
            exit when Pos > Max_Row_Width;
            Result (Pos) := '-';
            Pos := Pos + 1;
         end loop;

         if Pos <= Max_Row_Width then
            if Col < T.Col_Count then
               Result (Pos) := '-';
            else
               Result (Pos) := '+';
            end if;
            Pos := Pos + 1;
         end if;
      end loop;

      return Result (1 .. Pos - 1);
   end Box_Top;

   ----------------
   -- Box_Bottom --
   ----------------

   function Box_Bottom (T : Table) return String is
   begin
      return Box_Top (T);
   end Box_Bottom;

   -----------------
   -- Box_Row_Sep --
   -----------------

   function Box_Row_Sep (T : Table) return String is
   begin
      return Separator_Line (T, '-');
   end Box_Row_Sep;

end GNAT.Table_Format;
