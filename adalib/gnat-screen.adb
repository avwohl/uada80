-- GNAT.Screen body for Z80/CP/M
-- Screen management implementation

package body GNAT.Screen is

   -- BDOS console output
   procedure Put_Char_Raw (C : Character) is
      procedure BDOS_Call (Func : Natural; Param : Natural := 0);
      pragma Import (Assembler, BDOS_Call, "bdos_call");
   begin
      BDOS_Call (2, Character'Pos (C));
   end Put_Char_Raw;

   procedure Put_String_Raw (S : String) is
   begin
      for C of S loop
         Put_Char_Raw (C);
      end loop;
   end Put_String_Raw;

   -- Escape sequence helper
   ESC : constant Character := Character'Val (27);

   procedure Send_ESC (Cmd : String) is
   begin
      Put_Char_Raw (ESC);
      Put_String_Raw (Cmd);
   end Send_ESC;

   function Int_To_Str (N : Natural) return String is
      Buf : String (1 .. 4);
      Pos : Natural := 4;
      V   : Natural := N;
   begin
      if V = 0 then
         return "0";
      end if;
      while V > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + (V mod 10));
         V := V / 10;
         Pos := Pos - 1;
      end loop;
      return Buf (Pos + 1 .. 4);
   end Int_To_Str;

   -----------------
   -- Init_Buffer --
   -----------------

   procedure Init_Buffer (B : out Screen_Buffer) is
   begin
      B.Width := Default_Width;
      B.Height := Default_Height;
      Clear_Buffer (B);
   end Init_Buffer;

   procedure Init_Buffer (B : out Screen_Buffer; Width, Height : Positive) is
      W : Natural := Width;
      H : Natural := Height;
   begin
      if W > Max_Width then
         W := Max_Width;
      end if;
      if H > Max_Height then
         H := Max_Height;
      end if;
      B.Width := W;
      B.Height := H;
      Clear_Buffer (B);
   end Init_Buffer;

   ------------------
   -- Clear_Buffer --
   ------------------

   procedure Clear_Buffer (B : in Out Screen_Buffer) is
   begin
      Clear_Buffer (B, ' ');
   end Clear_Buffer;

   procedure Clear_Buffer (B : in Out Screen_Buffer; C : Character) is
   begin
      for Y in 1 .. B.Height loop
         for X in 1 .. B.Width loop
            B.Data (Y) (X) := C;
         end loop;
      end loop;
   end Clear_Buffer;

   procedure Clear_Line_In_Buffer (B : in Out Screen_Buffer; Y : Positive) is
   begin
      if Y <= B.Height then
         for X in 1 .. B.Width loop
            B.Data (Y) (X) := ' ';
         end loop;
      end if;
   end Clear_Line_In_Buffer;

   procedure Clear_Region (B : in Out Screen_Buffer;
                           X1, Y1, X2, Y2 : Positive) is
   begin
      for Y in Y1 .. Y2 loop
         if Y <= B.Height then
            for X in X1 .. X2 loop
               if X <= B.Width then
                  B.Data (Y) (X) := ' ';
               end if;
            end loop;
         end if;
      end loop;
   end Clear_Region;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (B : in Out Screen_Buffer; X, Y : Positive; C : Character) is
   begin
      if X <= B.Width and Y <= B.Height then
         B.Data (Y) (X) := C;
      end if;
   end Put_Char;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (B : Screen_Buffer; X, Y : Positive) return Character is
   begin
      if X <= B.Width and Y <= B.Height then
         return B.Data (Y) (X);
      else
         return ' ';
      end if;
   end Get_Char;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (B : in Out Screen_Buffer;
                         X, Y : Positive; S : String) is
      Pos : Natural := X;
   begin
      if Y > B.Height then
         return;
      end if;

      for C of S loop
         exit when Pos > B.Width;
         B.Data (Y) (Pos) := C;
         Pos := Pos + 1;
      end loop;
   end Put_String;

   procedure Put_String_Centered (B : in Out Screen_Buffer;
                                  Y : Positive; S : String) is
      X : Positive;
   begin
      if S'Length >= B.Width then
         X := 1;
      else
         X := (B.Width - S'Length) / 2 + 1;
      end if;
      Put_String (B, X, Y, S);
   end Put_String_Centered;

   procedure Put_String_Right (B : in Out Screen_Buffer;
                               Y : Positive; S : String) is
      X : Positive;
   begin
      if S'Length >= B.Width then
         X := 1;
      else
         X := B.Width - S'Length + 1;
      end if;
      Put_String (B, X, Y, S);
   end Put_String_Right;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int (B : in Out Screen_Buffer;
                      X, Y : Positive; N : Integer) is
      S : constant String := Integer'Image (N);
   begin
      if N >= 0 then
         Put_String (B, X, Y, S (S'First + 1 .. S'Last));
      else
         Put_String (B, X, Y, S);
      end if;
   end Put_Int;

   procedure Put_Int (B : in Out Screen_Buffer;
                      X, Y : Positive; N : Integer; Width : Positive) is
      S   : constant String := Integer'Image (N);
      Len : Natural;
      Pos : Natural := X;
   begin
      if N >= 0 then
         Len := S'Length - 1;
         -- Pad with spaces
         for I in 1 .. Width - Len loop
            if Pos <= B.Width then
               B.Data (Y) (Pos) := ' ';
               Pos := Pos + 1;
            end if;
         end loop;
         Put_String (B, Pos, Y, S (S'First + 1 .. S'Last));
      else
         Len := S'Length;
         for I in 1 .. Width - Len loop
            if Pos <= B.Width then
               B.Data (Y) (Pos) := ' ';
               Pos := Pos + 1;
            end if;
         end loop;
         Put_String (B, Pos, Y, S);
      end if;
   end Put_Int;

   ---------------------
   -- Horizontal_Line --
   ---------------------

   procedure Horizontal_Line (B : in Out Screen_Buffer;
                              X1, X2, Y : Positive; C : Character := '-') is
   begin
      if Y <= B.Height then
         for X in X1 .. X2 loop
            if X <= B.Width then
               B.Data (Y) (X) := C;
            end if;
         end loop;
      end if;
   end Horizontal_Line;

   -------------------
   -- Vertical_Line --
   -------------------

   procedure Vertical_Line (B : in Out Screen_Buffer;
                            X, Y1, Y2 : Positive; C : Character := '|') is
   begin
      if X <= B.Width then
         for Y in Y1 .. Y2 loop
            if Y <= B.Height then
               B.Data (Y) (X) := C;
            end if;
         end loop;
      end if;
   end Vertical_Line;

   ---------
   -- Box --
   ---------

   procedure Box (B : in Out Screen_Buffer;
                  X1, Y1, X2, Y2 : Positive) is
   begin
      Horizontal_Line (B, X1, X2, Y1, '-');
      Horizontal_Line (B, X1, X2, Y2, '-');
      Vertical_Line (B, X1, Y1, Y2, '|');
      Vertical_Line (B, X2, Y1, Y2, '|');
      -- Corners
      Put_Char (B, X1, Y1, '+');
      Put_Char (B, X2, Y1, '+');
      Put_Char (B, X1, Y2, '+');
      Put_Char (B, X2, Y2, '+');
   end Box;

   ----------------
   -- Box_Filled --
   ----------------

   procedure Box_Filled (B : in Out Screen_Buffer;
                         X1, Y1, X2, Y2 : Positive; Fill : Character := ' ') is
   begin
      Fill_Region (B, X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, Fill);
      Box (B, X1, Y1, X2, Y2);
   end Box_Filled;

   ---------------
   -- Scroll_Up --
   ---------------

   procedure Scroll_Up (B : in Out Screen_Buffer; Lines : Positive := 1) is
   begin
      for L in 1 .. Lines loop
         for Y in 1 .. B.Height - 1 loop
            B.Data (Y) := B.Data (Y + 1);
         end loop;
         Clear_Line_In_Buffer (B, B.Height);
      end loop;
   end Scroll_Up;

   -----------------
   -- Scroll_Down --
   -----------------

   procedure Scroll_Down (B : in Out Screen_Buffer; Lines : Positive := 1) is
   begin
      for L in 1 .. Lines loop
         for Y in reverse 2 .. B.Height loop
            B.Data (Y) := B.Data (Y - 1);
         end loop;
         Clear_Line_In_Buffer (B, 1);
      end loop;
   end Scroll_Down;

   -----------------
   -- Scroll_Left --
   -----------------

   procedure Scroll_Left (B : in Out Screen_Buffer; Cols : Positive := 1) is
   begin
      for C in 1 .. Cols loop
         for Y in 1 .. B.Height loop
            for X in 1 .. B.Width - 1 loop
               B.Data (Y) (X) := B.Data (Y) (X + 1);
            end loop;
            B.Data (Y) (B.Width) := ' ';
         end loop;
      end loop;
   end Scroll_Left;

   ------------------
   -- Scroll_Right --
   ------------------

   procedure Scroll_Right (B : in Out Screen_Buffer; Cols : Positive := 1) is
   begin
      for C in 1 .. Cols loop
         for Y in 1 .. B.Height loop
            for X in reverse 2 .. B.Width loop
               B.Data (Y) (X) := B.Data (Y) (X - 1);
            end loop;
            B.Data (Y) (1) := ' ';
         end loop;
      end loop;
   end Scroll_Right;

   ------------------
   -- Buffer_Width --
   ------------------

   function Buffer_Width (B : Screen_Buffer) return Natural is
   begin
      return B.Width;
   end Buffer_Width;

   -------------------
   -- Buffer_Height --
   -------------------

   function Buffer_Height (B : Screen_Buffer) return Natural is
   begin
      return B.Height;
   end Buffer_Height;

   -----------------
   -- Copy_Region --
   -----------------

   procedure Copy_Region (Src, Dst : in Out Screen_Buffer;
                          Src_X, Src_Y, Width, Height : Positive;
                          Dst_X, Dst_Y : Positive) is
   begin
      for DY in 0 .. Height - 1 loop
         for DX in 0 .. Width - 1 loop
            if Src_Y + DY <= Src.Height and Src_X + DX <= Src.Width and
               Dst_Y + DY <= Dst.Height and Dst_X + DX <= Dst.Width then
               Dst.Data (Dst_Y + DY) (Dst_X + DX) :=
                 Src.Data (Src_Y + DY) (Src_X + DX);
            end if;
         end loop;
      end loop;
   end Copy_Region;

   -----------------
   -- Fill_Region --
   -----------------

   procedure Fill_Region (B : in Out Screen_Buffer;
                          X1, Y1, X2, Y2 : Positive; C : Character) is
   begin
      for Y in Y1 .. Y2 loop
         if Y <= B.Height then
            for X in X1 .. X2 loop
               if X <= B.Width then
                  B.Data (Y) (X) := C;
               end if;
            end loop;
         end if;
      end loop;
   end Fill_Region;

   ---------------------
   -- Get_Line_String --
   ---------------------

   function Get_Line_String (B : Screen_Buffer; Y : Positive) return String is
   begin
      if Y <= B.Height then
         declare
            Result : String (1 .. B.Width);
         begin
            for X in 1 .. B.Width loop
               Result (X) := B.Data (Y) (X);
            end loop;
            return Result;
         end;
      else
         return "";
      end if;
   end Get_Line_String;

   -------------------
   -- Render_Buffer --
   -------------------

   procedure Render_Buffer (B : Screen_Buffer) is
   begin
      Home;
      for Y in 1 .. B.Height loop
         for X in 1 .. B.Width loop
            Put_Char_Raw (B.Data (Y) (X));
         end loop;
         if Y < B.Height then
            Put_Char_Raw (Character'Val (13));
            Put_Char_Raw (Character'Val (10));
         end if;
      end loop;
   end Render_Buffer;

   -----------------
   -- Render_Line --
   -----------------

   procedure Render_Line (B : Screen_Buffer; Y : Positive) is
   begin
      if Y <= B.Height then
         Goto_XY (1, Y);
         for X in 1 .. B.Width loop
            Put_Char_Raw (B.Data (Y) (X));
         end loop;
      end if;
   end Render_Line;

   -------------------
   -- Render_Region --
   -------------------

   procedure Render_Region (B : Screen_Buffer; X1, Y1, X2, Y2 : Positive) is
   begin
      for Y in Y1 .. Y2 loop
         if Y <= B.Height then
            Goto_XY (X1, Y);
            for X in X1 .. X2 loop
               if X <= B.Width then
                  Put_Char_Raw (B.Data (Y) (X));
               end if;
            end loop;
         end if;
      end loop;
   end Render_Region;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      Send_ESC ("[2J");
      Home;
   end Clear_Screen;

   --------------------------
   -- Clear_To_End_Of_Line --
   --------------------------

   procedure Clear_To_End_Of_Line is
   begin
      Send_ESC ("[K");
   end Clear_To_End_Of_Line;

   ----------------------------
   -- Clear_To_End_Of_Screen --
   ----------------------------

   procedure Clear_To_End_Of_Screen is
   begin
      Send_ESC ("[J");
   end Clear_To_End_Of_Screen;

   -------------
   -- Goto_XY --
   -------------

   procedure Goto_XY (X, Y : Positive) is
   begin
      Send_ESC ("[" & Int_To_Str (Y) & ";" & Int_To_Str (X) & "H");
   end Goto_XY;

   ----------
   -- Home --
   ----------

   procedure Home is
   begin
      Send_ESC ("[H");
   end Home;

   ------------
   -- Put_At --
   ------------

   procedure Put_At (X, Y : Positive; C : Character) is
   begin
      Goto_XY (X, Y);
      Put_Char_Raw (C);
   end Put_At;

   procedure Put_At (X, Y : Positive; S : String) is
   begin
      Goto_XY (X, Y);
      Put_String_Raw (S);
   end Put_At;

   ---------------
   -- Cursor_On --
   ---------------

   procedure Cursor_On is
   begin
      Send_ESC ("[?25h");
   end Cursor_On;

   ----------------
   -- Cursor_Off --
   ----------------

   procedure Cursor_Off is
   begin
      Send_ESC ("[?25l");
   end Cursor_Off;

   -----------------
   -- Save_Cursor --
   -----------------

   procedure Save_Cursor is
   begin
      Send_ESC ("[s");
   end Save_Cursor;

   --------------------
   -- Restore_Cursor --
   --------------------

   procedure Restore_Cursor is
   begin
      Send_ESC ("[u");
   end Restore_Cursor;

   ----------------
   -- Set_Normal --
   ----------------

   procedure Set_Normal is
   begin
      Send_ESC ("[0m");
   end Set_Normal;

   --------------
   -- Set_Bold --
   --------------

   procedure Set_Bold is
   begin
      Send_ESC ("[1m");
   end Set_Bold;

   -------------------
   -- Set_Underline --
   -------------------

   procedure Set_Underline is
   begin
      Send_ESC ("[4m");
   end Set_Underline;

   -----------------
   -- Set_Reverse --
   -----------------

   procedure Set_Reverse is
   begin
      Send_ESC ("[7m");
   end Set_Reverse;

   ---------------
   -- Set_Blink --
   ---------------

   procedure Set_Blink is
   begin
      Send_ESC ("[5m");
   end Set_Blink;

   -----------------
   -- Init_Window --
   -----------------

   procedure Init_Window (W : out Window; X, Y, Width, Height : Positive) is
   begin
      W.X := X;
      W.Y := Y;
      W.Width := Width;
      W.Height := Height;
   end Init_Window;

   ------------------
   -- Clear_Window --
   ------------------

   procedure Clear_Window (W : in Out Window) is
   begin
      for Y in W.Y .. W.Y + W.Height - 1 loop
         Goto_XY (W.X, Y);
         for X in 1 .. W.Width loop
            Put_Char_Raw (' ');
         end loop;
      end loop;
   end Clear_Window;

   -------------------
   -- Put_In_Window --
   -------------------

   procedure Put_In_Window (W : in Out Window; X, Y : Positive; C : Character) is
   begin
      if X <= W.Width and Y <= W.Height then
         Goto_XY (W.X + X - 1, W.Y + Y - 1);
         Put_Char_Raw (C);
      end if;
   end Put_In_Window;

   procedure Put_In_Window (W : in Out Window; X, Y : Positive; S : String) is
   begin
      if Y <= W.Height then
         Goto_XY (W.X + X - 1, W.Y + Y - 1);
         for I in S'Range loop
            exit when X + I - S'First >= W.Width;
            Put_Char_Raw (S (I));
         end loop;
      end if;
   end Put_In_Window;

   ------------------------
   -- Draw_Window_Border --
   ------------------------

   procedure Draw_Window_Border (W : Window) is
   begin
      -- Top border
      Goto_XY (W.X, W.Y);
      Put_Char_Raw ('+');
      for I in 2 .. W.Width - 1 loop
         Put_Char_Raw ('-');
      end loop;
      Put_Char_Raw ('+');

      -- Side borders
      for Y in W.Y + 1 .. W.Y + W.Height - 2 loop
         Goto_XY (W.X, Y);
         Put_Char_Raw ('|');
         Goto_XY (W.X + W.Width - 1, Y);
         Put_Char_Raw ('|');
      end loop;

      -- Bottom border
      Goto_XY (W.X, W.Y + W.Height - 1);
      Put_Char_Raw ('+');
      for I in 2 .. W.Width - 1 loop
         Put_Char_Raw ('-');
      end loop;
      Put_Char_Raw ('+');
   end Draw_Window_Border;

   -----------------------
   -- Draw_Window_Title --
   -----------------------

   procedure Draw_Window_Title (W : Window; Title : String) is
      Start : Positive;
      Len   : Natural := Title'Length;
   begin
      if Len > W.Width - 4 then
         Len := W.Width - 4;
      end if;
      Start := W.X + (W.Width - Len) / 2;
      Goto_XY (Start - 1, W.Y);
      Put_Char_Raw ('[');
      for I in 1 .. Len loop
         Put_Char_Raw (Title (Title'First + I - 1));
      end loop;
      Put_Char_Raw (']');
   end Draw_Window_Title;

end GNAT.Screen;
