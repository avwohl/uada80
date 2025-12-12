-- GNAT.Simple_IO body for Z80/CP/M
-- Console I/O implementation using BDOS calls

package body GNAT.Simple_IO is

   -- CP/M BDOS function codes
   BDOS_CONSOLE_INPUT  : constant := 1;
   BDOS_CONSOLE_OUTPUT : constant := 2;
   BDOS_CONSOLE_STATUS : constant := 11;
   BDOS_READ_CONSOLE   : constant := 10;

   -- Control characters
   CR  : constant Character := Character'Val (13);
   LF  : constant Character := Character'Val (10);
   BS  : constant Character := Character'Val (8);
   BEL : constant Character := Character'Val (7);
   ESC : constant Character := Character'Val (27);
   TAB_CHAR : constant Character := Character'Val (9);

   procedure BDOS_Call (Func : Natural; Param : Natural := 0) is
      pragma Import (Assembler, BDOS_Call, "bdos_call");
   begin
      null;  -- Implemented in assembly
   end BDOS_Call;

   function BDOS_Call_Ret (Func : Natural; Param : Natural := 0) return Natural is
      pragma Import (Assembler, BDOS_Call_Ret, "bdos_call_ret");
   begin
      return 0;  -- Implemented in assembly
   end BDOS_Call_Ret;

   -- Raw character output
   procedure Put_Char_Raw (C : Character) is
   begin
      BDOS_Call (BDOS_CONSOLE_OUTPUT, Character'Pos (C));
   end Put_Char_Raw;

   -- Raw character input
   function Get_Char_Raw return Character is
   begin
      return Character'Val (BDOS_Call_Ret (BDOS_CONSOLE_INPUT));
   end Get_Char_Raw;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Put_Char_Raw (C);
   end Put;

   procedure Put_Line (C : Character) is
   begin
      Put (C);
      New_Line;
   end Put_Line;

   procedure Put (S : String) is
   begin
      for C of S loop
         Put (C);
      end loop;
   end Put;

   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;

   procedure Put_Line is
   begin
      New_Line;
   end Put_Line;

   procedure Put (N : Integer) is
      S : constant String := Integer'Image (N);
   begin
      if N >= 0 then
         Put (S (S'First + 1 .. S'Last));
      else
         Put (S);
      end if;
   end Put;

   procedure Put (N : Integer; Width : Positive) is
      S : constant String := Integer'Image (N);
      L : Natural;
   begin
      if N >= 0 then
         L := S'Length - 1;
         for I in 1 .. Width - L loop
            Put (' ');
         end loop;
         Put (S (S'First + 1 .. S'Last));
      else
         L := S'Length;
         for I in 1 .. Width - L loop
            Put (' ');
         end loop;
         Put (S);
      end if;
   end Put;

   procedure Put_Line (N : Integer) is
   begin
      Put (N);
      New_Line;
   end Put_Line;

   procedure Put (B : Boolean) is
   begin
      if B then
         Put ("TRUE");
      else
         Put ("FALSE");
      end if;
   end Put;

   procedure Put_Line (B : Boolean) is
   begin
      Put (B);
      New_Line;
   end Put_Line;

   -------------
   -- Put_Hex --
   -------------

   procedure Put_Hex (N : Natural) is
      Hex : constant String := "0123456789ABCDEF";
      V   : Natural := N;
      Buf : String (1 .. 8);
      Pos : Natural := 8;
   begin
      if V = 0 then
         Put ('0');
         return;
      end if;

      while V > 0 loop
         Buf (Pos) := Hex ((V mod 16) + 1);
         V := V / 16;
         Pos := Pos - 1;
      end loop;

      Put (Buf (Pos + 1 .. 8));
   end Put_Hex;

   procedure Put_Hex (N : Natural; Width : Positive) is
      Hex : constant String := "0123456789ABCDEF";
      V   : Natural := N;
      Buf : String (1 .. 8);
      Pos : Natural := 8;
   begin
      while V > 0 loop
         Buf (Pos) := Hex ((V mod 16) + 1);
         V := V / 16;
         Pos := Pos - 1;
      end loop;

      -- Pad with zeros
      for I in 1 .. Width - (8 - Pos) loop
         Put ('0');
      end loop;

      if Pos < 8 then
         Put (Buf (Pos + 1 .. 8));
      else
         Put ('0');
      end if;
   end Put_Hex;

   ----------------
   -- Put_Binary --
   ----------------

   procedure Put_Binary (N : Natural) is
      V   : Natural := N;
      Buf : String (1 .. 16);
      Pos : Natural := 16;
   begin
      if V = 0 then
         Put ('0');
         return;
      end if;

      while V > 0 loop
         if V mod 2 = 1 then
            Buf (Pos) := '1';
         else
            Buf (Pos) := '0';
         end if;
         V := V / 2;
         Pos := Pos - 1;
      end loop;

      Put (Buf (Pos + 1 .. 16));
   end Put_Binary;

   procedure Put_Binary (N : Natural; Width : Positive) is
      V   : Natural := N;
      Buf : String (1 .. 16);
      Pos : Natural := 16;
   begin
      while V > 0 loop
         if V mod 2 = 1 then
            Buf (Pos) := '1';
         else
            Buf (Pos) := '0';
         end if;
         V := V / 2;
         Pos := Pos - 1;
      end loop;

      for I in 1 .. Width - (16 - Pos) loop
         Put ('0');
      end loop;

      if Pos < 16 then
         Put (Buf (Pos + 1 .. 16));
      else
         Put ('0');
      end if;
   end Put_Binary;

   ---------
   -- Get --
   ---------

   function Get return Character is
      C : Character;
   begin
      C := Get_Char_Raw;
      if Echo_Enabled then
         Put (C);
      end if;
      return C;
   end Get;

   -------------------
   -- Get_Immediate --
   -------------------

   function Get_Immediate return Character is
   begin
      return Get_Char_Raw;
   end Get_Immediate;

   -------------------
   -- Key_Available --
   -------------------

   function Key_Available return Boolean is
   begin
      return BDOS_Call_Ret (BDOS_CONSOLE_STATUS) /= 0;
   end Key_Available;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return String is
      Buf  : String (1 .. Max_Line_Length);
      Last : Natural;
   begin
      Get_Line (Buf, Last);
      return Buf (1 .. Last);
   end Get_Line;

   procedure Get_Line (S : out String; Last : out Natural) is
      C   : Character;
      Pos : Natural := 0;
   begin
      loop
         C := Get_Char_Raw;

         if C = CR or C = LF then
            if Echo_Enabled then
               New_Line;
            end if;
            exit;
         elsif C = BS and Pos > 0 then
            Pos := Pos - 1;
            if Echo_Enabled then
               Put (BS);
               Put (' ');
               Put (BS);
            end if;
         elsif C >= ' ' and C <= '~' and Pos < S'Length then
            Pos := Pos + 1;
            S (S'First + Pos - 1) := C;
            if Echo_Enabled then
               Put (C);
            end if;
         end if;
      end loop;

      Last := Pos;
   end Get_Line;

   -------------------
   -- Yes_No_Prompt --
   -------------------

   function Yes_No_Prompt (Prompt : String := "Continue? (Y/N): ") return Boolean is
      C : Character;
   begin
      Put (Prompt);
      loop
         C := Get_Immediate;
         if C = 'Y' or C = 'y' then
            Put ('Y');
            New_Line;
            return True;
         elsif C = 'N' or C = 'n' then
            Put ('N');
            New_Line;
            return False;
         end if;
      end loop;
   end Yes_No_Prompt;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer return Integer is
      S : constant String := Get_Line;
      Result : Integer := 0;
      Negative : Boolean := False;
      Start : Natural := S'First;
   begin
      -- Skip leading spaces
      while Start <= S'Last and then S (Start) = ' ' loop
         Start := Start + 1;
      end loop;

      if Start <= S'Last and then S (Start) = '-' then
         Negative := True;
         Start := Start + 1;
      elsif Start <= S'Last and then S (Start) = '+' then
         Start := Start + 1;
      end if;

      for I in Start .. S'Last loop
         exit when S (I) < '0' or S (I) > '9';
         Result := Result * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Get_Integer;

   function Get_Integer (Prompt : String) return Integer is
   begin
      Put (Prompt);
      return Get_Integer;
   end Get_Integer;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      Put (ESC);
      Put ("[2J");
      Cursor_Home;
   end Clear_Screen;

   ----------------
   -- Clear_Line --
   ----------------

   procedure Clear_Line is
   begin
      Put (ESC);
      Put ("[2K");
      Put (CR);
   end Clear_Line;

   -------------
   -- Goto_XY --
   -------------

   procedure Goto_XY (X, Y : Positive) is
   begin
      Put (ESC);
      Put ('[');
      Put (Y);
      Put (';');
      Put (X);
      Put ('H');
   end Goto_XY;

   -----------------
   -- Cursor_Home --
   -----------------

   procedure Cursor_Home is
   begin
      Put (ESC);
      Put ("[H");
   end Cursor_Home;

   ---------------
   -- Cursor_Up --
   ---------------

   procedure Cursor_Up (N : Positive := 1) is
   begin
      Put (ESC);
      Put ('[');
      Put (N);
      Put ('A');
   end Cursor_Up;

   -----------------
   -- Cursor_Down --
   -----------------

   procedure Cursor_Down (N : Positive := 1) is
   begin
      Put (ESC);
      Put ('[');
      Put (N);
      Put ('B');
   end Cursor_Down;

   -----------------
   -- Cursor_Left --
   -----------------

   procedure Cursor_Left (N : Positive := 1) is
   begin
      Put (ESC);
      Put ('[');
      Put (N);
      Put ('D');
   end Cursor_Left;

   ------------------
   -- Cursor_Right --
   ------------------

   procedure Cursor_Right (N : Positive := 1) is
   begin
      Put (ESC);
      Put ('[');
      Put (N);
      Put ('C');
   end Cursor_Right;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (CR);
      Put (LF);
   end New_Line;

   procedure New_Line (Count : Positive) is
   begin
      for I in 1 .. Count loop
         New_Line;
      end loop;
   end New_Line;

   ----------------
   -- Put_Spaces --
   ----------------

   procedure Put_Spaces (Count : Natural) is
   begin
      for I in 1 .. Count loop
         Put (' ');
      end loop;
   end Put_Spaces;

   ---------
   -- Tab --
   ---------

   procedure Tab is
   begin
      Put (TAB_CHAR);
   end Tab;

   -------------------
   -- Put_Line_Char --
   -------------------

   procedure Put_Line_Char (C : Character; Width : Positive) is
   begin
      for I in 1 .. Width loop
         Put (C);
      end loop;
   end Put_Line_Char;

   ----------
   -- Beep --
   ----------

   procedure Beep is
   begin
      Put (BEL);
   end Beep;

   ---------------
   -- Backspace --
   ---------------

   procedure Backspace is
   begin
      Put (BS);
      Put (' ');
      Put (BS);
   end Backspace;

   ----------------
   -- Put_Status --
   ----------------

   procedure Put_Status (S : String) is
   begin
      Put (CR);
      Put (S);
      Put (ESC);
      Put ("[K");  -- Clear to end of line
   end Put_Status;

   -------------------
   -- Prompt_String --
   -------------------

   function Prompt_String (Prompt : String; Default : String := "") return String is
      S : String (1 .. Max_Line_Length);
      L : Natural;
   begin
      Put (Prompt);
      if Default'Length > 0 then
         Put ('[');
         Put (Default);
         Put ("] ");
      end if;

      Get_Line (S, L);

      if L = 0 then
         return Default;
      else
         return S (1 .. L);
      end if;
   end Prompt_String;

   --------------
   -- Wait_Key --
   --------------

   procedure Wait_Key is
      C : Character;
      pragma Unreferenced (C);
   begin
      C := Get_Immediate;
   end Wait_Key;

   procedure Wait_Key (Prompt : String) is
   begin
      Put (Prompt);
      Wait_Key;
   end Wait_Key;

   -----------
   -- Pause --
   -----------

   procedure Pause is
   begin
      Wait_Key ("Press any key to continue...");
      New_Line;
   end Pause;

   procedure Pause (Message : String) is
   begin
      Wait_Key (Message);
      New_Line;
   end Pause;

   -------------
   -- Echo_On --
   -------------

   procedure Echo_On is
   begin
      Echo_Enabled := True;
   end Echo_On;

   --------------
   -- Echo_Off --
   --------------

   procedure Echo_Off is
   begin
      Echo_Enabled := False;
   end Echo_Off;

end GNAT.Simple_IO;
