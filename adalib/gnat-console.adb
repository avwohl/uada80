-- GNAT.Console body for Z80 / CP/M
-- Console I/O implementation using BDOS calls

package body GNAT.Console is

   -- BDOS function numbers
   BDOS_Console_Input  : constant := 1;
   BDOS_Console_Output : constant := 2;
   BDOS_Console_Status : constant := 11;

   -- Escape sequences
   ESC : constant Character := ASCII.ESC;

   procedure BDOS_Call (Func : Natural; Param : Natural := 0);
   pragma Import (Ada, BDOS_Call);

   function BDOS_Call_Ret (Func : Natural; Param : Natural := 0) return Natural;
   pragma Import (Ada, BDOS_Call_Ret);

   -- Internal output
   procedure Raw_Put (C : Character) is
   begin
      BDOS_Call (BDOS_Console_Output, Character'Pos (C));
   end Raw_Put;

   procedure ESC_Seq (S : String) is
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      for C of S loop
         Raw_Put (C);
      end loop;
   end ESC_Seq;

   function Int_Image (N : Natural) return String is
      Temp : String (1 .. 10);
      Idx  : Natural := 0;
      Val  : Natural := N;
   begin
      if N = 0 then
         return "0";
      end if;

      while Val > 0 loop
         Idx := Idx + 1;
         Temp (Idx) := Character'Val (Character'Pos ('0') + Val mod 10);
         Val := Val / 10;
      end loop;

      -- Reverse
      declare
         Result : String (1 .. Idx);
      begin
         for I in 1 .. Idx loop
            Result (I) := Temp (Idx - I + 1);
         end loop;
         return Result;
      end;
   end Int_Image;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Raw_Put (C);
   end Put;

   procedure Put (S : String) is
   begin
      for C of S loop
         Raw_Put (C);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String := "") is
   begin
      Put (S);
      New_Line;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Count : Positive := 1) is
   begin
      for I in 1 .. Count loop
         Raw_Put (ASCII.CR);
         Raw_Put (ASCII.LF);
      end loop;
   end New_Line;

   --------------
   -- Get_Char --
   --------------

   function Get_Char return Character is
   begin
      return Character'Val (BDOS_Call_Ret (BDOS_Console_Input) mod 256);
   end Get_Char;

   ------------------------
   -- Get_Char_Immediate --
   ------------------------

   function Get_Char_Immediate return Character is
   begin
      if Char_Available then
         return Get_Char;
      else
         return ASCII.NUL;
      end if;
   end Get_Char_Immediate;

   --------------------
   -- Char_Available --
   --------------------

   function Char_Available return Boolean is
   begin
      return BDOS_Call_Ret (BDOS_Console_Status) /= 0;
   end Char_Available;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (S : out String; Last : out Natural) is
      C : Character;
   begin
      Last := S'First - 1;

      loop
         C := Get_Char;

         case C is
            when ASCII.CR | ASCII.LF =>
               New_Line;
               exit;

            when ASCII.BS | ASCII.DEL =>
               if Last >= S'First then
                  Last := Last - 1;
                  Raw_Put (ASCII.BS);
                  Raw_Put (' ');
                  Raw_Put (ASCII.BS);
               end if;

            when ASCII.ETX =>  -- Ctrl-C
               exit;

            when others =>
               if Last < S'Last and C >= ' ' then
                  Last := Last + 1;
                  S (Last) := C;
                  Raw_Put (C);  -- Echo
               end if;
         end case;
      end loop;
   end Get_Line;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      ESC_Seq ("2J");
      Home_Cursor;
   end Clear_Screen;

   ----------------
   -- Clear_Line --
   ----------------

   procedure Clear_Line is
   begin
      ESC_Seq ("2K");
   end Clear_Line;

   --------------------------
   -- Clear_To_End_Of_Line --
   --------------------------

   procedure Clear_To_End_Of_Line is
   begin
      ESC_Seq ("K");
   end Clear_To_End_Of_Line;

   ----------------------------
   -- Clear_To_End_Of_Screen --
   ----------------------------

   procedure Clear_To_End_Of_Screen is
   begin
      ESC_Seq ("J");
   end Clear_To_End_Of_Screen;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Row, Column : Positive) is
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Row));
      Raw_Put (';');
      Put (Int_Image (Column));
      Raw_Put ('H');
   end Set_Cursor;

   --------------------
   -- Move_Cursor_Up --
   --------------------

   procedure Move_Cursor_Up (Count : Positive := 1) is
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Count));
      Raw_Put ('A');
   end Move_Cursor_Up;

   ----------------------
   -- Move_Cursor_Down --
   ----------------------

   procedure Move_Cursor_Down (Count : Positive := 1) is
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Count));
      Raw_Put ('B');
   end Move_Cursor_Down;

   ----------------------
   -- Move_Cursor_Left --
   ----------------------

   procedure Move_Cursor_Left (Count : Positive := 1) is
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Count));
      Raw_Put ('D');
   end Move_Cursor_Left;

   -----------------------
   -- Move_Cursor_Right --
   -----------------------

   procedure Move_Cursor_Right (Count : Positive := 1) is
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Count));
      Raw_Put ('C');
   end Move_Cursor_Right;

   -----------------
   -- Home_Cursor --
   -----------------

   procedure Home_Cursor is
   begin
      ESC_Seq ("H");
   end Home_Cursor;

   -----------------
   -- Save_Cursor --
   -----------------

   procedure Save_Cursor is
   begin
      ESC_Seq ("s");
   end Save_Cursor;

   --------------------
   -- Restore_Cursor --
   --------------------

   procedure Restore_Cursor is
   begin
      ESC_Seq ("u");
   end Restore_Cursor;

   -----------------
   -- Hide_Cursor --
   -----------------

   procedure Hide_Cursor is
   begin
      ESC_Seq ("?25l");
   end Hide_Cursor;

   -----------------
   -- Show_Cursor --
   -----------------

   procedure Show_Cursor is
   begin
      ESC_Seq ("?25h");
   end Show_Cursor;

   ----------------
   -- Set_Normal --
   ----------------

   procedure Set_Normal is
   begin
      ESC_Seq ("0m");
   end Set_Normal;

   --------------
   -- Set_Bold --
   --------------

   procedure Set_Bold is
   begin
      ESC_Seq ("1m");
   end Set_Bold;

   -------------------
   -- Set_Underline --
   -------------------

   procedure Set_Underline is
   begin
      ESC_Seq ("4m");
   end Set_Underline;

   -----------------
   -- Set_Reverse --
   -----------------

   procedure Set_Reverse is
   begin
      ESC_Seq ("7m");
   end Set_Reverse;

   ---------------
   -- Set_Blink --
   ---------------

   procedure Set_Blink is
   begin
      ESC_Seq ("5m");
   end Set_Blink;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (C : Color) is
      Code : constant Natural := 30 + Color'Pos (C);
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Code));
      Raw_Put ('m');
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (C : Color) is
      Code : constant Natural := 40 + Color'Pos (C);
   begin
      Raw_Put (ESC);
      Raw_Put ('[');
      Put (Int_Image (Code));
      Raw_Put ('m');
   end Set_Background;

   ------------------
   -- Reset_Colors --
   ------------------

   procedure Reset_Colors is
   begin
      Set_Normal;
   end Reset_Colors;

   ----------
   -- Bell --
   ----------

   procedure Bell is
   begin
      Raw_Put (ASCII.BEL);
   end Bell;

   ------------------
   -- Screen_Width --
   ------------------

   function Screen_Width return Positive is
   begin
      return Default_Width;
   end Screen_Width;

   -------------------
   -- Screen_Height --
   -------------------

   function Screen_Height return Positive is
   begin
      return Default_Height;
   end Screen_Height;

   ------------------
   -- Set_Raw_Mode --
   ------------------

   procedure Set_Raw_Mode is
   begin
      -- On CP/M, console is already essentially in raw mode
      null;
   end Set_Raw_Mode;

   ---------------------
   -- Set_Cooked_Mode --
   ---------------------

   procedure Set_Cooked_Mode is
   begin
      null;
   end Set_Cooked_Mode;

   -----------
   -- Pause --
   -----------

   procedure Pause is
      C : Character;
      pragma Unreferenced (C);
   begin
      C := Get_Char;
   end Pause;

   --------------
   -- Delay_MS --
   --------------

   procedure Delay_MS (Milliseconds : Natural) is
      -- Very rough delay loop for Z80
      Iterations : constant Natural := Milliseconds * 100;
   begin
      for I in 1 .. Iterations loop
         null;
      end loop;
   end Delay_MS;

end GNAT.Console;
