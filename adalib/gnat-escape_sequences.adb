-- GNAT.Escape_Sequences body for Z80
-- ANSI/VT100 escape sequence implementation

package body GNAT.Escape_Sequences is

   CSI : constant String := ESC & "[";  -- Control Sequence Introducer

   function Num_Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);  -- Remove leading space
   end Num_Image;

   ---------------
   -- Cursor_Up --
   ---------------

   function Cursor_Up (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "A";
      else
         return CSI & Num_Image (N) & "A";
      end if;
   end Cursor_Up;

   -----------------
   -- Cursor_Down --
   -----------------

   function Cursor_Down (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "B";
      else
         return CSI & Num_Image (N) & "B";
      end if;
   end Cursor_Down;

   --------------------
   -- Cursor_Forward --
   --------------------

   function Cursor_Forward (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "C";
      else
         return CSI & Num_Image (N) & "C";
      end if;
   end Cursor_Forward;

   -----------------
   -- Cursor_Back --
   -----------------

   function Cursor_Back (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "D";
      else
         return CSI & Num_Image (N) & "D";
      end if;
   end Cursor_Back;

   ---------------------
   -- Cursor_Position --
   ---------------------

   function Cursor_Position (Row, Col : Positive) return String is
   begin
      return CSI & Num_Image (Row) & ";" & Num_Image (Col) & "H";
   end Cursor_Position;

   -----------------
   -- Cursor_Home --
   -----------------

   function Cursor_Home return String is
   begin
      return CSI & "H";
   end Cursor_Home;

   -----------------
   -- Save_Cursor --
   -----------------

   function Save_Cursor return String is
   begin
      return CSI & "s";
   end Save_Cursor;

   --------------------
   -- Restore_Cursor --
   --------------------

   function Restore_Cursor return String is
   begin
      return CSI & "u";
   end Restore_Cursor;

   ------------------
   -- Clear_Screen --
   ------------------

   function Clear_Screen return String is
   begin
      return CSI & "2J";
   end Clear_Screen;

   ---------------------------
   -- Clear_To_End_Of_Screen --
   ---------------------------

   function Clear_To_End_Of_Screen return String is
   begin
      return CSI & "0J";
   end Clear_To_End_Of_Screen;

   -----------------------------
   -- Clear_To_Start_Of_Screen --
   -----------------------------

   function Clear_To_Start_Of_Screen return String is
   begin
      return CSI & "1J";
   end Clear_To_Start_Of_Screen;

   ----------------
   -- Clear_Line --
   ----------------

   function Clear_Line return String is
   begin
      return CSI & "2K";
   end Clear_Line;

   -------------------------
   -- Clear_To_End_Of_Line --
   -------------------------

   function Clear_To_End_Of_Line return String is
   begin
      return CSI & "0K";
   end Clear_To_End_Of_Line;

   ---------------------------
   -- Clear_To_Start_Of_Line --
   ---------------------------

   function Clear_To_Start_Of_Line return String is
   begin
      return CSI & "1K";
   end Clear_To_Start_Of_Line;

   ----------------------
   -- Reset_Attributes --
   ----------------------

   function Reset_Attributes return String is
   begin
      return CSI & "0m";
   end Reset_Attributes;

   ----------
   -- Bold --
   ----------

   function Bold return String is
   begin
      return CSI & "1m";
   end Bold;

   ---------
   -- Dim --
   ---------

   function Dim return String is
   begin
      return CSI & "2m";
   end Dim;

   ---------------
   -- Underline --
   ---------------

   function Underline return String is
   begin
      return CSI & "4m";
   end Underline;

   -----------
   -- Blink --
   -----------

   function Blink return String is
   begin
      return CSI & "5m";
   end Blink;

   -------------------
   -- Reverse_Video --
   -------------------

   function Reverse_Video return String is
   begin
      return CSI & "7m";
   end Reverse_Video;

   ------------
   -- Hidden --
   ------------

   function Hidden return String is
   begin
      return CSI & "8m";
   end Hidden;

   --------------
   -- Bold_Off --
   --------------

   function Bold_Off return String is
   begin
      return CSI & "22m";
   end Bold_Off;

   -------------------
   -- Underline_Off --
   -------------------

   function Underline_Off return String is
   begin
      return CSI & "24m";
   end Underline_Off;

   ---------------
   -- Blink_Off --
   ---------------

   function Blink_Off return String is
   begin
      return CSI & "25m";
   end Blink_Off;

   -----------------
   -- Reverse_Off --
   -----------------

   function Reverse_Off return String is
   begin
      return CSI & "27m";
   end Reverse_Off;

   -- Foreground colors

   function FG_Black return String is
   begin
      return CSI & "30m";
   end FG_Black;

   function FG_Red return String is
   begin
      return CSI & "31m";
   end FG_Red;

   function FG_Green return String is
   begin
      return CSI & "32m";
   end FG_Green;

   function FG_Yellow return String is
   begin
      return CSI & "33m";
   end FG_Yellow;

   function FG_Blue return String is
   begin
      return CSI & "34m";
   end FG_Blue;

   function FG_Magenta return String is
   begin
      return CSI & "35m";
   end FG_Magenta;

   function FG_Cyan return String is
   begin
      return CSI & "36m";
   end FG_Cyan;

   function FG_White return String is
   begin
      return CSI & "37m";
   end FG_White;

   function FG_Default return String is
   begin
      return CSI & "39m";
   end FG_Default;

   -- Background colors

   function BG_Black return String is
   begin
      return CSI & "40m";
   end BG_Black;

   function BG_Red return String is
   begin
      return CSI & "41m";
   end BG_Red;

   function BG_Green return String is
   begin
      return CSI & "42m";
   end BG_Green;

   function BG_Yellow return String is
   begin
      return CSI & "43m";
   end BG_Yellow;

   function BG_Blue return String is
   begin
      return CSI & "44m";
   end BG_Blue;

   function BG_Magenta return String is
   begin
      return CSI & "45m";
   end BG_Magenta;

   function BG_Cyan return String is
   begin
      return CSI & "46m";
   end BG_Cyan;

   function BG_White return String is
   begin
      return CSI & "47m";
   end BG_White;

   function BG_Default return String is
   begin
      return CSI & "49m";
   end BG_Default;

   -- Bright foreground colors

   function FG_Bright_Black return String is
   begin
      return CSI & "90m";
   end FG_Bright_Black;

   function FG_Bright_Red return String is
   begin
      return CSI & "91m";
   end FG_Bright_Red;

   function FG_Bright_Green return String is
   begin
      return CSI & "92m";
   end FG_Bright_Green;

   function FG_Bright_Yellow return String is
   begin
      return CSI & "93m";
   end FG_Bright_Yellow;

   function FG_Bright_Blue return String is
   begin
      return CSI & "94m";
   end FG_Bright_Blue;

   function FG_Bright_Magenta return String is
   begin
      return CSI & "95m";
   end FG_Bright_Magenta;

   function FG_Bright_Cyan return String is
   begin
      return CSI & "96m";
   end FG_Bright_Cyan;

   function FG_Bright_White return String is
   begin
      return CSI & "97m";
   end FG_Bright_White;

   -- Scrolling

   function Scroll_Up (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "S";
      else
         return CSI & Num_Image (N) & "S";
      end if;
   end Scroll_Up;

   function Scroll_Down (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "T";
      else
         return CSI & Num_Image (N) & "T";
      end if;
   end Scroll_Down;

   -- Line operations

   function Insert_Line (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "L";
      else
         return CSI & Num_Image (N) & "L";
      end if;
   end Insert_Line;

   function Delete_Line (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "M";
      else
         return CSI & Num_Image (N) & "M";
      end if;
   end Delete_Line;

   -- Character operations

   function Insert_Char (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "@";
      else
         return CSI & Num_Image (N) & "@";
      end if;
   end Insert_Char;

   function Delete_Char (N : Positive := 1) return String is
   begin
      if N = 1 then
         return CSI & "P";
      else
         return CSI & Num_Image (N) & "P";
      end if;
   end Delete_Char;

   -- Cursor visibility

   function Hide_Cursor return String is
   begin
      return CSI & "?25l";
   end Hide_Cursor;

   function Show_Cursor return String is
   begin
      return CSI & "?25h";
   end Show_Cursor;

   -- Terminal queries

   function Query_Cursor_Position return String is
   begin
      return CSI & "6n";
   end Query_Cursor_Position;

   function Query_Device_Status return String is
   begin
      return CSI & "5n";
   end Query_Device_Status;

   -- Control characters

   function Bell return String is
   begin
      return (1 => Character'Val (7));
   end Bell;

   function CR return String is
   begin
      return (1 => Character'Val (13));
   end CR;

   function LF return String is
   begin
      return (1 => Character'Val (10));
   end LF;

   function CRLF return String is
   begin
      return Character'Val (13) & Character'Val (10);
   end CRLF;

   function Tab return String is
   begin
      return (1 => Character'Val (9));
   end Tab;

   function Backspace return String is
   begin
      return (1 => Character'Val (8));
   end Backspace;

   --------------
   -- Colorize --
   --------------

   function Colorize (Text : String; FG_Code : Natural) return String is
   begin
      return CSI & Num_Image (FG_Code) & "m" & Text & Reset_Attributes;
   end Colorize;

end GNAT.Escape_Sequences;
