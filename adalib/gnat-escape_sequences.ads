-- GNAT.Escape_Sequences for Z80
-- ANSI/VT100 escape sequence generation for terminal control

package GNAT.Escape_Sequences is
   pragma Pure;

   -- Escape character
   ESC : constant Character := Character'Val (27);

   -- Cursor movement
   function Cursor_Up (N : Positive := 1) return String;
   function Cursor_Down (N : Positive := 1) return String;
   function Cursor_Forward (N : Positive := 1) return String;
   function Cursor_Back (N : Positive := 1) return String;
   function Cursor_Position (Row, Col : Positive) return String;
   function Cursor_Home return String;
   function Save_Cursor return String;
   function Restore_Cursor return String;

   -- Screen clearing
   function Clear_Screen return String;
   function Clear_To_End_Of_Screen return String;
   function Clear_To_Start_Of_Screen return String;
   function Clear_Line return String;
   function Clear_To_End_Of_Line return String;
   function Clear_To_Start_Of_Line return String;

   -- Text attributes
   function Reset_Attributes return String;
   function Bold return String;
   function Dim return String;
   function Underline return String;
   function Blink return String;
   function Reverse_Video return String;
   function Hidden return String;

   function Bold_Off return String;
   function Underline_Off return String;
   function Blink_Off return String;
   function Reverse_Off return String;

   -- Foreground colors (standard)
   function FG_Black return String;
   function FG_Red return String;
   function FG_Green return String;
   function FG_Yellow return String;
   function FG_Blue return String;
   function FG_Magenta return String;
   function FG_Cyan return String;
   function FG_White return String;
   function FG_Default return String;

   -- Background colors (standard)
   function BG_Black return String;
   function BG_Red return String;
   function BG_Green return String;
   function BG_Yellow return String;
   function BG_Blue return String;
   function BG_Magenta return String;
   function BG_Cyan return String;
   function BG_White return String;
   function BG_Default return String;

   -- Bright foreground colors
   function FG_Bright_Black return String;
   function FG_Bright_Red return String;
   function FG_Bright_Green return String;
   function FG_Bright_Yellow return String;
   function FG_Bright_Blue return String;
   function FG_Bright_Magenta return String;
   function FG_Bright_Cyan return String;
   function FG_Bright_White return String;

   -- Scrolling
   function Scroll_Up (N : Positive := 1) return String;
   function Scroll_Down (N : Positive := 1) return String;

   -- Line operations
   function Insert_Line (N : Positive := 1) return String;
   function Delete_Line (N : Positive := 1) return String;

   -- Character operations
   function Insert_Char (N : Positive := 1) return String;
   function Delete_Char (N : Positive := 1) return String;

   -- Cursor visibility
   function Hide_Cursor return String;
   function Show_Cursor return String;

   -- Terminal queries (responses come from terminal)
   function Query_Cursor_Position return String;
   function Query_Device_Status return String;

   -- Bell/beep
   function Bell return String;

   -- Carriage return and line feed
   function CR return String;
   function LF return String;
   function CRLF return String;

   -- Tab
   function Tab return String;
   function Backspace return String;

   -- Wrap text with color
   function Colorize (Text : String; FG_Code : Natural) return String;

end GNAT.Escape_Sequences;
