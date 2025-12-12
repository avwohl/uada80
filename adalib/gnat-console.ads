-- GNAT.Console for Z80 / CP/M
-- Console I/O utilities

package GNAT.Console is
   pragma Preelaborate;

   -- Screen dimensions (typical CP/M terminal)
   Default_Width  : constant := 80;
   Default_Height : constant := 24;

   -- Basic output
   procedure Put (C : Character);
   procedure Put (S : String);
   procedure Put_Line (S : String := "");
   procedure New_Line (Count : Positive := 1);

   -- Basic input
   function Get_Char return Character;
   --  Read single character (blocking)

   function Get_Char_Immediate return Character;
   --  Read character without waiting (returns NUL if none available)

   function Char_Available return Boolean;
   --  Check if character is waiting

   procedure Get_Line (S : out String; Last : out Natural);
   --  Read line with basic editing

   -- Cursor control (VT100/ANSI compatible)
   procedure Clear_Screen;
   procedure Clear_Line;
   procedure Clear_To_End_Of_Line;
   procedure Clear_To_End_Of_Screen;

   procedure Set_Cursor (Row, Column : Positive);
   procedure Move_Cursor_Up (Count : Positive := 1);
   procedure Move_Cursor_Down (Count : Positive := 1);
   procedure Move_Cursor_Left (Count : Positive := 1);
   procedure Move_Cursor_Right (Count : Positive := 1);

   procedure Home_Cursor;
   procedure Save_Cursor;
   procedure Restore_Cursor;

   procedure Hide_Cursor;
   procedure Show_Cursor;

   -- Text attributes (where supported)
   procedure Set_Normal;
   procedure Set_Bold;
   procedure Set_Underline;
   procedure Set_Reverse;
   procedure Set_Blink;

   -- Colors (ANSI)
   type Color is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);

   procedure Set_Foreground (C : Color);
   procedure Set_Background (C : Color);
   procedure Reset_Colors;

   -- Beep
   procedure Bell;

   -- Screen info
   function Screen_Width return Positive;
   function Screen_Height return Positive;

   -- Raw mode (no echo, no line buffering)
   procedure Set_Raw_Mode;
   procedure Set_Cooked_Mode;

   -- Utility
   procedure Pause;
   --  Wait for keypress

   procedure Delay_MS (Milliseconds : Natural);
   --  Approximate delay

end GNAT.Console;
