-- GNAT.Simple_IO for Z80/CP/M
-- Simplified console I/O operations

package GNAT.Simple_IO is
   pragma Preelaborate;

   Max_Line_Length : constant := 128;

   -- Character output
   procedure Put (C : Character);
   procedure Put_Line (C : Character);

   -- String output
   procedure Put (S : String);
   procedure Put_Line (S : String);
   procedure Put_Line;  -- Just newline

   -- Integer output
   procedure Put (N : Integer);
   procedure Put (N : Integer; Width : Positive);
   procedure Put_Line (N : Integer);

   -- Boolean output
   procedure Put (B : Boolean);
   procedure Put_Line (B : Boolean);

   -- Formatted number output
   procedure Put_Hex (N : Natural);
   procedure Put_Hex (N : Natural; Width : Positive);
   procedure Put_Binary (N : Natural);
   procedure Put_Binary (N : Natural; Width : Positive);

   -- Character input
   function Get return Character;
   function Get_Immediate return Character;  -- No echo
   function Key_Available return Boolean;

   -- Line input
   function Get_Line return String;
   procedure Get_Line (S : out String; Last : out Natural);

   -- Yes/No input
   function Yes_No_Prompt (Prompt : String := "Continue? (Y/N): ") return Boolean;

   -- Integer input
   function Get_Integer return Integer;
   function Get_Integer (Prompt : String) return Integer;

   -- Cursor control (using escape sequences if available)
   procedure Clear_Screen;
   procedure Clear_Line;
   procedure Goto_XY (X, Y : Positive);
   procedure Cursor_Home;
   procedure Cursor_Up (N : Positive := 1);
   procedure Cursor_Down (N : Positive := 1);
   procedure Cursor_Left (N : Positive := 1);
   procedure Cursor_Right (N : Positive := 1);

   -- Newline
   procedure New_Line;
   procedure New_Line (Count : Positive);

   -- Spacing
   procedure Put_Spaces (Count : Natural);
   procedure Tab;

   -- Horizontal line
   procedure Put_Line_Char (C : Character; Width : Positive);

   -- Bell/beep
   procedure Beep;

   -- Backspace (erase previous character)
   procedure Backspace;

   -- Status line (for progress display)
   procedure Put_Status (S : String);  -- Overwrites current line

   -- Prompt with default
   function Prompt_String (Prompt : String; Default : String := "") return String;

   -- Wait for any key
   procedure Wait_Key;
   procedure Wait_Key (Prompt : String);

   -- Pause with message
   procedure Pause;
   procedure Pause (Message : String);

   -- Echo control
   procedure Echo_On;
   procedure Echo_Off;

private

   Echo_Enabled : Boolean := True;

end GNAT.Simple_IO;
