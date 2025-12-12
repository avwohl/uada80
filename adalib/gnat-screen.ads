-- GNAT.Screen for Z80/CP/M
-- Text screen management utilities

package GNAT.Screen is
   pragma Preelaborate;

   -- Default screen dimensions (80x24 typical for CP/M)
   Default_Width  : constant := 80;
   Default_Height : constant := 24;

   -- Screen buffer (for double-buffering)
   Max_Width  : constant := 80;
   Max_Height : constant := 25;

   type Screen_Buffer is limited private;

   -- Initialize screen buffer
   procedure Init_Buffer (B : out Screen_Buffer);
   procedure Init_Buffer (B : out Screen_Buffer; Width, Height : Positive);

   -- Clear operations
   procedure Clear_Buffer (B : in Out Screen_Buffer);
   procedure Clear_Buffer (B : in Out Screen_Buffer; C : Character);
   procedure Clear_Line_In_Buffer (B : in Out Screen_Buffer; Y : Positive);
   procedure Clear_Region (B : in Out Screen_Buffer;
                           X1, Y1, X2, Y2 : Positive);

   -- Character operations
   procedure Put_Char (B : in Out Screen_Buffer; X, Y : Positive; C : Character);
   function Get_Char (B : Screen_Buffer; X, Y : Positive) return Character;

   -- String operations
   procedure Put_String (B : in Out Screen_Buffer;
                         X, Y : Positive; S : String);
   procedure Put_String_Centered (B : in Out Screen_Buffer;
                                  Y : Positive; S : String);
   procedure Put_String_Right (B : in Out Screen_Buffer;
                               Y : Positive; S : String);

   -- Integer output
   procedure Put_Int (B : in Out Screen_Buffer;
                      X, Y : Positive; N : Integer);
   procedure Put_Int (B : in Out Screen_Buffer;
                      X, Y : Positive; N : Integer; Width : Positive);

   -- Line drawing
   procedure Horizontal_Line (B : in Out Screen_Buffer;
                              X1, X2, Y : Positive; C : Character := '-');
   procedure Vertical_Line (B : in Out Screen_Buffer;
                            X, Y1, Y2 : Positive; C : Character := '|');
   procedure Box (B : in Out Screen_Buffer;
                  X1, Y1, X2, Y2 : Positive);
   procedure Box_Filled (B : in Out Screen_Buffer;
                         X1, Y1, X2, Y2 : Positive; Fill : Character := ' ');

   -- Scrolling
   procedure Scroll_Up (B : in Out Screen_Buffer; Lines : Positive := 1);
   procedure Scroll_Down (B : in Out Screen_Buffer; Lines : Positive := 1);
   procedure Scroll_Left (B : in Out Screen_Buffer; Cols : Positive := 1);
   procedure Scroll_Right (B : in Out Screen_Buffer; Cols : Positive := 1);

   -- Buffer info
   function Buffer_Width (B : Screen_Buffer) return Natural;
   function Buffer_Height (B : Screen_Buffer) return Natural;

   -- Copy regions
   procedure Copy_Region (Src, Dst : in Out Screen_Buffer;
                          Src_X, Src_Y, Width, Height : Positive;
                          Dst_X, Dst_Y : Positive);

   -- Fill region
   procedure Fill_Region (B : in Out Screen_Buffer;
                          X1, Y1, X2, Y2 : Positive; C : Character);

   -- Get line as string
   function Get_Line_String (B : Screen_Buffer; Y : Positive) return String;

   -- Output buffer to screen (via console output)
   procedure Render_Buffer (B : Screen_Buffer);
   procedure Render_Line (B : Screen_Buffer; Y : Positive);
   procedure Render_Region (B : Screen_Buffer; X1, Y1, X2, Y2 : Positive);

   -- Direct screen operations (no buffering)
   procedure Clear_Screen;
   procedure Clear_To_End_Of_Line;
   procedure Clear_To_End_Of_Screen;
   procedure Goto_XY (X, Y : Positive);
   procedure Home;
   procedure Put_At (X, Y : Positive; C : Character);
   procedure Put_At (X, Y : Positive; S : String);

   -- Cursor control
   procedure Cursor_On;
   procedure Cursor_Off;
   procedure Save_Cursor;
   procedure Restore_Cursor;

   -- Screen attributes (if supported by terminal)
   procedure Set_Normal;
   procedure Set_Bold;
   procedure Set_Underline;
   procedure Set_Reverse;
   procedure Set_Blink;

   -- Simple window support
   type Window is limited private;

   procedure Init_Window (W : out Window; X, Y, Width, Height : Positive);
   procedure Clear_Window (W : in Out Window);
   procedure Put_In_Window (W : in Out Window; X, Y : Positive; C : Character);
   procedure Put_In_Window (W : in Out Window; X, Y : Positive; S : String);
   procedure Draw_Window_Border (W : Window);
   procedure Draw_Window_Title (W : Window; Title : String);

private

   type Char_Array is array (1 .. Max_Width) of Character;
   type Line_Array is array (1 .. Max_Height) of Char_Array;

   type Screen_Buffer is limited record
      Data   : Line_Array;
      Width  : Natural := Default_Width;
      Height : Natural := Default_Height;
   end record;

   type Window is limited record
      X      : Positive := 1;
      Y      : Positive := 1;
      Width  : Positive := 20;
      Height : Positive := 10;
   end record;

end GNAT.Screen;
