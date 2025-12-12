-- GNAT.Progress for Z80
-- Progress bar and status indicator utilities

package GNAT.Progress is
   pragma Preelaborate;

   Max_Bar_Width : constant := 40;
   Max_Label_Len : constant := 20;

   type Progress_Bar is limited private;
   type Spinner is limited private;
   type Counter is limited private;

   ------------------
   -- Progress_Bar --
   ------------------

   -- Initialize progress bar
   procedure Initialize (PB : out Progress_Bar;
                         Min_Value : Integer := 0;
                         Max_Value : Integer := 100;
                         Width : Positive := 20);

   -- Set current value
   procedure Set_Value (PB : in Out Progress_Bar; Value : Integer);

   -- Increment by amount
   procedure Increment (PB : in Out Progress_Bar; Amount : Positive := 1);

   -- Set label
   procedure Set_Label (PB : in Out Progress_Bar; Label : String);

   -- Get current percentage (0-100)
   function Get_Percent (PB : Progress_Bar) return Natural;

   -- Get current value
   function Get_Value (PB : Progress_Bar) return Integer;

   -- Check if complete
   function Is_Complete (PB : Progress_Bar) return Boolean;

   -- Render progress bar as string
   function Render (PB : Progress_Bar) return String;

   -- Render with percentage
   function Render_With_Percent (PB : Progress_Bar) return String;

   -- Render with label
   function Render_Full (PB : Progress_Bar) return String;

   -- Different bar styles
   type Bar_Style is (Hash_Style, Block_Style, Arrow_Style, Dots_Style);

   procedure Set_Style (PB : in Out Progress_Bar; Style : Bar_Style);

   -- Set filled/empty characters
   procedure Set_Chars (PB : in Out Progress_Bar;
                        Filled : Character;
                        Empty : Character);

   -------------
   -- Spinner --
   -------------

   -- Initialize spinner
   procedure Initialize (S : out Spinner);

   -- Advance spinner to next frame
   procedure Tick (S : in Out Spinner);

   -- Get current spinner character
   function Current_Char (S : Spinner) return Character;

   -- Get spinner with optional label
   function Render (S : Spinner; Label : String := "") return String;

   -- Set spinner style
   type Spinner_Style is (Line_Style, Circle_Style, Dots_Style, Arrow_Style);

   procedure Set_Style (S : in Out Spinner; Style : Spinner_Style);

   -------------
   -- Counter --
   -------------

   -- Initialize counter (e.g., "Processing item 5 of 100")
   procedure Initialize (C : out Counter;
                         Total : Positive;
                         Prefix : String := "Item ";
                         Suffix : String := "");

   -- Set current count
   procedure Set_Count (C : in Out Counter; Value : Positive);

   -- Increment count
   procedure Increment (C : in Out Counter);

   -- Get current count
   function Get_Count (C : Counter) return Positive;

   -- Get total
   function Get_Total (C : Counter) return Positive;

   -- Check if complete
   function Is_Complete (C : Counter) return Boolean;

   -- Render as string (e.g., "Item 5 of 100")
   function Render (C : Counter) return String;

   -- Render with percentage
   function Render_With_Percent (C : Counter) return String;

   ---------------
   -- Utilities --
   ---------------

   -- Simple percentage bar (standalone function)
   function Percent_Bar (Percent : Natural; Width : Positive := 20) return String;

   -- Simple ratio display
   function Ratio_Display (Current, Total : Natural) return String;

   -- Elapsed time display (in seconds)
   function Elapsed_Display (Seconds : Natural) return String;

   -- ETA display (estimated seconds remaining)
   function ETA_Display (Percent : Natural; Elapsed_Seconds : Natural) return String;

private

   type Progress_Bar is limited record
      Min_Val   : Integer := 0;
      Max_Val   : Integer := 100;
      Current   : Integer := 0;
      Width     : Positive := 20;
      Style     : Bar_Style := Hash_Style;
      Filled_C  : Character := '#';
      Empty_C   : Character := '-';
      Label     : String (1 .. Max_Label_Len) := (others => ' ');
      Label_Len : Natural := 0;
   end record;

   type Spinner is limited record
      Frame : Natural := 0;
      Style : Spinner_Style := Line_Style;
   end record;

   type Counter is limited record
      Current    : Positive := 1;
      Total      : Positive := 1;
      Prefix     : String (1 .. Max_Label_Len) := (others => ' ');
      Prefix_Len : Natural := 0;
      Suffix     : String (1 .. Max_Label_Len) := (others => ' ');
      Suffix_Len : Natural := 0;
   end record;

end GNAT.Progress;
