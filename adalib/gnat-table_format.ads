-- GNAT.Table_Format for Z80
-- Simple text table formatting for console output

package GNAT.Table_Format is
   pragma Preelaborate;

   Max_Columns  : constant := 8;
   Max_Col_Width : constant := 20;
   Max_Row_Width : constant := 80;

   type Alignment is (Left, Center, Right);

   type Column_Info is record
      Width     : Positive := 10;
      Align     : Alignment := Left;
      Title     : String (1 .. Max_Col_Width);
      Title_Len : Natural := 0;
   end record;

   type Column_Array is array (1 .. Max_Columns) of Column_Info;

   type Table is limited private;

   -- Initialize table with column count
   procedure Initialize (T : out Table; Columns : Positive);

   -- Configure columns
   procedure Set_Column (T : in Out Table;
                         Col : Positive;
                         Width : Positive;
                         Align : Alignment := Left;
                         Title : String := "");

   -- Set column titles
   procedure Set_Title (T : in Out Table; Col : Positive; Title : String);

   -- Set column width
   procedure Set_Width (T : in Out Table; Col : Positive; Width : Positive);

   -- Set column alignment
   procedure Set_Align (T : in Out Table; Col : Positive; Align : Alignment);

   -- Get formatted row with string values
   function Format_Row (T : Table; Values : String) return String;
   -- Values are separated by '|' character

   -- Format a single cell
   function Format_Cell (T : Table; Col : Positive; Value : String) return String;

   -- Format integer cell
   function Format_Cell (T : Table; Col : Positive; Value : Integer) return String;

   -- Get header line (column titles)
   function Header_Line (T : Table) return String;

   -- Get separator line (dashes)
   function Separator_Line (T : Table) return String;
   function Separator_Line (T : Table; C : Character) return String;

   -- Get double separator line
   function Double_Line (T : Table) return String;

   -- Total width of table
   function Total_Width (T : Table) return Natural;

   -- Simple box drawing characters
   function Box_Top (T : Table) return String;
   function Box_Bottom (T : Table) return String;
   function Box_Row_Sep (T : Table) return String;

   -- Utility: pad string to width with alignment
   function Pad (S : String; Width : Positive; Align : Alignment) return String;

private

   type Table is limited record
      Columns   : Column_Array;
      Col_Count : Natural := 0;
   end record;

end GNAT.Table_Format;
