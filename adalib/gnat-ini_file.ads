-- GNAT.INI_File for Z80
-- INI file format parsing and generation

package GNAT.INI_File is
   pragma Preelaborate;

   Max_Sections   : constant := 8;
   Max_Keys       : constant := 32;
   Max_Name_Len   : constant := 16;
   Max_Value_Len  : constant := 48;

   type INI_Data is limited private;

   -- Initialize/Clear
   procedure Initialize (D : out INI_Data);
   procedure Clear (D : out INI_Data);

   -- Section operations
   procedure Add_Section (D : in Out INI_Data; Name : String);
   function Has_Section (D : INI_Data; Name : String) return Boolean;
   function Section_Count (D : INI_Data) return Natural;
   function Section_Name (D : INI_Data; Index : Positive) return String;
   procedure Remove_Section (D : in Out INI_Data; Name : String);

   -- Key/Value operations (with section)
   procedure Set_Value (D : in Out INI_Data;
                        Section, Key, Value : String);
   function Get_Value (D : INI_Data;
                       Section, Key : String) return String;
   function Get_Value_Or_Default (D : INI_Data;
                                  Section, Key : String;
                                  Default : String) return String;
   function Has_Key (D : INI_Data; Section, Key : String) return Boolean;
   procedure Remove_Key (D : in Out INI_Data; Section, Key : String);

   -- Integer values
   procedure Set_Int (D : in Out INI_Data;
                      Section, Key : String; Value : Integer);
   function Get_Int (D : INI_Data;
                     Section, Key : String) return Integer;
   function Get_Int_Or_Default (D : INI_Data;
                                Section, Key : String;
                                Default : Integer) return Integer;

   -- Boolean values
   procedure Set_Bool (D : in Out INI_Data;
                       Section, Key : String; Value : Boolean);
   function Get_Bool (D : INI_Data;
                      Section, Key : String) return Boolean;

   -- Count keys in section
   function Key_Count (D : INI_Data; Section : String) return Natural;
   function Key_Name (D : INI_Data;
                      Section : String; Index : Positive) return String;

   -- Generate INI format lines
   -- Returns line by line for file output
   type Line_Iterator is private;

   function First_Line (D : INI_Data) return Line_Iterator;
   function Has_Line (It : Line_Iterator) return Boolean;
   function Current_Line (D : INI_Data; It : Line_Iterator) return String;
   procedure Next_Line (D : INI_Data; It : in Out Line_Iterator);

   -- Parse a single INI line
   -- Returns True if line was valid and parsed
   function Parse_Line (D : in Out INI_Data;
                        Line : String;
                        Current_Section : in Out String) return Boolean;

   -- Parse helpers
   function Is_Section_Line (Line : String) return Boolean;
   function Is_Key_Value_Line (Line : String) return Boolean;
   function Is_Comment_Line (Line : String) return Boolean;
   function Extract_Section_Name (Line : String) return String;
   function Extract_Key (Line : String) return String;
   function Extract_Value (Line : String) return String;

   -- Trim whitespace
   function Trim (S : String) return String;
   function Trim_Left (S : String) return String;
   function Trim_Right (S : String) return String;

private

   type Key_Record is record
      Key       : String (1 .. Max_Name_Len);
      Key_Len   : Natural;
      Value     : String (1 .. Max_Value_Len);
      Value_Len : Natural;
      Section   : Natural;  -- Section index (0 = global)
      Used      : Boolean;
   end record;

   type Key_Array is array (1 .. Max_Keys) of Key_Record;

   type Section_Record is record
      Name    : String (1 .. Max_Name_Len);
      Name_Len : Natural;
      Used    : Boolean;
   end record;

   type Section_Array is array (1 .. Max_Sections) of Section_Record;

   type INI_Data is limited record
      Sections     : Section_Array;
      Section_Count : Natural := 0;
      Keys         : Key_Array;
      Key_Count    : Natural := 0;
   end record;

   type Line_Iterator is record
      Phase        : Natural;  -- 0=sections header, 1=section name, 2=keys
      Section_Idx  : Natural;
      Key_Idx      : Natural;
      Done         : Boolean;
   end record;

end GNAT.INI_File;
