-- GNAT.INI_Files for Z80
-- Simple INI file parser

package GNAT.INI_Files is
   pragma Preelaborate;

   Max_Sections : constant := 8;    -- Maximum sections for Z80
   Max_Keys : constant := 32;       -- Maximum keys total
   Max_Name_Length : constant := 32;
   Max_Value_Length : constant := 64;

   type INI_File is private;

   procedure Initialize (F : out INI_File);
   --  Initialize empty INI structure

   procedure Parse (F : in Out INI_File; Content : String);
   --  Parse INI content string

   function Section_Count (F : INI_File) return Natural;
   --  Return number of sections

   function Section_Name (F : INI_File; Index : Positive) return String;
   --  Get section name by index

   function Has_Section (F : INI_File; Section : String) return Boolean;
   --  Check if section exists

   function Key_Count (F : INI_File; Section : String) return Natural;
   --  Return number of keys in section

   function Has_Key (F : INI_File; Section, Key : String) return Boolean;
   --  Check if key exists in section

   function Get_Value (F : INI_File; Section, Key : String) return String;
   --  Get value for key in section

   function Get_Value (F : INI_File; Section, Key : String;
                       Default : String) return String;
   --  Get value with default

   function Get_Integer (F : INI_File; Section, Key : String;
                         Default : Integer := 0) return Integer;
   --  Get value as integer

   function Get_Boolean (F : INI_File; Section, Key : String;
                         Default : Boolean := False) return Boolean;
   --  Get value as boolean (true/false, yes/no, 1/0)

   procedure Set_Value (F : in Out INI_File; Section, Key, Value : String);
   --  Set or add key-value pair

   procedure Remove_Key (F : in Out INI_File; Section, Key : String);
   --  Remove key from section

   procedure Remove_Section (F : in Out INI_File; Section : String);
   --  Remove entire section

   function To_String (F : INI_File) return String;
   --  Convert back to INI format string

   function Has_Error (F : INI_File) return Boolean;
   function Error_Line (F : INI_File) return Natural;

private

   type Name_String is array (1 .. Max_Name_Length) of Character;
   type Value_String is array (1 .. Max_Value_Length) of Character;

   type Key_Entry is record
      Name      : Name_String := (others => ASCII.NUL);
      Name_Len  : Natural := 0;
      Value     : Value_String := (others => ASCII.NUL);
      Value_Len : Natural := 0;
      Section   : Natural := 0;  -- Section index this key belongs to
      Valid     : Boolean := False;
   end record;

   type Section_Entry is record
      Name     : Name_String := (others => ASCII.NUL);
      Name_Len : Natural := 0;
      Valid    : Boolean := False;
   end record;

   type Section_Array is array (1 .. Max_Sections) of Section_Entry;
   type Key_Array is array (1 .. Max_Keys) of Key_Entry;

   type INI_File is record
      Sections    : Section_Array;
      Sec_Count   : Natural := 0;
      Keys        : Key_Array;
      Key_Count   : Natural := 0;
      Has_Err     : Boolean := False;
      Err_Line    : Natural := 0;
   end record;

end GNAT.INI_Files;
