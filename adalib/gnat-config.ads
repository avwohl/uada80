-- GNAT.Config for Z80
-- Application configuration management

package GNAT.Config is
   pragma Preelaborate;

   Max_Config_Entries : constant := 24;
   Max_Key_Length     : constant := 16;
   Max_Value_Length   : constant := 32;
   Max_Section_Length : constant := 12;

   type Config_Store is limited private;

   -- Initialize config
   procedure Initialize (C : out Config_Store);

   -- Set/Get values (with optional section)
   procedure Set (C : in Out Config_Store; Key, Value : String);
   procedure Set (C : in Out Config_Store; Section, Key, Value : String);
   function Get (C : Config_Store; Key : String) return String;
   function Get (C : Config_Store; Section, Key : String) return String;
   function Get_Or_Default (C : Config_Store; Key : String;
                            Default : String) return String;
   function Get_Or_Default (C : Config_Store; Section, Key : String;
                            Default : String) return String;

   -- Check if key exists
   function Contains (C : Config_Store; Key : String) return Boolean;
   function Contains (C : Config_Store; Section, Key : String) return Boolean;

   -- Remove entry
   procedure Remove (C : in Out Config_Store; Key : String);
   procedure Remove (C : in Out Config_Store; Section, Key : String);

   -- Type-specific getters/setters
   procedure Set_Int (C : in Out Config_Store; Key : String; Value : Integer);
   procedure Set_Int (C : in Out Config_Store; Section, Key : String; Value : Integer);
   function Get_Int (C : Config_Store; Key : String) return Integer;
   function Get_Int (C : Config_Store; Section, Key : String) return Integer;
   function Get_Int_Or_Default (C : Config_Store; Key : String;
                                Default : Integer) return Integer;

   procedure Set_Bool (C : in Out Config_Store; Key : String; Value : Boolean);
   function Get_Bool (C : Config_Store; Key : String) return Boolean;
   function Get_Bool_Or_Default (C : Config_Store; Key : String;
                                 Default : Boolean) return Boolean;

   -- Clear all entries
   procedure Clear (C : out Config_Store);

   -- Clear section
   procedure Clear_Section (C : in Out Config_Store; Section : String);

   -- Count entries
   function Count (C : Config_Store) return Natural;
   function Count_In_Section (C : Config_Store; Section : String) return Natural;

   -- Get all keys (comma-separated)
   function All_Keys (C : Config_Store) return String;
   function Keys_In_Section (C : Config_Store; Section : String) return String;

   -- Get all sections (comma-separated)
   function All_Sections (C : Config_Store) return String;

   -- Iteration
   type Cursor is private;
   function First (C : Config_Store) return Cursor;
   function First_In_Section (C : Config_Store; Section : String) return Cursor;
   function Has_Element (Cur : Cursor) return Boolean;
   procedure Next (C : Config_Store; Cur : in Out Cursor);
   function Key (C : Config_Store; Cur : Cursor) return String;
   function Value (C : Config_Store; Cur : Cursor) return String;
   function Section (C : Config_Store; Cur : Cursor) return String;

   -- Load/Save to string format (for file I/O)
   -- Format: "section.key=value" or "key=value" per line
   function To_String (C : Config_Store) return String;
   procedure From_Line (C : in Out Config_Store; Line : String);

   -- Merge another config
   procedure Merge (Target : in Out Config_Store; Source : Config_Store);

   -- Copy value from one key to another
   procedure Copy_Value (C : in Out Config_Store; From_Key, To_Key : String);

   -- Defaults management
   procedure Set_Default (C : in Out Config_Store; Key, Value : String);
   function Has_Default (C : Config_Store; Key : String) return Boolean;
   procedure Apply_Defaults (C : in Out Config_Store);

private

   type Entry_Record is record
      Section    : String (1 .. Max_Section_Length);
      Section_Len : Natural;
      Key        : String (1 .. Max_Key_Length);
      Key_Len    : Natural;
      Value      : String (1 .. Max_Value_Length);
      Value_Len  : Natural;
      Used       : Boolean;
      Is_Default : Boolean;
   end record;

   type Entry_Array is array (1 .. Max_Config_Entries) of Entry_Record;

   type Config_Store is limited record
      Entries : Entry_Array;
      Count   : Natural := 0;
   end record;

   type Cursor is record
      Index : Natural := 0;
   end record;

end GNAT.Config;
