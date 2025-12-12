-- GNAT.String_Maps for Z80
-- String-to-string mapping with bounded storage

package GNAT.String_Maps is
   pragma Preelaborate;

   Max_Entries    : constant := 16;
   Max_Key_Length : constant := 16;
   Max_Val_Length : constant := 32;

   type String_Map is limited private;

   -- Initialize map
   procedure Initialize (M : out String_Map);

   -- Set a key-value pair (overwrites if exists)
   procedure Set (M : in Out String_Map; Key, Value : String);

   -- Get value for key (returns empty string if not found)
   function Get (M : String_Map; Key : String) return String;

   -- Check if key exists
   function Contains (M : String_Map; Key : String) return Boolean;

   -- Remove a key
   procedure Remove (M : in Out String_Map; Key : String);

   -- Get default value if key not found
   function Get_Or_Default (M : String_Map; Key : String;
                            Default : String) return String;

   -- Clear all entries
   procedure Clear (M : out String_Map);

   -- Count of entries
   function Size (M : String_Map) return Natural;

   -- Check if empty/full
   function Is_Empty (M : String_Map) return Boolean;
   function Is_Full (M : String_Map) return Boolean;

   -- Get key at index (1-based)
   function Key_At (M : String_Map; Index : Positive) return String;

   -- Get value at index (1-based)
   function Value_At (M : String_Map; Index : Positive) return String;

   -- Find key by value (returns empty string if not found)
   function Find_Key (M : String_Map; Value : String) return String;

   -- Iteration support
   type Cursor is private;

   function First (M : String_Map) return Cursor;
   function Has_Element (C : Cursor) return Boolean;
   procedure Next (M : String_Map; C : in Out Cursor);
   function Key (M : String_Map; C : Cursor) return String;
   function Value (M : String_Map; C : Cursor) return String;

   -- Merge another map into this one (overwrites on conflict)
   procedure Merge (Target : in Out String_Map; Source : String_Map);

   -- Get all keys as concatenated string (separator between)
   function All_Keys (M : String_Map; Sep : String := ",") return String;

   -- Get all values as concatenated string
   function All_Values (M : String_Map; Sep : String := ",") return String;

private

   type Entry_Record is record
      Key     : String (1 .. Max_Key_Length);
      Key_Len : Natural;
      Value   : String (1 .. Max_Val_Length);
      Val_Len : Natural;
      Used    : Boolean;
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Record;

   type String_Map is limited record
      Entries : Entry_Array;
      Count   : Natural := 0;
   end record;

   type Cursor is record
      Index : Natural := 0;
   end record;

end GNAT.String_Maps;
