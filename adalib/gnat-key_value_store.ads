-- GNAT.Key_Value_Store for Z80
-- Persistent key-value storage using CP/M files

package GNAT.Key_Value_Store is
   pragma Preelaborate;

   Max_Entries    : constant := 32;
   Max_Key_Length : constant := 16;
   Max_Val_Length : constant := 64;

   type KV_Store is limited private;

   -- Initialize store
   procedure Initialize (S : out KV_Store);

   -- Basic operations
   procedure Put (S : in out KV_Store; Key, Value : String);
   function Get (S : KV_Store; Key : String) return String;
   function Get_Or_Default (S : KV_Store; Key : String;
                            Default : String) return String;
   function Contains (S : KV_Store; Key : String) return Boolean;
   procedure Delete (S : in Out KV_Store; Key : String);

   -- Integer values
   procedure Put_Int (S : in Out KV_Store; Key : String; Value : Integer);
   function Get_Int (S : KV_Store; Key : String) return Integer;
   function Get_Int_Or_Default (S : KV_Store; Key : String;
                                Default : Integer) return Integer;

   -- Boolean values
   procedure Put_Bool (S : in Out KV_Store; Key : String; Value : Boolean);
   function Get_Bool (S : KV_Store; Key : String) return Boolean;

   -- Increment/Decrement (for counters)
   procedure Increment (S : in Out KV_Store; Key : String; Amount : Integer := 1);
   procedure Decrement (S : in Out KV_Store; Key : String; Amount : Integer := 1);

   -- Clear all entries
   procedure Clear (S : out KV_Store);

   -- Count entries
   function Count (S : KV_Store) return Natural;
   function Is_Empty (S : KV_Store) return Boolean;
   function Is_Full (S : KV_Store) return Boolean;

   -- Iteration
   type Cursor is private;
   function First (S : KV_Store) return Cursor;
   function Has_Element (C : Cursor) return Boolean;
   procedure Next (S : KV_Store; C : in Out Cursor);
   function Key (S : KV_Store; C : Cursor) return String;
   function Value (S : KV_Store; C : Cursor) return String;

   -- Get key at index (1-based)
   function Key_At (S : KV_Store; Index : Positive) return String;
   function Value_At (S : KV_Store; Index : Positive) return String;

   -- Find key by prefix (returns first match)
   function Find_By_Prefix (S : KV_Store; Prefix : String) return String;

   -- Count keys with prefix
   function Count_With_Prefix (S : KV_Store; Prefix : String) return Natural;

   -- Delete all keys with prefix
   procedure Delete_With_Prefix (S : in Out KV_Store; Prefix : String);

   -- Rename a key (preserves value)
   procedure Rename (S : in Out KV_Store; Old_Key, New_Key : String);

   -- Copy value from one key to another
   procedure Copy_Value (S : in Out KV_Store; From_Key, To_Key : String);

   -- Append to existing value (with optional separator)
   procedure Append (S : in Out KV_Store; Key : String;
                     Value : String; Sep : String := "");

   -- Check if value matches
   function Value_Equals (S : KV_Store; Key : String;
                          Expected : String) return Boolean;

private

   type Entry_Record is record
      Key     : String (1 .. Max_Key_Length);
      Key_Len : Natural;
      Value   : String (1 .. Max_Val_Length);
      Val_Len : Natural;
      Used    : Boolean;
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Record;

   type KV_Store is limited record
      Entries : Entry_Array;
      Count   : Natural := 0;
   end record;

   type Cursor is record
      Index : Natural := 0;
   end record;

end GNAT.Key_Value_Store;
