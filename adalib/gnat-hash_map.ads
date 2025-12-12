-- GNAT.Hash_Map for Z80
-- Simple hash map with string keys and integer values

package GNAT.Hash_Map is
   pragma Preelaborate;

   Bucket_Count : constant := 16;  -- Hash buckets for Z80
   Max_Entries : constant := 32;   -- Maximum entries
   Max_Key_Length : constant := 16;

   type Hash_Map is private;

   procedure Initialize (M : out Hash_Map);
   --  Initialize empty map

   function Is_Empty (M : Hash_Map) return Boolean;
   --  Check if map is empty

   function Size (M : Hash_Map) return Natural;
   --  Return number of entries

   function Contains (M : Hash_Map; Key : String) return Boolean;
   --  Check if key exists

   function Get (M : Hash_Map; Key : String) return Integer;
   --  Get value for key (0 if not found)

   function Get (M : Hash_Map; Key : String; Default : Integer) return Integer;
   --  Get value with default

   procedure Put (M : in Out Hash_Map; Key : String; Value : Integer);
   --  Insert or update key-value pair

   procedure Remove (M : in Out Hash_Map; Key : String);
   --  Remove key from map

   procedure Clear (M : out Hash_Map);
   --  Clear all entries

   -- Iteration
   type Entry_Info is record
      Key    : String (1 .. Max_Key_Length);
      Key_Len : Natural;
      Value  : Integer;
      Valid  : Boolean;
   end record;

   function Entry_Count (M : Hash_Map) return Natural;
   function Get_Entry (M : Hash_Map; Index : Positive) return Entry_Info;

   -- Statistics
   function Collision_Count (M : Hash_Map) return Natural;
   function Load_Factor (M : Hash_Map) return Natural;  -- Percentage

private

   type Key_String is array (1 .. Max_Key_Length) of Character;

   type Entry_Type is record
      Key     : Key_String := (others => ASCII.NUL);
      Key_Len : Natural := 0;
      Value   : Integer := 0;
      Valid   : Boolean := False;
      Next    : Natural := 0;  -- For chaining
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Type;
   type Bucket_Array is array (1 .. Bucket_Count) of Natural;

   type Hash_Map is record
      Entries    : Entry_Array;
      Buckets    : Bucket_Array := (others => 0);
      Count      : Natural := 0;
      Collisions : Natural := 0;
   end record;

end GNAT.Hash_Map;
