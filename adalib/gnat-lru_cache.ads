-- GNAT.LRU_Cache for Z80
-- Least Recently Used cache implementation

package GNAT.LRU_Cache is
   pragma Preelaborate;

   Max_Cache_Size : constant := 16;  -- Limited for Z80 memory
   Max_Key_Length : constant := 16;
   Max_Value_Length : constant := 64;

   type Cache is private;

   procedure Initialize (C : out Cache; Capacity : Positive := Max_Cache_Size);
   --  Initialize cache with given capacity

   function Contains (C : Cache; Key : String) return Boolean;
   --  Check if key exists in cache

   procedure Get
     (C       : in Out Cache;
      Key     : String;
      Value   : out String;
      Last    : out Natural;
      Found   : out Boolean);
   --  Get value for key, updates access time

   procedure Put
     (C     : in Out Cache;
      Key   : String;
      Value : String);
   --  Put key-value pair, evicts LRU if full

   procedure Remove (C : in Out Cache; Key : String);
   --  Remove key from cache

   procedure Clear (C : out Cache);
   --  Clear all entries

   function Size (C : Cache) return Natural;
   --  Return number of entries

   function Capacity (C : Cache) return Positive;
   --  Return maximum capacity

   function Hits (C : Cache) return Natural;
   --  Return cache hit count

   function Misses (C : Cache) return Natural;
   --  Return cache miss count

   procedure Reset_Stats (C : in Out Cache);
   --  Reset hit/miss counters

private

   type Key_String is array (1 .. Max_Key_Length) of Character;
   type Value_String is array (1 .. Max_Value_Length) of Character;

   type Cache_Entry is record
      Key        : Key_String := (others => ASCII.NUL);
      Key_Len    : Natural := 0;
      Value      : Value_String := (others => ASCII.NUL);
      Value_Len  : Natural := 0;
      Access_Seq : Natural := 0;  -- Access sequence number
      Valid      : Boolean := False;
   end record;

   type Entry_Array is array (1 .. Max_Cache_Size) of Cache_Entry;

   type Cache is record
      Entries    : Entry_Array;
      Count      : Natural := 0;
      Cap        : Positive := Max_Cache_Size;
      Seq        : Natural := 0;  -- Global sequence counter
      Hit_Count  : Natural := 0;
      Miss_Count : Natural := 0;
   end record;

end GNAT.LRU_Cache;
