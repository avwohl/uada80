-- GNAT.Dynamic_HTables for Z80
-- Dynamic hash tables with reallocation

with Ada.Containers;

generic
   type Key_Type is private;
   type Value_Type is private;
   No_Value : Value_Type;
   with function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package GNAT.Dynamic_HTables is
   pragma Preelaborate;

   type Instance is private;

   function Create (Initial_Size : Positive := 37) return Instance;
   --  Create a new hash table

   procedure Destroy (T : in Out Instance);
   --  Destroy hash table and free memory

   procedure Put (T : in Out Instance; Key : Key_Type; Value : Value_Type);
   --  Insert or update key-value pair

   function Get (T : Instance; Key : Key_Type) return Value_Type;
   --  Get value for key, returns No_Value if not found

   function Contains (T : Instance; Key : Key_Type) return Boolean;
   --  Check if key exists

   procedure Remove (T : in Out Instance; Key : Key_Type);
   --  Remove key from table

   function Size (T : Instance) return Natural;
   --  Number of entries

   -- Iteration
   type Cursor is private;

   function First (T : Instance) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Has_Element (Position : Cursor) return Boolean;
   function Key (Position : Cursor) return Key_Type;
   function Value (Position : Cursor) return Value_Type;

private
   type Entry_State is (Empty, Used, Deleted);

   type Entry_Type is record
      State : Entry_State := Empty;
      Key   : Key_Type;
      Value : Value_Type;
   end record;

   Max_Size : constant := 1000;  -- Max entries for Z80
   type Entry_Array is array (1 .. Max_Size) of Entry_Type;

   type Instance is record
      Entries  : Entry_Array;
      Capacity : Positive := 37;
      Count    : Natural := 0;
   end record;

   type Cursor is record
      Table : access Instance;
      Index : Natural := 0;
   end record;

end GNAT.Dynamic_HTables;
