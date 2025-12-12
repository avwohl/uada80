-- Ada.Containers.Hashed_Maps for Z80
-- Generic hash-based key-value map container
--
-- Note: On Z80 with limited memory, maps have practical size limits.
-- This implementation uses open addressing with linear probing.

with Ada.Containers;

generic
   type Key_Type is private;
   type Element_Type is private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Hashed_Maps is
   pragma Preelaborate;

   type Map is tagged private;
   type Cursor is private;

   Empty_Map : constant Map;
   No_Element : constant Cursor;

   -- Comparison
   function "=" (Left, Right : Map) return Boolean;

   -- Capacity
   function Capacity (Container : Map) return Count_Type;
   procedure Reserve_Capacity (Container : in Out Map; Capacity : Count_Type);

   -- Query operations
   function Length (Container : Map) return Count_Type;
   function Is_Empty (Container : Map) return Boolean;
   procedure Clear (Container : in Out Map);

   -- Key/Element access
   function Key (Position : Cursor) return Key_Type;
   function Element (Position : Cursor) return Element_Type;
   function Element (Container : Map; Key : Key_Type) return Element_Type;

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   -- Cursor operations
   function First (Container : Map) return Cursor;
   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find (Container : Map; Key : Key_Type) return Cursor;
   function Contains (Container : Map; Key : Key_Type) return Boolean;

   -- Insertion
   procedure Insert
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Include
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Replace
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   -- Deletion
   procedure Delete (Container : in Out Map; Key : Key_Type);
   procedure Delete (Container : in Out Map; Position : in Out Cursor);
   procedure Exclude (Container : in Out Map; Key : Key_Type);

   -- Iteration
   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

private

   -- Hash table size for Z80 implementation (power of 2 for fast mod)
   Table_Size : constant := 64;

   type Bucket_Index is range 0 .. Table_Size - 1;

   type Entry_State is (Empty, Occupied, Deleted);

   type Entry_Type is record
      Key     : Key_Type;
      Element : Element_Type;
      State   : Entry_State := Empty;
   end record;

   type Entry_Array is array (Bucket_Index) of Entry_Type;

   type Map is tagged record
      Entries : Entry_Array;
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Map;
      Index     : Bucket_Index := 0;
   end record;

   Empty_Map : constant Map := (Entries => (others => (Key => <>, Element => <>, State => Empty)), Length => 0);
   No_Element : constant Cursor := (Container => null, Index => 0);

end Ada.Containers.Hashed_Maps;
