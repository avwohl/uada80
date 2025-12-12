-- Ada.Containers.Hashed_Sets for Z80
-- Generic hash-based set container
--
-- Note: On Z80 with limited memory, sets have practical size limits.
-- This implementation uses open addressing with linear probing.

with Ada.Containers;

generic
   type Element_Type is private;
   with function Hash (Element : Element_Type) return Hash_Type;
   with function Equivalent_Elements (Left, Right : Element_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Hashed_Sets is
   pragma Preelaborate;

   type Set is tagged private;
   type Cursor is private;

   Empty_Set : constant Set;
   No_Element : constant Cursor;

   -- Comparison
   function "=" (Left, Right : Set) return Boolean;
   function Equivalent_Sets (Left, Right : Set) return Boolean;

   -- Capacity
   function Capacity (Container : Set) return Count_Type;
   procedure Reserve_Capacity (Container : in Out Set; Capacity : Count_Type);

   -- Query operations
   function Length (Container : Set) return Count_Type;
   function Is_Empty (Container : Set) return Boolean;
   procedure Clear (Container : in Out Set);

   -- Element access
   function Element (Position : Cursor) return Element_Type;

   -- Cursor operations
   function First (Container : Set) return Cursor;
   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find (Container : Set; Item : Element_Type) return Cursor;
   function Contains (Container : Set; Item : Element_Type) return Boolean;

   -- Insertion
   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type);

   procedure Include (Container : in Out Set; New_Item : Element_Type);

   procedure Replace (Container : in Out Set; New_Item : Element_Type);

   -- Deletion
   procedure Delete (Container : in Out Set; Item : Element_Type);
   procedure Delete (Container : in Out Set; Position : in Out Cursor);
   procedure Exclude (Container : in Out Set; Item : Element_Type);

   -- Set operations
   procedure Union (Target : in Out Set; Source : Set);
   function Union (Left, Right : Set) return Set;
   function "or" (Left, Right : Set) return Set renames Union;

   procedure Intersection (Target : in Out Set; Source : Set);
   function Intersection (Left, Right : Set) return Set;
   function "and" (Left, Right : Set) return Set renames Intersection;

   procedure Difference (Target : in Out Set; Source : Set);
   function Difference (Left, Right : Set) return Set;
   function "-" (Left, Right : Set) return Set renames Difference;

   procedure Symmetric_Difference (Target : in Out Set; Source : Set);
   function Symmetric_Difference (Left, Right : Set) return Set;
   function "xor" (Left, Right : Set) return Set renames Symmetric_Difference;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;
   function Overlap (Left, Right : Set) return Boolean;

   -- Iteration
   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));

private

   -- Hash table size for Z80 implementation
   Table_Size : constant := 64;

   type Bucket_Index is range 0 .. Table_Size - 1;

   type Entry_State is (Empty, Occupied, Deleted);

   type Entry_Type is record
      Element : Element_Type;
      State   : Entry_State := Empty;
   end record;

   type Entry_Array is array (Bucket_Index) of Entry_Type;

   type Set is tagged record
      Entries : Entry_Array;
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Set;
      Index     : Bucket_Index := 0;
   end record;

   Empty_Set : constant Set := (Entries => (others => (Element => <>, State => Empty)), Length => 0);
   No_Element : constant Cursor := (Container => null, Index => 0);

end Ada.Containers.Hashed_Sets;
