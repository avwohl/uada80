-- Ada.Containers.Ordered_Sets for Z80
-- Generic ordered set container
--
-- Note: On Z80 with limited memory, sets have practical size limits.
-- This implementation uses a sorted array for simplicity.

with Ada.Containers;

generic
   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Ordered_Sets is
   pragma Preelaborate;

   type Set is tagged private;
   type Cursor is private;

   Empty_Set : constant Set;
   No_Element : constant Cursor;

   -- Comparison
   function "=" (Left, Right : Set) return Boolean;
   function Equivalent_Sets (Left, Right : Set) return Boolean;

   -- Query operations
   function Length (Container : Set) return Count_Type;
   function Is_Empty (Container : Set) return Boolean;
   procedure Clear (Container : in Out Set);

   -- Element access
   function Element (Position : Cursor) return Element_Type;

   -- Cursor operations
   function First (Container : Set) return Cursor;
   function Last (Container : Set) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find (Container : Set; Item : Element_Type) return Cursor;
   function Contains (Container : Set; Item : Element_Type) return Boolean;

   function Floor (Container : Set; Item : Element_Type) return Cursor;
   function Ceiling (Container : Set; Item : Element_Type) return Cursor;

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
   procedure Delete_First (Container : in Out Set);
   procedure Delete_Last (Container : in Out Set);

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

   procedure Reverse_Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));

private

   -- Maximum elements for Z80 implementation
   Max_Elements : constant := 64;

   type Element_Index is range 0 .. Max_Elements;
   Null_Index : constant Element_Index := 0;

   type Element_Array is array (1 .. Max_Elements) of Element_Type;

   type Set is tagged record
      Elements : Element_Array;
      Length   : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Set;
      Index     : Element_Index := Null_Index;
   end record;

   Empty_Set : constant Set := (Elements => (others => <>), Length => 0);
   No_Element : constant Cursor := (Container => null, Index => Null_Index);

end Ada.Containers.Ordered_Sets;
