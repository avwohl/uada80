-- Ada.Containers.Bounded_Ordered_Sets for Z80
-- Generic bounded ordered set with compile-time capacity
--
-- Elements are maintained in sorted order

with Ada.Containers;

generic
   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Bounded_Ordered_Sets is
   pragma Preelaborate;

   type Set (Capacity : Count_Type) is tagged private;
   pragma Preelaborable_Initialization (Set);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   No_Element : constant Cursor;

   function "=" (Left, Right : Set) return Boolean;

   function Length (Container : Set) return Count_Type;
   function Is_Empty (Container : Set) return Boolean;
   procedure Clear (Container : in Out Set);

   function Element (Position : Cursor) return Element_Type;

   -- Iteration
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

   -- Modification
   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert (Container : in Out Set; New_Item : Element_Type);
   procedure Include (Container : in Out Set; New_Item : Element_Type);
   procedure Replace (Container : in Out Set; New_Item : Element_Type);
   procedure Exclude (Container : in Out Set; Item : Element_Type);
   procedure Delete (Container : in Out Set; Item : Element_Type);
   procedure Delete (Container : in Out Set; Position : in Out Cursor);
   procedure Delete_First (Container : in Out Set);
   procedure Delete_Last (Container : in Out Set);

   -- Set operations
   procedure Union (Target : in Out Set; Source : Set);
   procedure Intersection (Target : in Out Set; Source : Set);
   procedure Difference (Target : in Out Set; Source : Set);
   procedure Symmetric_Difference (Target : in Out Set; Source : Set);

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

   type Element_Array is array (Count_Type range <>) of Element_Type;

   type Set (Capacity : Count_Type) is tagged record
      Elements : Element_Array (1 .. Capacity);
      Length   : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Set;
      Index     : Count_Type := 0;
   end record;

   No_Element : constant Cursor := (Container => null, Index => 0);

end Ada.Containers.Bounded_Ordered_Sets;
