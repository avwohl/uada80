-- Ada.Containers.Indefinite_Ordered_Sets for Z80
-- Generic ordered set for indefinite element types

with Ada.Containers;

generic
   type Element_Type (<>) is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Ordered_Sets is
   pragma Preelaborate;

   -- Maximum entries for Z80
   Max_Entries : constant := 32;

   type Set is tagged private;
   type Cursor is private;

   No_Element : constant Cursor;
   Empty_Set  : constant Set;

   function "=" (Left, Right : Set) return Boolean;

   function Length (Container : Set) return Count_Type;
   function Is_Empty (Container : Set) return Boolean;
   procedure Clear (Container : in Out Set);

   function Element (Position : Cursor) return Element_Type;

   function First (Container : Set) return Cursor;
   function Last (Container : Set) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   function Find (Container : Set; Item : Element_Type) return Cursor;
   function Contains (Container : Set; Item : Element_Type) return Boolean;
   function Floor (Container : Set; Item : Element_Type) return Cursor;
   function Ceiling (Container : Set; Item : Element_Type) return Cursor;

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

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));

   -- Set operations
   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;
   function Overlap (Left, Right : Set) return Boolean;

private

   type Element_Access is access Element_Type;

   type Entry_Array is array (1 .. Max_Entries) of Element_Access;

   type Set is tagged record
      Entries : Entry_Array := (others => null);
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Set;
      Index     : Count_Type := 0;
   end record;

   No_Element : constant Cursor := (Container => null, Index => 0);

   Empty_Set : constant Set := (Entries => (others => null), Length => 0);

end Ada.Containers.Indefinite_Ordered_Sets;
