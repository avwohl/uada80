-- Ada.Containers.Bounded_Hashed_Sets for Z80
-- Generic bounded hashed set with compile-time capacity

with Ada.Containers;

generic
   type Element_Type is private;
   with function Hash (Element : Element_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Elements (Left, Right : Element_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Bounded_Hashed_Sets is
   pragma Preelaborate;

   type Set (Capacity : Count_Type; Modulus : Ada.Containers.Hash_Type) is tagged private;
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
   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find (Container : Set; Item : Element_Type) return Cursor;
   function Contains (Container : Set; Item : Element_Type) return Boolean;

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

private

   type Node_Index is range 0 .. Count_Type'Last;
   No_Node : constant Node_Index := 0;

   type Node is record
      Element : Element_Type;
      Next    : Node_Index := No_Node;
      In_Use  : Boolean := False;
   end record;

   type Node_Array is array (Node_Index range <>) of Node;
   type Bucket_Array is array (Ada.Containers.Hash_Type range <>) of Node_Index;

   type Set (Capacity : Count_Type; Modulus : Ada.Containers.Hash_Type) is tagged record
      Nodes   : Node_Array (1 .. Node_Index (Capacity));
      Buckets : Bucket_Array (0 .. Modulus - 1) := (others => No_Node);
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Set;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

end Ada.Containers.Bounded_Hashed_Sets;
