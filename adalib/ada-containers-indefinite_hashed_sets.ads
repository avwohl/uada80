-- Ada.Containers.Indefinite_Hashed_Sets for Z80
-- Generic hashed set for indefinite element types

with Ada.Containers;

generic
   type Element_Type (<>) is private;
   with function Hash (Element : Element_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Elements (Left, Right : Element_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Hashed_Sets is
   pragma Preelaborate;

   -- Maximum entries for Z80
   Max_Entries : constant := 32;
   Num_Buckets : constant := 16;

   type Set is tagged private;
   type Cursor is private;

   No_Element : constant Cursor;
   Empty_Set  : constant Set;

   function "=" (Left, Right : Set) return Boolean;

   function Length (Container : Set) return Count_Type;
   function Is_Empty (Container : Set) return Boolean;
   procedure Clear (Container : in out Set);

   function Element (Position : Cursor) return Element_Type;

   function First (Container : Set) return Cursor;
   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   function Find (Container : Set; Item : Element_Type) return Cursor;
   function Contains (Container : Set; Item : Element_Type) return Boolean;

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

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor));

   -- Set operations
   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;
   function Overlap (Left, Right : Set) return Boolean;

private

   type Element_Access is access Element_Type;

   type Node_Index is range 0 .. Max_Entries;
   No_Node : constant Node_Index := 0;

   type Node is record
      Element : Element_Access := null;
      Next    : Node_Index := No_Node;
      In_Use  : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Entries) of Node;
   type Bucket_Array is array (0 .. Num_Buckets - 1) of Node_Index;

   type Set is tagged record
      Nodes   : Node_Array;
      Buckets : Bucket_Array := (others => No_Node);
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Set;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

   Empty_Set : constant Set := (Nodes => (others => (Element => null, Next => No_Node, In_Use => False)), Buckets => (others => No_Node), Length => 0);

end Ada.Containers.Indefinite_Hashed_Sets;
