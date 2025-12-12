-- Ada.Containers.Indefinite_Hashed_Maps for Z80
-- Generic hashed map for indefinite element types

with Ada.Containers;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;
   with function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Hashed_Maps is
   pragma Preelaborate;

   -- Maximum entries for Z80
   Max_Entries : constant := 32;
   Num_Buckets : constant := 16;

   type Map is tagged private;
   type Cursor is private;

   No_Element : constant Cursor;
   Empty_Map  : constant Map;

   function "=" (Left, Right : Map) return Boolean;

   function Length (Container : Map) return Count_Type;
   function Is_Empty (Container : Map) return Boolean;
   procedure Clear (Container : in Out Map);

   function Key (Position : Cursor) return Key_Type;
   function Element (Position : Cursor) return Element_Type;
   function Element (Container : Map; Key : Key_Type) return Element_Type;

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   function First (Container : Map) return Cursor;
   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   function Find (Container : Map; Key : Key_Type) return Cursor;
   function Contains (Container : Map; Key : Key_Type) return Boolean;

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

   procedure Exclude (Container : in Out Map; Key : Key_Type);
   procedure Delete (Container : in Out Map; Key : Key_Type);
   procedure Delete (Container : in Out Map; Position : in Out Cursor);

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

private

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Node_Index is range 0 .. Max_Entries;
   No_Node : constant Node_Index := 0;

   type Node is record
      Key     : Key_Access := null;
      Element : Element_Access := null;
      Next    : Node_Index := No_Node;
      In_Use  : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Entries) of Node;
   type Bucket_Array is array (0 .. Num_Buckets - 1) of Node_Index;

   type Map is tagged record
      Nodes   : Node_Array;
      Buckets : Bucket_Array := (others => No_Node);
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Map;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

   Empty_Map : constant Map := (Nodes => (others => (Key => null, Element => null, Next => No_Node, In_Use => False)), Buckets => (others => No_Node), Length => 0);

end Ada.Containers.Indefinite_Hashed_Maps;
