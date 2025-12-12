-- Ada.Containers.Bounded_Hashed_Maps for Z80
-- Generic bounded hashed map with compile-time capacity
--
-- More memory-efficient than unbounded maps, suitable for Z80

with Ada.Containers;

generic
   type Key_Type is private;
   type Element_Type is private;
   with function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Bounded_Hashed_Maps is
   pragma Preelaborate;

   type Map (Capacity : Count_Type; Modulus : Ada.Containers.Hash_Type) is tagged private;
   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   No_Element : constant Cursor;

   function "=" (Left, Right : Map) return Boolean;

   function Length (Container : Map) return Count_Type;
   function Is_Empty (Container : Map) return Boolean;
   procedure Clear (Container : in Out Map);

   function Key (Position : Cursor) return Key_Type;
   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Key : Key_Type; Element : Element_Type));

   procedure Update_Element
     (Container : in Out Map;
      Position  : Cursor;
      Process   : not null access procedure (Key : Key_Type; Element : in Out Element_Type));

   -- Iteration
   function First (Container : Map) return Cursor;
   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in Out Cursor);
   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find (Container : Map; Key : Key_Type) return Cursor;
   function Contains (Container : Map; Key : Key_Type) return Boolean;
   function Element (Container : Map; Key : Key_Type) return Element_Type;

   -- Modification
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

   -- Iteration
   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

private

   type Node_Index is range 0 .. Count_Type'Last;
   No_Node : constant Node_Index := 0;

   type Node is record
      Key     : Key_Type;
      Element : Element_Type;
      Next    : Node_Index := No_Node;
      In_Use  : Boolean := False;
   end record;

   type Node_Array is array (Node_Index range <>) of Node;
   type Bucket_Array is array (Ada.Containers.Hash_Type range <>) of Node_Index;

   type Map (Capacity : Count_Type; Modulus : Ada.Containers.Hash_Type) is tagged record
      Nodes   : Node_Array (1 .. Node_Index (Capacity));
      Buckets : Bucket_Array (0 .. Modulus - 1) := (others => No_Node);
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Map;
      Node      : Node_Index := No_Node;
   end record;

   No_Element : constant Cursor := (Container => null, Node => No_Node);

end Ada.Containers.Bounded_Hashed_Maps;
