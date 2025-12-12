-- Ada.Containers.Bounded_Ordered_Maps for Z80
-- Generic bounded ordered map with compile-time capacity
--
-- Keys are maintained in sorted order

with Ada.Containers;

generic
   type Key_Type is private;
   type Element_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Bounded_Ordered_Maps is
   pragma Preelaborate;

   type Map (Capacity : Count_Type) is tagged private;
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
   function Element (Container : Map; Key : Key_Type) return Element_Type;

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   -- Iteration
   function First (Container : Map) return Cursor;
   function Last (Container : Map) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find (Container : Map; Key : Key_Type) return Cursor;
   function Contains (Container : Map; Key : Key_Type) return Boolean;
   function Floor (Container : Map; Key : Key_Type) return Cursor;
   function Ceiling (Container : Map; Key : Key_Type) return Cursor;

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
   procedure Delete_First (Container : in Out Map);
   procedure Delete_Last (Container : in Out Map);

   -- Iteration
   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

private

   type Entry_Type is record
      Key     : Key_Type;
      Element : Element_Type;
   end record;

   type Entry_Array is array (Count_Type range <>) of Entry_Type;

   type Map (Capacity : Count_Type) is tagged record
      Entries : Entry_Array (1 .. Capacity);
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Map;
      Index     : Count_Type := 0;
   end record;

   No_Element : constant Cursor := (Container => null, Index => 0);

end Ada.Containers.Bounded_Ordered_Maps;
