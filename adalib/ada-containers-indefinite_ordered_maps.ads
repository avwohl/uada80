-- Ada.Containers.Indefinite_Ordered_Maps for Z80
-- Generic ordered map for indefinite element types

with Ada.Containers;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Ordered_Maps is
   pragma Preelaborate;

   -- Maximum entries for Z80
   Max_Entries : constant := 32;

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
   function Last (Container : Map) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   function Find (Container : Map; Key : Key_Type) return Cursor;
   function Contains (Container : Map; Key : Key_Type) return Boolean;
   function Floor (Container : Map; Key : Key_Type) return Cursor;
   function Ceiling (Container : Map; Key : Key_Type) return Cursor;

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

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

private

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Entry_Type is record
      Key     : Key_Access := null;
      Element : Element_Access := null;
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Type;

   type Map is tagged record
      Entries : Entry_Array;
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Map;
      Index     : Count_Type := 0;
   end record;

   No_Element : constant Cursor := (Container => null, Index => 0);

   Empty_Map : constant Map := (Entries => (others => (Key => null, Element => null)), Length => 0);

end Ada.Containers.Indefinite_Ordered_Maps;
