-- Ada.Containers.Ordered_Maps for Z80
-- Generic ordered key-value map container
--
-- Note: On Z80 with limited memory, maps have practical size limits.
-- This implementation uses a sorted array for simplicity and efficiency.

with Ada.Containers;

generic
   type Key_Type is private;
   type Element_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Ada.Containers.Ordered_Maps is
   pragma Preelaborate;

   type Map is tagged private;
   type Cursor is private;

   Empty_Map : constant Map;
   No_Element : constant Cursor;

   -- Comparison
   function "=" (Left, Right : Map) return Boolean;

   -- Query operations
   function Length (Container : Map) return Count_Type;
   function Is_Empty (Container : Map) return Boolean;
   procedure Clear (Container : in Out Map);

   -- Key operations
   function Key (Position : Cursor) return Key_Type;
   function Element (Position : Cursor) return Element_Type;
   function Element (Container : Map; Key : Key_Type) return Element_Type;

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   -- Cursor operations
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

   -- Insertion
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

   -- Deletion
   procedure Delete (Container : in Out Map; Key : Key_Type);
   procedure Delete (Container : in Out Map; Position : in Out Cursor);
   procedure Delete_First (Container : in Out Map);
   procedure Delete_Last (Container : in Out Map);

   procedure Exclude (Container : in Out Map; Key : Key_Type);

   -- Iteration
   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor));

private

   -- Maximum entries for Z80 implementation
   Max_Entries : constant := 64;

   type Entry_Index is range 0 .. Max_Entries;
   Null_Entry : constant Entry_Index := 0;

   type Entry_Type is record
      Key     : Key_Type;
      Element : Element_Type;
      Used    : Boolean := False;
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Type;

   type Map is tagged record
      Entries : Entry_Array;
      Length  : Count_Type := 0;
   end record;

   type Cursor is record
      Container : access constant Map;
      Index     : Entry_Index := Null_Entry;
   end record;

   Empty_Map : constant Map := (Entries => (others => (Key => <>, Element => <>, Used => False)), Length => 0);
   No_Element : constant Cursor := (Container => null, Index => Null_Entry);

end Ada.Containers.Ordered_Maps;
