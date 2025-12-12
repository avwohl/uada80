-- Ada.Containers.Bounded_Vectors for Z80
-- Generic bounded vector with compile-time capacity
--
-- This is more efficient than regular Vectors as it has no dynamic allocation.

with Ada.Containers;

generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Bounded_Vectors is
   pragma Preelaborate;

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 .. Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   type Vector (Capacity : Count_Type) is tagged private;
   type Cursor is private;

   No_Element : constant Cursor;

   -- Comparison
   function "=" (Left, Right : Vector) return Boolean;

   -- Length and capacity
   function Length (Container : Vector) return Count_Type;
   function Is_Empty (Container : Vector) return Boolean;
   procedure Clear (Container : in out Vector);

   -- Element access
   function Element (Container : Vector; Index : Index_Type) return Element_Type;
   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in Out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type);

   procedure Replace_Element
     (Container : in Out Vector;
      Position  : Cursor;
      New_Item  : Element_Type);

   -- Index operations
   function First_Index (Container : Vector) return Index_Type;
   function Last_Index (Container : Vector) return Extended_Index;

   function First (Container : Vector) return Cursor;
   function Last (Container : Vector) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in Out Cursor);
   procedure Previous (Position : in Out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   -- Search
   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index;

   function Find
     (Container : Vector;
      Item      : Element_Type) return Cursor;

   function Contains (Container : Vector; Item : Element_Type) return Boolean;

   -- Modification
   procedure Append (Container : in Out Vector; New_Item : Element_Type);

   procedure Prepend (Container : in Out Vector; New_Item : Element_Type);

   procedure Insert
     (Container : in Out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type);

   procedure Insert
     (Container : in Out Vector;
      Before    : Cursor;
      New_Item  : Element_Type);

   procedure Delete (Container : in Out Vector; Index : Extended_Index);
   procedure Delete (Container : in Out Vector; Position : in Out Cursor);
   procedure Delete_First (Container : in Out Vector);
   procedure Delete_Last (Container : in Out Vector);

   -- Iteration
   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor));

private

   type Element_Array is array (Index_Type range <>) of Element_Type;

   type Vector (Capacity : Count_Type) is tagged record
      Elements : Element_Array (Index_Type'First .. Index_Type'First + Index_Type'Base (Capacity) - 1);
      Last     : Extended_Index := No_Index;
   end record;

   type Cursor is record
      Container : access constant Vector;
      Index     : Extended_Index := No_Index;
   end record;

   No_Element : constant Cursor := (Container => null, Index => No_Index);

end Ada.Containers.Bounded_Vectors;
