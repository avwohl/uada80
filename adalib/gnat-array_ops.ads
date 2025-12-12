-- GNAT.Array_Ops for Z80
-- Generic array operations

generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package GNAT.Array_Ops is
   pragma Pure;

   function Contains
     (Container : Array_Type;
      Item      : Element_Type) return Boolean;
   --  Check if array contains item

   function Index
     (Container : Array_Type;
      Item      : Element_Type) return Index_Type'Base;
   --  Return index of item or Index_Type'First - 1 if not found

   function Count
     (Container : Array_Type;
      Item      : Element_Type) return Natural;
   --  Count occurrences of item

   procedure Reverse_Array (Container : in Out Array_Type);
   --  Reverse array in place

   procedure Rotate_Left (Container : in Out Array_Type; Amount : Natural := 1);
   --  Rotate left by Amount positions

   procedure Rotate_Right (Container : in Out Array_Type; Amount : Natural := 1);
   --  Rotate right by Amount positions

end GNAT.Array_Ops;
