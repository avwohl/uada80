-- GNAT.Array_Utils for Z80
-- Generic array utilities

package GNAT.Array_Utils is
   pragma Pure;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Find_Min (A : Array_Type) return Index_Type;
   --  Find index of minimum element

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Find_Max (A : Array_Type) return Index_Type;
   --  Find index of maximum element

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   procedure Reverse_Array (A : in Out Array_Type);
   --  Reverse array in place

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   function Contains
     (A       : Array_Type;
      Element : Element_Type) return Boolean;
   --  Check if Element is in array

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   function Index_Of
     (A       : Array_Type;
      Element : Element_Type) return Integer;
   --  Return index of Element, or -1 if not found

end GNAT.Array_Utils;
