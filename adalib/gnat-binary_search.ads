-- GNAT.Binary_Search for Z80
-- Generic binary search utilities

package GNAT.Binary_Search is
   pragma Pure;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Find
     (A       : Array_Type;
      Element : Element_Type) return Index_Type;
   --  Find Element in sorted array, return index or raise Constraint_Error

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Find_Or_Nothing
     (A       : Array_Type;
      Element : Element_Type) return Natural;
   --  Find Element in sorted array, return Pos or 0 if not found

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Lower_Bound
     (A       : Array_Type;
      Element : Element_Type) return Index_Type;
   --  Find first position where Element could be inserted

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Upper_Bound
     (A       : Array_Type;
      Element : Element_Type) return Index_Type;
   --  Find last position where Element could be inserted

end GNAT.Binary_Search;
