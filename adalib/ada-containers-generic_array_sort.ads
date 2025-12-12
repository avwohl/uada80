-- Ada.Containers.Generic_Array_Sort for Z80
-- Generic in-place array sorting procedure
--
-- Implements insertion sort which is efficient for small arrays
-- and has low memory overhead (important for Z80)

generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
procedure Ada.Containers.Generic_Array_Sort (Container : in Out Array_Type);

pragma Preelaborate (Ada.Containers.Generic_Array_Sort);
