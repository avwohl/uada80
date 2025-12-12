-- Ada.Containers.Generic_Anonymous_Array_Sort for Z80
-- Generic sorting via indexed access procedures
--
-- This allows sorting arrays indirectly through accessor functions,
-- useful when the array is not directly accessible.

generic
   type Index_Type is (<>);
   with function Less (Left, Right : Index_Type) return Boolean;
   with procedure Swap (Left, Right : Index_Type);
procedure Ada.Containers.Generic_Anonymous_Array_Sort
  (First, Last : Index_Type'Base);

pragma Preelaborate (Ada.Containers.Generic_Anonymous_Array_Sort);
