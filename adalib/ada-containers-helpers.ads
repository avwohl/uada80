-- Ada.Containers.Helpers for Z80
-- Internal container helpers

package Ada.Containers.Helpers is
   pragma Pure;

   generic
      type Element_Type is private;
   function Generic_Equal (Left, Right : Element_Type) return Boolean;
   --  Generic equality function

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   function Generic_Array_Equal
     (Left, Right : Array_Type) return Boolean;
   --  Compare two arrays for equality

   procedure Raise_Capacity_Error;
   pragma No_Return (Raise_Capacity_Error);
   --  Raise Capacity_Error exception

   procedure Raise_Tampering_Error;
   pragma No_Return (Raise_Tampering_Error);
   --  Raise Program_Error for tampering

end Ada.Containers.Helpers;
