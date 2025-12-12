-- Ada.Containers.Helpers body for Z80
-- Internal container helpers implementation

package body Ada.Containers.Helpers is

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal (Left, Right : Element_Type) return Boolean is
   begin
      return Left = Right;
   end Generic_Equal;

   -------------------------
   -- Generic_Array_Equal --
   -------------------------

   function Generic_Array_Equal
     (Left, Right : Array_Type) return Boolean
   is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in Left'Range loop
         if Left (I) /= Right (Index_Type'Val
           (Index_Type'Pos (Right'First) + Index_Type'Pos (I) - Index_Type'Pos (Left'First)))
         then
            return False;
         end if;
      end loop;
      return True;
   end Generic_Array_Equal;

   --------------------------
   -- Raise_Capacity_Error --
   --------------------------

   procedure Raise_Capacity_Error is
   begin
      raise Capacity_Error with "container capacity exceeded";
   end Raise_Capacity_Error;

   ---------------------------
   -- Raise_Tampering_Error --
   ---------------------------

   procedure Raise_Tampering_Error is
   begin
      raise Program_Error with "tampering with cursors";
   end Raise_Tampering_Error;

end Ada.Containers.Helpers;
