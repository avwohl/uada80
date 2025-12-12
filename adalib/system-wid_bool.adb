-- System.Wid_Bool body for Z80
-- Boolean width computation for 'Width attribute

package body System.Wid_Bool is

   -------------------
   -- Width_Boolean --
   -------------------

   function Width_Boolean (Lo, Hi : Boolean) return Natural is
   begin
      if Lo = Hi then
         if Lo then
            return 4;  -- "TRUE"
         else
            return 5;  -- "FALSE"
         end if;
      else
         return 5;  -- Max of "TRUE" (4) and "FALSE" (5)
      end if;
   end Width_Boolean;

end System.Wid_Bool;
