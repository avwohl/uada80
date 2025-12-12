-- System.Wid_Int body for Z80
-- Integer width computation

package body System.Wid_Int is

   -------------------
   -- Width_Integer --
   -------------------

   function Width_Integer (Lo, Hi : Integer) return Natural is
      Max_Abs : Integer;
      Width   : Natural := 1;
   begin
      -- Find maximum absolute value
      if abs Lo > abs Hi then
         Max_Abs := abs Lo;
      else
         Max_Abs := abs Hi;
      end if;

      -- Count digits needed
      while Max_Abs >= 10 loop
         Max_Abs := Max_Abs / 10;
         Width := Width + 1;
      end loop;

      -- Add 1 for sign (negative values have '-', positive have ' ')
      return Width + 1;
   end Width_Integer;

end System.Wid_Int;
