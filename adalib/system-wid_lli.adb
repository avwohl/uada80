-- System.Wid_LLI body for Z80
-- Long_Long_Integer width computation

package body System.Wid_LLI is

   -----------------------------
   -- Width_Long_Long_Integer --
   -----------------------------

   function Width_Long_Long_Integer
     (Lo, Hi : Long_Long_Integer) return Natural
   is
      Max_Abs : Long_Long_Integer;
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
   end Width_Long_Long_Integer;

end System.Wid_LLI;
