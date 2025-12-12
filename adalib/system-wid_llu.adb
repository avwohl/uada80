-- System.Wid_LLU body for Z80
-- Long_Long_Unsigned width computation

package body System.Wid_LLU is

   ------------------------------
   -- Width_Long_Long_Unsigned --
   ------------------------------

   function Width_Long_Long_Unsigned
     (Lo, Hi : Long_Long_Unsigned) return Natural
   is
      Max_Val : Long_Long_Unsigned;
      Width   : Natural := 1;
   begin
      -- Maximum width is determined by largest value
      if Hi > Lo then
         Max_Val := Hi;
      else
         Max_Val := Lo;
      end if;

      -- Count digits needed
      while Max_Val >= 10 loop
         Max_Val := Max_Val / 10;
         Width := Width + 1;
      end loop;

      -- Add 1 for leading space (standard Ada Image format)
      return Width + 1;
   end Width_Long_Long_Unsigned;

end System.Wid_LLU;
