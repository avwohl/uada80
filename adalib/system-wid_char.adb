-- System.Wid_Char body for Z80
-- Character width computation for 'Width attribute

package body System.Wid_Char is

   ---------------------
   -- Width_Character --
   ---------------------

   function Width_Character (Lo, Hi : Character) return Natural is
      W   : Natural := 0;
      Max : Natural := 0;
   begin
      for C in Lo .. Hi loop
         W := Image_Width (C);
         if W > Max then
            Max := W;
         end if;
      end loop;
      return Max;
   end Width_Character;

   -----------------
   -- Image_Width --
   -----------------

   function Image_Width (C : Character) return Natural is
      Pos : constant Natural := Character'Pos (C);
   begin
      -- Control characters have 3-character names (e.g., NUL, SOH)
      -- except for some 2-character ones (BS, HT, LF, VT, FF, CR, SO, SI)
      if Pos <= 31 then
         case C is
            when ASCII.BS | ASCII.HT | ASCII.LF | ASCII.VT |
                 ASCII.FF | ASCII.CR | ASCII.SO | ASCII.SI |
                 ASCII.FS | ASCII.GS | ASCII.RS | ASCII.US |
                 ASCII.EM =>
               return 2;
            when others =>
               return 3;
         end case;
      elsif Pos = 127 then
         -- DEL
         return 3;
      elsif Pos >= 128 and Pos <= 159 then
         -- Extended control characters have longer names
         return 4;  -- Approximate
      else
         -- Graphic characters: 'X' form has width 3
         return 3;
      end if;
   end Image_Width;

end System.Wid_Char;
