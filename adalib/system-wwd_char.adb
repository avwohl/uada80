-- System.Wwd_Char body for Z80
-- Wide_Wide_Character width calculation

package body System.Wwd_Char is

   ---------------------------------
   -- Width_Wide_Wide_Character --
   ---------------------------------

   function Width_Wide_Wide_Character
     (Lo, Hi : Wide_Wide_Character) return Natural
   is
      pragma Unreferenced (Lo, Hi);
   begin
      -- Wide_Wide_Character'Image returns format like
      -- 'x' for printable characters or hex notation
      -- Maximum width is for hex notation: Hex_XXXXXXXX (12 chars)
      return 12;
   end Width_Wide_Wide_Character;

end System.Wwd_Char;
