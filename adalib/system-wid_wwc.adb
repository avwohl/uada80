-- System.Wid_WWC body for Z80
-- Wide_Wide_Character width

package body System.Wid_WWC is

   --------------------------
   -- Width_Wide_Wide_Char --
   --------------------------

   function Width_Wide_Wide_Char
     (Lo, Hi : Wide_Wide_Character) return Natural
   is
      pragma Unreferenced (Lo, Hi);
   begin
      -- Maximum width for Wide_Wide_Character image
      -- Format can be 'X' or Hex_XXXXXXXX
      return 12;
   end Width_Wide_Wide_Char;

end System.Wid_WWC;
