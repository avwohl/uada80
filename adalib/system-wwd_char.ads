-- System.Wwd_Char for Z80
-- Wide_Wide_Character width calculation

package System.Wwd_Char is
   pragma Pure;

   type Wide_Wide_Character is new Natural range 0 .. 16#10FFFF#;

   -- Width of Wide_Wide_Character image
   function Width_Wide_Wide_Character
     (Lo, Hi : Wide_Wide_Character) return Natural;

end System.Wwd_Char;
