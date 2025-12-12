-- System.Wid_WWC for Z80
-- Wide_Wide_Character width

package System.Wid_WWC is
   pragma Pure;

   type Wide_Wide_Character is new Natural range 0 .. 16#10FFFF#;

   -- Width of Wide_Wide_Character for given range
   function Width_Wide_Wide_Char
     (Lo, Hi : Wide_Wide_Character) return Natural;

end System.Wid_WWC;
