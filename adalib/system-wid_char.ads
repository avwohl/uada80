-- System.Wid_Char for Z80
-- Character width computation for 'Width attribute

package System.Wid_Char is
   pragma Pure;

   -- Width of Character type (for 'Width attribute)
   function Width_Character (Lo, Hi : Character) return Natural;
   -- Returns the maximum width of Character'Image(X)
   -- for all X in Lo .. Hi

   -- Width of a single character image
   function Image_Width (C : Character) return Natural;
   -- Returns the width of Character'Image(C)

end System.Wid_Char;
