-- System.Wid_Bool for Z80
-- Boolean width computation for 'Width attribute

package System.Wid_Bool is
   pragma Pure;

   -- Width of Boolean type (for 'Width attribute)
   function Width_Boolean (Lo, Hi : Boolean) return Natural;
   -- Returns the maximum width of Boolean'Image(X)
   -- for all X in Lo .. Hi

end System.Wid_Bool;
