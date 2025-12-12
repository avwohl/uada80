-- System.Wid_Int for Z80
-- Integer width computation for 'Width attribute

package System.Wid_Int is
   pragma Pure;

   -- Width of Integer type (for 'Width attribute)
   function Width_Integer (Lo, Hi : Integer) return Natural;
   -- Returns the maximum width of Integer'Image(X)
   -- for all X in Lo .. Hi

end System.Wid_Int;
