-- System.Wid_LLI for Z80
-- Long_Long_Integer width computation for 'Width attribute

package System.Wid_LLI is
   pragma Pure;

   -- Width of Long_Long_Integer type (for 'Width attribute)
   function Width_Long_Long_Integer
     (Lo, Hi : Long_Long_Integer) return Natural;
   -- Returns the maximum width of Long_Long_Integer'Image(X)
   -- for all X in Lo .. Hi

end System.Wid_LLI;
