-- System.Wid_LLU for Z80
-- Long_Long_Unsigned width computation for 'Width attribute

package System.Wid_LLU is
   pragma Pure;

   type Long_Long_Unsigned is mod 2 ** 64;

   -- Width of Long_Long_Unsigned type (for 'Width attribute)
   function Width_Long_Long_Unsigned
     (Lo, Hi : Long_Long_Unsigned) return Natural;
   -- Returns the maximum width of Long_Long_Unsigned'Image(X)
   -- for all X in Lo .. Hi

end System.Wid_LLU;
