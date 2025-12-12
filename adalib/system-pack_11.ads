-- System.Pack_11 for Z80
-- Packed array support for 11-bit elements

package System.Pack_11 is
   pragma Pure;

   type Bits_11 is mod 2 ** 11;
   for Bits_11'Size use 11;

   function Get_11
     (Arr : System.Address;
      N   : Natural) return Bits_11;

   procedure Set_11
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_11);

end System.Pack_11;
