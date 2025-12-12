-- System.Pack_10 for Z80
-- Packed array support for 10-bit elements

package System.Pack_10 is
   pragma Pure;

   type Bits_10 is mod 2 ** 10;
   for Bits_10'Size use 10;

   function Get_10
     (Arr : System.Address;
      N   : Natural) return Bits_10;

   procedure Set_10
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_10);

end System.Pack_10;
