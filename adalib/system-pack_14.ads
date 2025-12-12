-- System.Pack_14 for Z80
-- Packed array support for 14-bit elements

package System.Pack_14 is
   pragma Pure;

   type Bits_14 is mod 2 ** 14;
   for Bits_14'Size use 14;

   function Get_14
     (Arr : System.Address;
      N   : Natural) return Bits_14;

   procedure Set_14
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_14);

end System.Pack_14;
