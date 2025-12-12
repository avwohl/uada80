-- System.Pack_09 for Z80
-- Packed array support for 9-bit elements

package System.Pack_09 is
   pragma Pure;

   type Bits_09 is mod 2 ** 9;
   for Bits_09'Size use 9;

   function Get_09
     (Arr : System.Address;
      N   : Natural) return Bits_09;

   procedure Set_09
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_09);

end System.Pack_09;
