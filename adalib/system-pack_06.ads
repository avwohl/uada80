-- System.Pack_06 for Z80
-- Packed array support for 6-bit elements

package System.Pack_06 is
   pragma Pure;

   type Bits_06 is mod 2 ** 6;
   for Bits_06'Size use 6;

   function Get_06
     (Arr : System.Address;
      N   : Natural) return Bits_06;

   procedure Set_06
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_06);

end System.Pack_06;
