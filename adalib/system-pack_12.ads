-- System.Pack_12 for Z80
-- Packed array support for 12-bit elements

package System.Pack_12 is
   pragma Pure;

   type Bits_12 is mod 2 ** 12;
   for Bits_12'Size use 12;

   function Get_12
     (Arr : System.Address;
      N   : Natural) return Bits_12;

   procedure Set_12
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_12);

end System.Pack_12;
