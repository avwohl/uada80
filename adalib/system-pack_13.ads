-- System.Pack_13 for Z80
-- Packed array support for 13-bit elements

package System.Pack_13 is
   pragma Pure;

   type Bits_13 is mod 2 ** 13;
   for Bits_13'Size use 13;

   function Get_13
     (Arr : System.Address;
      N   : Natural) return Bits_13;

   procedure Set_13
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_13);

end System.Pack_13;
