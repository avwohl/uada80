-- System.Pack_15 for Z80
-- Packed array support for 15-bit elements

package System.Pack_15 is
   pragma Pure;

   type Bits_15 is mod 2 ** 15;
   for Bits_15'Size use 15;

   function Get_15
     (Arr : System.Address;
      N   : Natural) return Bits_15;

   procedure Set_15
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_15);

end System.Pack_15;
