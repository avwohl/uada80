-- System.Pack_07 for Z80
-- Packed array support for 7-bit elements

package System.Pack_07 is
   pragma Pure;

   type Bits_07 is mod 2 ** 7;
   for Bits_07'Size use 7;

   function Get_07
     (Arr : System.Address;
      N   : Natural) return Bits_07;

   procedure Set_07
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_07);

end System.Pack_07;
