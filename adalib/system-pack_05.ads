-- System.Pack_05 for Z80
-- Packed array support for 5-bit elements

package System.Pack_05 is
   pragma Pure;

   type Bits_05 is mod 2 ** 5;
   for Bits_05'Size use 5;

   function Get_05
     (Arr : System.Address;
      N   : Natural) return Bits_05;

   procedure Set_05
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_05);

end System.Pack_05;
