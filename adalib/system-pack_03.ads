-- System.Pack_03 for Z80
-- Packed array support for 3-bit elements

with System.Unsigned_Types;

package System.Pack_03 is
   pragma Pure;

   type Bits_03 is mod 2 ** 3;
   for Bits_03'Size use 3;

   -- Packed array indexing
   function Get_03
     (Arr : System.Address;
      N   : Natural) return Bits_03;

   procedure Set_03
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_03);

end System.Pack_03;
