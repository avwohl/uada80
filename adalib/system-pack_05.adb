-- System.Pack_05 body for Z80
-- Packed array support for 5-bit elements

with System.Storage_Elements;

package body System.Pack_05 is

   use System.Storage_Elements;

   type Cluster is mod 2 ** 8;
   for Cluster'Size use 8;

   ------------
   -- Get_05 --
   ------------

   function Get_05
     (Arr : System.Address;
      N   : Natural) return Bits_05
   is
      Byte_Pos  : constant Natural := (N * 5) / 8;
      Bit_Pos   : constant Natural := (N * 5) mod 8;
      Byte_Addr : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Val : Natural;
   begin
      B1 := To_Ptr (Byte_Addr).all;

      if Bit_Pos <= 3 then
         Val := Natural (B1 / (2 ** Bit_Pos)) mod 32;
      else
         B2 := To_Ptr (Byte_Addr + 1).all;
         Val := Natural (B1 / (2 ** Bit_Pos)) +
                Natural ((B2 mod (2 ** (Bit_Pos + 5 - 8))) * (2 ** (8 - Bit_Pos)));
         Val := Val mod 32;
      end if;

      return Bits_05 (Val);
   end Get_05;

   ------------
   -- Set_05 --
   ------------

   procedure Set_05
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_05)
   is
      Byte_Pos  : constant Natural := (N * 5) / 8;
      Bit_Pos   : constant Natural := (N * 5) mod 8;
      Byte_Addr : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Mask1 : Cluster;
      Mask2 : Cluster;
   begin
      B1 := To_Ptr (Byte_Addr).all;

      if Bit_Pos <= 3 then
         Mask1 := not Cluster (31 * (2 ** Bit_Pos));
         B1 := (B1 and Mask1) or Cluster (Natural (E) * (2 ** Bit_Pos));
         To_Ptr (Byte_Addr).all := B1;
      else
         B2 := To_Ptr (Byte_Addr + 1).all;

         Mask1 := Cluster (2 ** Bit_Pos - 1);
         Mask2 := not Cluster (2 ** (Bit_Pos + 5 - 8) - 1);

         B1 := (B1 and Mask1) or Cluster ((Natural (E) mod (2 ** (8 - Bit_Pos))) * (2 ** Bit_Pos));
         B2 := (B2 and Mask2) or Cluster (Natural (E) / (2 ** (8 - Bit_Pos)));

         To_Ptr (Byte_Addr).all := B1;
         To_Ptr (Byte_Addr + 1).all := B2;
      end if;
   end Set_05;

end System.Pack_05;
