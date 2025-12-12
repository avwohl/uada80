-- System.Pack_03 body for Z80
-- Packed array support for 3-bit elements

with System.Storage_Elements;

package body System.Pack_03 is

   use System.Storage_Elements;

   type Cluster is mod 2 ** 8;
   for Cluster'Size use 8;

   ------------
   -- Get_03 --
   ------------

   function Get_03
     (Arr : System.Address;
      N   : Natural) return Bits_03
   is
      -- 3-bit element packing: 8 elements per 3 bytes
      Byte_Pos  : constant Natural := (N * 3) / 8;
      Bit_Pos   : constant Natural := (N * 3) mod 8;
      Byte_Addr : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Val : Natural;
   begin
      B1 := To_Ptr (Byte_Addr).all;

      if Bit_Pos <= 5 then
         -- Fits in single byte
         Val := Natural (B1 / (2 ** Bit_Pos)) mod 8;
      else
         -- Spans two bytes
         B2 := To_Ptr (Byte_Addr + 1).all;
         Val := Natural (B1 / (2 ** Bit_Pos)) +
                Natural ((B2 mod (2 ** (Bit_Pos + 3 - 8))) * (2 ** (8 - Bit_Pos)));
         Val := Val mod 8;
      end if;

      return Bits_03 (Val);
   end Get_03;

   ------------
   -- Set_03 --
   ------------

   procedure Set_03
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_03)
   is
      Byte_Pos  : constant Natural := (N * 3) / 8;
      Bit_Pos   : constant Natural := (N * 3) mod 8;
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

      if Bit_Pos <= 5 then
         -- Fits in single byte
         Mask1 := not Cluster (7 * (2 ** Bit_Pos));
         B1 := (B1 and Mask1) or Cluster (Natural (E) * (2 ** Bit_Pos));
         To_Ptr (Byte_Addr).all := B1;
      else
         -- Spans two bytes
         B2 := To_Ptr (Byte_Addr + 1).all;

         Mask1 := Cluster (2 ** Bit_Pos - 1);
         Mask2 := not Cluster (2 ** (Bit_Pos + 3 - 8) - 1);

         B1 := (B1 and Mask1) or Cluster ((Natural (E) mod (2 ** (8 - Bit_Pos))) * (2 ** Bit_Pos));
         B2 := (B2 and Mask2) or Cluster (Natural (E) / (2 ** (8 - Bit_Pos)));

         To_Ptr (Byte_Addr).all := B1;
         To_Ptr (Byte_Addr + 1).all := B2;
      end if;
   end Set_03;

end System.Pack_03;
