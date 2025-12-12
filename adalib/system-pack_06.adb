-- System.Pack_06 body for Z80
-- Packed array support for 6-bit elements

with System.Storage_Elements;

package body System.Pack_06 is

   use System.Storage_Elements;

   type Cluster is mod 2 ** 8;

   ------------
   -- Get_06 --
   ------------

   function Get_06
     (Arr : System.Address;
      N   : Natural) return Bits_06
   is
      -- 6-bit packing: 4 elements per 3 bytes
      Byte_Pos  : constant Natural := (N * 6) / 8;
      Bit_Pos   : constant Natural := (N * 6) mod 8;
      Byte_Addr : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Val : Natural;
   begin
      B1 := To_Ptr (Byte_Addr).all;

      if Bit_Pos <= 2 then
         Val := Natural (B1 / (2 ** Bit_Pos)) mod 64;
      else
         B2 := To_Ptr (Byte_Addr + 1).all;
         Val := Natural (B1 / (2 ** Bit_Pos)) +
                Natural ((B2 mod (2 ** (Bit_Pos + 6 - 8))) * (2 ** (8 - Bit_Pos)));
         Val := Val mod 64;
      end if;

      return Bits_06 (Val);
   end Get_06;

   ------------
   -- Set_06 --
   ------------

   procedure Set_06
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_06)
   is
      Byte_Pos  : constant Natural := (N * 6) / 8;
      Bit_Pos   : constant Natural := (N * 6) mod 8;
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

      if Bit_Pos <= 2 then
         Mask1 := not Cluster (63 * (2 ** Bit_Pos));
         B1 := (B1 and Mask1) or Cluster (Natural (E) * (2 ** Bit_Pos));
         To_Ptr (Byte_Addr).all := B1;
      else
         B2 := To_Ptr (Byte_Addr + 1).all;

         Mask1 := Cluster (2 ** Bit_Pos - 1);
         Mask2 := not Cluster (2 ** (Bit_Pos + 6 - 8) - 1);

         B1 := (B1 and Mask1) or Cluster ((Natural (E) mod (2 ** (8 - Bit_Pos))) * (2 ** Bit_Pos));
         B2 := (B2 and Mask2) or Cluster (Natural (E) / (2 ** (8 - Bit_Pos)));

         To_Ptr (Byte_Addr).all := B1;
         To_Ptr (Byte_Addr + 1).all := B2;
      end if;
   end Set_06;

end System.Pack_06;
