-- System.Pack_07 body for Z80
-- Packed array support for 7-bit elements

with System.Storage_Elements;

package body System.Pack_07 is

   use System.Storage_Elements;

   type Cluster is mod 2 ** 8;

   ------------
   -- Get_07 --
   ------------

   function Get_07
     (Arr : System.Address;
      N   : Natural) return Bits_07
   is
      Byte_Pos  : constant Natural := (N * 7) / 8;
      Bit_Pos   : constant Natural := (N * 7) mod 8;
      Byte_Addr : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Val : Natural;
   begin
      B1 := To_Ptr (Byte_Addr).all;

      if Bit_Pos <= 1 then
         Val := Natural (B1 / (2 ** Bit_Pos)) mod 128;
      else
         B2 := To_Ptr (Byte_Addr + 1).all;
         Val := Natural (B1 / (2 ** Bit_Pos)) +
                Natural ((B2 mod (2 ** (Bit_Pos + 7 - 8))) * (2 ** (8 - Bit_Pos)));
         Val := Val mod 128;
      end if;

      return Bits_07 (Val);
   end Get_07;

   ------------
   -- Set_07 --
   ------------

   procedure Set_07
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_07)
   is
      Byte_Pos  : constant Natural := (N * 7) / 8;
      Bit_Pos   : constant Natural := (N * 7) mod 8;
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

      if Bit_Pos <= 1 then
         Mask1 := not Cluster (127 * (2 ** Bit_Pos));
         B1 := (B1 and Mask1) or Cluster (Natural (E) * (2 ** Bit_Pos));
         To_Ptr (Byte_Addr).all := B1;
      else
         B2 := To_Ptr (Byte_Addr + 1).all;

         Mask1 := Cluster (2 ** Bit_Pos - 1);
         Mask2 := not Cluster (2 ** (Bit_Pos + 7 - 8) - 1);

         B1 := (B1 and Mask1) or Cluster ((Natural (E) mod (2 ** (8 - Bit_Pos))) * (2 ** Bit_Pos));
         B2 := (B2 and Mask2) or Cluster (Natural (E) / (2 ** (8 - Bit_Pos)));

         To_Ptr (Byte_Addr).all := B1;
         To_Ptr (Byte_Addr + 1).all := B2;
      end if;
   end Set_07;

end System.Pack_07;
