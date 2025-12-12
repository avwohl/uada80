-- System.Pack_09 body for Z80
-- Packed array support for 9-bit elements

with System.Storage_Elements;

package body System.Pack_09 is

   use System.Storage_Elements;

   type Cluster is mod 2 ** 8;

   ------------
   -- Get_09 --
   ------------

   function Get_09
     (Arr : System.Address;
      N   : Natural) return Bits_09
   is
      Bit_Offset : constant Natural := N * 9;
      Byte_Pos   : constant Natural := Bit_Offset / 8;
      Bit_Pos    : constant Natural := Bit_Offset mod 8;
      Byte_Addr  : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Val : Natural;
   begin
      B1 := To_Ptr (Byte_Addr).all;
      B2 := To_Ptr (Byte_Addr + 1).all;

      -- Always spans at least 2 bytes for 9 bits
      Val := Natural (B1 / (2 ** Bit_Pos)) +
             Natural (B2) * (2 ** (8 - Bit_Pos));

      if Bit_Pos > 7 then
         -- Spans into third byte
         declare
            B3 : constant Cluster := To_Ptr (Byte_Addr + 2).all;
         begin
            Val := Val + Natural (B3 mod (2 ** (Bit_Pos + 9 - 16))) * 256;
         end;
      end if;

      return Bits_09 (Val mod 512);
   end Get_09;

   ------------
   -- Set_09 --
   ------------

   procedure Set_09
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_09)
   is
      Bit_Offset : constant Natural := N * 9;
      Byte_Pos   : constant Natural := Bit_Offset / 8;
      Bit_Pos    : constant Natural := Bit_Offset mod 8;
      Byte_Addr  : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;

      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      Mask1 : Cluster;
      Mask2 : Cluster;
      V : constant Natural := Natural (E);
   begin
      B1 := To_Ptr (Byte_Addr).all;
      B2 := To_Ptr (Byte_Addr + 1).all;

      Mask1 := Cluster (2 ** Bit_Pos - 1);
      B1 := (B1 and Mask1) or Cluster ((V mod (2 ** (8 - Bit_Pos))) * (2 ** Bit_Pos));

      if Bit_Pos <= 7 then
         Mask2 := not Cluster (2 ** (Bit_Pos + 1) - 1);
         B2 := (B2 and Mask2) or Cluster ((V / (2 ** (8 - Bit_Pos))) mod 256);
      end if;

      To_Ptr (Byte_Addr).all := B1;
      To_Ptr (Byte_Addr + 1).all := B2;

      if Bit_Pos > 7 then
         declare
            B3 : Cluster := To_Ptr (Byte_Addr + 2).all;
            Mask3 : constant Cluster := not Cluster (2 ** (Bit_Pos + 9 - 16) - 1);
         begin
            B3 := (B3 and Mask3) or Cluster (V / 256);
            To_Ptr (Byte_Addr + 2).all := B3;
         end;
      end if;
   end Set_09;

end System.Pack_09;
