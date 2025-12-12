-- System.Pack_12 body for Z80
-- Packed array support for 12-bit elements

with System.Storage_Elements;

package body System.Pack_12 is

   use System.Storage_Elements;

   type Cluster is mod 2 ** 8;

   function Get_12
     (Arr : System.Address;
      N   : Natural) return Bits_12
   is
      -- 12-bit packing: 2 elements per 3 bytes
      Bit_Offset : constant Natural := N * 12;
      Byte_Pos   : constant Natural := Bit_Offset / 8;
      Bit_Pos    : constant Natural := Bit_Offset mod 8;
      Byte_Addr  : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;
      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : constant Cluster := To_Ptr (Byte_Addr).all;
      B2 : constant Cluster := To_Ptr (Byte_Addr + 1).all;
      Val : Natural;
   begin
      Val := Natural (B1 / (2 ** Bit_Pos)) + Natural (B2) * (2 ** (8 - Bit_Pos));

      if Bit_Pos > 4 then
         declare
            B3 : constant Cluster := To_Ptr (Byte_Addr + 2).all;
         begin
            Val := Val + Natural (B3 mod (2 ** (Bit_Pos + 12 - 16))) * 256;
         end;
      end if;

      return Bits_12 (Val mod 4096);
   end Get_12;

   procedure Set_12
     (Arr : System.Address;
      N   : Natural;
      E   : Bits_12)
   is
      Bit_Offset : constant Natural := N * 12;
      Byte_Pos   : constant Natural := Bit_Offset / 8;
      Bit_Pos    : constant Natural := Bit_Offset mod 8;
      Byte_Addr  : constant System.Address := Arr + Storage_Offset (Byte_Pos);

      type Byte_Ptr is access all Cluster;
      for Byte_Ptr'Storage_Size use 0;
      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

      B1 : Cluster;
      B2 : Cluster;
      V : constant Natural := Natural (E);
   begin
      B1 := To_Ptr (Byte_Addr).all;
      B2 := To_Ptr (Byte_Addr + 1).all;

      B1 := (B1 and Cluster (2 ** Bit_Pos - 1)) or
            Cluster ((V mod (2 ** (8 - Bit_Pos))) * (2 ** Bit_Pos));
      B2 := Cluster ((V / (2 ** (8 - Bit_Pos))) mod 256);

      To_Ptr (Byte_Addr).all := B1;
      To_Ptr (Byte_Addr + 1).all := B2;

      if Bit_Pos > 4 then
         declare
            B3 : Cluster := To_Ptr (Byte_Addr + 2).all;
         begin
            B3 := (B3 and not Cluster (2 ** (Bit_Pos + 12 - 16) - 1)) or
                  Cluster (V / 256);
            To_Ptr (Byte_Addr + 2).all := B3;
         end;
      end if;
   end Set_12;

end System.Pack_12;
