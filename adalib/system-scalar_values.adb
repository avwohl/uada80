-- System.Scalar_Values body for Z80
-- Invalid scalar value checking support

with System.Storage_Elements;

package body System.Scalar_Values is

   use System.Storage_Elements;

   ------------------
   -- Is_Invalid_8 --
   ------------------

   function Is_Invalid_8 (Val : Invalid_Value_Type) return Boolean is
   begin
      return Val = Invalid_8_Value;
   end Is_Invalid_8;

   -------------------
   -- Set_Invalid_8 --
   -------------------

   procedure Set_Invalid_8 (Addr : System.Address) is
      type Byte_Ptr is access all Storage_Element;
      for Byte_Ptr'Storage_Size use 0;
      P : constant Byte_Ptr := Byte_Ptr (Addr);
   begin
      P.all := Storage_Element (Invalid_8_Value);
   end Set_Invalid_8;

   --------------------
   -- Set_Invalid_16 --
   --------------------

   procedure Set_Invalid_16 (Addr : System.Address) is
      type Byte_Ptr is access all Storage_Element;
      for Byte_Ptr'Storage_Size use 0;
      P1 : constant Byte_Ptr := Byte_Ptr (Addr);
      P2 : constant Byte_Ptr := Byte_Ptr (Addr + 1);
   begin
      P1.all := Storage_Element (Invalid_8_Value);
      P2.all := Storage_Element (Invalid_8_Value);
   end Set_Invalid_16;

end System.Scalar_Values;
