-- System.Address_To_Access_Conversions body for Z80
-- Conversion implementations

with Ada.Unchecked_Conversion;

package body System.Address_To_Access_Conversions is

   -- Unchecked conversion between Address and Object_Pointer
   -- Both are 16-bit values on Z80

   function Addr_To_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Object_Pointer);

   function Ptr_To_Addr is new Ada.Unchecked_Conversion
     (Source => Object_Pointer, Target => Address);

   function To_Pointer (Value : Address) return Object_Pointer is
   begin
      return Addr_To_Ptr (Value);
   end To_Pointer;

   function To_Address (Value : Object_Pointer) return Address is
   begin
      return Ptr_To_Addr (Value);
   end To_Address;

end System.Address_To_Access_Conversions;
