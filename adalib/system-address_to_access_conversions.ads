-- System.Address_To_Access_Conversions for Z80
-- Provides conversions between addresses and access types
--
-- This package allows low-level address manipulation common in
-- embedded and systems programming.

with System;

generic
   type Object (<>) is limited private;
package System.Address_To_Access_Conversions is
   pragma Preelaborate;

   type Object_Pointer is access all Object;
   for Object_Pointer'Size use Standard'Address_Size;

   function To_Pointer (Value : Address) return Object_Pointer;
   pragma Convention (Intrinsic, To_Pointer);

   function To_Address (Value : Object_Pointer) return Address;
   pragma Convention (Intrinsic, To_Address);

   pragma No_Strict_Aliasing (Object_Pointer);

end System.Address_To_Access_Conversions;
