-- Interfaces.C.Pointers_G for Z80
-- Generic C pointer operations

with System.Address_To_Access_Conversions;

generic
   type Element is private;
package Interfaces.C.Pointers_G is
   pragma Preelaborate;

   type Pointer is access all Element;
   pragma Convention (C, Pointer);

   Null_Pointer : constant Pointer := null;

   function "+" (P : Pointer; Offset : ptrdiff_t) return Pointer;
   function "-" (P : Pointer; Offset : ptrdiff_t) return Pointer;
   function "-" (Left, Right : Pointer) return ptrdiff_t;

   procedure Increment (P : in Out Pointer);
   procedure Decrement (P : in Out Pointer);

   function Value (P : Pointer) return Element;
   procedure Copy (Target : Pointer; Source : Element);

end Interfaces.C.Pointers_G;
