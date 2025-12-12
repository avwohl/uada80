-- Interfaces.C.Pointers_G body for Z80
-- Generic C pointer operations implementation

with System.Storage_Elements;

package body Interfaces.C.Pointers_G is

   use System.Storage_Elements;

   Element_Size : constant Storage_Offset := Element'Max_Size_In_Storage_Elements;

   ---------
   -- "+" --
   ---------

   function "+" (P : Pointer; Offset : ptrdiff_t) return Pointer is
   begin
      if P = null then
         return null;
      end if;
      declare
         Addr : constant System.Address := P.all'Address;
         New_Addr : constant System.Address :=
           Addr + Storage_Offset (Offset) * Element_Size;
         Result : Pointer;
         for Result'Address use New_Addr'Address;
      begin
         return Result;
      end;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (P : Pointer; Offset : ptrdiff_t) return Pointer is
   begin
      return P + (-Offset);
   end "-";

   function "-" (Left, Right : Pointer) return ptrdiff_t is
   begin
      if Left = null or Right = null then
         return 0;
      end if;
      return ptrdiff_t ((Left.all'Address - Right.all'Address) / Element_Size);
   end "-";

   ---------------
   -- Increment --
   ---------------

   procedure Increment (P : in Out Pointer) is
   begin
      P := P + 1;
   end Increment;

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement (P : in Out Pointer) is
   begin
      P := P - 1;
   end Decrement;

   -----------
   -- Value --
   -----------

   function Value (P : Pointer) return Element is
   begin
      return P.all;
   end Value;

   ----------
   -- Copy --
   ----------

   procedure Copy (Target : Pointer; Source : Element) is
   begin
      Target.all := Source;
   end Copy;

end Interfaces.C.Pointers_G;
