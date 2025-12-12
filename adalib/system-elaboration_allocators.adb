-- System.Elaboration_Allocators body for Z80
-- Memory allocators for elaboration time

with System.Storage_Elements;

package body System.Elaboration_Allocators is

   use System.Storage_Elements;

   -- Static pool for elaboration-time allocations
   Pool : Storage_Array (1 .. Storage_Offset (Elaboration_Pool_Size));
   Pool_Index : Storage_Offset := 1;

   --------------
   -- Allocate --
   --------------

   function Allocate (Size : Natural) return System.Address is
      Result : System.Address;
      Aligned_Size : Storage_Offset;
   begin
      -- Align to 2-byte boundary (Z80 word alignment)
      Aligned_Size := Storage_Offset ((Size + 1) / 2 * 2);

      if Pool_Index + Aligned_Size - 1 > Pool'Last then
         -- Out of elaboration pool memory
         raise Storage_Error;
      end if;

      Result := Pool (Pool_Index)'Address;
      Pool_Index := Pool_Index + Aligned_Size;

      return Result;
   end Allocate;

   -----------------
   -- Memory_Used --
   -----------------

   function Memory_Used return Natural is
   begin
      return Natural (Pool_Index - 1);
   end Memory_Used;

   ----------------------
   -- Memory_Remaining --
   ----------------------

   function Memory_Remaining return Natural is
   begin
      return Natural (Pool'Last - Pool_Index + 1);
   end Memory_Remaining;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Pool_Index := 1;
   end Reset;

end System.Elaboration_Allocators;
