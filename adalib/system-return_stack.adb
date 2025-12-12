-- System.Return_Stack body for Z80
-- Secondary stack for function return values

with System.Storage_Elements;

package body System.Return_Stack is

   use System.Storage_Elements;

   -- Secondary stack storage
   Stack : Storage_Array (1 .. Storage_Offset (Max_Stack_Size));

   -- Current stack pointer (points to next free location)
   Stack_Ptr : Natural := 0;

   ----------
   -- Push --
   ----------

   procedure Push (Data : System.Address; Size : Natural) is
      type Byte_Ptr is access all Storage_Element;
      for Byte_Ptr'Storage_Size use 0;

      Src : System.Address := Data;
   begin
      for I in 1 .. Size loop
         if Stack_Ptr >= Max_Stack_Size then
            raise Storage_Error;
         end if;
         Stack_Ptr := Stack_Ptr + 1;
         Stack (Storage_Offset (Stack_Ptr)) := Byte_Ptr (Src).all;
         Src := Src + 1;
      end loop;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Data : System.Address; Size : Natural) is
      type Byte_Ptr is access all Storage_Element;
      for Byte_Ptr'Storage_Size use 0;

      Dst : System.Address;
   begin
      if Stack_Ptr < Size then
         raise Storage_Error;
      end if;

      -- Copy in reverse order (stack grows up)
      Dst := Data + Storage_Offset (Size - 1);
      for I in 1 .. Size loop
         Byte_Ptr (Dst).all := Stack (Storage_Offset (Stack_Ptr));
         Stack_Ptr := Stack_Ptr - 1;
         Dst := Dst - 1;
      end loop;
   end Pop;

   --------------
   -- Allocate --
   --------------

   function Allocate (Size : Natural) return System.Address is
      Result : System.Address;
   begin
      if Stack_Ptr + Size > Max_Stack_Size then
         raise Storage_Error;
      end if;

      Stack_Ptr := Stack_Ptr + 1;
      Result := Stack (Storage_Offset (Stack_Ptr))'Address;
      Stack_Ptr := Stack_Ptr + Size - 1;
      return Result;
   end Allocate;

   -------------
   -- Release --
   -------------

   procedure Release (Size : Natural) is
   begin
      if Stack_Ptr < Size then
         Stack_Ptr := 0;
      else
         Stack_Ptr := Stack_Ptr - Size;
      end if;
   end Release;

   -----------------
   -- Get_Pointer --
   -----------------

   function Get_Pointer return Natural is
   begin
      return Stack_Ptr;
   end Get_Pointer;

   -----------------
   -- Set_Pointer --
   -----------------

   procedure Set_Pointer (Ptr : Natural) is
   begin
      Stack_Ptr := Ptr;
   end Set_Pointer;

   ---------------------
   -- Release_To_Mark --
   ---------------------

   procedure Release_To_Mark (Mark : Natural) is
   begin
      Stack_Ptr := Mark;
   end Release_To_Mark;

end System.Return_Stack;
