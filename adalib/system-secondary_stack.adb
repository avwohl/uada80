-- System.Secondary_Stack body for Z80
-- Secondary stack management implementation

with System.Storage_Elements;

package body System.Secondary_Stack is

   use System.Storage_Elements;

   -- Secondary stack buffer
   Stack : array (1 .. Default_Secondary_Stack_Size) of
     Storage_Element;
   pragma Volatile_Components (Stack);

   -- Current top of stack
   Top : Natural := 0;
   pragma Volatile (Top);

   -- Maximum usage
   Max_Used : Natural := 0;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return SS_Mark is
   begin
      return SS_Mark (Top);
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : SS_Mark) is
   begin
      Top := Natural (M);
   end SS_Release;

   -----------------
   -- SS_Allocate --
   -----------------

   function SS_Allocate (Size : Natural) return System.Address is
      Result : System.Address;
      Aligned_Size : Natural;
   begin
      -- Align to 2 bytes (Z80 word boundary)
      Aligned_Size := (Size + 1) / 2 * 2;

      if Top + Aligned_Size > Stack'Last then
         raise Storage_Error with "Secondary stack overflow";
      end if;

      Result := Stack (Top + 1)'Address;
      Top := Top + Aligned_Size;

      if Top > Max_Used then
         Max_Used := Top;
      end if;

      return Result;
   end SS_Allocate;

   ----------------
   -- SS_Get_Max --
   ----------------

   function SS_Get_Max return Natural is
   begin
      return Max_Used;
   end SS_Get_Max;

end System.Secondary_Stack;
