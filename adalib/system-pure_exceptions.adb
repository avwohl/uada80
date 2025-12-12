-- System.Pure_Exceptions body for Z80
-- Pure exception raising implementation

package body System.Pure_Exceptions is

   ------------------------------
   -- Raise_Constraint_Error --
   ------------------------------

   procedure Raise_Constraint_Error is
   begin
      raise Constraint_Error;
   end Raise_Constraint_Error;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error is
   begin
      raise Program_Error;
   end Raise_Program_Error;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error is
   begin
      raise Storage_Error;
   end Raise_Storage_Error;

end System.Pure_Exceptions;
