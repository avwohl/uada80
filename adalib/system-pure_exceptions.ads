-- System.Pure_Exceptions for Z80
-- Exception declarations for pure units

package System.Pure_Exceptions is
   pragma Pure;

   -- These are the predefined exceptions re-exported for Pure packages
   -- that need to reference them without violating the Pure restriction

   Constraint_Error : exception renames Standard.Constraint_Error;
   Program_Error    : exception renames Standard.Program_Error;
   Storage_Error    : exception renames Standard.Storage_Error;
   Tasking_Error    : exception renames Standard.Tasking_Error;

end System.Pure_Exceptions;
