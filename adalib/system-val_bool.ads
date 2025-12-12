-- System.Val_Bool for Z80
-- Boolean value conversion from string

package System.Val_Bool is
   pragma Pure;

   -- Convert string representation to Boolean
   function Value_Boolean (Str : String) return Boolean;
   -- Accepts "TRUE", "FALSE", "true", "false" (case-insensitive)
   -- Raises Constraint_Error for invalid input

   -- Check if string is valid boolean literal
   function Is_Valid_Boolean (Str : String) return Boolean;

end System.Val_Bool;
