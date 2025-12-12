-- System.Val_Enum for Z80
-- Enumeration value conversion from string

package System.Val_Enum is
   pragma Pure;

   -- Generic enumeration value lookup
   -- Used by compiler-generated Value attribute implementations

   generic
      type Enum is (<>);
   function Value_Enumeration (Str : String) return Enum;
   -- Converts string representation to enumeration value
   -- Case-insensitive comparison
   -- Raises Constraint_Error for invalid input

   -- Check if string position matches enumeration literal
   function Scan_Enum_Literal
     (Str    : String;
      Ptr    : not null access Integer;
      Max    : Integer;
      Names  : String;
      Starts : access Integer) return Natural;
   -- Returns position (0-based) of matching literal, or raises Constraint_Error
   -- Names contains concatenated enumeration literal names
   -- Starts contains starting positions for each name

end System.Val_Enum;
