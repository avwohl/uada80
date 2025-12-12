-- Ada.Environment_Variables for Z80/CP/M
-- Environment variable access
--
-- Note: CP/M does not have environment variables in the Unix sense.
-- This is a stub implementation that provides the standard interface
-- but always returns empty/default values. On systems with environment
-- support (like MSX-DOS 2), this could be extended.

package Ada.Environment_Variables is
   pragma Preelaborate;

   -- Query operations
   function Argument_Count return Natural;
   -- Returns 0 on CP/M (no environment variables)

   function Exists (Name : String) return Boolean;
   -- Always returns False on CP/M

   function Value (Name : String) return String;
   -- Raises Constraint_Error on CP/M (variable doesn't exist)

   function Value (Name : String; Default : String) return String;
   -- Always returns Default on CP/M

   -- Modification operations (stubs on CP/M)
   procedure Set (Name : String; Value : String);
   -- No-op on CP/M

   procedure Clear (Name : String);
   -- No-op on CP/M

   procedure Clear;
   -- No-op on CP/M

   -- Iteration
   procedure Iterate
     (Process : not null access procedure (Name, Value : String));
   -- No-op on CP/M (no variables to iterate)

end Ada.Environment_Variables;
