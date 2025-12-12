-- Ada.Environment_Variables body for Z80/CP/M
-- Stub implementation for systems without environment variables

package body Ada.Environment_Variables is

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      -- CP/M has no environment variables
      return 0;
   end Argument_Count;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
      pragma Unreferenced (Name);
   begin
      -- No environment variables on CP/M
      return False;
   end Exists;

   -----------
   -- Value --
   -----------

   function Value (Name : String) return String is
      pragma Unreferenced (Name);
   begin
      -- Variable doesn't exist, raise Constraint_Error per Ada RM
      raise Constraint_Error;
      return "";  -- Never reached
   end Value;

   function Value (Name : String; Default : String) return String is
      pragma Unreferenced (Name);
   begin
      -- Always return default on CP/M
      return Default;
   end Value;

   ---------
   -- Set --
   ---------

   procedure Set (Name : String; Value : String) is
      pragma Unreferenced (Name);
      pragma Unreferenced (Value);
   begin
      -- No-op on CP/M
      null;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Name : String) is
      pragma Unreferenced (Name);
   begin
      -- No-op on CP/M
      null;
   end Clear;

   procedure Clear is
   begin
      -- No-op on CP/M
      null;
   end Clear;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access procedure (Name, Value : String))
   is
      pragma Unreferenced (Process);
   begin
      -- No variables to iterate on CP/M
      null;
   end Iterate;

end Ada.Environment_Variables;
