-- GNAT.OS_Lib.Aux body for Z80
-- Auxiliary OS library implementation

package body GNAT.OS_Lib.Aux is

   ---------------------------
   -- Get_Environment_Count --
   ---------------------------

   function Get_Environment_Count return Natural is
   begin
      -- CP/M has no environment variables
      return 0;
   end Get_Environment_Count;

   -------------------------
   -- Get_Environment_Var --
   -------------------------

   function Get_Environment_Var (Index : Natural) return String is
      pragma Unreferenced (Index);
   begin
      return "";
   end Get_Environment_Var;

   -------------------------
   -- Set_Environment_Var --
   -------------------------

   procedure Set_Environment_Var (Name, Value : String) is
      pragma Unreferenced (Name, Value);
   begin
      -- No-op on CP/M
      null;
   end Set_Environment_Var;

   ----------------------
   -- Get_Program_Name --
   ----------------------

   function Get_Program_Name return String is
   begin
      -- CP/M doesn't provide program name easily
      return "PROGRAM.COM";
   end Get_Program_Name;

end GNAT.OS_Lib.Aux;
