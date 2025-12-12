-- GNAT.OS_Lib.Aux for Z80
-- Auxiliary OS library functions

package GNAT.OS_Lib.Aux is

   function Get_Environment_Count return Natural;
   --  Return number of environment variables (0 for CP/M)

   function Get_Environment_Var (Index : Natural) return String;
   --  Get environment variable by index

   procedure Set_Environment_Var (Name, Value : String);
   --  Set environment variable (no-op on CP/M)

   function Get_Program_Name return String;
   --  Return name of executing program

end GNAT.OS_Lib.Aux;
