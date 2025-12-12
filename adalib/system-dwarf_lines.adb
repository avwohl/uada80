-- System.Dwarf_Lines body for Z80
-- DWARF line number info (stub implementation)

package body System.Dwarf_Lines is

   ------------
   -- Create --
   ------------

   function Create return Line_Info_Context_Access is
   begin
      -- DWARF not supported on Z80
      return null;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (C : in Out Line_Info_Context_Access) is
   begin
      C := null;
   end Destroy;

   -------------------
   -- Get_Line_Info --
   -------------------

   function Get_Line_Info
     (C       : Line_Info_Context;
      Address : System.Address) return Line_Info
   is
      pragma Unreferenced (C, Address);
   begin
      return (File_Name    => System.Null_Address,
              Line_Number  => 0,
              Column       => 0,
              Address      => System.Null_Address);
   end Get_Line_Info;

end System.Dwarf_Lines;
