-- System.Dwarf_Lines for Z80
-- DWARF line number info (stub)

package System.Dwarf_Lines is
   pragma Preelaborate;

   -- Line info context
   type Line_Info_Context is limited private;
   type Line_Info_Context_Access is access all Line_Info_Context;

   -- Line info entry
   type Line_Info is record
      File_Name    : System.Address;
      Line_Number  : Natural;
      Column       : Natural;
      Address      : System.Address;
   end record;

   -- Operations (stubs)
   function Create return Line_Info_Context_Access;
   procedure Destroy (C : in Out Line_Info_Context_Access);

   function Get_Line_Info
     (C       : Line_Info_Context;
      Address : System.Address) return Line_Info;

private
   type Line_Info_Context is record
      Valid : Boolean := False;
   end record;

end System.Dwarf_Lines;
