-- GNAT.Strings body for Z80
-- String utilities implementation

with Ada.Unchecked_Deallocation;

package body GNAT.Strings is

   procedure Dealloc is new Ada.Unchecked_Deallocation
     (String, String_Access);

   ----------
   -- Free --
   ----------

   procedure Free (S : in Out String_Access) is
   begin
      Dealloc (S);
   end Free;

end GNAT.Strings;
