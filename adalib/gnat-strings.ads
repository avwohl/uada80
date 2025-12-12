-- GNAT.Strings for Z80
-- String utilities

package GNAT.Strings is
   pragma Preelaborate;

   type String_Access is access all String;

   -- Free allocated string
   procedure Free (S : in Out String_Access);

end GNAT.Strings;
