-- GNAT.IO for Z80
-- Simple I/O routines (compatibility)

package GNAT.IO is
   pragma Preelaborate;

   -- Character output
   procedure Put (C : Character);
   procedure Put_Line (S : String);
   procedure New_Line;

   -- Character input
   function Get return Character;
   procedure Get (C : out Character);
   procedure Get_Line (S : out String; Last : out Natural);

end GNAT.IO;
