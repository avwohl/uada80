-- GNAT.Memory_Dump for Z80
-- Memory dump utilities for debugging

package GNAT.Memory_Dump is

   procedure Dump
     (Addr   : System.Address;
      Count  : Natural);
   --  Dump Count bytes starting at Addr in hex format

   procedure Dump
     (Addr   : System.Address;
      Count  : Natural;
      Prefix : String);
   --  Dump with custom prefix

end GNAT.Memory_Dump;
