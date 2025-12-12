-- GNAT.Sockets.Convenience body for Z80
-- Socket convenience stubs

package body GNAT.Sockets.Convenience is

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return "localhost";
   end Host_Name;

   ----------------
   -- IP_Address --
   ----------------

   function IP_Address return String is
   begin
      return "127.0.0.1";
   end IP_Address;

end GNAT.Sockets.Convenience;
