-- GNAT.Sockets.Thin body for Z80
-- Low-level socket stubs

package body GNAT.Sockets.Thin is

   -- All socket functions return error (no networking on CP/M)

   function C_Socket (Domain, Typ, Protocol : C_Int) return C_Int is
      pragma Unreferenced (Domain, Typ, Protocol);
   begin
      return Invalid_Socket;
   end C_Socket;

   function C_Connect (S : C_Int; Name : System.Address; Namelen : C_Int) return C_Int is
      pragma Unreferenced (S, Name, Namelen);
   begin
      return -1;
   end C_Connect;

   function C_Bind (S : C_Int; Name : System.Address; Namelen : C_Int) return C_Int is
      pragma Unreferenced (S, Name, Namelen);
   begin
      return -1;
   end C_Bind;

   function C_Listen (S : C_Int; Backlog : C_Int) return C_Int is
      pragma Unreferenced (S, Backlog);
   begin
      return -1;
   end C_Listen;

   function C_Accept (S : C_Int; Addr : System.Address; Addrlen : access C_Int) return C_Int is
      pragma Unreferenced (S, Addr, Addrlen);
   begin
      return Invalid_Socket;
   end C_Accept;

   function C_Close (S : C_Int) return C_Int is
      pragma Unreferenced (S);
   begin
      return 0;
   end C_Close;

end GNAT.Sockets.Thin;
