-- GNAT.Sockets.Thin for Z80
-- Low-level socket interface (stub)

package GNAT.Sockets.Thin is
   pragma Preelaborate;

   -- Socket types (stubs for compatibility)
   type C_Int is new Integer;
   type C_Size_T is new Natural;

   Invalid_Socket : constant C_Int := -1;

   -- Socket constants
   AF_INET     : constant C_Int := 2;
   SOCK_STREAM : constant C_Int := 1;
   SOCK_DGRAM  : constant C_Int := 2;

   -- Error codes
   EBADF       : constant C_Int := 9;
   ENOTSOCK    : constant C_Int := 88;
   ECONNREFUSED : constant C_Int := 111;

   -- Stub functions (all return error on Z80)
   function C_Socket (Domain, Typ, Protocol : C_Int) return C_Int;
   function C_Connect (S : C_Int; Name : System.Address; Namelen : C_Int) return C_Int;
   function C_Bind (S : C_Int; Name : System.Address; Namelen : C_Int) return C_Int;
   function C_Listen (S : C_Int; Backlog : C_Int) return C_Int;
   function C_Accept (S : C_Int; Addr : System.Address; Addrlen : access C_Int) return C_Int;
   function C_Close (S : C_Int) return C_Int;

end GNAT.Sockets.Thin;
