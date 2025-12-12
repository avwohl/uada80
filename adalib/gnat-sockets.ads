-- GNAT.Sockets for Z80
-- Socket communication (stub for CP/M - no networking)

with Ada.Streams;

package GNAT.Sockets is
   pragma Preelaborate;

   -- Socket type
   type Socket_Type is private;
   No_Socket : constant Socket_Type;

   -- Address families
   type Family_Type is (Family_Inet, Family_Inet6, Family_Unix);

   -- Socket modes
   type Mode_Type is (Socket_Stream, Socket_Datagram);

   -- Port number
   type Port_Type is range 0 .. 65535;
   Any_Port : constant Port_Type := 0;

   -- Internet address
   type Inet_Addr_Type is private;
   Any_Inet_Addr : constant Inet_Addr_Type;
   No_Inet_Addr  : constant Inet_Addr_Type;

   -- Socket address
   type Sock_Addr_Type is record
      Addr   : Inet_Addr_Type;
      Port   : Port_Type;
      Family : Family_Type := Family_Inet;
   end record;

   -- Exceptions
   Socket_Error : exception;
   Host_Error   : exception;

   -- Socket operations (all raise Socket_Error on Z80)
   procedure Create_Socket
     (Socket : out Socket_Type;
      Family : Family_Type := Family_Inet;
      Mode   : Mode_Type := Socket_Stream);

   procedure Close_Socket (Socket : in Out Socket_Type);

   procedure Connect_Socket
     (Socket : Socket_Type;
      Server : Sock_Addr_Type);

   procedure Bind_Socket
     (Socket  : Socket_Type;
      Address : Sock_Addr_Type);

   procedure Listen_Socket
     (Socket : Socket_Type;
      Length : Natural := 15);

   procedure Accept_Socket
     (Server  : Socket_Type;
      Socket  : out Socket_Type;
      Address : out Sock_Addr_Type);

   -- Address conversion
   function Inet_Addr (Image : String) return Inet_Addr_Type;
   function Image (Addr : Inet_Addr_Type) return String;

   -- Host name resolution
   function Get_Host_By_Name (Name : String) return Inet_Addr_Type;

private

   type Socket_Type is new Integer;
   No_Socket : constant Socket_Type := -1;

   type Inet_Addr_Type is array (1 .. 4) of Interfaces.Unsigned_8;
   Any_Inet_Addr : constant Inet_Addr_Type := (0, 0, 0, 0);
   No_Inet_Addr  : constant Inet_Addr_Type := (255, 255, 255, 255);

end GNAT.Sockets;
