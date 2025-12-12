-- GNAT.Sockets body for Z80
-- Socket communication (stub - no networking on CP/M)

package body GNAT.Sockets is

   Not_Supported : constant String := "Sockets not supported on Z80/CP-M";

   -------------------
   -- Create_Socket --
   -------------------

   procedure Create_Socket
     (Socket : out Socket_Type;
      Family : Family_Type := Family_Inet;
      Mode   : Mode_Type := Socket_Stream)
   is
      pragma Unreferenced (Family, Mode);
   begin
      Socket := No_Socket;
      raise Socket_Error with Not_Supported;
   end Create_Socket;

   ------------------
   -- Close_Socket --
   ------------------

   procedure Close_Socket (Socket : in Out Socket_Type) is
   begin
      Socket := No_Socket;
   end Close_Socket;

   --------------------
   -- Connect_Socket --
   --------------------

   procedure Connect_Socket
     (Socket : Socket_Type;
      Server : Sock_Addr_Type)
   is
      pragma Unreferenced (Socket, Server);
   begin
      raise Socket_Error with Not_Supported;
   end Connect_Socket;

   -----------------
   -- Bind_Socket --
   -----------------

   procedure Bind_Socket
     (Socket  : Socket_Type;
      Address : Sock_Addr_Type)
   is
      pragma Unreferenced (Socket, Address);
   begin
      raise Socket_Error with Not_Supported;
   end Bind_Socket;

   -------------------
   -- Listen_Socket --
   -------------------

   procedure Listen_Socket
     (Socket : Socket_Type;
      Length : Natural := 15)
   is
      pragma Unreferenced (Socket, Length);
   begin
      raise Socket_Error with Not_Supported;
   end Listen_Socket;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Server  : Socket_Type;
      Socket  : out Socket_Type;
      Address : out Sock_Addr_Type)
   is
      pragma Unreferenced (Server);
   begin
      Socket := No_Socket;
      Address := (Addr => No_Inet_Addr, Port => 0, Family => Family_Inet);
      raise Socket_Error with Not_Supported;
   end Accept_Socket;

   ---------------
   -- Inet_Addr --
   ---------------

   function Inet_Addr (Image : String) return Inet_Addr_Type is
      Result : Inet_Addr_Type := (0, 0, 0, 0);
      Octet  : Natural := 0;
      Index  : Natural := 1;
   begin
      for I in Image'Range loop
         if Image (I) = '.' then
            if Index <= 4 then
               Result (Index) := Interfaces.Unsigned_8 (Octet);
               Index := Index + 1;
               Octet := 0;
            end if;
         elsif Image (I) in '0' .. '9' then
            Octet := Octet * 10 + (Character'Pos (Image (I)) - Character'Pos ('0'));
         end if;
      end loop;

      if Index <= 4 then
         Result (Index) := Interfaces.Unsigned_8 (Octet);
      end if;

      return Result;
   end Inet_Addr;

   -----------
   -- Image --
   -----------

   function Image (Addr : Inet_Addr_Type) return String is
      function Octet_Image (N : Interfaces.Unsigned_8) return String is
         V : Natural := Natural (N);
         S : String (1 .. 3);
         P : Natural := 3;
      begin
         loop
            S (P) := Character'Val (Character'Pos ('0') + V mod 10);
            V := V / 10;
            exit when V = 0;
            P := P - 1;
         end loop;
         return S (P .. 3);
      end Octet_Image;
   begin
      return Octet_Image (Addr (1)) & "." &
             Octet_Image (Addr (2)) & "." &
             Octet_Image (Addr (3)) & "." &
             Octet_Image (Addr (4));
   end Image;

   ----------------------
   -- Get_Host_By_Name --
   ----------------------

   function Get_Host_By_Name (Name : String) return Inet_Addr_Type is
      pragma Unreferenced (Name);
   begin
      raise Host_Error with "DNS not supported on Z80/CP-M";
      return No_Inet_Addr;
   end Get_Host_By_Name;

end GNAT.Sockets;
