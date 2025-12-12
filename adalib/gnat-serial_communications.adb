-- GNAT.Serial_Communications body for Z80
-- Serial port interface implementation
-- Uses CP/M console I/O as serial port

with System.CRTL;

package body GNAT.Serial_Communications is

   ----------
   -- Open --
   ----------

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name)
   is
      pragma Unreferenced (Name);
   begin
      -- CP/M uses console as "serial port"
      Port.Is_Open := True;
   end Open;

   ---------
   -- Set --
   ---------

   procedure Set
     (Port      : Serial_Port;
      Rate      : Data_Rate := B9600;
      Bits      : Data_Bits := CS8;
      Stop_Bits : Stop_Bits := One;
      Parity    : Parity := None;
      Block     : Boolean := True;
      Local     : Boolean := True;
      Flow      : Flow_Control := None;
      Timeout   : Duration := 10.0)
   is
      pragma Unreferenced (Port, Rate, Bits, Stop_Bits, Parity);
      pragma Unreferenced (Block, Local, Flow, Timeout);
   begin
      -- Parameters are ignored on CP/M
      -- Hardware configuration is fixed by the system
      null;
   end Set;

   ----------
   -- Read --
   ----------

   procedure Read
     (Port   : Serial_Port;
      Buffer : out String;
      Last   : out Natural)
   is
      C : Character;
   begin
      if not Port.Is_Open then
         raise Serial_Error with "Port not open";
      end if;

      Last := Buffer'First - 1;
      for I in Buffer'Range loop
         -- Use BDOS console input
         C := Character'Val (System.CRTL.BDOS_Console_Input);
         if C = ASCII.CR or C = ASCII.LF then
            exit;
         end if;
         Last := I;
         Buffer (I) := C;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Port   : Serial_Port;
      Buffer : String)
   is
   begin
      if not Port.Is_Open then
         raise Serial_Error with "Port not open";
      end if;

      for C of Buffer loop
         System.CRTL.BDOS_Console_Output (Character'Pos (C));
      end loop;
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Port : in Out Serial_Port) is
   begin
      Port.Is_Open := False;
   end Close;

end GNAT.Serial_Communications;
