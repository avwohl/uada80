-- GNAT.Serial_Communications for Z80
-- Serial port interface (simplified for CP/M)

package GNAT.Serial_Communications is

   type Serial_Port is limited private;

   type Port_Name is new String;

   type Data_Rate is
     (B75, B110, B150, B300, B600, B1200, B2400, B4800,
      B9600, B19200, B38400, B57600, B115200);

   type Data_Bits is (CS7, CS8);
   type Stop_Bits is (One, Two);
   type Parity is (None, Even, Odd);

   type Flow_Control is (None, RTS_CTS, Xon_Xoff);

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name);

   procedure Set
     (Port      : Serial_Port;
      Rate      : Data_Rate := B9600;
      Bits      : Data_Bits := CS8;
      Stop_Bits : Stop_Bits := One;
      Parity    : Parity := None;
      Block     : Boolean := True;
      Local     : Boolean := True;
      Flow      : Flow_Control := None;
      Timeout   : Duration := 10.0);

   procedure Read
     (Port   : Serial_Port;
      Buffer : out String;
      Last   : out Natural);

   procedure Write
     (Port   : Serial_Port;
      Buffer : String);

   procedure Close (Port : in Out Serial_Port);

   Serial_Error : exception;

private
   type Serial_Port is record
      Is_Open : Boolean := False;
   end record;

end GNAT.Serial_Communications;
