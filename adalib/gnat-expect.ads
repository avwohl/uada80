-- GNAT.Expect for Z80
-- Process control and expect-style interaction (stub)

package GNAT.Expect is
   pragma Preelaborate;

   -- Process descriptor
   type Process_Descriptor is limited private;

   -- Filter type
   type Filter_Type is (No_Filter, Strip_CR, Strip_LF);

   -- Exceptions
   Invalid_Process : exception;
   Process_Died    : exception;

   -- Process control (not supported on CP/M)
   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor;
      Command     : String;
      Args        : GNAT.OS_Lib.Argument_List;
      Buffer_Size : Natural := 4096);

   procedure Close (Descriptor : in Out Process_Descriptor);

   -- Expect-style waiting (stub)
   procedure Expect
     (Descriptor  : in Out Process_Descriptor;
      Result      : out Natural;
      Regexp      : String;
      Timeout     : Integer := 10000);

   -- Send to process
   procedure Send
     (Descriptor : in Out Process_Descriptor;
      Str        : String);

   -- Get output
   function Expect_Out (Descriptor : Process_Descriptor) return String;

private

   type Process_Descriptor is limited record
      PID        : Integer := -1;
      Buffer     : String (1 .. 256);
      Buffer_Len : Natural := 0;
   end record;

end GNAT.Expect;
