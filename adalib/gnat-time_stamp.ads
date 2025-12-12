-- GNAT.Time_Stamp for Z80
-- Time stamp generation

package GNAT.Time_Stamp is
   pragma Preelaborate;

   function Current_Time return String;
   --  Return current time as "YYYY-MM-DD HH:MM:SS.ss"
   --  On Z80/CP/M, returns a placeholder if RTC not available

end GNAT.Time_Stamp;
