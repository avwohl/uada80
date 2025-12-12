-- GNAT.Calendar.Time_IO for Z80
-- Time formatting and parsing

with Ada.Calendar;

package GNAT.Calendar.Time_IO is
   pragma Preelaborate;

   function Image
     (Date    : Ada.Calendar.Time;
      Pattern : String := "%Y-%m-%d %H:%M:%S") return String;
   --  Format time according to pattern
   --  %Y = year, %m = month, %d = day
   --  %H = hour, %M = minute, %S = second

   function Value (Date_String : String) return Ada.Calendar.Time;
   --  Parse time from string (ISO format: YYYY-MM-DD HH:MM:SS)

   function Image_Date (Date : Ada.Calendar.Time) return String;
   --  Return date as "YYYY-MM-DD"

   function Image_Time (Date : Ada.Calendar.Time) return String;
   --  Return time as "HH:MM:SS"

end GNAT.Calendar.Time_IO;
