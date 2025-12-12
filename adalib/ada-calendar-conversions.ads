-- Ada.Calendar.Conversions for Z80
-- Calendar type conversions

with Ada.Calendar;
with Interfaces.C;

package Ada.Calendar.Conversions is
   pragma Preelaborate;

   function To_Ada_Time (Unix_Time : Interfaces.C.long) return Ada.Calendar.Time;
   --  Convert Unix time (seconds since 1970-01-01) to Ada.Calendar.Time

   function To_Unix_Time (Date : Ada.Calendar.Time) return Interfaces.C.long;
   --  Convert Ada.Calendar.Time to Unix time

   function To_Duration (TV_Sec : Interfaces.C.long;
                         TV_Usec : Interfaces.C.long) return Duration;
   --  Convert seconds and microseconds to Duration

   procedure To_Struct_Timespec
     (D       : Duration;
      TV_Sec  : out Interfaces.C.long;
      TV_Nsec : out Interfaces.C.long);
   --  Convert Duration to seconds and nanoseconds

   procedure To_Struct_Timeval
     (D       : Duration;
      TV_Sec  : out Interfaces.C.long;
      TV_Usec : out Interfaces.C.long);
   --  Convert Duration to seconds and microseconds

end Ada.Calendar.Conversions;
