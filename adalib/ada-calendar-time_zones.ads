-- Ada.Calendar.Time_Zones for Z80/CP/M
-- Time zone support

with Ada.Calendar;

package Ada.Calendar.Time_Zones is
   pragma Preelaborate;

   type Time_Offset is range -28 * 60 .. 28 * 60;
   --  Time zone offset in minutes from UTC

   Unknown_Zone_Error : exception;

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset;
   --  Return the offset from UTC for the given date
   --  On CP/M, this always returns 0 (assumes UTC)

end Ada.Calendar.Time_Zones;
