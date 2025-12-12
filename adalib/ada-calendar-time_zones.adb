-- Ada.Calendar.Time_Zones body for Z80/CP/M
-- Time zone support implementation

package body Ada.Calendar.Time_Zones is

   ---------------------
   -- UTC_Time_Offset --
   ---------------------

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
      pragma Unreferenced (Date);
   begin
      -- CP/M has no timezone support, assume UTC
      return 0;
   end UTC_Time_Offset;

end Ada.Calendar.Time_Zones;
