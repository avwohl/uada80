-- Ada.Calendar.Formatting for Z80
-- Calendar formatting utilities

with Ada.Calendar;

package Ada.Calendar.Formatting is
   pragma Preelaborate;

   -- Day of week type
   type Day_Name is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

   function Day_Of_Week (Date : Time) return Day_Name;

   -- Image functions
   function Year (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Year_Number
     renames Ada.Calendar.Year;

   function Month (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Month_Number
     renames Ada.Calendar.Month;

   function Day (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Day_Number
     renames Ada.Calendar.Day;

   function Hour (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Natural;
   function Minute (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Natural;
   function Second (Date : Time) return Natural;
   function Sub_Second (Date : Time) return Day_Duration;

   function Seconds_Of
     (Hour       : Natural;
      Minute     : Natural;
      Second     : Natural := 0;
      Sub_Second : Day_Duration := 0.0) return Day_Duration;

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Natural;
      Minute     : out Natural;
      Second     : out Natural;
      Sub_Second : out Day_Duration);

   function Time_Of
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Natural;
      Minute     : Natural;
      Second     : Natural := 0;
      Sub_Second : Day_Duration := 0.0;
      Leap_Second: Boolean := False;
      Time_Zone  : Time_Zones.Time_Offset := 0) return Time;

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Natural;
      Minute     : out Natural;
      Second     : out Natural;
      Sub_Second : out Day_Duration;
      Time_Zone  : Time_Zones.Time_Offset := 0);

   -- Formatting
   function Image
     (Date      : Time;
      Include_Time_Fraction : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0) return String;

   function Value (Date : String; Time_Zone : Time_Zones.Time_Offset := 0) return Time;

   function Image (Elapsed_Time : Duration; Include_Time_Fraction : Boolean := False) return String;
   function Value (Elapsed_Time : String) return Duration;

   -- Time Zones child
   package Time_Zones is
      type Time_Offset is range -28 * 60 .. 28 * 60;

      Unknown_Zone_Error : exception;

      function UTC_Time_Offset (Date : Time := Clock) return Time_Offset;
   end Time_Zones;

end Ada.Calendar.Formatting;
