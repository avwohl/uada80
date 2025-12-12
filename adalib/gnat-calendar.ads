-- GNAT.Calendar for Z80
-- Extended calendar operations

with Ada.Calendar;

package GNAT.Calendar is
   pragma Preelaborate;

   subtype Year_Number   is Ada.Calendar.Year_Number;
   subtype Month_Number  is Ada.Calendar.Month_Number;
   subtype Day_Number    is Ada.Calendar.Day_Number;
   subtype Day_Duration  is Ada.Calendar.Day_Duration;

   -- Day of week (1 = Monday .. 7 = Sunday)
   subtype Day_Name is Integer range 1 .. 7;

   Monday    : constant Day_Name := 1;
   Tuesday   : constant Day_Name := 2;
   Wednesday : constant Day_Name := 3;
   Thursday  : constant Day_Name := 4;
   Friday    : constant Day_Name := 5;
   Saturday  : constant Day_Name := 6;
   Sunday    : constant Day_Name := 7;

   -- Get day of week
   function Day_Of_Week (Date : Ada.Calendar.Time) return Day_Name;

   -- Time components
   subtype Hour_Number   is Natural range 0 .. 23;
   subtype Minute_Number is Natural range 0 .. 59;
   subtype Second_Number is Natural range 0 .. 59;

   -- Split day duration into H:M:S
   procedure Split
     (Date    : Ada.Calendar.Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Hour    : out Hour_Number;
      Minute  : out Minute_Number;
      Second  : out Second_Number);

   -- Create time from components
   function Time_Of
     (Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      Hour   : Hour_Number   := 0;
      Minute : Minute_Number := 0;
      Second : Second_Number := 0) return Ada.Calendar.Time;

   -- Week number (ISO 8601)
   function Week_In_Year (Date : Ada.Calendar.Time) return Natural;

end GNAT.Calendar;
