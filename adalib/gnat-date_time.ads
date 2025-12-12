-- GNAT.Date_Time for Z80
-- Simple date/time handling

package GNAT.Date_Time is
   pragma Preelaborate;

   subtype Year_Number is Integer range 1900 .. 2099;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number is Integer range 1 .. 31;
   subtype Hour_Number is Integer range 0 .. 23;
   subtype Minute_Number is Integer range 0 .. 59;
   subtype Second_Number is Integer range 0 .. 59;
   subtype Day_Of_Week is Integer range 0 .. 6;  -- 0 = Sunday

   type Date is record
      Year  : Year_Number := 1970;
      Month : Month_Number := 1;
      Day   : Day_Number := 1;
   end record;

   type Time is record
      Hour   : Hour_Number := 0;
      Minute : Minute_Number := 0;
      Second : Second_Number := 0;
   end record;

   type Date_Time is record
      D : Date;
      T : Time;
   end record;

   -- Constructors
   function Make_Date (Year : Year_Number; Month : Month_Number;
                       Day : Day_Number) return Date;
   function Make_Time (Hour : Hour_Number; Minute : Minute_Number;
                       Second : Second_Number := 0) return Time;
   function Make_Date_Time (D : Date; T : Time) return Date_Time;

   -- Validation
   function Is_Valid_Date (D : Date) return Boolean;
   function Is_Leap_Year (Year : Year_Number) return Boolean;
   function Days_In_Month (Year : Year_Number; Month : Month_Number) return Day_Number;

   -- Day of week (0 = Sunday)
   function Get_Day_Of_Week (D : Date) return Day_Of_Week;
   function Day_Name (DOW : Day_Of_Week) return String;
   function Month_Name (Month : Month_Number) return String;
   function Month_Name_Short (Month : Month_Number) return String;

   -- Comparison
   function "<" (Left, Right : Date) return Boolean;
   function ">" (Left, Right : Date) return Boolean;
   function "<=" (Left, Right : Date) return Boolean;
   function ">=" (Left, Right : Date) return Boolean;
   function "=" (Left, Right : Date) return Boolean;

   function "<" (Left, Right : Time) return Boolean;
   function ">" (Left, Right : Time) return Boolean;
   function "=" (Left, Right : Time) return Boolean;

   function "<" (Left, Right : Date_Time) return Boolean;
   function ">" (Left, Right : Date_Time) return Boolean;
   function "=" (Left, Right : Date_Time) return Boolean;

   -- Arithmetic
   function Add_Days (D : Date; Days : Integer) return Date;
   function Add_Months (D : Date; Months : Integer) return Date;
   function Add_Years (D : Date; Years : Integer) return Date;
   function Days_Between (D1, D2 : Date) return Integer;

   function Add_Seconds (T : Time; Seconds : Integer) return Time;
   function Add_Minutes (T : Time; Minutes : Integer) return Time;
   function Add_Hours (T : Time; Hours : Integer) return Time;

   -- Day of year (1-366)
   function Day_Of_Year (D : Date) return Natural;

   -- Week number (1-53, ISO week)
   function Week_Number (D : Date) return Natural;

   -- String conversion
   function Date_Image (D : Date) return String;         -- YYYY-MM-DD
   function Time_Image (T : Time) return String;         -- HH:MM:SS
   function Date_Time_Image (DT : Date_Time) return String;

   -- Parsing (basic)
   function Parse_Date (S : String) return Date;         -- YYYY-MM-DD
   function Parse_Time (S : String) return Time;         -- HH:MM:SS

end GNAT.Date_Time;
