-- Ada.Calendar body for Z80
-- Basic time/date implementation using tick counter

with System;

package body Ada.Calendar is

   -- System tick rate (ticks per second)
   -- Typical Z80 interrupt rate is 50Hz
   Ticks_Per_Second : constant := 50;
   Ticks_Per_Day    : constant := 86400 * Ticks_Per_Second;

   -- Epoch: January 1, 2000
   Epoch_Year  : constant := 2000;
   Epoch_Month : constant := 1;
   Epoch_Day   : constant := 1;

   -- Runtime timer access
   function Rt_Get_Ticks return Integer;
   pragma Import (C, Rt_Get_Ticks, "_get_ticks");

   function Rt_Get_Days return Integer;
   pragma Import (C, Rt_Get_Days, "_get_days");

   -- Helper: Check if year is leap year
   function Is_Leap_Year (Y : Year_Number) return Boolean is
   begin
      return (Y mod 4 = 0) and ((Y mod 100 /= 0) or (Y mod 400 = 0));
   end Is_Leap_Year;

   -- Helper: Days in a specific month
   function Days_In (Y : Year_Number; M : Month_Number) return Day_Number is
   begin
      if M = 2 and Is_Leap_Year (Y) then
         return 29;
      else
         return Days_In_Month (M);
      end if;
   end Days_In;

   -- Helper: Days since epoch
   function Days_Since_Epoch (Y : Year_Number; M : Month_Number; D : Day_Number) return Integer is
      Result : Integer := 0;
      Year   : Year_Number;
      Month  : Month_Number;
   begin
      -- Add days for complete years
      Year := Epoch_Year;
      while Year < Y loop
         if Is_Leap_Year (Year) then
            Result := Result + 366;
         else
            Result := Result + 365;
         end if;
         Year := Year + 1;
      end loop;

      -- Add days for complete months
      Month := 1;
      while Month < M loop
         Result := Result + Integer (Days_In (Y, Month));
         Month := Month + 1;
      end loop;

      -- Add remaining days
      Result := Result + Integer (D) - 1;

      return Result;
   end Days_Since_Epoch;

   -- Helper: Convert days since epoch to date
   procedure Days_To_Date
     (Total_Days : Integer;
      Y : out Year_Number;
      M : out Month_Number;
      D : out Day_Number)
   is
      Remaining : Integer := Total_Days;
      Year_Days : Integer;
   begin
      Y := Epoch_Year;

      -- Find year
      loop
         if Is_Leap_Year (Y) then
            Year_Days := 366;
         else
            Year_Days := 365;
         end if;

         exit when Remaining < Year_Days;
         Remaining := Remaining - Year_Days;
         Y := Y + 1;
      end loop;

      -- Find month
      M := 1;
      loop
         exit when M > 12;
         exit when Remaining < Integer (Days_In (Y, M));
         Remaining := Remaining - Integer (Days_In (Y, M));
         M := M + 1;
      end loop;

      if M > 12 then
         M := 12;
      end if;

      -- Remaining is day of month (0-based)
      D := Day_Number (Remaining + 1);
   end Days_To_Date;

   -- Clock function
   function Clock return Time is
      Result : Time;
   begin
      Result.Ticks := Rt_Get_Ticks;
      Result.Days := Rt_Get_Days;
      return Result;
   end Clock;

   -- Year extraction
   function Year (Date : Time) return Year_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
   begin
      Days_To_Date (Date.Days, Y, M, D);
      return Y;
   end Year;

   -- Month extraction
   function Month (Date : Time) return Month_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
   begin
      Days_To_Date (Date.Days, Y, M, D);
      return M;
   end Month;

   -- Day extraction
   function Day (Date : Time) return Day_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
   begin
      Days_To_Date (Date.Days, Y, M, D);
      return D;
   end Day;

   -- Seconds extraction (seconds since midnight)
   function Seconds (Date : Time) return Day_Duration is
   begin
      return Day_Duration (Date.Ticks / Ticks_Per_Second);
   end Seconds;

   -- Split into components
   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
   begin
      Days_To_Date (Date.Days, Year, Month, Day);
      Seconds := Day_Duration (Date.Ticks / Ticks_Per_Second);
   end Split;

   -- Time construction
   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0) return Time
   is
      Result : Time;
   begin
      -- Validate day
      if Day > Days_In (Year, Month) then
         raise Time_Error;
      end if;

      Result.Days := Days_Since_Epoch (Year, Month, Day);
      Result.Ticks := Seconds * Ticks_Per_Second;

      return Result;
   end Time_Of;

   -- Arithmetic: Time + seconds
   function "+" (Left : Time; Right : Integer) return Time is
      Result : Time := Left;
      New_Ticks : Integer;
   begin
      New_Ticks := Result.Ticks + (Right * Ticks_Per_Second);

      -- Handle overflow into days
      while New_Ticks >= Ticks_Per_Day loop
         New_Ticks := New_Ticks - Ticks_Per_Day;
         Result.Days := Result.Days + 1;
      end loop;

      -- Handle underflow
      while New_Ticks < 0 loop
         New_Ticks := New_Ticks + Ticks_Per_Day;
         Result.Days := Result.Days - 1;
      end loop;

      Result.Ticks := New_Ticks;
      return Result;
   end "+";

   function "+" (Left : Integer; Right : Time) return Time is
   begin
      return Right + Left;
   end "+";

   function "-" (Left : Time; Right : Integer) return Time is
   begin
      return Left + (-Right);
   end "-";

   function "-" (Left : Time; Right : Time) return Integer is
      Day_Diff : Integer;
      Tick_Diff : Integer;
   begin
      Day_Diff := Left.Days - Right.Days;
      Tick_Diff := Left.Ticks - Right.Ticks;

      return (Day_Diff * 86400) + (Tick_Diff / Ticks_Per_Second);
   end "-";

   -- Comparisons
   function "<" (Left, Right : Time) return Boolean is
   begin
      if Left.Days < Right.Days then
         return True;
      elsif Left.Days > Right.Days then
         return False;
      else
         return Left.Ticks < Right.Ticks;
      end if;
   end "<";

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function ">" (Left, Right : Time) return Boolean is
   begin
      if Left.Days > Right.Days then
         return True;
      elsif Left.Days < Right.Days then
         return False;
      else
         return Left.Ticks > Right.Ticks;
      end if;
   end ">";

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

end Ada.Calendar;
