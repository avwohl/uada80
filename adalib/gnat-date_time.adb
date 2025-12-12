-- GNAT.Date_Time body for Z80
-- Simple date/time handling implementation

package body GNAT.Date_Time is

   Days_Per_Month : constant array (1 .. 12) of Day_Number :=
     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   ---------------
   -- Make_Date --
   ---------------

   function Make_Date (Year : Year_Number; Month : Month_Number;
                       Day : Day_Number) return Date
   is
   begin
      return (Year => Year, Month => Month, Day => Day);
   end Make_Date;

   ---------------
   -- Make_Time --
   ---------------

   function Make_Time (Hour : Hour_Number; Minute : Minute_Number;
                       Second : Second_Number := 0) return Time
   is
   begin
      return (Hour => Hour, Minute => Minute, Second => Second);
   end Make_Time;

   --------------------
   -- Make_Date_Time --
   --------------------

   function Make_Date_Time (D : Date; T : Time) return Date_Time is
   begin
      return (D => D, T => T);
   end Make_Date_Time;

   ------------------
   -- Is_Leap_Year --
   ------------------

   function Is_Leap_Year (Year : Year_Number) return Boolean is
   begin
      return (Year mod 4 = 0 and Year mod 100 /= 0) or Year mod 400 = 0;
   end Is_Leap_Year;

   --------------------
   -- Days_In_Month --
   --------------------

   function Days_In_Month (Year : Year_Number; Month : Month_Number) return Day_Number is
   begin
      if Month = 2 and Is_Leap_Year (Year) then
         return 29;
      else
         return Days_Per_Month (Month);
      end if;
   end Days_In_Month;

   -------------------
   -- Is_Valid_Date --
   -------------------

   function Is_Valid_Date (D : Date) return Boolean is
   begin
      return D.Day <= Days_In_Month (D.Year, D.Month);
   end Is_Valid_Date;

   --------------------
   -- Get_Day_Of_Week --
   --------------------

   function Get_Day_Of_Week (D : Date) return Day_Of_Week is
      -- Zeller's congruence (simplified)
      Y : Integer := D.Year;
      M : Integer := D.Month;
      K : Integer;
      J : Integer;
      H : Integer;
   begin
      if M < 3 then
         M := M + 12;
         Y := Y - 1;
      end if;

      K := Y mod 100;
      J := Y / 100;

      H := (D.Day + (13 * (M + 1)) / 5 + K + K / 4 + J / 4 - 2 * J) mod 7;

      -- Adjust: Zeller returns 0=Saturday, we want 0=Sunday
      return (H + 6) mod 7;
   end Get_Day_Of_Week;

   --------------
   -- Day_Name --
   --------------

   function Day_Name (DOW : Day_Of_Week) return String is
   begin
      case DOW is
         when 0 => return "Sunday";
         when 1 => return "Monday";
         when 2 => return "Tuesday";
         when 3 => return "Wednesday";
         when 4 => return "Thursday";
         when 5 => return "Friday";
         when 6 => return "Saturday";
      end case;
   end Day_Name;

   ----------------
   -- Month_Name --
   ----------------

   function Month_Name (Month : Month_Number) return String is
   begin
      case Month is
         when 1  => return "January";
         when 2  => return "February";
         when 3  => return "March";
         when 4  => return "April";
         when 5  => return "May";
         when 6  => return "June";
         when 7  => return "July";
         when 8  => return "August";
         when 9  => return "September";
         when 10 => return "October";
         when 11 => return "November";
         when 12 => return "December";
      end case;
   end Month_Name;

   ----------------------
   -- Month_Name_Short --
   ----------------------

   function Month_Name_Short (Month : Month_Number) return String is
   begin
      return Month_Name (Month) (1 .. 3);
   end Month_Name_Short;

   -- Comparison operators for Date

   function "<" (Left, Right : Date) return Boolean is
   begin
      if Left.Year /= Right.Year then
         return Left.Year < Right.Year;
      elsif Left.Month /= Right.Month then
         return Left.Month < Right.Month;
      else
         return Left.Day < Right.Day;
      end if;
   end "<";

   function ">" (Left, Right : Date) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<=" (Left, Right : Date) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">=" (Left, Right : Date) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function "=" (Left, Right : Date) return Boolean is
   begin
      return Left.Year = Right.Year and Left.Month = Right.Month
        and Left.Day = Right.Day;
   end "=";

   -- Comparison operators for Time

   function "<" (Left, Right : Time) return Boolean is
   begin
      if Left.Hour /= Right.Hour then
         return Left.Hour < Right.Hour;
      elsif Left.Minute /= Right.Minute then
         return Left.Minute < Right.Minute;
      else
         return Left.Second < Right.Second;
      end if;
   end "<";

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "=" (Left, Right : Time) return Boolean is
   begin
      return Left.Hour = Right.Hour and Left.Minute = Right.Minute
        and Left.Second = Right.Second;
   end "=";

   -- Comparison for Date_Time

   function "<" (Left, Right : Date_Time) return Boolean is
   begin
      if Left.D /= Right.D then
         return Left.D < Right.D;
      else
         return Left.T < Right.T;
      end if;
   end "<";

   function ">" (Left, Right : Date_Time) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "=" (Left, Right : Date_Time) return Boolean is
   begin
      return Left.D = Right.D and Left.T = Right.T;
   end "=";

   --------------
   -- Add_Days --
   --------------

   function Add_Days (D : Date; Days : Integer) return Date is
      Result : Date := D;
      Remaining : Integer := Days;
      Days_Left : Integer;
   begin
      if Days > 0 then
         while Remaining > 0 loop
            Days_Left := Days_In_Month (Result.Year, Result.Month) - Result.Day;
            if Remaining <= Days_Left then
               Result.Day := Result.Day + Remaining;
               Remaining := 0;
            else
               Remaining := Remaining - Days_Left - 1;
               Result.Day := 1;
               if Result.Month = 12 then
                  Result.Month := 1;
                  Result.Year := Result.Year + 1;
               else
                  Result.Month := Result.Month + 1;
               end if;
            end if;
         end loop;
      elsif Days < 0 then
         Remaining := -Days;
         while Remaining > 0 loop
            if Remaining < Result.Day then
               Result.Day := Result.Day - Remaining;
               Remaining := 0;
            else
               Remaining := Remaining - Result.Day;
               if Result.Month = 1 then
                  Result.Month := 12;
                  Result.Year := Result.Year - 1;
               else
                  Result.Month := Result.Month - 1;
               end if;
               Result.Day := Days_In_Month (Result.Year, Result.Month);
            end if;
         end loop;
      end if;
      return Result;
   end Add_Days;

   ----------------
   -- Add_Months --
   ----------------

   function Add_Months (D : Date; Months : Integer) return Date is
      Result : Date := D;
      Total_Months : Integer := D.Month - 1 + Months;
   begin
      Result.Year := D.Year + Total_Months / 12;
      Result.Month := (Total_Months mod 12) + 1;
      if Result.Month < 1 then
         Result.Month := Result.Month + 12;
         Result.Year := Result.Year - 1;
      end if;

      -- Clamp day to valid range
      if Result.Day > Days_In_Month (Result.Year, Result.Month) then
         Result.Day := Days_In_Month (Result.Year, Result.Month);
      end if;

      return Result;
   end Add_Months;

   ---------------
   -- Add_Years --
   ---------------

   function Add_Years (D : Date; Years : Integer) return Date is
      Result : Date := D;
   begin
      Result.Year := D.Year + Years;
      if Result.Day > Days_In_Month (Result.Year, Result.Month) then
         Result.Day := Days_In_Month (Result.Year, Result.Month);
      end if;
      return Result;
   end Add_Years;

   ------------------
   -- Days_Between --
   ------------------

   function Days_Between (D1, D2 : Date) return Integer is
      function To_Days (D : Date) return Integer is
         Days : Integer := D.Day;
         Y    : Integer := D.Year;
         M    : Integer := D.Month;
      begin
         for I in 1 .. M - 1 loop
            Days := Days + Days_In_Month (Y, I);
         end loop;
         Days := Days + (Y - 1) * 365 + (Y - 1) / 4 - (Y - 1) / 100 + (Y - 1) / 400;
         return Days;
      end To_Days;
   begin
      return To_Days (D2) - To_Days (D1);
   end Days_Between;

   -----------------
   -- Add_Seconds --
   -----------------

   function Add_Seconds (T : Time; Seconds : Integer) return Time is
      Total : Integer := T.Hour * 3600 + T.Minute * 60 + T.Second + Seconds;
   begin
      Total := Total mod 86400;  -- Wrap within day
      if Total < 0 then
         Total := Total + 86400;
      end if;
      return (Hour => Total / 3600,
              Minute => (Total / 60) mod 60,
              Second => Total mod 60);
   end Add_Seconds;

   -----------------
   -- Add_Minutes --
   -----------------

   function Add_Minutes (T : Time; Minutes : Integer) return Time is
   begin
      return Add_Seconds (T, Minutes * 60);
   end Add_Minutes;

   ---------------
   -- Add_Hours --
   ---------------

   function Add_Hours (T : Time; Hours : Integer) return Time is
   begin
      return Add_Seconds (T, Hours * 3600);
   end Add_Hours;

   -----------------
   -- Day_Of_Year --
   -----------------

   function Day_Of_Year (D : Date) return Natural is
      Days : Natural := D.Day;
   begin
      for M in 1 .. D.Month - 1 loop
         Days := Days + Days_In_Month (D.Year, M);
      end loop;
      return Days;
   end Day_Of_Year;

   -----------------
   -- Week_Number --
   -----------------

   function Week_Number (D : Date) return Natural is
      DOY : constant Natural := Day_Of_Year (D);
   begin
      return (DOY + 6) / 7;
   end Week_Number;

   ----------------
   -- Date_Image --
   ----------------

   function Date_Image (D : Date) return String is
      function Pad2 (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         if N < 10 then
            return "0" & S (S'Last);
         else
            return S (S'Last - 1 .. S'Last);
         end if;
      end Pad2;

      function Pad4 (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         return S (S'First + 1 .. S'Last);
      end Pad4;
   begin
      return Pad4 (D.Year) & "-" & Pad2 (D.Month) & "-" & Pad2 (D.Day);
   end Date_Image;

   ----------------
   -- Time_Image --
   ----------------

   function Time_Image (T : Time) return String is
      function Pad2 (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         if N < 10 then
            return "0" & S (S'Last);
         else
            return S (S'Last - 1 .. S'Last);
         end if;
      end Pad2;
   begin
      return Pad2 (T.Hour) & ":" & Pad2 (T.Minute) & ":" & Pad2 (T.Second);
   end Time_Image;

   ---------------------
   -- Date_Time_Image --
   ---------------------

   function Date_Time_Image (DT : Date_Time) return String is
   begin
      return Date_Image (DT.D) & " " & Time_Image (DT.T);
   end Date_Time_Image;

   ----------------
   -- Parse_Date --
   ----------------

   function Parse_Date (S : String) return Date is
      Result : Date := (Year => 1970, Month => 1, Day => 1);
   begin
      if S'Length >= 10 then
         Result.Year := Integer'Value (S (S'First .. S'First + 3));
         Result.Month := Integer'Value (S (S'First + 5 .. S'First + 6));
         Result.Day := Integer'Value (S (S'First + 8 .. S'First + 9));
      end if;
      return Result;
   exception
      when others => return Result;
   end Parse_Date;

   ----------------
   -- Parse_Time --
   ----------------

   function Parse_Time (S : String) return Time is
      Result : Time := (Hour => 0, Minute => 0, Second => 0);
   begin
      if S'Length >= 8 then
         Result.Hour := Integer'Value (S (S'First .. S'First + 1));
         Result.Minute := Integer'Value (S (S'First + 3 .. S'First + 4));
         Result.Second := Integer'Value (S (S'First + 6 .. S'First + 7));
      elsif S'Length >= 5 then
         Result.Hour := Integer'Value (S (S'First .. S'First + 1));
         Result.Minute := Integer'Value (S (S'First + 3 .. S'First + 4));
      end if;
      return Result;
   exception
      when others => return Result;
   end Parse_Time;

end GNAT.Date_Time;
