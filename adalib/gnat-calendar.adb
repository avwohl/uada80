-- GNAT.Calendar body for Z80
-- Extended calendar operations implementation

package body GNAT.Calendar is

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Date : Ada.Calendar.Time) return Day_Name is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
      A, Year_Val, Month_Val : Integer;
   begin
      Ada.Calendar.Split (Date, Y, M, D, S);

      -- Zeller's congruence (adjusted for Monday = 1)
      Year_Val := Integer (Y);
      Month_Val := Integer (M);

      if Month_Val < 3 then
         Month_Val := Month_Val + 12;
         Year_Val := Year_Val - 1;
      end if;

      A := Integer (D) + (13 * (Month_Val + 1)) / 5 + Year_Val +
           Year_Val / 4 - Year_Val / 100 + Year_Val / 400;

      -- Convert to Monday = 1 .. Sunday = 7
      A := ((A + 5) mod 7) + 1;

      return Day_Name (A);
   end Day_Of_Week;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    : Ada.Calendar.Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Hour    : out Hour_Number;
      Minute  : out Minute_Number;
      Second  : out Second_Number)
   is
      Secs : Day_Duration;
      Remaining : Natural;
   begin
      Ada.Calendar.Split (Date, Year, Month, Day, Secs);

      Remaining := Natural (Secs);
      Hour := Remaining / 3600;
      Remaining := Remaining mod 3600;
      Minute := Remaining / 60;
      Second := Remaining mod 60;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      Hour   : Hour_Number   := 0;
      Minute : Minute_Number := 0;
      Second : Second_Number := 0) return Ada.Calendar.Time
   is
      Secs : constant Day_Duration :=
        Day_Duration (Hour * 3600 + Minute * 60 + Second);
   begin
      return Ada.Calendar.Time_Of (Year, Month, Day, Secs);
   end Time_Of;

   ------------------
   -- Week_In_Year --
   ------------------

   function Week_In_Year (Date : Ada.Calendar.Time) return Natural is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
      Day_Of_Year : Natural;
      First_Day   : Day_Name;
      First_Date  : Ada.Calendar.Time;
   begin
      Ada.Calendar.Split (Date, Y, M, D, S);

      -- Calculate day of year
      Day_Of_Year := Natural (D);
      for Month in 1 .. M - 1 loop
         case Month is
            when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
               Day_Of_Year := Day_Of_Year + 31;
            when 4 | 6 | 9 | 11 =>
               Day_Of_Year := Day_Of_Year + 30;
            when 2 =>
               if (Y mod 4 = 0 and Y mod 100 /= 0) or Y mod 400 = 0 then
                  Day_Of_Year := Day_Of_Year + 29;
               else
                  Day_Of_Year := Day_Of_Year + 28;
               end if;
         end case;
      end loop;

      -- Get day of week for Jan 1
      First_Date := Ada.Calendar.Time_Of (Y, 1, 1);
      First_Day := Day_Of_Week (First_Date);

      -- ISO week calculation (simplified)
      return (Day_Of_Year + Natural (First_Day) - 2) / 7 + 1;
   end Week_In_Year;

end GNAT.Calendar;
