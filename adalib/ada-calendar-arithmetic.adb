-- Ada.Calendar.Arithmetic body for Z80
-- Calendar arithmetic operations implementation

package body Ada.Calendar.Arithmetic is

   Seconds_Per_Day : constant := 86_400;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Day_Count) return Time is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
      Days_To_Add : Integer := Integer (Right);
   begin
      Split (Left, Y, M, D, S);

      -- Simple day-by-day addition (not efficient but correct for Z80)
      while Days_To_Add > 0 loop
         D := D + 1;

         -- Check for month overflow
         declare
            Days_In_Month : constant array (Month_Number) of Day_Number :=
              (1 => 31, 2 => 28, 3 => 31, 4 => 30, 5 => 31, 6 => 30,
               7 => 31, 8 => 31, 9 => 30, 10 => 31, 11 => 30, 12 => 31);
            Max_Day : Day_Number := Days_In_Month (M);
         begin
            -- Leap year check for February
            if M = 2 and then ((Y mod 4 = 0 and Y mod 100 /= 0) or Y mod 400 = 0) then
               Max_Day := 29;
            end if;

            if D > Max_Day then
               D := 1;
               M := M + 1;
               if M > 12 then
                  M := 1;
                  Y := Y + 1;
               end if;
            end if;
         end;

         Days_To_Add := Days_To_Add - 1;
      end loop;

      while Days_To_Add < 0 loop
         D := D - 1;

         if D < 1 then
            M := M - 1;
            if M < 1 then
               M := 12;
               Y := Y - 1;
            end if;

            declare
               Days_In_Month : constant array (Month_Number) of Day_Number :=
                 (1 => 31, 2 => 28, 3 => 31, 4 => 30, 5 => 31, 6 => 30,
                  7 => 31, 8 => 31, 9 => 30, 10 => 31, 11 => 30, 12 => 31);
            begin
               D := Days_In_Month (M);
               if M = 2 and then ((Y mod 4 = 0 and Y mod 100 /= 0) or Y mod 400 = 0) then
                  D := 29;
               end if;
            end;
         end if;

         Days_To_Add := Days_To_Add + 1;
      end loop;

      return Time_Of (Y, M, D, S);
   end "+";

   function "+" (Left : Day_Count; Right : Time) return Time is
   begin
      return Right + Left;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Day_Count) return Time is
   begin
      return Left + (-Right);
   end "-";

   function "-" (Left, Right : Time) return Day_Count is
      Days    : Day_Count;
      Seconds : Duration;
      Leap    : Leap_Seconds_Count;
   begin
      Difference (Left, Right, Days, Seconds, Leap);
      return Days;
   end "-";

   ----------------
   -- Difference --
   ----------------

   procedure Difference
     (Left         : Time;
      Right        : Time;
      Days         : out Day_Count;
      Seconds      : out Duration;
      Leap_Seconds : out Leap_Seconds_Count)
   is
      L_Year, R_Year   : Year_Number;
      L_Month, R_Month : Month_Number;
      L_Day, R_Day     : Day_Number;
      L_Secs, R_Secs   : Day_Duration;
   begin
      Split (Left, L_Year, L_Month, L_Day, L_Secs);
      Split (Right, R_Year, R_Month, R_Day, R_Secs);

      -- Convert both dates to day count from epoch (simple calculation)
      declare
         function Days_From_Epoch (Y : Year_Number; M : Month_Number; D : Day_Number) return Integer is
            Total : Integer := 0;
            Days_In_Month : constant array (Month_Number) of Integer :=
              (1 => 31, 2 => 28, 3 => 31, 4 => 30, 5 => 31, 6 => 30,
               7 => 31, 8 => 31, 9 => 30, 10 => 31, 11 => 30, 12 => 31);
         begin
            -- Years
            for YY in Year_Number'First .. Y - 1 loop
               if (YY mod 4 = 0 and YY mod 100 /= 0) or YY mod 400 = 0 then
                  Total := Total + 366;
               else
                  Total := Total + 365;
               end if;
            end loop;

            -- Months
            for MM in 1 .. M - 1 loop
               Total := Total + Days_In_Month (MM);
               if MM = 2 and then ((Y mod 4 = 0 and Y mod 100 /= 0) or Y mod 400 = 0) then
                  Total := Total + 1;
               end if;
            end loop;

            -- Days
            Total := Total + Integer (D);

            return Total;
         end Days_From_Epoch;

         L_Days : constant Integer := Days_From_Epoch (L_Year, L_Month, L_Day);
         R_Days : constant Integer := Days_From_Epoch (R_Year, R_Month, R_Day);
      begin
         Days := Day_Count (L_Days - R_Days);
         Seconds := Duration (L_Secs - R_Secs);
         Leap_Seconds := 0;  -- Z80 doesn't track leap seconds
      end;
   end Difference;

end Ada.Calendar.Arithmetic;
