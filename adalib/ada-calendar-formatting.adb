-- Ada.Calendar.Formatting body for Z80
-- Calendar formatting utilities implementation

package body Ada.Calendar.Formatting is

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Date : Time) return Day_Name is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
      -- Zeller's congruence variables
      Q, K, J, H : Integer;
   begin
      Split (Date, Y, M, D, S);

      Q := Integer (D);
      if M < 3 then
         M := M + 12;
         Y := Y - 1;
      end if;
      K := Integer (Y) mod 100;
      J := Integer (Y) / 100;

      H := (Q + (13 * (Integer (M) + 1)) / 5 + K + K / 4 + J / 4 - 2 * J) mod 7;

      -- Adjust: 0 = Saturday, 1 = Sunday, ..., 6 = Friday
      -- Convert to: Monday = 0, Sunday = 6
      case H is
         when 0 => return Saturday;
         when 1 => return Sunday;
         when 2 => return Monday;
         when 3 => return Tuesday;
         when 4 => return Wednesday;
         when 5 => return Thursday;
         when 6 => return Friday;
         when others => return Monday;
      end case;
   end Day_Of_Week;

   ----------
   -- Hour --
   ----------

   function Hour (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Natural is
      pragma Unreferenced (Time_Zone);
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return Natural (S) / 3600;
   end Hour;

   ------------
   -- Minute --
   ------------

   function Minute (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Natural is
      pragma Unreferenced (Time_Zone);
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return (Natural (S) mod 3600) / 60;
   end Minute;

   ------------
   -- Second --
   ------------

   function Second (Date : Time) return Natural is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return Natural (S) mod 60;
   end Second;

   ----------------
   -- Sub_Second --
   ----------------

   function Sub_Second (Date : Time) return Day_Duration is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return S - Day_Duration (Natural (S));
   end Sub_Second;

   ----------------
   -- Seconds_Of --
   ----------------

   function Seconds_Of
     (Hour       : Natural;
      Minute     : Natural;
      Second     : Natural := 0;
      Sub_Second : Day_Duration := 0.0) return Day_Duration
   is
   begin
      return Day_Duration (Hour * 3600 + Minute * 60 + Second) + Sub_Second;
   end Seconds_Of;

   -----------
   -- Split --
   -----------

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Natural;
      Minute     : out Natural;
      Second     : out Natural;
      Sub_Second : out Day_Duration)
   is
      Total_Secs : constant Natural := Natural (Seconds);
   begin
      Hour := Total_Secs / 3600;
      Minute := (Total_Secs mod 3600) / 60;
      Second := Total_Secs mod 60;
      Sub_Second := Seconds - Day_Duration (Total_Secs);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Natural;
      Minute     : Natural;
      Second     : Natural := 0;
      Sub_Second : Day_Duration := 0.0;
      Leap_Second: Boolean := False;
      Time_Zone  : Time_Zones.Time_Offset := 0) return Time
   is
      pragma Unreferenced (Leap_Second, Time_Zone);
   begin
      return Ada.Calendar.Time_Of (Year, Month, Day, Seconds_Of (Hour, Minute, Second, Sub_Second));
   end Time_Of;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Natural;
      Minute     : out Natural;
      Second     : out Natural;
      Sub_Second : out Day_Duration;
      Time_Zone  : Time_Zones.Time_Offset := 0)
   is
      pragma Unreferenced (Time_Zone);
      S : Day_Duration;
   begin
      Ada.Calendar.Split (Date, Year, Month, Day, S);
      Split (S, Hour, Minute, Second, Sub_Second);
   end Split;

   -----------
   -- Image --
   -----------

   function Image
     (Date      : Time;
      Include_Time_Fraction : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0) return String
   is
      pragma Unreferenced (Time_Zone);
      Y  : Year_Number;
      Mo : Month_Number;
      D  : Day_Number;
      H, Mi, S : Natural;
      SS : Day_Duration;
      Result : String (1 .. 23);
   begin
      Split (Date, Y, Mo, D, H, Mi, S, SS);

      -- Format: YYYY-MM-DD HH:MM:SS.ss
      Result (1 .. 4) := Year_Number'Image (Y)(2 .. 5);
      Result (5) := '-';
      Result (6 .. 7) := (if Mo < 10 then "0" & Month_Number'Image (Mo)(2) else Month_Number'Image (Mo)(2 .. 3));
      Result (8) := '-';
      Result (9 .. 10) := (if D < 10 then "0" & Day_Number'Image (D)(2) else Day_Number'Image (D)(2 .. 3));
      Result (11) := ' ';
      Result (12 .. 13) := (if H < 10 then "0" & Natural'Image (H)(2) else Natural'Image (H)(2 .. 3));
      Result (14) := ':';
      Result (15 .. 16) := (if Mi < 10 then "0" & Natural'Image (Mi)(2) else Natural'Image (Mi)(2 .. 3));
      Result (17) := ':';
      Result (18 .. 19) := (if S < 10 then "0" & Natural'Image (S)(2) else Natural'Image (S)(2 .. 3));

      if Include_Time_Fraction then
         Result (20) := '.';
         declare
            Frac : constant Natural := Natural (SS * 100.0) mod 100;
         begin
            Result (21 .. 22) := (if Frac < 10 then "0" & Natural'Image (Frac)(2) else Natural'Image (Frac)(2 .. 3));
         end;
         return Result (1 .. 22);
      else
         return Result (1 .. 19);
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Date : String; Time_Zone : Time_Zones.Time_Offset := 0) return Time is
      pragma Unreferenced (Time_Zone);
      Y  : Year_Number;
      Mo : Month_Number;
      D  : Day_Number;
      H, Mi, S : Natural := 0;
   begin
      -- Parse YYYY-MM-DD [HH:MM:SS]
      if Date'Length >= 10 then
         Y := Year_Number'Value (Date (Date'First .. Date'First + 3));
         Mo := Month_Number'Value (Date (Date'First + 5 .. Date'First + 6));
         D := Day_Number'Value (Date (Date'First + 8 .. Date'First + 9));

         if Date'Length >= 19 then
            H := Natural'Value (Date (Date'First + 11 .. Date'First + 12));
            Mi := Natural'Value (Date (Date'First + 14 .. Date'First + 15));
            S := Natural'Value (Date (Date'First + 17 .. Date'First + 18));
         end if;

         return Time_Of (Y, Mo, D, H, Mi, S);
      else
         raise Constraint_Error;
      end if;
   end Value;

   -----------
   -- Image --
   -----------

   function Image (Elapsed_Time : Duration; Include_Time_Fraction : Boolean := False) return String is
      Total_Secs : constant Natural := Natural (abs Elapsed_Time);
      H  : constant Natural := Total_Secs / 3600;
      Mi : constant Natural := (Total_Secs mod 3600) / 60;
      S  : constant Natural := Total_Secs mod 60;
      Result : String (1 .. 12);
      Pos : Natural := 0;
   begin
      if Elapsed_Time < 0.0 then
         Pos := 1;
         Result (1) := '-';
      end if;

      -- Format: HH:MM:SS or H:MM:SS
      if H >= 10 then
         Pos := Pos + 1;
         Result (Pos) := Character'Val (Character'Pos ('0') + H / 10);
      end if;
      Pos := Pos + 1;
      Result (Pos) := Character'Val (Character'Pos ('0') + H mod 10);
      Pos := Pos + 1;
      Result (Pos) := ':';
      Pos := Pos + 1;
      Result (Pos) := Character'Val (Character'Pos ('0') + Mi / 10);
      Pos := Pos + 1;
      Result (Pos) := Character'Val (Character'Pos ('0') + Mi mod 10);
      Pos := Pos + 1;
      Result (Pos) := ':';
      Pos := Pos + 1;
      Result (Pos) := Character'Val (Character'Pos ('0') + S / 10);
      Pos := Pos + 1;
      Result (Pos) := Character'Val (Character'Pos ('0') + S mod 10);

      if Include_Time_Fraction then
         Pos := Pos + 1;
         Result (Pos) := '.';
         declare
            Frac : constant Natural := Natural ((abs Elapsed_Time - Duration (Total_Secs)) * 100.0) mod 100;
         begin
            Pos := Pos + 1;
            Result (Pos) := Character'Val (Character'Pos ('0') + Frac / 10);
            Pos := Pos + 1;
            Result (Pos) := Character'Val (Character'Pos ('0') + Frac mod 10);
         end;
      end if;

      return Result (1 .. Pos);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Elapsed_Time : String) return Duration is
      Negative : Boolean := False;
      Start    : Positive := Elapsed_Time'First;
      H, Mi, S : Natural := 0;
   begin
      if Elapsed_Time'Length > 0 and then Elapsed_Time (Start) = '-' then
         Negative := True;
         Start := Start + 1;
      end if;

      -- Parse H:MM:SS or HH:MM:SS
      declare
         Colon1 : Natural := 0;
         Colon2 : Natural := 0;
      begin
         for I in Start .. Elapsed_Time'Last loop
            if Elapsed_Time (I) = ':' then
               if Colon1 = 0 then
                  Colon1 := I;
               else
                  Colon2 := I;
               end if;
            end if;
         end loop;

         if Colon1 > 0 and Colon2 > 0 then
            H := Natural'Value (Elapsed_Time (Start .. Colon1 - 1));
            Mi := Natural'Value (Elapsed_Time (Colon1 + 1 .. Colon2 - 1));
            S := Natural'Value (Elapsed_Time (Colon2 + 1 .. Elapsed_Time'Last));
         end if;
      end;

      declare
         Result : Duration := Duration (H * 3600 + Mi * 60 + S);
      begin
         if Negative then
            Result := -Result;
         end if;
         return Result;
      end;
   end Value;

   ----------------
   -- Time_Zones --
   ----------------

   package body Time_Zones is

      ---------------------
      -- UTC_Time_Offset --
      ---------------------

      function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
         pragma Unreferenced (Date);
      begin
         -- Z80/CP/M doesn't have time zone support; assume UTC
         return 0;
      end UTC_Time_Offset;

   end Time_Zones;

end Ada.Calendar.Formatting;
