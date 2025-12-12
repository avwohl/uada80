-- GNAT.Calendar.Time_IO body for Z80
-- Time formatting and parsing implementation

package body GNAT.Calendar.Time_IO is

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
      if N < 10 then
         return "000" & S (S'Last);
      elsif N < 100 then
         return "00" & S (S'Last - 1 .. S'Last);
      elsif N < 1000 then
         return "0" & S (S'Last - 2 .. S'Last);
      else
         return S (S'Last - 3 .. S'Last);
      end if;
   end Pad4;

   -----------
   -- Image --
   -----------

   function Image
     (Date    : Ada.Calendar.Time;
      Pattern : String := "%Y-%m-%d %H:%M:%S") return String
   is
      use Ada.Calendar;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
      Hour    : Natural;
      Minute  : Natural;
      Second  : Natural;
      Result  : String (1 .. 100);
      R_Idx   : Natural := 0;
      P_Idx   : Positive := Pattern'First;
   begin
      Split (Date, Year, Month, Day, Seconds);
      Hour := Natural (Seconds) / 3600;
      Minute := (Natural (Seconds) / 60) mod 60;
      Second := Natural (Seconds) mod 60;

      while P_Idx <= Pattern'Last loop
         if Pattern (P_Idx) = '%' and P_Idx < Pattern'Last then
            P_Idx := P_Idx + 1;
            case Pattern (P_Idx) is
               when 'Y' =>
                  declare
                     Y : constant String := Pad4 (Year);
                  begin
                     for C of Y loop
                        R_Idx := R_Idx + 1;
                        Result (R_Idx) := C;
                     end loop;
                  end;
               when 'm' =>
                  declare
                     M : constant String := Pad2 (Month);
                  begin
                     for C of M loop
                        R_Idx := R_Idx + 1;
                        Result (R_Idx) := C;
                     end loop;
                  end;
               when 'd' =>
                  declare
                     D : constant String := Pad2 (Day);
                  begin
                     for C of D loop
                        R_Idx := R_Idx + 1;
                        Result (R_Idx) := C;
                     end loop;
                  end;
               when 'H' =>
                  declare
                     H : constant String := Pad2 (Hour);
                  begin
                     for C of H loop
                        R_Idx := R_Idx + 1;
                        Result (R_Idx) := C;
                     end loop;
                  end;
               when 'M' =>
                  declare
                     Mi : constant String := Pad2 (Minute);
                  begin
                     for C of Mi loop
                        R_Idx := R_Idx + 1;
                        Result (R_Idx) := C;
                     end loop;
                  end;
               when 'S' =>
                  declare
                     S : constant String := Pad2 (Second);
                  begin
                     for C of S loop
                        R_Idx := R_Idx + 1;
                        Result (R_Idx) := C;
                     end loop;
                  end;
               when others =>
                  R_Idx := R_Idx + 1;
                  Result (R_Idx) := Pattern (P_Idx);
            end case;
         else
            R_Idx := R_Idx + 1;
            Result (R_Idx) := Pattern (P_Idx);
         end if;
         P_Idx := P_Idx + 1;
      end loop;

      return Result (1 .. R_Idx);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Date_String : String) return Ada.Calendar.Time is
      use Ada.Calendar;
      Year   : Year_Number := 2000;
      Month  : Month_Number := 1;
      Day    : Day_Number := 1;
      Hour   : Natural := 0;
      Minute : Natural := 0;
      Second : Natural := 0;
      Idx    : Positive := Date_String'First;

      function Parse_Number (Digits : Positive) return Natural is
         Result : Natural := 0;
      begin
         for I in 1 .. Digits loop
            exit when Idx > Date_String'Last;
            exit when Date_String (Idx) not in '0' .. '9';
            Result := Result * 10 +
              (Character'Pos (Date_String (Idx)) - Character'Pos ('0'));
            Idx := Idx + 1;
         end loop;
         return Result;
      end Parse_Number;

      procedure Skip_Non_Digit is
      begin
         while Idx <= Date_String'Last and then
           Date_String (Idx) not in '0' .. '9'
         loop
            Idx := Idx + 1;
         end loop;
      end Skip_Non_Digit;

   begin
      -- Parse YYYY-MM-DD HH:MM:SS
      Year := Year_Number (Parse_Number (4));
      Skip_Non_Digit;
      Month := Month_Number (Parse_Number (2));
      Skip_Non_Digit;
      Day := Day_Number (Parse_Number (2));
      Skip_Non_Digit;

      if Idx <= Date_String'Last then
         Hour := Parse_Number (2);
         Skip_Non_Digit;
         Minute := Parse_Number (2);
         Skip_Non_Digit;
         Second := Parse_Number (2);
      end if;

      return Time_Of (Year, Month, Day,
                      Day_Duration (Hour * 3600 + Minute * 60 + Second));
   end Value;

   ----------------
   -- Image_Date --
   ----------------

   function Image_Date (Date : Ada.Calendar.Time) return String is
   begin
      return Image (Date, "%Y-%m-%d");
   end Image_Date;

   ----------------
   -- Image_Time --
   ----------------

   function Image_Time (Date : Ada.Calendar.Time) return String is
   begin
      return Image (Date, "%H:%M:%S");
   end Image_Time;

end GNAT.Calendar.Time_IO;
