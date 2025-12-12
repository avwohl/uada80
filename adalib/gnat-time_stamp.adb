-- GNAT.Time_Stamp body for Z80
-- Time stamp generation implementation

with Ada.Calendar;

package body GNAT.Time_Stamp is

   ------------------
   -- Current_Time --
   ------------------

   function Current_Time return String is
      Now   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Year  : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day   : Ada.Calendar.Day_Number;
      Secs  : Ada.Calendar.Day_Duration;

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
         return S (2 .. S'Last);
      end Pad4;

      Hour, Min, Sec, HSec : Natural;
      Dur : Natural;
   begin
      Ada.Calendar.Split (Now, Year, Month, Day, Secs);

      Dur := Natural (Secs);
      Hour := Dur / 3600;
      Min := (Dur mod 3600) / 60;
      Sec := Dur mod 60;
      HSec := Natural ((Secs - Ada.Calendar.Day_Duration (Dur)) * 100.0);

      return Pad4 (Year) & "-" & Pad2 (Month) & "-" & Pad2 (Day) & " " &
             Pad2 (Hour) & ":" & Pad2 (Min) & ":" & Pad2 (Sec) & "." & Pad2 (HSec);
   end Current_Time;

end GNAT.Time_Stamp;
