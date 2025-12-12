-- Ada.Calendar.Conversions body for Z80
-- Calendar type conversions implementation

package body Ada.Calendar.Conversions is

   -- Unix epoch is 1970-01-01
   -- Ada epoch is 1901-01-01
   Unix_Epoch_Offset : constant Duration := Duration (
     (1970 - 1901) * 365 * 86400 +  -- years
     17 * 86400);  -- leap years 1904-1968

   -----------------
   -- To_Ada_Time --
   -----------------

   function To_Ada_Time (Unix_Time : Interfaces.C.long) return Ada.Calendar.Time is
      Seconds : constant Duration := Duration (Unix_Time);
   begin
      return Ada.Calendar.Time_Of (1970, 1, 1, Seconds);
   end To_Ada_Time;

   ------------------
   -- To_Unix_Time --
   ------------------

   function To_Unix_Time (Date : Ada.Calendar.Time) return Interfaces.C.long is
      Year    : Ada.Calendar.Year_Number;
      Month   : Ada.Calendar.Month_Number;
      Day     : Ada.Calendar.Day_Number;
      Seconds : Ada.Calendar.Day_Duration;
      Result  : Long_Long_Integer;
   begin
      Ada.Calendar.Split (Date, Year, Month, Day, Seconds);

      -- Calculate days since Unix epoch
      Result := Long_Long_Integer (Year - 1970) * 365;

      -- Add leap years
      for Y in 1970 .. Year - 1 loop
         if (Y mod 4 = 0 and Y mod 100 /= 0) or Y mod 400 = 0 then
            Result := Result + 1;
         end if;
      end loop;

      -- Add months
      declare
         Days_In_Month : constant array (1 .. 12) of Integer :=
           (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
         Is_Leap : constant Boolean :=
           (Year mod 4 = 0 and Year mod 100 /= 0) or Year mod 400 = 0;
      begin
         for M in 1 .. Month - 1 loop
            Result := Result + Long_Long_Integer (Days_In_Month (M));
            if M = 2 and Is_Leap then
               Result := Result + 1;
            end if;
         end loop;
      end;

      -- Add days and convert to seconds
      Result := (Result + Long_Long_Integer (Day - 1)) * 86400;
      Result := Result + Long_Long_Integer (Seconds);

      return Interfaces.C.long (Result);
   end To_Unix_Time;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TV_Sec : Interfaces.C.long;
                         TV_Usec : Interfaces.C.long) return Duration
   is
   begin
      return Duration (TV_Sec) + Duration (TV_Usec) / 1_000_000.0;
   end To_Duration;

   ------------------------
   -- To_Struct_Timespec --
   ------------------------

   procedure To_Struct_Timespec
     (D       : Duration;
      TV_Sec  : out Interfaces.C.long;
      TV_Nsec : out Interfaces.C.long)
   is
      Secs : constant Duration := Duration'Truncation (D);
   begin
      TV_Sec := Interfaces.C.long (Secs);
      TV_Nsec := Interfaces.C.long ((D - Secs) * 1_000_000_000);
   end To_Struct_Timespec;

   -----------------------
   -- To_Struct_Timeval --
   -----------------------

   procedure To_Struct_Timeval
     (D       : Duration;
      TV_Sec  : out Interfaces.C.long;
      TV_Usec : out Interfaces.C.long)
   is
      Secs : constant Duration := Duration'Truncation (D);
   begin
      TV_Sec := Interfaces.C.long (Secs);
      TV_Usec := Interfaces.C.long ((D - Secs) * 1_000_000);
   end To_Struct_Timeval;

end Ada.Calendar.Conversions;
