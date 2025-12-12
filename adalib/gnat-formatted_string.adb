-- GNAT.Formatted_String body for Z80
-- Printf-style formatted strings implementation

package body GNAT.Formatted_String is

   -- Find next format specifier
   procedure Find_Next_Spec
     (Fmt   : Formatted_String;
      Start : out Natural;
      Stop  : out Natural)
   is
      I : Natural := Fmt.Arg_Pos;
   begin
      Start := 0;
      Stop := 0;

      while I <= Fmt.Fmt_Len loop
         if Fmt.Format (I) = '%' then
            if I < Fmt.Fmt_Len and then Fmt.Format (I + 1) = '%' then
               -- Escaped %
               I := I + 2;
            else
               Start := I;
               I := I + 1;
               -- Skip flags
               while I <= Fmt.Fmt_Len and then
                     Fmt.Format (I) in '-' | '+' | ' ' | '#' | '0'
               loop
                  I := I + 1;
               end loop;
               -- Skip width
               while I <= Fmt.Fmt_Len and then Fmt.Format (I) in '0' .. '9' loop
                  I := I + 1;
               end loop;
               -- Skip precision
               if I <= Fmt.Fmt_Len and then Fmt.Format (I) = '.' then
                  I := I + 1;
                  while I <= Fmt.Fmt_Len and then Fmt.Format (I) in '0' .. '9' loop
                     I := I + 1;
                  end loop;
               end if;
               -- Skip length modifier
               if I <= Fmt.Fmt_Len and then Fmt.Format (I) in 'h' | 'l' | 'L' then
                  I := I + 1;
               end if;
               -- Specifier
               if I <= Fmt.Fmt_Len then
                  Stop := I;
                  return;
               end if;
            end if;
         else
            I := I + 1;
         end if;
      end loop;
   end Find_Next_Spec;

   -- Copy format text up to position
   procedure Copy_Up_To
     (Fmt  : in Out Formatted_String;
      Pos  : Natural)
   is
   begin
      for I in Fmt.Arg_Pos .. Pos - 1 loop
         if Fmt.Format (I) = '%' and then
            I < Fmt.Fmt_Len and then Fmt.Format (I + 1) = '%'
         then
            Fmt.Res_Len := Fmt.Res_Len + 1;
            Fmt.Result (Fmt.Res_Len) := '%';
         elsif Fmt.Format (I) /= '%' or else
               (I > 1 and then Fmt.Format (I - 1) /= '%')
         then
            Fmt.Res_Len := Fmt.Res_Len + 1;
            Fmt.Result (Fmt.Res_Len) := Fmt.Format (I);
         end if;
      end loop;
   end Copy_Up_To;

   ---------
   -- "+" --
   ---------

   function "+" (Format : String) return Formatted_String is
      Result : Formatted_String;
   begin
      Result.Fmt_Len := Natural'Min (Format'Length, Max_Format_Length);
      Result.Format (1 .. Result.Fmt_Len) :=
        Format (Format'First .. Format'First + Result.Fmt_Len - 1);
      Result.Arg_Pos := 1;
      Result.Res_Len := 0;
      return Result;
   end "+";

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Formatted_String; Value : String) return Formatted_String is
      Result : Formatted_String := Fmt;
      Start, Stop : Natural;
   begin
      Find_Next_Spec (Result, Start, Stop);
      if Start = 0 then
         raise Format_Error with "No format specifier for string argument";
      end if;

      Copy_Up_To (Result, Start);

      -- Insert value
      for I in Value'Range loop
         exit when Result.Res_Len >= Max_Result_Length;
         Result.Res_Len := Result.Res_Len + 1;
         Result.Result (Result.Res_Len) := Value (I);
      end loop;

      Result.Arg_Pos := Stop + 1;
      return Result;
   end "&";

   function "&" (Fmt : Formatted_String; Value : Character) return Formatted_String is
   begin
      return Fmt & String'(1 => Value);
   end "&";

   function "&" (Fmt : Formatted_String; Value : Integer) return Formatted_String is
      Buf : String (1 .. 12);
      Pos : Natural := Buf'Last;
      V   : Integer := Value;
      Neg : constant Boolean := V < 0;
   begin
      if V = 0 then
         return Fmt & "0";
      end if;

      if Neg then
         V := -V;
      end if;

      while V > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + (V mod 10));
         V := V / 10;
         Pos := Pos - 1;
      end loop;

      if Neg then
         Buf (Pos) := '-';
         Pos := Pos - 1;
      end if;

      return Fmt & Buf (Pos + 1 .. Buf'Last);
   end "&";

   function "&" (Fmt : Formatted_String; Value : Float) return Formatted_String is
      -- Simplified float formatting
      Int_Part : constant Integer := Integer (Value);
   begin
      return Fmt & Integer'Image (Int_Part);
   end "&";

   ---------
   -- "-" --
   ---------

   function "-" (Fmt : Formatted_String) return String is
      Result : Formatted_String := Fmt;
   begin
      -- Copy remaining format text
      for I in Result.Arg_Pos .. Result.Fmt_Len loop
         exit when Result.Res_Len >= Max_Result_Length;
         if Result.Format (I) = '%' and then
            I < Result.Fmt_Len and then Result.Format (I + 1) = '%'
         then
            Result.Res_Len := Result.Res_Len + 1;
            Result.Result (Result.Res_Len) := '%';
         elsif Result.Format (I) /= '%' or else
               (I > 1 and then Result.Format (I - 1) /= '%')
         then
            Result.Res_Len := Result.Res_Len + 1;
            Result.Result (Result.Res_Len) := Result.Format (I);
         end if;
      end loop;

      return Result.Result (1 .. Result.Res_Len);
   end "-";

end GNAT.Formatted_String;
