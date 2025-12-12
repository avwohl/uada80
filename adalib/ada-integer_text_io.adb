-- Ada.Integer_Text_IO implementation for Z80
-- Integer I/O for 16-bit integers

with Ada.Text_IO; use Ada.Text_IO;

package body Ada.Integer_Text_IO is

   Digits : constant String := "0123456789ABCDEF";

   -- Helper: Convert integer to string
   procedure Int_To_String
     (Value : Integer;
      Base  : Number_Base;
      Str   : out String;
      Last  : out Natural)
   is
      Temp  : Integer := Value;
      Neg   : Boolean := Value < 0;
      Buf   : String (1 .. 20);
      I     : Natural := Buf'Last;
   begin
      if Neg then
         Temp := -Temp;
      end if;

      -- Convert to string (reverse order)
      loop
         Buf (I) := Digits ((Temp mod Base) + 1);
         Temp := Temp / Base;
         I := I - 1;
         exit when Temp = 0;
      end loop;

      -- Add base prefix if not decimal
      if Base /= 10 then
         I := I - 1;
         Buf (I) := '#';
         if Base = 16 then
            I := I - 1;
            Buf (I) := '6';
            I := I - 1;
            Buf (I) := '1';
         elsif Base = 8 then
            I := I - 1;
            Buf (I) := '8';
         elsif Base = 2 then
            I := I - 1;
            Buf (I) := '2';
         end if;
      end if;

      -- Add minus sign
      if Neg then
         I := I - 1;
         Buf (I) := '-';
      end if;

      -- Copy to output
      Last := Str'First - 1;
      for J in I + 1 .. Buf'Last loop
         Last := Last + 1;
         exit when Last > Str'Last;
         Str (Last) := Buf (J);
      end loop;

      -- Add trailing # for non-decimal
      if Base /= 10 and Last < Str'Last then
         Last := Last + 1;
         Str (Last) := '#';
      end if;
   end Int_To_String;

   -- Helper: Parse integer from string
   procedure String_To_Int
     (Str   : String;
      Value : out Integer;
      Last  : out Natural;
      Base  : Number_Base := 10)
   is
      I      : Natural := Str'First;
      Result : Integer := 0;
      Neg    : Boolean := False;
      C      : Character;
      Digit  : Integer;
      B      : Number_Base := Base;
   begin
      Value := 0;
      Last := Str'First - 1;

      -- Skip leading spaces
      while I <= Str'Last and then Str (I) = ' ' loop
         I := I + 1;
      end loop;

      if I > Str'Last then
         return;
      end if;

      -- Check for sign
      if Str (I) = '-' then
         Neg := True;
         I := I + 1;
      elsif Str (I) = '+' then
         I := I + 1;
      end if;

      -- Check for base prefix (e.g., 16#FF#)
      declare
         Save_I : Natural := I;
         N : Integer := 0;
      begin
         while I <= Str'Last and then Str (I) in '0' .. '9' loop
            N := N * 10 + (Character'Pos (Str (I)) - Character'Pos ('0'));
            I := I + 1;
         end loop;
         if I <= Str'Last and then Str (I) = '#' then
            if N >= 2 and N <= 16 then
               B := N;
               I := I + 1;  -- Skip #
            else
               I := Save_I;  -- Invalid base, reset
            end if;
         else
            I := Save_I;  -- No #, reset and parse as decimal
         end if;
      end;

      -- Parse digits
      while I <= Str'Last loop
         C := Str (I);
         if C in '0' .. '9' then
            Digit := Character'Pos (C) - Character'Pos ('0');
         elsif C in 'A' .. 'F' then
            Digit := Character'Pos (C) - Character'Pos ('A') + 10;
         elsif C in 'a' .. 'f' then
            Digit := Character'Pos (C) - Character'Pos ('a') + 10;
         elsif C = '#' then
            I := I + 1;  -- Skip trailing #
            exit;
         elsif C = '_' then
            I := I + 1;  -- Skip underscores in numbers
            goto Continue;
         else
            exit;
         end if;

         if Digit >= B then
            exit;  -- Invalid digit for this base
         end if;

         Result := Result * B + Digit;
         I := I + 1;
         <<Continue>>
      end loop;

      if Neg then
         Result := -Result;
      end if;

      Value := Result;
      Last := I - 1;
   end String_To_Int;

   -- File-based Get
   procedure Get
     (File  : File_Type;
      Item  : out Integer;
      Width : Field := 0)
   is
      Buffer : String (1 .. 20);
      C      : Character;
      I      : Natural := 0;
      Last   : Natural;
   begin
      -- Skip leading spaces
      loop
         Get (File, C);
         exit when C /= ' ' and C /= Character'Val (9);  -- Space or tab
      end loop;

      -- Read digits (and optional sign, base prefix)
      Buffer (1) := C;
      I := 1;
      while I < Buffer'Last loop
         exit when Width > 0 and I >= Width;
         begin
            Get (File, C);
            exit when C = ' ' or C = Character'Val (10) or C = Character'Val (13);
            I := I + 1;
            Buffer (I) := C;
         exception
            when End_Error => exit;
         end;
      end loop;

      String_To_Int (Buffer (1 .. I), Item, Last);
   end Get;

   procedure Get
     (Item  : out Integer;
      Width : Field := 0)
   is
   begin
      Get (Current_Input, Item, Width);
   end Get;

   -- File-based Put
   procedure Put
     (File  : File_Type;
      Item  : Integer;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
      Buffer : String (1 .. 20);
      Last   : Natural;
   begin
      Int_To_String (Item, Base, Buffer, Last);

      -- Pad with leading spaces if needed
      for I in Last + 1 .. Width loop
         Put (File, ' ');
      end loop;

      -- Output the number
      Put (File, Buffer (1 .. Last));
   end Put;

   procedure Put
     (Item  : Integer;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      Put (Current_Output, Item, Width, Base);
   end Put;

   -- String-based Get
   procedure Get
     (From : String;
      Item : out Integer;
      Last : out Positive)
   is
      L : Natural;
   begin
      String_To_Int (From, Item, L);
      if L >= From'First then
         Last := L;
      else
         raise Data_Error;
      end if;
   end Get;

   -- String-based Put
   procedure Put
     (To   : out String;
      Item : Integer;
      Base : Number_Base := Default_Base)
   is
      Buffer : String (1 .. 20);
      Last   : Natural;
   begin
      Int_To_String (Item, Base, Buffer, Last);

      -- Right-justify in output string
      if Last > To'Length then
         raise Layout_Error;
      end if;

      -- Fill with leading spaces
      for I in To'First .. To'Last - Last loop
         To (I) := ' ';
      end loop;

      -- Copy number
      for I in 1 .. Last loop
         To (To'Last - Last + I) := Buffer (I);
      end loop;
   end Put;

end Ada.Integer_Text_IO;
