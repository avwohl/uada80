-- Ada.Long_Long_Integer_Text_IO body for Z80
-- Text I/O for Long_Long_Integer type implementation

package body Ada.Long_Long_Integer_Text_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Long_Long_Integer;
      Width : Ada.Text_IO.Field := 0)
   is
      pragma Unreferenced (File, Width);
   begin
      -- Simplified stub - real implementation needs proper parsing
      Item := 0;
   end Get;

   procedure Get
     (Item  : out Long_Long_Integer;
      Width : Ada.Text_IO.Field := 0)
   is
   begin
      Get (Ada.Text_IO.Current_Input, Item, Width);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : Ada.Text_IO.File_Type;
      Item  : Long_Long_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base)
   is
      pragma Unreferenced (File);
      Buffer : String (1 .. 30);
   begin
      Put (Buffer, Item, Base);
      -- Output with padding
      declare
         First : Natural := Buffer'First;
      begin
         while First <= Buffer'Last and then Buffer (First) = ' ' loop
            First := First + 1;
         end loop;
         if First > Buffer'Last then
            First := Buffer'Last;
         end if;
         declare
            Str_Len : constant Natural := Buffer'Last - First + 1;
         begin
            if Width > Str_Len then
               for I in 1 .. Width - Str_Len loop
                  Ada.Text_IO.Put (' ');
               end loop;
            end if;
            Ada.Text_IO.Put (Buffer (First .. Buffer'Last));
         end;
      end;
   end Put;

   procedure Put
     (Item  : Long_Long_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base)
   is
   begin
      Put (Ada.Text_IO.Current_Output, Item, Width, Base);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Long_Long_Integer;
      Last : out Positive)
   is
      Value    : Long_Long_Integer := 0;
      Negative : Boolean := False;
      First    : Positive := From'First;
      Digit    : Integer;
   begin
      -- Skip leading spaces
      while First <= From'Last and then From (First) = ' ' loop
         First := First + 1;
      end loop;

      if First > From'Last then
         raise Ada.Text_IO.Data_Error;
      end if;

      -- Check for sign
      if From (First) = '-' then
         Negative := True;
         First := First + 1;
      elsif From (First) = '+' then
         First := First + 1;
      end if;

      -- Parse digits
      Last := First;
      while Last <= From'Last and then From (Last) in '0' .. '9' loop
         Digit := Character'Pos (From (Last)) - Character'Pos ('0');
         Value := Value * 10 + Long_Long_Integer (Digit);
         Last := Last + 1;
      end loop;

      if Last = First then
         raise Ada.Text_IO.Data_Error;
      end if;

      Last := Last - 1;

      if Negative then
         Item := -Value;
      else
         Item := Value;
      end if;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Long_Long_Integer;
      Base : Ada.Text_IO.Number_Base := Default_Base)
   is
      Digits    : constant String := "0123456789ABCDEF";
      Value     : Long_Long_Integer := abs Item;
      Pos       : Natural := To'Last;
      Negative  : constant Boolean := Item < 0;
      Base_Val  : constant Long_Long_Integer := Long_Long_Integer (Base);
   begin
      -- Initialize with spaces
      To := (others => ' ');

      -- Handle zero
      if Value = 0 then
         To (Pos) := '0';
         Pos := Pos - 1;
      else
         -- Convert digits
         while Value > 0 loop
            To (Pos) := Digits (Integer (Value mod Base_Val) + 1);
            Value := Value / Base_Val;
            Pos := Pos - 1;
            exit when Pos < To'First;
         end loop;
      end if;

      -- Add base prefix/suffix if not decimal
      if Base /= 10 and Pos >= To'First + 1 then
         To (Pos) := '#';
         Pos := Pos - 1;
         if Base >= 10 then
            To (Pos) := Digits (Base / 10 + 1);
            Pos := Pos - 1;
         end if;
         To (Pos) := Digits (Base mod 10 + 1);
         Pos := Pos - 1;
         -- Also need trailing #
         if To'Last < To'Length then
            null; -- Would need to handle this better
         end if;
      end if;

      -- Add sign for negative
      if Negative and Pos >= To'First then
         To (Pos) := '-';
      end if;
   end Put;

end Ada.Long_Long_Integer_Text_IO;
