-- Ada.Long_Integer_Text_IO body for Z80
-- Text I/O for Long_Integer implementation

with Ada.Integer_Text_IO;

package body Ada.Long_Integer_Text_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Long_Integer;
      Width : Ada.Text_IO.Field := 0)
   is
      Temp : Integer;
   begin
      -- Read as Integer and convert
      Ada.Integer_Text_IO.Get (File, Temp, Width);
      Item := Long_Integer (Temp);
   end Get;

   procedure Get
     (Item  : out Long_Integer;
      Width : Ada.Text_IO.Field := 0)
   is
      Temp : Integer;
   begin
      Ada.Integer_Text_IO.Get (Temp, Width);
      Item := Long_Integer (Temp);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : Ada.Text_IO.File_Type;
      Item  : Long_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base)
   is
      Buffer : String (1 .. 32);
      Pos    : Natural := Buffer'Last;
      Val    : Long_Integer := abs Item;
      Digit  : Natural;
      Digits : constant String := "0123456789ABCDEF";
   begin
      -- Convert to string
      if Val = 0 then
         Buffer (Pos) := '0';
         Pos := Pos - 1;
      else
         while Val > 0 loop
            Digit := Natural (Val mod Long_Integer (Base));
            Buffer (Pos) := Digits (Digit + 1);
            Pos := Pos - 1;
            Val := Val / Long_Integer (Base);
         end loop;
      end if;

      if Item < 0 then
         Buffer (Pos) := '-';
         Pos := Pos - 1;
      end if;

      -- Add base prefix if not decimal
      if Base /= 10 then
         Buffer (Pos) := '#';
         Pos := Pos - 1;
         if Base >= 10 then
            Buffer (Pos) := Digits (Base / 10 + 1);
            Pos := Pos - 1;
         end if;
         Buffer (Pos) := Digits (Base mod 10 + 1);
         Pos := Pos - 1;
      end if;

      -- Pad and output
      declare
         Result : constant String := Buffer (Pos + 1 .. Buffer'Last);
         Padding : constant Natural := Natural'Max (0, Width - Result'Length);
      begin
         for I in 1 .. Padding loop
            Ada.Text_IO.Put (File, ' ');
         end loop;
         Ada.Text_IO.Put (File, Result);
         if Base /= 10 then
            Ada.Text_IO.Put (File, '#');
         end if;
      end;
   end Put;

   procedure Put
     (Item  : Long_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base)
   is
   begin
      Put (Ada.Text_IO.Standard_Output, Item, Width, Base);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Long_Integer;
      Last : out Positive)
   is
      Temp : Integer;
   begin
      Ada.Integer_Text_IO.Get (From, Temp, Last);
      Item := Long_Integer (Temp);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Long_Integer;
      Base : Ada.Text_IO.Number_Base := Default_Base)
   is
      Buffer : String (1 .. 32);
      Pos    : Natural := Buffer'Last;
      Val    : Long_Integer := abs Item;
      Digit  : Natural;
      Digits : constant String := "0123456789ABCDEF";
   begin
      if Val = 0 then
         Buffer (Pos) := '0';
         Pos := Pos - 1;
      else
         while Val > 0 loop
            Digit := Natural (Val mod Long_Integer (Base));
            Buffer (Pos) := Digits (Digit + 1);
            Pos := Pos - 1;
            Val := Val / Long_Integer (Base);
         end loop;
      end if;

      if Item < 0 then
         Buffer (Pos) := '-';
         Pos := Pos - 1;
      end if;

      declare
         Result : constant String := Buffer (Pos + 1 .. Buffer'Last);
         Start  : constant Positive := To'Last - Result'Length + 1;
      begin
         To := (others => ' ');
         if Start >= To'First then
            To (Start .. To'Last) := Result;
         else
            To := Result (Result'Last - To'Length + 1 .. Result'Last);
         end if;
      end;
   end Put;

end Ada.Long_Integer_Text_IO;
