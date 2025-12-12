-- GNAT.Formatted_IO body for Z80
-- Formatted I/O implementation

with Ada.Text_IO;

package body GNAT.Formatted_IO is

   procedure Put_Format
     (Format : String;
      Items  : array (Positive range <>) of access constant String)
   is
      I       : Natural := Format'First;
      Item_Idx : Natural := 1;
   begin
      while I <= Format'Last loop
         if Format (I) = '%' and I < Format'Last then
            case Format (I + 1) is
               when 's' =>
                  if Item_Idx <= Items'Last then
                     Ada.Text_IO.Put (Items (Item_Idx).all);
                     Item_Idx := Item_Idx + 1;
                  end if;
                  I := I + 2;
               when '%' =>
                  Ada.Text_IO.Put ('%');
                  I := I + 2;
               when 'n' =>
                  Ada.Text_IO.New_Line;
                  I := I + 2;
               when others =>
                  Ada.Text_IO.Put (Format (I));
                  I := I + 1;
            end case;
         else
            Ada.Text_IO.Put (Format (I));
            I := I + 1;
         end if;
      end loop;
   end Put_Format;

   ---------
   -- Put --
   ---------

   procedure Put (Format : String) is
      Empty : constant array (1 .. 0) of access constant String := (others => <>);
   begin
      Put_Format (Format, Empty);
   end Put;

   procedure Put (Format : String; Item1 : String) is
      S1 : aliased constant String := Item1;
   begin
      Put_Format (Format, (1 => S1'Access));
   end Put;

   procedure Put (Format : String; Item1, Item2 : String) is
      S1 : aliased constant String := Item1;
      S2 : aliased constant String := Item2;
   begin
      Put_Format (Format, (S1'Access, S2'Access));
   end Put;

   procedure Put (Format : String; Item1, Item2, Item3 : String) is
      S1 : aliased constant String := Item1;
      S2 : aliased constant String := Item2;
      S3 : aliased constant String := Item3;
   begin
      Put_Format (Format, (S1'Access, S2'Access, S3'Access));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Format : String) is
   begin
      Put (Format);
      Ada.Text_IO.New_Line;
   end Put_Line;

   procedure Put_Line (Format : String; Item1 : String) is
   begin
      Put (Format, Item1);
      Ada.Text_IO.New_Line;
   end Put_Line;

   procedure Put_Line (Format : String; Item1, Item2 : String) is
   begin
      Put (Format, Item1, Item2);
      Ada.Text_IO.New_Line;
   end Put_Line;

   ----------------
   -- Format_Int --
   ----------------

   function Format_Int
     (Value : Integer;
      Width : Natural := 0;
      Fill  : Character := ' ') return String
   is
      Img    : constant String := Integer'Image (Value);
      Actual : constant String := (if Img (1) = ' ' then Img (2 .. Img'Last) else Img);
   begin
      if Actual'Length >= Width then
         return Actual;
      end if;

      declare
         Result : String (1 .. Width) := (others => Fill);
      begin
         Result (Width - Actual'Length + 1 .. Width) := Actual;
         return Result;
      end;
   end Format_Int;

   ------------------
   -- Format_Float --
   ------------------

   function Format_Float
     (Value     : Float;
      Precision : Natural := 2) return String
   is
      Int_Part  : constant Integer := Integer (Float'Floor (abs Value));
      Frac_Part : constant Float := abs Value - Float (Int_Part);
      Frac_Int  : Integer;
      Result    : String (1 .. 32);
      Pos       : Natural := 0;
   begin
      -- Sign
      if Value < 0.0 then
         Pos := Pos + 1;
         Result (Pos) := '-';
      end if;

      -- Integer part
      declare
         Int_Img : constant String := Integer'Image (Int_Part);
         Start   : constant Natural := (if Int_Img (1) = ' ' then 2 else 1);
      begin
         Result (Pos + 1 .. Pos + Int_Img'Last - Start + 1) :=
           Int_Img (Start .. Int_Img'Last);
         Pos := Pos + Int_Img'Last - Start + 1;
      end;

      -- Decimal point and fraction
      if Precision > 0 then
         Pos := Pos + 1;
         Result (Pos) := '.';

         Frac_Int := Integer (Frac_Part * Float (10 ** Precision));
         declare
            Frac_Img : constant String := Integer'Image (Frac_Int);
            Start    : constant Natural := (if Frac_Img (1) = ' ' then 2 else 1);
            Frac_Len : constant Natural := Frac_Img'Last - Start + 1;
         begin
            -- Pad with leading zeros
            for I in 1 .. Precision - Frac_Len loop
               Pos := Pos + 1;
               Result (Pos) := '0';
            end loop;
            Result (Pos + 1 .. Pos + Frac_Len) := Frac_Img (Start .. Frac_Img'Last);
            Pos := Pos + Frac_Len;
         end;
      end if;

      return Result (1 .. Pos);
   end Format_Float;

end GNAT.Formatted_IO;
