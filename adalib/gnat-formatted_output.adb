-- GNAT.Formatted_Output body for Z80
-- Printf-style formatted output implementation

with GNAT.IO;

package body GNAT.Formatted_Output is

   function Find_Format (S : String) return Natural is
   begin
      for I in S'Range loop
         if S (I) = '%' and I < S'Last then
            if S (I + 1) /= '%' then
               return I;
            end if;
         end if;
      end loop;
      return 0;
   end Find_Format;

   ------------
   -- Format --
   ------------

   function Format (Fmt : String) return String is
   begin
      return Fmt;
   end Format;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : String; Value : Integer) return String is
      Pos : constant Natural := Find_Format (Fmt);
   begin
      if Pos = 0 then
         return Fmt;
      end if;

      declare
         Val_Str : constant String := Integer'Image (Value);
         Trimmed : constant String :=
           (if Val_Str (Val_Str'First) = ' '
            then Val_Str (Val_Str'First + 1 .. Val_Str'Last)
            else Val_Str);
      begin
         return Fmt (Fmt'First .. Pos - 1) & Trimmed &
                Fmt (Pos + 2 .. Fmt'Last);
      end;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : String; Value : Float) return String is
      Pos : constant Natural := Find_Format (Fmt);
   begin
      if Pos = 0 then
         return Fmt;
      end if;

      -- Simple float to string
      declare
         Int_Part  : constant Integer := Integer (Value);
         Frac_Part : constant Integer := Integer ((abs Value - abs Float (Int_Part)) * 1000.0);
         Val_Str   : constant String :=
           Integer'Image (Int_Part) & "." &
           (if Frac_Part < 10 then "00" elsif Frac_Part < 100 then "0" else "") &
           Integer'Image (Frac_Part);
      begin
         return Fmt (Fmt'First .. Pos - 1) & Val_Str & Fmt (Pos + 2 .. Fmt'Last);
      end;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : String; Value : String) return String is
      Pos : constant Natural := Find_Format (Fmt);
   begin
      if Pos = 0 then
         return Fmt;
      end if;
      return Fmt (Fmt'First .. Pos - 1) & Value & Fmt (Pos + 2 .. Fmt'Last);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : String; Value : Character) return String is
      Pos : constant Natural := Find_Format (Fmt);
   begin
      if Pos = 0 then
         return Fmt;
      end if;
      return Fmt (Fmt'First .. Pos - 1) & Value & Fmt (Pos + 2 .. Fmt'Last);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : String; Value : Boolean) return String is
      Pos : constant Natural := Find_Format (Fmt);
   begin
      if Pos = 0 then
         return Fmt;
      end if;
      if Value then
         return Fmt (Fmt'First .. Pos - 1) & "True" & Fmt (Pos + 2 .. Fmt'Last);
      else
         return Fmt (Fmt'First .. Pos - 1) & "False" & Fmt (Pos + 2 .. Fmt'Last);
      end if;
   end "&";

   ---------
   -- Put --
   ---------

   procedure Put (Fmt : String) is
   begin
      GNAT.IO.Put (Fmt);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Fmt : String) is
   begin
      GNAT.IO.Put_Line (Fmt);
   end Put_Line;

end GNAT.Formatted_Output;
