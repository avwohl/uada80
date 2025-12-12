-- Ada.Text_IO.Enum_Aux body for Z80
-- Enumeration type I/O support

package body Ada.Text_IO.Enum_Aux is

   --------------
   -- Put_Enum --
   --------------

   procedure Put_Enum
     (To    : out String;
      Image : String;
      Width : Natural;
      Set   : Type_Set := Upper_Case)
   is
      Formatted : String := Format_Enum (Image, Width, Set);
   begin
      if Formatted'Length <= To'Length then
         To (To'First .. To'First + Formatted'Length - 1) := Formatted;
         if Formatted'Length < To'Length then
            To (To'First + Formatted'Length .. To'Last) := (others => ' ');
         end if;
      else
         To := Formatted (Formatted'First .. Formatted'First + To'Length - 1);
      end if;
   end Put_Enum;

   -----------------
   -- Format_Enum --
   -----------------

   function Format_Enum
     (Image : String;
      Width : Natural;
      Set   : Type_Set := Upper_Case) return String
   is
      Result : String (1 .. Natural'Max (Image'Length, Width));
      Len    : constant Natural := Image'Length;
      Pad    : Natural := 0;
   begin
      if Width > Len then
         Pad := Width - Len;
      end if;

      -- Copy image with case conversion
      for I in 1 .. Len loop
         declare
            C : Character := Image (Image'First + I - 1);
         begin
            case Set is
               when Upper_Case =>
                  if C in 'a' .. 'z' then
                     C := Character'Val (Character'Pos (C) - 32);
                  end if;
               when Lower_Case =>
                  if C in 'A' .. 'Z' then
                     C := Character'Val (Character'Pos (C) + 32);
                  end if;
            end case;
            Result (Pad + I) := C;
         end;
      end loop;

      -- Add leading spaces
      for I in 1 .. Pad loop
         Result (I) := ' ';
      end loop;

      return Result (1 .. Pad + Len);
   end Format_Enum;

   --------------
   -- To_Upper --
   --------------

   procedure To_Upper (S : in Out String) is
   begin
      for I in S'Range loop
         if S (I) in 'a' .. 'z' then
            S (I) := Character'Val (Character'Pos (S (I)) - 32);
         end if;
      end loop;
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in Out String) is
   begin
      for I in S'Range loop
         if S (I) in 'A' .. 'Z' then
            S (I) := Character'Val (Character'Pos (S (I)) + 32);
         end if;
      end loop;
   end To_Lower;

end Ada.Text_IO.Enum_Aux;
