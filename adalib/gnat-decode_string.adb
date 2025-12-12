-- GNAT.Decode_String body for Z80
-- String decoding implementation

package body GNAT.Decode_String is

   function Hex_Value (C : Character) return Natural is
   begin
      if C in '0' .. '9' then
         return Character'Pos (C) - Character'Pos ('0');
      elsif C in 'A' .. 'F' then
         return Character'Pos (C) - Character'Pos ('A') + 10;
      elsif C in 'a' .. 'f' then
         return Character'Pos (C) - Character'Pos ('a') + 10;
      else
         return 0;
      end if;
   end Hex_Value;

   ----------------
   -- Decode_Hex --
   ----------------

   function Decode_Hex (S : String) return String is
      Result : String (1 .. S'Length / 2);
      R_Idx  : Natural := 0;
      I      : Positive := S'First;
   begin
      while I + 1 <= S'Last loop
         R_Idx := R_Idx + 1;
         Result (R_Idx) := Character'Val (
           Hex_Value (S (I)) * 16 + Hex_Value (S (I + 1)));
         I := I + 2;
      end loop;
      return Result (1 .. R_Idx);
   end Decode_Hex;

   -------------------
   -- Decode_Base64 --
   -------------------

   function Decode_Base64 (S : String) return String is
      Base64_Chars : constant String :=
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
      Result : String (1 .. (S'Length * 3) / 4);
      R_Idx  : Natural := 0;
      I      : Positive := S'First;
      V      : array (1 .. 4) of Natural;

      function Decode_Char (C : Character) return Natural is
      begin
         if C = '=' then
            return 0;
         end if;
         for J in Base64_Chars'Range loop
            if Base64_Chars (J) = C then
               return J - 1;
            end if;
         end loop;
         return 0;
      end Decode_Char;

   begin
      while I + 3 <= S'Last loop
         V (1) := Decode_Char (S (I));
         V (2) := Decode_Char (S (I + 1));
         V (3) := Decode_Char (S (I + 2));
         V (4) := Decode_Char (S (I + 3));

         R_Idx := R_Idx + 1;
         Result (R_Idx) := Character'Val ((V (1) * 4) + (V (2) / 16));

         if S (I + 2) /= '=' then
            R_Idx := R_Idx + 1;
            Result (R_Idx) := Character'Val (((V (2) mod 16) * 16) + (V (3) / 4));
         end if;

         if S (I + 3) /= '=' then
            R_Idx := R_Idx + 1;
            Result (R_Idx) := Character'Val (((V (3) mod 4) * 64) + V (4));
         end if;

         I := I + 4;
      end loop;

      return Result (1 .. R_Idx);
   end Decode_Base64;

   ----------------
   -- Decode_URL --
   ----------------

   function Decode_URL (S : String) return String is
      Result : String (1 .. S'Length);
      R_Idx  : Natural := 0;
      I      : Positive := S'First;
   begin
      while I <= S'Last loop
         if S (I) = '%' and I + 2 <= S'Last then
            R_Idx := R_Idx + 1;
            Result (R_Idx) := Character'Val (
              Hex_Value (S (I + 1)) * 16 + Hex_Value (S (I + 2)));
            I := I + 3;
         elsif S (I) = '+' then
            R_Idx := R_Idx + 1;
            Result (R_Idx) := ' ';
            I := I + 1;
         else
            R_Idx := R_Idx + 1;
            Result (R_Idx) := S (I);
            I := I + 1;
         end if;
      end loop;
      return Result (1 .. R_Idx);
   end Decode_URL;

   -------------------
   -- Decode_Escape --
   -------------------

   function Decode_Escape (S : String) return String is
      Result : String (1 .. S'Length);
      R_Idx  : Natural := 0;
      I      : Positive := S'First;
   begin
      while I <= S'Last loop
         if S (I) = '\' and I < S'Last then
            I := I + 1;
            R_Idx := R_Idx + 1;
            case S (I) is
               when 'n' => Result (R_Idx) := ASCII.LF;
               when 'r' => Result (R_Idx) := ASCII.CR;
               when 't' => Result (R_Idx) := ASCII.HT;
               when '0' => Result (R_Idx) := ASCII.NUL;
               when '\' => Result (R_Idx) := '\';
               when '"' => Result (R_Idx) := '"';
               when ''' => Result (R_Idx) := ''';
               when others => Result (R_Idx) := S (I);
            end case;
         else
            R_Idx := R_Idx + 1;
            Result (R_Idx) := S (I);
         end if;
         I := I + 1;
      end loop;
      return Result (1 .. R_Idx);
   end Decode_Escape;

end GNAT.Decode_String;
