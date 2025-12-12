-- GNAT.Encode_String body for Z80
-- String encoding implementation

package body GNAT.Encode_String is

   Hex_Digits : constant String := "0123456789ABCDEF";
   Base64_Chars : constant String :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   ----------------
   -- Encode_Hex --
   ----------------

   function Encode_Hex (S : String) return String is
      Result : String (1 .. S'Length * 2);
      R_Idx  : Natural := 0;
      B      : Natural;
   begin
      for C of S loop
         B := Character'Pos (C);
         R_Idx := R_Idx + 1;
         Result (R_Idx) := Hex_Digits (B / 16 + 1);
         R_Idx := R_Idx + 1;
         Result (R_Idx) := Hex_Digits (B mod 16 + 1);
      end loop;
      return Result;
   end Encode_Hex;

   -------------------
   -- Encode_Base64 --
   -------------------

   function Encode_Base64 (S : String) return String is
      Result : String (1 .. ((S'Length + 2) / 3) * 4);
      R_Idx  : Natural := 0;
      I      : Positive := S'First;
      B1, B2, B3 : Natural;
   begin
      while I <= S'Last loop
         B1 := Character'Pos (S (I));
         if I + 1 <= S'Last then
            B2 := Character'Pos (S (I + 1));
         else
            B2 := 0;
         end if;
         if I + 2 <= S'Last then
            B3 := Character'Pos (S (I + 2));
         else
            B3 := 0;
         end if;

         R_Idx := R_Idx + 1;
         Result (R_Idx) := Base64_Chars (B1 / 4 + 1);

         R_Idx := R_Idx + 1;
         Result (R_Idx) := Base64_Chars (((B1 mod 4) * 16) + (B2 / 16) + 1);

         R_Idx := R_Idx + 1;
         if I + 1 <= S'Last then
            Result (R_Idx) := Base64_Chars (((B2 mod 16) * 4) + (B3 / 64) + 1);
         else
            Result (R_Idx) := '=';
         end if;

         R_Idx := R_Idx + 1;
         if I + 2 <= S'Last then
            Result (R_Idx) := Base64_Chars (B3 mod 64 + 1);
         else
            Result (R_Idx) := '=';
         end if;

         I := I + 3;
      end loop;

      return Result (1 .. R_Idx);
   end Encode_Base64;

   ----------------
   -- Encode_URL --
   ----------------

   function Encode_URL (S : String) return String is
      Result : String (1 .. S'Length * 3);
      R_Idx  : Natural := 0;
      B      : Natural;
   begin
      for C of S loop
         if C in 'A' .. 'Z' or C in 'a' .. 'z' or C in '0' .. '9'
           or C = '-' or C = '_' or C = '.' or C = '~'
         then
            R_Idx := R_Idx + 1;
            Result (R_Idx) := C;
         elsif C = ' ' then
            R_Idx := R_Idx + 1;
            Result (R_Idx) := '+';
         else
            B := Character'Pos (C);
            R_Idx := R_Idx + 1;
            Result (R_Idx) := '%';
            R_Idx := R_Idx + 1;
            Result (R_Idx) := Hex_Digits (B / 16 + 1);
            R_Idx := R_Idx + 1;
            Result (R_Idx) := Hex_Digits (B mod 16 + 1);
         end if;
      end loop;
      return Result (1 .. R_Idx);
   end Encode_URL;

   -------------------
   -- Encode_Escape --
   -------------------

   function Encode_Escape (S : String) return String is
      Result : String (1 .. S'Length * 2);
      R_Idx  : Natural := 0;
   begin
      for C of S loop
         case C is
            when ASCII.LF =>
               R_Idx := R_Idx + 1; Result (R_Idx) := '\';
               R_Idx := R_Idx + 1; Result (R_Idx) := 'n';
            when ASCII.CR =>
               R_Idx := R_Idx + 1; Result (R_Idx) := '\';
               R_Idx := R_Idx + 1; Result (R_Idx) := 'r';
            when ASCII.HT =>
               R_Idx := R_Idx + 1; Result (R_Idx) := '\';
               R_Idx := R_Idx + 1; Result (R_Idx) := 't';
            when '\' =>
               R_Idx := R_Idx + 1; Result (R_Idx) := '\';
               R_Idx := R_Idx + 1; Result (R_Idx) := '\';
            when '"' =>
               R_Idx := R_Idx + 1; Result (R_Idx) := '\';
               R_Idx := R_Idx + 1; Result (R_Idx) := '"';
            when others =>
               R_Idx := R_Idx + 1;
               Result (R_Idx) := C;
         end case;
      end loop;
      return Result (1 .. R_Idx);
   end Encode_Escape;

end GNAT.Encode_String;
