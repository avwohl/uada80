-- GNAT.Decode_UTF8_String body for Z80
-- UTF-8 to Wide_String decoding implementation

package body GNAT.Decode_UTF8_String is

   ------------------------
   -- Decode_Wide_String --
   ------------------------

   function Decode_Wide_String (S : String) return Wide_String is
      Result : Wide_String (1 .. S'Length);
      Len    : Natural := 0;
      I      : Natural := S'First;
      C      : Character;
      Code   : Natural;
   begin
      while I <= S'Last loop
         C := S (I);

         if Character'Pos (C) < 128 then
            -- ASCII
            Code := Character'Pos (C);
            I := I + 1;
         elsif Character'Pos (C) < 224 and then I + 1 <= S'Last then
            -- 2-byte sequence
            Code := (Character'Pos (C) mod 32) * 64 +
                    (Character'Pos (S (I + 1)) mod 64);
            I := I + 2;
         elsif Character'Pos (C) < 240 and then I + 2 <= S'Last then
            -- 3-byte sequence
            Code := (Character'Pos (C) mod 16) * 4096 +
                    (Character'Pos (S (I + 1)) mod 64) * 64 +
                    (Character'Pos (S (I + 2)) mod 64);
            I := I + 3;
         else
            -- Invalid or 4-byte (use replacement)
            Code := 16#FFFD#;
            I := I + 1;
         end if;

         Len := Len + 1;
         if Code <= 16#FFFF# then
            Result (Len) := Wide_Character'Val (Code);
         else
            Result (Len) := Wide_Character'Val (16#FFFD#);
         end if;
      end loop;

      return Result (1 .. Len);
   end Decode_Wide_String;

   -----------------------------
   -- Decode_Wide_Wide_String --
   -----------------------------

   function Decode_Wide_Wide_String (S : String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. S'Length);
      Len    : Natural := 0;
      I      : Natural := S'First;
      C      : Character;
      Code   : Natural;
   begin
      while I <= S'Last loop
         C := S (I);

         if Character'Pos (C) < 128 then
            Code := Character'Pos (C);
            I := I + 1;
         elsif Character'Pos (C) < 224 and then I + 1 <= S'Last then
            Code := (Character'Pos (C) mod 32) * 64 +
                    (Character'Pos (S (I + 1)) mod 64);
            I := I + 2;
         elsif Character'Pos (C) < 240 and then I + 2 <= S'Last then
            Code := (Character'Pos (C) mod 16) * 4096 +
                    (Character'Pos (S (I + 1)) mod 64) * 64 +
                    (Character'Pos (S (I + 2)) mod 64);
            I := I + 3;
         elsif Character'Pos (C) < 248 and then I + 3 <= S'Last then
            -- 4-byte sequence
            Code := (Character'Pos (C) mod 8) * 262144 +
                    (Character'Pos (S (I + 1)) mod 64) * 4096 +
                    (Character'Pos (S (I + 2)) mod 64) * 64 +
                    (Character'Pos (S (I + 3)) mod 64);
            I := I + 4;
         else
            Code := 16#FFFD#;
            I := I + 1;
         end if;

         Len := Len + 1;
         Result (Len) := Wide_Wide_Character'Val (Code);
      end loop;

      return Result (1 .. Len);
   end Decode_Wide_Wide_String;

end GNAT.Decode_UTF8_String;
