-- GNAT.Encode_UTF8_String body for Z80
-- Wide_String to UTF-8 encoding implementation

package body GNAT.Encode_UTF8_String is

   ------------------------
   -- Encode_Wide_String --
   ------------------------

   function Encode_Wide_String (S : Wide_String) return String is
      Result : String (1 .. S'Length * 3);  -- Max 3 bytes per char
      Len    : Natural := 0;
      Code   : Natural;
   begin
      for I in S'Range loop
         Code := Wide_Character'Pos (S (I));

         if Code < 128 then
            Len := Len + 1;
            Result (Len) := Character'Val (Code);
         elsif Code < 2048 then
            -- 2-byte sequence
            Len := Len + 1;
            Result (Len) := Character'Val (192 + Code / 64);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + Code mod 64);
         else
            -- 3-byte sequence
            Len := Len + 1;
            Result (Len) := Character'Val (224 + Code / 4096);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + (Code / 64) mod 64);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + Code mod 64);
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode_Wide_String;

   -----------------------------
   -- Encode_Wide_Wide_String --
   -----------------------------

   function Encode_Wide_Wide_String (S : Wide_Wide_String) return String is
      Result : String (1 .. S'Length * 4);  -- Max 4 bytes per char
      Len    : Natural := 0;
      Code   : Natural;
   begin
      for I in S'Range loop
         Code := Wide_Wide_Character'Pos (S (I));

         if Code < 128 then
            Len := Len + 1;
            Result (Len) := Character'Val (Code);
         elsif Code < 2048 then
            Len := Len + 1;
            Result (Len) := Character'Val (192 + Code / 64);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + Code mod 64);
         elsif Code < 65536 then
            Len := Len + 1;
            Result (Len) := Character'Val (224 + Code / 4096);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + (Code / 64) mod 64);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + Code mod 64);
         else
            -- 4-byte sequence
            Len := Len + 1;
            Result (Len) := Character'Val (240 + Code / 262144);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + (Code / 4096) mod 64);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + (Code / 64) mod 64);
            Len := Len + 1;
            Result (Len) := Character'Val (128 + Code mod 64);
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode_Wide_Wide_String;

end GNAT.Encode_UTF8_String;
