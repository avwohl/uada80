-- GNAT.Encoding body for Z80
-- Character encoding implementation

package body GNAT.Encoding is

   Base64_Chars : constant String :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   Hex_Chars : constant String := "0123456789ABCDEF";

   -------------------
   -- Base64_Encode --
   -------------------

   procedure Base64_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural)
   is
      I   : Natural := Input'First;
      O   : Natural := 1;
      B1, B2, B3 : Natural;
   begin
      Length := 0;
      Output := (others => ' ');

      while I <= Input'Last loop
         -- Get up to 3 bytes
         B1 := Character'Pos (Input (I));
         B2 := 0;
         B3 := 0;

         if I + 1 <= Input'Last then
            B2 := Character'Pos (Input (I + 1));
         end if;

         if I + 2 <= Input'Last then
            B3 := Character'Pos (Input (I + 2));
         end if;

         -- Encode to 4 characters
         if O <= Max_Output_Length then
            Output (O) := Base64_Chars ((B1 / 4) + 1);
            O := O + 1;
         end if;

         if O <= Max_Output_Length then
            Output (O) := Base64_Chars (((B1 mod 4) * 16 + B2 / 16) + 1);
            O := O + 1;
         end if;

         if I + 1 <= Input'Last then
            if O <= Max_Output_Length then
               Output (O) := Base64_Chars (((B2 mod 16) * 4 + B3 / 64) + 1);
               O := O + 1;
            end if;
         else
            if O <= Max_Output_Length then
               Output (O) := '=';
               O := O + 1;
            end if;
         end if;

         if I + 2 <= Input'Last then
            if O <= Max_Output_Length then
               Output (O) := Base64_Chars ((B3 mod 64) + 1);
               O := O + 1;
            end if;
         else
            if O <= Max_Output_Length then
               Output (O) := '=';
               O := O + 1;
            end if;
         end if;

         I := I + 3;
      end loop;

      Length := O - 1;
   end Base64_Encode;

   -- Helper to decode Base64 character
   function Base64_Value (C : Character) return Natural is
   begin
      if C >= 'A' and C <= 'Z' then
         return Character'Pos (C) - Character'Pos ('A');
      elsif C >= 'a' and C <= 'z' then
         return Character'Pos (C) - Character'Pos ('a') + 26;
      elsif C >= '0' and C <= '9' then
         return Character'Pos (C) - Character'Pos ('0') + 52;
      elsif C = '+' then
         return 62;
      elsif C = '/' then
         return 63;
      else
         return 0;
      end if;
   end Base64_Value;

   -------------------
   -- Base64_Decode --
   -------------------

   procedure Base64_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural)
   is
      I   : Natural := Input'First;
      O   : Natural := 1;
      V1, V2, V3, V4 : Natural;
   begin
      Length := 0;
      Output := (others => Character'Val (0));

      while I + 3 <= Input'Last loop
         V1 := Base64_Value (Input (I));
         V2 := Base64_Value (Input (I + 1));
         V3 := Base64_Value (Input (I + 2));
         V4 := Base64_Value (Input (I + 3));

         if O <= Max_Input_Length then
            Output (O) := Character'Val (V1 * 4 + V2 / 16);
            O := O + 1;
         end if;

         if Input (I + 2) /= '=' and O <= Max_Input_Length then
            Output (O) := Character'Val ((V2 mod 16) * 16 + V3 / 4);
            O := O + 1;
         end if;

         if Input (I + 3) /= '=' and O <= Max_Input_Length then
            Output (O) := Character'Val ((V3 mod 4) * 64 + V4);
            O := O + 1;
         end if;

         I := I + 4;
      end loop;

      Length := O - 1;
   end Base64_Decode;

   ---------------------------
   -- Base64_Encoded_Length --
   ---------------------------

   function Base64_Encoded_Length (Input_Length : Natural) return Natural is
   begin
      return ((Input_Length + 2) / 3) * 4;
   end Base64_Encoded_Length;

   ---------------------------
   -- Base64_Decoded_Length --
   ---------------------------

   function Base64_Decoded_Length (Input_Length : Natural) return Natural is
   begin
      return (Input_Length / 4) * 3;
   end Base64_Decoded_Length;

   ----------------
   -- Hex_Encode --
   ----------------

   procedure Hex_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural)
   is
      O : Natural := 1;
      B : Natural;
   begin
      Output := (others => ' ');
      Length := 0;

      for I in Input'Range loop
         B := Character'Pos (Input (I));
         if O + 1 <= Max_Output_Length then
            Output (O) := Hex_Chars ((B / 16) + 1);
            Output (O + 1) := Hex_Chars ((B mod 16) + 1);
            O := O + 2;
         end if;
      end loop;

      Length := O - 1;
   end Hex_Encode;

   ----------------
   -- Hex_Decode --
   ----------------

   procedure Hex_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural)
   is
      function Hex_Digit (C : Character) return Natural is
      begin
         if C >= '0' and C <= '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C >= 'A' and C <= 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         elsif C >= 'a' and C <= 'f' then
            return Character'Pos (C) - Character'Pos ('a') + 10;
         else
            return 0;
         end if;
      end Hex_Digit;

      I : Natural := Input'First;
      O : Natural := 1;
   begin
      Output := (others => Character'Val (0));
      Length := 0;

      while I + 1 <= Input'Last and O <= Max_Input_Length loop
         Output (O) := Character'Val (Hex_Digit (Input (I)) * 16 +
                                      Hex_Digit (Input (I + 1)));
         I := I + 2;
         O := O + 1;
      end loop;

      Length := O - 1;
   end Hex_Decode;

   -----------------
   -- Byte_To_Hex --
   -----------------

   function Byte_To_Hex (B : Natural) return String is
   begin
      return Hex_Chars ((B / 16) + 1) & Hex_Chars ((B mod 16) + 1);
   end Byte_To_Hex;

   -----------------
   -- Hex_To_Byte --
   -----------------

   function Hex_To_Byte (S : String) return Natural is
      function Hex_Digit (C : Character) return Natural is
      begin
         if C >= '0' and C <= '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C >= 'A' and C <= 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         elsif C >= 'a' and C <= 'f' then
            return Character'Pos (C) - Character'Pos ('a') + 10;
         else
            return 0;
         end if;
      end Hex_Digit;
   begin
      if S'Length < 2 then
         return 0;
      end if;
      return Hex_Digit (S (S'First)) * 16 + Hex_Digit (S (S'First + 1));
   end Hex_To_Byte;

   -----------------
   -- Is_URL_Safe --
   -----------------

   function Is_URL_Safe (C : Character) return Boolean is
   begin
      return (C >= 'A' and C <= 'Z') or
             (C >= 'a' and C <= 'z') or
             (C >= '0' and C <= '9') or
             C = '-' or C = '_' or C = '.' or C = '~';
   end Is_URL_Safe;

   ----------------
   -- URL_Encode --
   ----------------

   procedure URL_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural)
   is
      O : Natural := 1;
      B : Natural;
   begin
      Output := (others => ' ');
      Length := 0;

      for I in Input'Range loop
         if Is_URL_Safe (Input (I)) then
            if O <= Max_Output_Length then
               Output (O) := Input (I);
               O := O + 1;
            end if;
         elsif Input (I) = ' ' then
            if O <= Max_Output_Length then
               Output (O) := '+';
               O := O + 1;
            end if;
         else
            if O + 2 <= Max_Output_Length then
               B := Character'Pos (Input (I));
               Output (O) := '%';
               Output (O + 1) := Hex_Chars ((B / 16) + 1);
               Output (O + 2) := Hex_Chars ((B mod 16) + 1);
               O := O + 3;
            end if;
         end if;
      end loop;

      Length := O - 1;
   end URL_Encode;

   ----------------
   -- URL_Decode --
   ----------------

   procedure URL_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural)
   is
      I : Natural := Input'First;
      O : Natural := 1;
   begin
      Output := (others => Character'Val (0));
      Length := 0;

      while I <= Input'Last and O <= Max_Input_Length loop
         if Input (I) = '%' and I + 2 <= Input'Last then
            Output (O) := Character'Val (Hex_To_Byte (Input (I + 1 .. I + 2)));
            I := I + 3;
         elsif Input (I) = '+' then
            Output (O) := ' ';
            I := I + 1;
         else
            Output (O) := Input (I);
            I := I + 1;
         end if;
         O := O + 1;
      end loop;

      Length := O - 1;
   end URL_Decode;

   -----------
   -- ROT13 --
   -----------

   function ROT13 (Input : String) return String is
      Result : String (Input'Range);
      C      : Character;
   begin
      for I in Input'Range loop
         C := Input (I);
         if C >= 'A' and C <= 'M' then
            Result (I) := Character'Val (Character'Pos (C) + 13);
         elsif C >= 'N' and C <= 'Z' then
            Result (I) := Character'Val (Character'Pos (C) - 13);
         elsif C >= 'a' and C <= 'm' then
            Result (I) := Character'Val (Character'Pos (C) + 13);
         elsif C >= 'n' and C <= 'z' then
            Result (I) := Character'Val (Character'Pos (C) - 13);
         else
            Result (I) := C;
         end if;
      end loop;
      return Result;
   end ROT13;

   ----------------
   -- XOR_Encode --
   ----------------

   procedure XOR_Encode
     (Input  : String;
      Key    : String;
      Output : out Decoded_String;
      Length : out Natural)
   is
      O   : Natural := 1;
      K   : Natural;
   begin
      Output := (others => Character'Val (0));
      Length := 0;

      if Key'Length = 0 then
         -- No key, just copy
         for I in Input'Range loop
            exit when O > Max_Input_Length;
            Output (O) := Input (I);
            O := O + 1;
         end loop;
      else
         K := Key'First;
         for I in Input'Range loop
            exit when O > Max_Input_Length;
            Output (O) := Character'Val (
              Character'Pos (Input (I)) xor Character'Pos (Key (K)));
            O := O + 1;
            K := K + 1;
            if K > Key'Last then
               K := Key'First;
            end if;
         end loop;
      end if;

      Length := O - 1;
   end XOR_Encode;

   ----------------
   -- RLE_Encode --
   ----------------

   procedure RLE_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural)
   is
      I     : Natural := Input'First;
      O     : Natural := 1;
      Count : Natural;
      C     : Character;
   begin
      Output := (others => ' ');
      Length := 0;

      while I <= Input'Last loop
         C := Input (I);
         Count := 1;

         -- Count consecutive characters
         while I + Count <= Input'Last and then
               Input (I + Count) = C and then
               Count < 9 loop
            Count := Count + 1;
         end loop;

         if O + 1 <= Max_Output_Length then
            if Count > 2 then
               -- Use run-length encoding
               Output (O) := Character'Val (Character'Pos ('0') + Count);
               Output (O + 1) := C;
               O := O + 2;
            else
               -- Just copy characters
               for J in 1 .. Count loop
                  exit when O > Max_Output_Length;
                  Output (O) := C;
                  O := O + 1;
               end loop;
            end if;
         end if;

         I := I + Count;
      end loop;

      Length := O - 1;
   end RLE_Encode;

   ----------------
   -- RLE_Decode --
   ----------------

   procedure RLE_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural)
   is
      I     : Natural := Input'First;
      O     : Natural := 1;
      Count : Natural;
   begin
      Output := (others => Character'Val (0));
      Length := 0;

      while I <= Input'Last loop
         if Input (I) >= '3' and Input (I) <= '9' and I + 1 <= Input'Last then
            Count := Character'Pos (Input (I)) - Character'Pos ('0');
            I := I + 1;
            for J in 1 .. Count loop
               exit when O > Max_Input_Length;
               Output (O) := Input (I);
               O := O + 1;
            end loop;
            I := I + 1;
         else
            if O <= Max_Input_Length then
               Output (O) := Input (I);
               O := O + 1;
            end if;
            I := I + 1;
         end if;
      end loop;

      Length := O - 1;
   end RLE_Decode;

end GNAT.Encoding;
