-- System.Wch_Cnv body for Z80
-- Wide character encoding/decoding conversions

package body System.Wch_Cnv is

   Hex_Digits : constant String := "0123456789ABCDEF";

   ----------------------------
   -- Encode_Wide_Character --
   ----------------------------

   procedure Encode_Wide_Character
     (Char   : Wide_Character;
      Method : WC_Encoding_Method;
      Buffer : out String;
      Length : out Natural)
   is
      Code : constant Natural := Wide_Character'Pos (Char);
   begin
      case Method is
         when WCEM_UTF8 =>
            if Code <= 16#7F# then
               -- Single byte
               Buffer (Buffer'First) := Character'Val (Code);
               Length := 1;
            elsif Code <= 16#7FF# then
               -- Two bytes
               Buffer (Buffer'First) :=
                 Character'Val (16#C0# + Code / 64);
               Buffer (Buffer'First + 1) :=
                 Character'Val (16#80# + Code mod 64);
               Length := 2;
            else
               -- Three bytes
               Buffer (Buffer'First) :=
                 Character'Val (16#E0# + Code / 4096);
               Buffer (Buffer'First + 1) :=
                 Character'Val (16#80# + (Code / 64) mod 64);
               Buffer (Buffer'First + 2) :=
                 Character'Val (16#80# + Code mod 64);
               Length := 3;
            end if;

         when WCEM_Hex =>
            -- ESC + 4 hex digits
            Buffer (Buffer'First) := ASCII.ESC;
            Buffer (Buffer'First + 1) := Hex_Digits ((Code / 4096) + 1);
            Buffer (Buffer'First + 2) := Hex_Digits (((Code / 256) mod 16) + 1);
            Buffer (Buffer'First + 3) := Hex_Digits (((Code / 16) mod 16) + 1);
            Buffer (Buffer'First + 4) := Hex_Digits ((Code mod 16) + 1);
            Length := 5;

         when WCEM_Brackets =>
            -- ["xxxx"]
            Buffer (Buffer'First) := '[';
            Buffer (Buffer'First + 1) := '"';
            Buffer (Buffer'First + 2) := Hex_Digits ((Code / 4096) + 1);
            Buffer (Buffer'First + 3) := Hex_Digits (((Code / 256) mod 16) + 1);
            Buffer (Buffer'First + 4) := Hex_Digits (((Code / 16) mod 16) + 1);
            Buffer (Buffer'First + 5) := Hex_Digits ((Code mod 16) + 1);
            Buffer (Buffer'First + 6) := '"';
            Buffer (Buffer'First + 7) := ']';
            Length := 8;

         when WCEM_Upper =>
            if Code <= 255 then
               Buffer (Buffer'First) := Character'Val (Code);
               Length := 1;
            else
               -- Fall back to hex for non-Latin1
               Buffer (Buffer'First) := ASCII.ESC;
               Buffer (Buffer'First + 1) := Hex_Digits ((Code / 4096) + 1);
               Buffer (Buffer'First + 2) := Hex_Digits (((Code / 256) mod 16) + 1);
               Buffer (Buffer'First + 3) := Hex_Digits (((Code / 16) mod 16) + 1);
               Buffer (Buffer'First + 4) := Hex_Digits ((Code mod 16) + 1);
               Length := 5;
            end if;

         when others =>
            -- For unsupported encodings, use hex
            Buffer (Buffer'First) := ASCII.ESC;
            Buffer (Buffer'First + 1) := Hex_Digits ((Code / 4096) + 1);
            Buffer (Buffer'First + 2) := Hex_Digits (((Code / 256) mod 16) + 1);
            Buffer (Buffer'First + 3) := Hex_Digits (((Code / 16) mod 16) + 1);
            Buffer (Buffer'First + 4) := Hex_Digits ((Code mod 16) + 1);
            Length := 5;
      end case;
   end Encode_Wide_Character;

   ----------------------------
   -- Decode_Wide_Character --
   ----------------------------

   procedure Decode_Wide_Character
     (Buffer : String;
      Index  : in Out Natural;
      Method : WC_Encoding_Method;
      Char   : out Wide_Character)
   is
      function Hex_Value (C : Character) return Natural is
      begin
         case C is
            when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
            when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
            when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
            when others => return 0;
         end case;
      end Hex_Value;

      C     : Character;
      Code  : Natural;
   begin
      C := Buffer (Index);

      case Method is
         when WCEM_UTF8 =>
            Code := Character'Pos (C);
            if Code <= 16#7F# then
               -- Single byte
               Char := Wide_Character'Val (Code);
               Index := Index + 1;
            elsif Code <= 16#DF# then
               -- Two bytes
               Code := (Code - 16#C0#) * 64 +
                       (Character'Pos (Buffer (Index + 1)) - 16#80#);
               Char := Wide_Character'Val (Code);
               Index := Index + 2;
            else
               -- Three bytes
               Code := (Code - 16#E0#) * 4096 +
                       (Character'Pos (Buffer (Index + 1)) - 16#80#) * 64 +
                       (Character'Pos (Buffer (Index + 2)) - 16#80#);
               Char := Wide_Character'Val (Code);
               Index := Index + 3;
            end if;

         when WCEM_Hex =>
            if C = ASCII.ESC then
               Code := Hex_Value (Buffer (Index + 1)) * 4096 +
                       Hex_Value (Buffer (Index + 2)) * 256 +
                       Hex_Value (Buffer (Index + 3)) * 16 +
                       Hex_Value (Buffer (Index + 4));
               Char := Wide_Character'Val (Code);
               Index := Index + 5;
            else
               Char := Wide_Character'Val (Character'Pos (C));
               Index := Index + 1;
            end if;

         when WCEM_Brackets =>
            if C = '[' and Buffer (Index + 1) = '"' then
               Code := Hex_Value (Buffer (Index + 2)) * 4096 +
                       Hex_Value (Buffer (Index + 3)) * 256 +
                       Hex_Value (Buffer (Index + 4)) * 16 +
                       Hex_Value (Buffer (Index + 5));
               Char := Wide_Character'Val (Code);
               Index := Index + 8;
            else
               Char := Wide_Character'Val (Character'Pos (C));
               Index := Index + 1;
            end if;

         when others =>
            -- Simple single-byte decode
            Char := Wide_Character'Val (Character'Pos (C));
            Index := Index + 1;
      end case;
   end Decode_Wide_Character;

   --------------------------
   -- Is_Start_Of_Wide_Char --
   --------------------------

   function Is_Start_Of_Wide_Char
     (C      : Character;
      Method : WC_Encoding_Method) return Boolean
   is
      Code : constant Natural := Character'Pos (C);
   begin
      case Method is
         when WCEM_UTF8 =>
            -- UTF-8 continuation bytes start with 10xxxxxx
            return Code < 16#80# or Code >= 16#C0#;

         when WCEM_Hex =>
            return C = ASCII.ESC;

         when WCEM_Brackets =>
            return C = '[';

         when others =>
            return Code >= 16#80#;
      end case;
   end Is_Start_Of_Wide_Char;

   ---------------------
   -- Sequence_Length --
   ---------------------

   function Sequence_Length
     (C      : Character;
      Method : WC_Encoding_Method) return Natural
   is
      Code : constant Natural := Character'Pos (C);
   begin
      case Method is
         when WCEM_UTF8 =>
            if Code <= 16#7F# then
               return 1;
            elsif Code <= 16#DF# then
               return 2;
            elsif Code <= 16#EF# then
               return 3;
            else
               return 4;
            end if;

         when WCEM_Hex =>
            if C = ASCII.ESC then
               return 5;
            else
               return 1;
            end if;

         when WCEM_Brackets =>
            if C = '[' then
               return 8;
            else
               return 1;
            end if;

         when others =>
            if Code >= 16#80# then
               return 2;
            else
               return 1;
            end if;
      end case;
   end Sequence_Length;

end System.Wch_Cnv;
