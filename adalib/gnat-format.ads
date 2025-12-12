-- GNAT.Format for Z80
-- String formatting utilities

package GNAT.Format is
   pragma Pure;

   Max_Format_Length : constant := 80;

   subtype Format_String is String (1 .. Max_Format_Length);

   -- Integer formatting
   function Int_Image (Value : Integer) return String;
   function Int_Image (Value : Integer; Width : Positive) return String;
   function Int_Image_Padded (Value : Integer; Width : Positive;
                              Pad : Character := '0') return String;

   -- Unsigned formatting (for bytes, addresses)
   function Unsigned_Image (Value : Natural) return String;
   function Unsigned_Image (Value : Natural; Base : Positive) return String;

   -- Hexadecimal formatting
   function Hex_Image (Value : Natural) return String;
   function Hex_Image (Value : Natural; Width : Positive) return String;

   -- Binary formatting
   function Binary_Image (Value : Natural) return String;
   function Binary_Image (Value : Natural; Width : Positive) return String;

   -- Octal formatting
   function Octal_Image (Value : Natural) return String;

   -- Boolean formatting
   function Bool_Image (Value : Boolean) return String;
   function Bool_Yes_No (Value : Boolean) return String;
   function Bool_On_Off (Value : Boolean) return String;

   -- String padding and alignment
   function Pad_Left (S : String; Width : Positive;
                      Pad : Character := ' ') return String;
   function Pad_Right (S : String; Width : Positive;
                       Pad : Character := ' ') return String;
   function Center (S : String; Width : Positive;
                    Pad : Character := ' ') return String;

   -- String truncation
   function Truncate_Left (S : String; Max_Width : Positive) return String;
   function Truncate_Right (S : String; Max_Width : Positive) return String;
   function Truncate_Middle (S : String; Max_Width : Positive) return String;

   -- Fixed-point formatting (8.8 format)
   function Fixed_Image (Value : Integer) return String;  -- Value is 8.8 scaled
   function Fixed_Image (Value : Integer; Decimals : Natural) return String;

   -- Percentage formatting
   function Percent_Image (Value : Natural) return String;  -- 0-100
   function Percent_Image (Num, Denom : Natural) return String;

   -- Currency-like formatting (cents)
   function Currency_Image (Cents : Integer) return String;

   -- Time formatting (seconds)
   function Time_HMS (Seconds : Natural) return String;  -- HH:MM:SS
   function Time_MS (Seconds : Natural) return String;   -- MM:SS
   function Duration_Image (Seconds : Natural) return String;  -- "5h 30m"

   -- Size formatting (bytes)
   function Size_Image (Bytes : Natural) return String;  -- "1.5 KB"

   -- Ordinal suffix
   function Ordinal (N : Positive) return String;  -- "1st", "2nd", "3rd"

   -- Repeat character
   function Repeat_Char (C : Character; Count : Natural) return String;

   -- Join strings with separator
   function Join (S1, S2 : String; Sep : String := ", ") return String;
   function Join (S1, S2, S3 : String; Sep : String := ", ") return String;

   -- Quote string
   function Quote (S : String) return String;
   function Single_Quote (S : String) return String;

   -- Bracket string
   function Bracket (S : String) return String;
   function Paren (S : String) return String;
   function Brace (S : String) return String;

end GNAT.Format;
