-- GNAT.Encoding for Z80
-- Character encoding utilities (Base64, Hex, URL encoding)

package GNAT.Encoding is
   pragma Pure;

   Max_Input_Length  : constant := 48;  -- Max input for Z80
   Max_Output_Length : constant := 72;  -- 4/3 of max input for Base64

   subtype Encoded_String is String (1 .. Max_Output_Length);
   subtype Decoded_String is String (1 .. Max_Input_Length);

   -- Base64 encoding/decoding
   procedure Base64_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural);

   procedure Base64_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural);

   function Base64_Encoded_Length (Input_Length : Natural) return Natural;
   function Base64_Decoded_Length (Input_Length : Natural) return Natural;

   -- Hexadecimal encoding/decoding
   procedure Hex_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural);

   procedure Hex_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural);

   function Byte_To_Hex (B : Natural) return String;  -- 2 chars
   function Hex_To_Byte (S : String) return Natural;

   -- URL encoding/decoding (percent encoding)
   procedure URL_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural);

   procedure URL_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural);

   -- Character classification for URL encoding
   function Is_URL_Safe (C : Character) return Boolean;

   -- ROT13 encoding (simple obfuscation)
   function ROT13 (Input : String) return String;

   -- XOR encoding with key
   procedure XOR_Encode
     (Input  : String;
      Key    : String;
      Output : out Decoded_String;
      Length : out Natural);

   -- Simple run-length encoding
   procedure RLE_Encode
     (Input  : String;
      Output : out Encoded_String;
      Length : out Natural);

   procedure RLE_Decode
     (Input  : String;
      Output : out Decoded_String;
      Length : out Natural);

end GNAT.Encoding;
