-- System.Wch_Cnv for Z80
-- Wide character encoding/decoding conversions

package System.Wch_Cnv is
   pragma Pure;

   -- Wide character encoding methods
   type WC_Encoding_Method is
     (WCEM_Hex,       -- Hex escape sequences
      WCEM_Upper,     -- Upper half encoding
      WCEM_Shift_JIS, -- Shift-JIS encoding
      WCEM_EUC,       -- EUC encoding
      WCEM_UTF8,      -- UTF-8 encoding
      WCEM_Brackets); -- Bracket notation ["xxxx"]

   -- Convert Wide_Character to UTF-8 sequence
   procedure Encode_Wide_Character
     (Char   : Wide_Character;
      Method : WC_Encoding_Method;
      Buffer : out String;
      Length : out Natural);
   -- Encodes Char into Buffer using specified method
   -- Length returns number of characters written

   -- Convert UTF-8 sequence to Wide_Character
   procedure Decode_Wide_Character
     (Buffer : String;
      Index  : in out Natural;
      Method : WC_Encoding_Method;
      Char   : out Wide_Character);
   -- Decodes a character from Buffer starting at Index
   -- Index is advanced past the decoded character

   -- Check if character starts a multi-byte sequence
   function Is_Start_Of_Wide_Char
     (C      : Character;
      Method : WC_Encoding_Method) return Boolean;

   -- Get length of multi-byte sequence
   function Sequence_Length
     (C      : Character;
      Method : WC_Encoding_Method) return Natural;
   -- Returns 1 for single-byte characters

end System.Wch_Cnv;
