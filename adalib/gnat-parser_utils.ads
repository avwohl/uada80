-- GNAT.Parser_Utils for Z80
-- Simple parsing utilities for text processing

package GNAT.Parser_Utils is
   pragma Pure;

   Max_Token_Length : constant := 32;
   Max_Tokens       : constant := 16;

   subtype Token_String is String (1 .. Max_Token_Length);

   type Token is record
      Data   : Token_String;
      Length : Natural;
      Kind   : Natural;  -- User-defined token type
   end record;

   type Token_Array is array (1 .. Max_Tokens) of Token;

   -- Parser state
   type Parser is limited private;

   -- Initialize parser with input string
   procedure Initialize (P : out Parser; Input : String);

   -- Current position
   function Position (P : Parser) return Natural;
   function At_End (P : Parser) return Boolean;
   function Remaining (P : Parser) return Natural;

   -- Peek at current character without advancing
   function Peek (P : Parser) return Character;
   function Peek_Ahead (P : Parser; Offset : Positive := 1) return Character;

   -- Get current character and advance
   function Get (P : in Out Parser) return Character;

   -- Skip characters
   procedure Skip (P : in Out Parser; Count : Positive := 1);
   procedure Skip_While (P : in Out Parser; C : Character);
   procedure Skip_Whitespace (P : in Out Parser);
   procedure Skip_To (P : in Out Parser; C : Character);
   procedure Skip_Line (P : in Out Parser);

   -- Match operations
   function Match (P : in Out Parser; C : Character) return Boolean;
   function Match (P : in Out Parser; S : String) return Boolean;
   function Match_Any (P : in Out Parser; Chars : String) return Boolean;
   function Match_Digit (P : in Out Parser) return Boolean;
   function Match_Alpha (P : in Out Parser) return Boolean;
   function Match_Alnum (P : in Out Parser) return Boolean;

   -- Expect (raises Constraint_Error if not matched)
   procedure Expect (P : in Out Parser; C : Character);
   procedure Expect (P : in Out Parser; S : String);

   -- Character tests (without advancing)
   function Is_Digit (P : Parser) return Boolean;
   function Is_Alpha (P : Parser) return Boolean;
   function Is_Alnum (P : Parser) return Boolean;
   function Is_Whitespace (P : Parser) return Boolean;
   function Is_Line_End (P : Parser) return Boolean;

   -- Read tokens
   function Read_Word (P : in Out Parser) return String;
   function Read_Number (P : in Out Parser) return Integer;
   function Read_Identifier (P : in Out Parser) return String;
   function Read_Quoted (P : in Out Parser; Quote : Character := '"') return String;
   function Read_Until (P : in Out Parser; Delimiter : Character) return String;
   function Read_Line (P : in Out Parser) return String;

   -- Tokenize entire string
   procedure Tokenize
     (Input     : String;
      Delimiter : Character;
      Tokens    : out Token_Array;
      Count     : out Natural);

   -- Parse delimited list
   procedure Parse_CSV
     (Input  : String;
      Tokens : out Token_Array;
      Count  : out Natural);

   -- Parse key=value pairs
   type KV_Pair is record
      Key   : Token_String;
      Key_Len : Natural;
      Value : Token_String;
      Val_Len : Natural;
   end record;

   type KV_Array is array (1 .. Max_Tokens) of KV_Pair;

   procedure Parse_Key_Values
     (Input  : String;
      Pairs  : out KV_Array;
      Count  : out Natural;
      Sep    : Character := '=';
      Delim  : Character := ',');

   -- Number parsing
   function Parse_Integer (S : String) return Integer;
   function Parse_Hex (S : String) return Natural;
   function Parse_Binary (S : String) return Natural;
   function Parse_Octal (S : String) return Natural;

   -- Boolean parsing
   function Parse_Boolean (S : String) return Boolean;

   -- Validation
   function Is_Valid_Integer (S : String) return Boolean;
   function Is_Valid_Identifier (S : String) return Boolean;

   -- String utilities
   function Trim (S : String) return String;
   function Trim_Left (S : String) return String;
   function Trim_Right (S : String) return String;

   -- Token to string
   function To_String (T : Token) return String;

private

   type Parser is limited record
      Buffer : String (1 .. 256);
      Length : Natural := 0;
      Pos    : Natural := 1;
   end record;

end GNAT.Parser_Utils;
