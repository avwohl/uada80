-- GNAT.Tokenizer for Z80
-- Simple string tokenizer

package GNAT.Tokenizer is
   pragma Preelaborate;

   Max_Token_Length : constant := 128;
   Max_Tokens       : constant := 32;

   type Token_Array is array (1 .. Max_Tokens) of String (1 .. Max_Token_Length);
   type Token_Lengths is array (1 .. Max_Tokens) of Natural;

   type Tokenizer is limited private;

   procedure Initialize
     (T          : out Tokenizer;
      Source     : String;
      Delimiters : String := " " & ASCII.HT);
   --  Initialize tokenizer with source string and delimiters

   function Has_More_Tokens (T : Tokenizer) return Boolean;
   --  Check if more tokens available

   procedure Next_Token
     (T     : in Out Tokenizer;
      Token : out String;
      Last  : out Natural);
   --  Get next token

   function Token_Count (T : Tokenizer) return Natural;
   --  Return total number of tokens

   procedure Split
     (Source     : String;
      Delimiters : String;
      Tokens     : out Token_Array;
      Lengths    : out Token_Lengths;
      Count      : out Natural);
   --  Split string into tokens

private

   type Tokenizer is record
      Source     : String (1 .. 256);
      Source_Len : Natural := 0;
      Delims     : String (1 .. 32);
      Delim_Len  : Natural := 0;
      Position   : Positive := 1;
   end record;

end GNAT.Tokenizer;
