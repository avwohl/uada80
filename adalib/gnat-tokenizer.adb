-- GNAT.Tokenizer body for Z80
-- Simple string tokenizer implementation

package body GNAT.Tokenizer is

   function Is_Delimiter (T : Tokenizer; C : Character) return Boolean is
   begin
      for I in 1 .. T.Delim_Len loop
         if T.Delims (I) = C then
            return True;
         end if;
      end loop;
      return False;
   end Is_Delimiter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (T          : out Tokenizer;
      Source     : String;
      Delimiters : String := " " & ASCII.HT)
   is
      Len : Natural;
   begin
      Len := Natural'Min (Source'Length, T.Source'Length);
      T.Source (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      T.Source_Len := Len;

      Len := Natural'Min (Delimiters'Length, T.Delims'Length);
      T.Delims (1 .. Len) := Delimiters (Delimiters'First .. Delimiters'First + Len - 1);
      T.Delim_Len := Len;

      T.Position := 1;
   end Initialize;

   ---------------------
   -- Has_More_Tokens --
   ---------------------

   function Has_More_Tokens (T : Tokenizer) return Boolean is
   begin
      -- Skip delimiters
      for I in T.Position .. T.Source_Len loop
         if not Is_Delimiter (T, T.Source (I)) then
            return True;
         end if;
      end loop;
      return False;
   end Has_More_Tokens;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (T     : in Out Tokenizer;
      Token : out String;
      Last  : out Natural)
   is
      Start : Natural := 0;
   begin
      Last := 0;

      -- Skip leading delimiters
      while T.Position <= T.Source_Len and then
            Is_Delimiter (T, T.Source (T.Position))
      loop
         T.Position := T.Position + 1;
      end loop;

      if T.Position > T.Source_Len then
         return;
      end if;

      Start := T.Position;

      -- Find end of token
      while T.Position <= T.Source_Len and then
            not Is_Delimiter (T, T.Source (T.Position))
      loop
         T.Position := T.Position + 1;
      end loop;

      -- Copy token
      declare
         Tok_Len : constant Natural := T.Position - Start;
         Copy_Len : constant Natural := Natural'Min (Tok_Len, Token'Length);
      begin
         Token (Token'First .. Token'First + Copy_Len - 1) :=
           T.Source (Start .. Start + Copy_Len - 1);
         Last := Token'First + Copy_Len - 1;
      end;
   end Next_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (T : Tokenizer) return Natural is
      Count    : Natural := 0;
      In_Token : Boolean := False;
   begin
      for I in 1 .. T.Source_Len loop
         if Is_Delimiter (T, T.Source (I)) then
            In_Token := False;
         elsif not In_Token then
            In_Token := True;
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Token_Count;

   -----------
   -- Split --
   -----------

   procedure Split
     (Source     : String;
      Delimiters : String;
      Tokens     : out Token_Array;
      Lengths    : out Token_Lengths;
      Count      : out Natural)
   is
      T : Tokenizer;
   begin
      Initialize (T, Source, Delimiters);
      Count := 0;
      Lengths := (others => 0);

      while Has_More_Tokens (T) and Count < Max_Tokens loop
         Count := Count + 1;
         Next_Token (T, Tokens (Count), Lengths (Count));
      end loop;
   end Split;

end GNAT.Tokenizer;
