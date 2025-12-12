-- GNAT.Parser_Utils body for Z80
-- Parsing utilities implementation

package body GNAT.Parser_Utils is

   function Is_Digit_Char (C : Character) return Boolean is
   begin
      return C >= '0' and C <= '9';
   end Is_Digit_Char;

   function Is_Alpha_Char (C : Character) return Boolean is
   begin
      return (C >= 'A' and C <= 'Z') or (C >= 'a' and C <= 'z');
   end Is_Alpha_Char;

   function Is_Alnum_Char (C : Character) return Boolean is
   begin
      return Is_Alpha_Char (C) or Is_Digit_Char (C);
   end Is_Alnum_Char;

   function Is_Space_Char (C : Character) return Boolean is
   begin
      return C = ' ' or C = Character'Val (9) or
             C = Character'Val (10) or C = Character'Val (13);
   end Is_Space_Char;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (P : out Parser; Input : String) is
   begin
      P.Buffer := (others => ' ');
      P.Length := Input'Length;
      if P.Length > P.Buffer'Length then
         P.Length := P.Buffer'Length;
      end if;
      for I in 1 .. P.Length loop
         P.Buffer (I) := Input (Input'First + I - 1);
      end loop;
      P.Pos := 1;
   end Initialize;

   --------------
   -- Position --
   --------------

   function Position (P : Parser) return Natural is
   begin
      return P.Pos;
   end Position;

   ------------
   -- At_End --
   ------------

   function At_End (P : Parser) return Boolean is
   begin
      return P.Pos > P.Length;
   end At_End;

   ---------------
   -- Remaining --
   ---------------

   function Remaining (P : Parser) return Natural is
   begin
      if P.Pos > P.Length then
         return 0;
      else
         return P.Length - P.Pos + 1;
      end if;
   end Remaining;

   ----------
   -- Peek --
   ----------

   function Peek (P : Parser) return Character is
   begin
      if P.Pos > P.Length then
         return Character'Val (0);
      else
         return P.Buffer (P.Pos);
      end if;
   end Peek;

   ----------------
   -- Peek_Ahead --
   ----------------

   function Peek_Ahead (P : Parser; Offset : Positive := 1) return Character is
   begin
      if P.Pos + Offset - 1 > P.Length then
         return Character'Val (0);
      else
         return P.Buffer (P.Pos + Offset - 1);
      end if;
   end Peek_Ahead;

   ---------
   -- Get --
   ---------

   function Get (P : in Out Parser) return Character is
      C : Character;
   begin
      if P.Pos > P.Length then
         return Character'Val (0);
      else
         C := P.Buffer (P.Pos);
         P.Pos := P.Pos + 1;
         return C;
      end if;
   end Get;

   ----------
   -- Skip --
   ----------

   procedure Skip (P : in Out Parser; Count : Positive := 1) is
   begin
      P.Pos := P.Pos + Count;
      if P.Pos > P.Length + 1 then
         P.Pos := P.Length + 1;
      end if;
   end Skip;

   ----------------
   -- Skip_While --
   ----------------

   procedure Skip_While (P : in Out Parser; C : Character) is
   begin
      while not At_End (P) and then Peek (P) = C loop
         Skip (P);
      end loop;
   end Skip_While;

   ---------------------
   -- Skip_Whitespace --
   ---------------------

   procedure Skip_Whitespace (P : in Out Parser) is
   begin
      while not At_End (P) and then Is_Space_Char (Peek (P)) loop
         Skip (P);
      end loop;
   end Skip_Whitespace;

   -------------
   -- Skip_To --
   -------------

   procedure Skip_To (P : in Out Parser; C : Character) is
   begin
      while not At_End (P) and then Peek (P) /= C loop
         Skip (P);
      end loop;
   end Skip_To;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (P : in Out Parser) is
   begin
      while not At_End (P) and then
            Peek (P) /= Character'Val (10) and then
            Peek (P) /= Character'Val (13) loop
         Skip (P);
      end loop;
      -- Skip line ending
      if not At_End (P) and then Peek (P) = Character'Val (13) then
         Skip (P);
      end if;
      if not At_End (P) and then Peek (P) = Character'Val (10) then
         Skip (P);
      end if;
   end Skip_Line;

   -----------
   -- Match --
   -----------

   function Match (P : in Out Parser; C : Character) return Boolean is
   begin
      if not At_End (P) and then Peek (P) = C then
         Skip (P);
         return True;
      else
         return False;
      end if;
   end Match;

   function Match (P : in Out Parser; S : String) return Boolean is
   begin
      if Remaining (P) < S'Length then
         return False;
      end if;

      for I in S'Range loop
         if P.Buffer (P.Pos + I - S'First) /= S (I) then
            return False;
         end if;
      end loop;

      P.Pos := P.Pos + S'Length;
      return True;
   end Match;

   ---------------
   -- Match_Any --
   ---------------

   function Match_Any (P : in Out Parser; Chars : String) return Boolean is
      C : constant Character := Peek (P);
   begin
      for Ch of Chars loop
         if C = Ch then
            Skip (P);
            return True;
         end if;
      end loop;
      return False;
   end Match_Any;

   -----------------
   -- Match_Digit --
   -----------------

   function Match_Digit (P : in Out Parser) return Boolean is
   begin
      if Is_Digit (P) then
         Skip (P);
         return True;
      else
         return False;
      end if;
   end Match_Digit;

   -----------------
   -- Match_Alpha --
   -----------------

   function Match_Alpha (P : in Out Parser) return Boolean is
   begin
      if Is_Alpha (P) then
         Skip (P);
         return True;
      else
         return False;
      end if;
   end Match_Alpha;

   -----------------
   -- Match_Alnum --
   -----------------

   function Match_Alnum (P : in Out Parser) return Boolean is
   begin
      if Is_Alnum (P) then
         Skip (P);
         return True;
      else
         return False;
      end if;
   end Match_Alnum;

   ------------
   -- Expect --
   ------------

   procedure Expect (P : in Out Parser; C : Character) is
   begin
      if not Match (P, C) then
         raise Constraint_Error;
      end if;
   end Expect;

   procedure Expect (P : in Out Parser; S : String) is
   begin
      if not Match (P, S) then
         raise Constraint_Error;
      end if;
   end Expect;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (P : Parser) return Boolean is
   begin
      return not At_End (P) and then Is_Digit_Char (Peek (P));
   end Is_Digit;

   --------------
   -- Is_Alpha --
   --------------

   function Is_Alpha (P : Parser) return Boolean is
   begin
      return not At_End (P) and then Is_Alpha_Char (Peek (P));
   end Is_Alpha;

   --------------
   -- Is_Alnum --
   --------------

   function Is_Alnum (P : Parser) return Boolean is
   begin
      return not At_End (P) and then Is_Alnum_Char (Peek (P));
   end Is_Alnum;

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (P : Parser) return Boolean is
   begin
      return not At_End (P) and then Is_Space_Char (Peek (P));
   end Is_Whitespace;

   -----------------
   -- Is_Line_End --
   -----------------

   function Is_Line_End (P : Parser) return Boolean is
      C : constant Character := Peek (P);
   begin
      return At_End (P) or else C = Character'Val (10) or else C = Character'Val (13);
   end Is_Line_End;

   ---------------
   -- Read_Word --
   ---------------

   function Read_Word (P : in Out Parser) return String is
      Result : String (1 .. Max_Token_Length);
      Len    : Natural := 0;
   begin
      Skip_Whitespace (P);

      while not At_End (P) and then not Is_Space_Char (Peek (P)) loop
         exit when Len >= Max_Token_Length;
         Len := Len + 1;
         Result (Len) := Get (P);
      end loop;

      return Result (1 .. Len);
   end Read_Word;

   -----------------
   -- Read_Number --
   -----------------

   function Read_Number (P : in Out Parser) return Integer is
      Result   : Integer := 0;
      Negative : Boolean := False;
   begin
      Skip_Whitespace (P);

      if Match (P, '-') then
         Negative := True;
      elsif Match (P, '+') then
         null;
      end if;

      while Is_Digit (P) loop
         Result := Result * 10 + (Character'Pos (Get (P)) - Character'Pos ('0'));
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Read_Number;

   ---------------------
   -- Read_Identifier --
   ---------------------

   function Read_Identifier (P : in Out Parser) return String is
      Result : String (1 .. Max_Token_Length);
      Len    : Natural := 0;
   begin
      Skip_Whitespace (P);

      -- First character must be alpha or underscore
      if not At_End (P) and then
         (Is_Alpha_Char (Peek (P)) or Peek (P) = '_') then
         Len := 1;
         Result (Len) := Get (P);

         -- Subsequent can be alnum or underscore
         while not At_End (P) and then
               (Is_Alnum_Char (Peek (P)) or Peek (P) = '_') loop
            exit when Len >= Max_Token_Length;
            Len := Len + 1;
            Result (Len) := Get (P);
         end loop;
      end if;

      return Result (1 .. Len);
   end Read_Identifier;

   -----------------
   -- Read_Quoted --
   -----------------

   function Read_Quoted (P : in Out Parser; Quote : Character := '"') return String is
      Result : String (1 .. Max_Token_Length);
      Len    : Natural := 0;
   begin
      Skip_Whitespace (P);

      if not Match (P, Quote) then
         return "";
      end if;

      while not At_End (P) and then Peek (P) /= Quote loop
         exit when Len >= Max_Token_Length;
         Len := Len + 1;
         Result (Len) := Get (P);
      end loop;

      if not At_End (P) then
         Skip (P);  -- Skip closing quote
      end if;

      return Result (1 .. Len);
   end Read_Quoted;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until (P : in Out Parser; Delimiter : Character) return String is
      Result : String (1 .. Max_Token_Length);
      Len    : Natural := 0;
   begin
      while not At_End (P) and then Peek (P) /= Delimiter loop
         exit when Len >= Max_Token_Length;
         Len := Len + 1;
         Result (Len) := Get (P);
      end loop;

      return Result (1 .. Len);
   end Read_Until;

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (P : in Out Parser) return String is
      Result : String (1 .. Max_Token_Length);
      Len    : Natural := 0;
   begin
      while not Is_Line_End (P) loop
         exit when Len >= Max_Token_Length;
         Len := Len + 1;
         Result (Len) := Get (P);
      end loop;

      Skip_Line (P);
      return Result (1 .. Len);
   end Read_Line;

   --------------
   -- Tokenize --
   --------------

   procedure Tokenize
     (Input     : String;
      Delimiter : Character;
      Tokens    : out Token_Array;
      Count     : out Natural)
   is
      P : Parser;
      T : Token;
      Len : Natural;
   begin
      Initialize (P, Input);
      Count := 0;

      while not At_End (P) and Count < Max_Tokens loop
         T.Data := (others => ' ');
         Len := 0;

         while not At_End (P) and then Peek (P) /= Delimiter loop
            exit when Len >= Max_Token_Length;
            Len := Len + 1;
            T.Data (Len) := Get (P);
         end loop;

         T.Length := Len;
         T.Kind := 0;
         Count := Count + 1;
         Tokens (Count) := T;

         if not At_End (P) then
            Skip (P);  -- Skip delimiter
         end if;
      end loop;
   end Tokenize;

   ---------------
   -- Parse_CSV --
   ---------------

   procedure Parse_CSV
     (Input  : String;
      Tokens : out Token_Array;
      Count  : out Natural)
   is
   begin
      Tokenize (Input, ',', Tokens, Count);
   end Parse_CSV;

   ----------------------
   -- Parse_Key_Values --
   ----------------------

   procedure Parse_Key_Values
     (Input  : String;
      Pairs  : out KV_Array;
      Count  : out Natural;
      Sep    : Character := '=';
      Delim  : Character := ',')
   is
      Tokens : Token_Array;
      Tok_Count : Natural;
      P : Parser;
   begin
      Tokenize (Input, Delim, Tokens, Tok_Count);
      Count := 0;

      for I in 1 .. Tok_Count loop
         exit when Count >= Max_Tokens;
         Initialize (P, To_String (Tokens (I)));

         Pairs (Count + 1).Key := (others => ' ');
         Pairs (Count + 1).Key_Len := 0;
         Pairs (Count + 1).Value := (others => ' ');
         Pairs (Count + 1).Val_Len := 0;

         -- Read key
         Skip_Whitespace (P);
         while not At_End (P) and then Peek (P) /= Sep loop
            if Pairs (Count + 1).Key_Len < Max_Token_Length then
               Pairs (Count + 1).Key_Len := Pairs (Count + 1).Key_Len + 1;
               Pairs (Count + 1).Key (Pairs (Count + 1).Key_Len) := Get (P);
            else
               Skip (P);
            end if;
         end loop;

         -- Trim key
         while Pairs (Count + 1).Key_Len > 0 and then
               Is_Space_Char (Pairs (Count + 1).Key (Pairs (Count + 1).Key_Len)) loop
            Pairs (Count + 1).Key_Len := Pairs (Count + 1).Key_Len - 1;
         end loop;

         if Match (P, Sep) then
            Skip_Whitespace (P);
            -- Read value
            while not At_End (P) loop
               if Pairs (Count + 1).Val_Len < Max_Token_Length then
                  Pairs (Count + 1).Val_Len := Pairs (Count + 1).Val_Len + 1;
                  Pairs (Count + 1).Value (Pairs (Count + 1).Val_Len) := Get (P);
               else
                  Skip (P);
               end if;
            end loop;

            -- Trim value
            while Pairs (Count + 1).Val_Len > 0 and then
                  Is_Space_Char (Pairs (Count + 1).Value (Pairs (Count + 1).Val_Len)) loop
               Pairs (Count + 1).Val_Len := Pairs (Count + 1).Val_Len - 1;
            end loop;
         end if;

         if Pairs (Count + 1).Key_Len > 0 then
            Count := Count + 1;
         end if;
      end loop;
   end Parse_Key_Values;

   -------------------
   -- Parse_Integer --
   -------------------

   function Parse_Integer (S : String) return Integer is
      P : Parser;
   begin
      Initialize (P, Trim (S));
      return Read_Number (P);
   end Parse_Integer;

   ---------------
   -- Parse_Hex --
   ---------------

   function Parse_Hex (S : String) return Natural is
      Result : Natural := 0;

      function Hex_Val (C : Character) return Natural is
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
      end Hex_Val;

      T : constant String := Trim (S);
      Start : Natural := T'First;
   begin
      -- Skip 0x prefix if present
      if T'Length >= 2 and then T (T'First) = '0' and then
         (T (T'First + 1) = 'x' or T (T'First + 1) = 'X') then
         Start := T'First + 2;
      end if;

      for I in Start .. T'Last loop
         Result := Result * 16 + Hex_Val (T (I));
      end loop;

      return Result;
   end Parse_Hex;

   ------------------
   -- Parse_Binary --
   ------------------

   function Parse_Binary (S : String) return Natural is
      Result : Natural := 0;
      T : constant String := Trim (S);
      Start : Natural := T'First;
   begin
      -- Skip 0b prefix if present
      if T'Length >= 2 and then T (T'First) = '0' and then
         (T (T'First + 1) = 'b' or T (T'First + 1) = 'B') then
         Start := T'First + 2;
      end if;

      for I in Start .. T'Last loop
         if T (I) = '1' then
            Result := Result * 2 + 1;
         elsif T (I) = '0' then
            Result := Result * 2;
         end if;
      end loop;

      return Result;
   end Parse_Binary;

   -----------------
   -- Parse_Octal --
   -----------------

   function Parse_Octal (S : String) return Natural is
      Result : Natural := 0;
      T : constant String := Trim (S);
   begin
      for C of T loop
         if C >= '0' and C <= '7' then
            Result := Result * 8 + (Character'Pos (C) - Character'Pos ('0'));
         end if;
      end loop;

      return Result;
   end Parse_Octal;

   -------------------
   -- Parse_Boolean --
   -------------------

   function Parse_Boolean (S : String) return Boolean is
      T : constant String := Trim (S);

      function To_Upper (C : Character) return Character is
      begin
         if C >= 'a' and C <= 'z' then
            return Character'Val (Character'Pos (C) - 32);
         else
            return C;
         end if;
      end To_Upper;

      Upper : String (T'Range);
   begin
      for I in T'Range loop
         Upper (I) := To_Upper (T (I));
      end loop;

      return Upper = "TRUE" or Upper = "YES" or Upper = "1" or Upper = "ON";
   end Parse_Boolean;

   ----------------------
   -- Is_Valid_Integer --
   ----------------------

   function Is_Valid_Integer (S : String) return Boolean is
      T : constant String := Trim (S);
      Start : Natural := T'First;
   begin
      if T'Length = 0 then
         return False;
      end if;

      if T (T'First) = '-' or T (T'First) = '+' then
         Start := T'First + 1;
      end if;

      if Start > T'Last then
         return False;
      end if;

      for I in Start .. T'Last loop
         if not Is_Digit_Char (T (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Integer;

   --------------------------
   -- Is_Valid_Identifier --
   --------------------------

   function Is_Valid_Identifier (S : String) return Boolean is
      T : constant String := Trim (S);
   begin
      if T'Length = 0 then
         return False;
      end if;

      if not (Is_Alpha_Char (T (T'First)) or T (T'First) = '_') then
         return False;
      end if;

      for I in T'First + 1 .. T'Last loop
         if not (Is_Alnum_Char (T (I)) or T (I) = '_') then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Identifier;

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String is
   begin
      return Trim_Right (Trim_Left (S));
   end Trim;

   ---------------
   -- Trim_Left --
   ---------------

   function Trim_Left (S : String) return String is
   begin
      for I in S'Range loop
         if not Is_Space_Char (S (I)) then
            return S (I .. S'Last);
         end if;
      end loop;
      return "";
   end Trim_Left;

   ----------------
   -- Trim_Right --
   ----------------

   function Trim_Right (S : String) return String is
   begin
      for I in reverse S'Range loop
         if not Is_Space_Char (S (I)) then
            return S (S'First .. I);
         end if;
      end loop;
      return "";
   end Trim_Right;

   ---------------
   -- To_String --
   ---------------

   function To_String (T : Token) return String is
   begin
      return T.Data (1 .. T.Length);
   end To_String;

end GNAT.Parser_Utils;
