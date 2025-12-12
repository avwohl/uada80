-- Ada.Text_IO.Enumeration_Aux body for Z80
-- Enumeration I/O auxiliary operations implementation

package body Ada.Text_IO.Enumeration_Aux is

   ------------------
   -- Get_Enum_Lit --
   ------------------

   procedure Get_Enum_Lit
     (File   : File_Type;
      Buffer : out String;
      Last   : out Natural)
   is
      C : Character;
      E : Boolean;
   begin
      Last := 0;

      -- Skip leading whitespace
      loop
         Look_Ahead (File, C, E);
         exit when E or else C /= ' ';
         Get (File, C);
      end loop;

      if E then
         return;
      end if;

      -- Check for character literal
      if C = ''' then
         Get (File, C);
         Last := Last + 1;
         Buffer (Last) := C;
         Get (File, C);  -- The character
         Last := Last + 1;
         Buffer (Last) := C;
         Get (File, C);  -- Closing quote
         Last := Last + 1;
         Buffer (Last) := C;
         return;
      end if;

      -- Read identifier
      while not E and then
        (C in 'A' .. 'Z' or C in 'a' .. 'z' or
         C in '0' .. '9' or C = '_')
      loop
         Get (File, C);
         Last := Last + 1;
         Buffer (Last) := C;
         Look_Ahead (File, C, E);
      end loop;
   end Get_Enum_Lit;

   -------------------
   -- Scan_Enum_Lit --
   -------------------

   procedure Scan_Enum_Lit
     (Buffer : String;
      Last   : out Natural)
   is
      Idx : Positive := Buffer'First;
   begin
      Last := 0;

      -- Skip leading blanks
      while Idx <= Buffer'Last and then Buffer (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;

      if Idx > Buffer'Last then
         return;
      end if;

      -- Check for character literal
      if Buffer (Idx) = ''' and then Idx + 2 <= Buffer'Last then
         Last := Idx + 2;
         return;
      end if;

      -- Scan identifier
      while Idx <= Buffer'Last and then
        (Buffer (Idx) in 'A' .. 'Z' or Buffer (Idx) in 'a' .. 'z' or
         Buffer (Idx) in '0' .. '9' or Buffer (Idx) = '_')
      loop
         Idx := Idx + 1;
      end loop;

      Last := Idx - 1;
   end Scan_Enum_Lit;

   ------------------
   -- Put_Enum_Lit --
   ------------------

   procedure Put_Enum_Lit
     (File  : File_Type;
      Value : String;
      Width : Field)
   is
      Pad : constant Integer := Integer (Width) - Value'Length;
   begin
      for I in 1 .. Pad loop
         Put (File, ' ');
      end loop;
      Put (File, Value);
   end Put_Enum_Lit;

end Ada.Text_IO.Enumeration_Aux;
