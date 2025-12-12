-- System.Val_Char body for Z80
-- Character value conversion from string representation

with Ada.Characters.Handling;

package body System.Val_Char is

   ---------------------
   -- Value_Character --
   ---------------------

   function Value_Character (Str : String) return Character is
      S     : constant String := Ada.Characters.Handling.To_Upper (Str);
      First : Integer := S'First;
      Last  : Integer := S'Last;
   begin
      -- Skip leading/trailing spaces
      while First <= Last and S (First) = ' ' loop
         First := First + 1;
      end loop;
      while Last >= First and S (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      if Last - First + 1 < 1 then
         raise Constraint_Error;
      end if;

      -- Check for 'X' form (character literal)
      if Last - First + 1 = 3 then
         if S (First) = ''' and S (Last) = ''' then
            return S (First + 1);
         end if;
      end if;

      -- Check for control character names
      declare
         Name : constant String := S (First .. Last);
      begin
         if Name = "NUL" then return ASCII.NUL;
         elsif Name = "SOH" then return ASCII.SOH;
         elsif Name = "STX" then return ASCII.STX;
         elsif Name = "ETX" then return ASCII.ETX;
         elsif Name = "EOT" then return ASCII.EOT;
         elsif Name = "ENQ" then return ASCII.ENQ;
         elsif Name = "ACK" then return ASCII.ACK;
         elsif Name = "BEL" then return ASCII.BEL;
         elsif Name = "BS" then return ASCII.BS;
         elsif Name = "HT" then return ASCII.HT;
         elsif Name = "LF" then return ASCII.LF;
         elsif Name = "VT" then return ASCII.VT;
         elsif Name = "FF" then return ASCII.FF;
         elsif Name = "CR" then return ASCII.CR;
         elsif Name = "SO" then return ASCII.SO;
         elsif Name = "SI" then return ASCII.SI;
         elsif Name = "DLE" then return ASCII.DLE;
         elsif Name = "DC1" then return ASCII.DC1;
         elsif Name = "DC2" then return ASCII.DC2;
         elsif Name = "DC3" then return ASCII.DC3;
         elsif Name = "DC4" then return ASCII.DC4;
         elsif Name = "NAK" then return ASCII.NAK;
         elsif Name = "SYN" then return ASCII.SYN;
         elsif Name = "ETB" then return ASCII.ETB;
         elsif Name = "CAN" then return ASCII.CAN;
         elsif Name = "EM" then return ASCII.EM;
         elsif Name = "SUB" then return ASCII.SUB;
         elsif Name = "ESC" then return ASCII.ESC;
         elsif Name = "FS" then return ASCII.FS;
         elsif Name = "GS" then return ASCII.GS;
         elsif Name = "RS" then return ASCII.RS;
         elsif Name = "US" then return ASCII.US;
         elsif Name = "DEL" then return ASCII.DEL;
         else
            raise Constraint_Error;
         end if;
      end;
   end Value_Character;

   --------------------------
   -- Is_Character_Literal --
   --------------------------

   function Is_Character_Literal (Str : String) return Boolean is
      S     : constant String := Str;
      First : Integer := S'First;
      Last  : Integer := S'Last;
   begin
      -- Skip leading/trailing spaces
      while First <= Last and S (First) = ' ' loop
         First := First + 1;
      end loop;
      while Last >= First and S (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      if Last - First + 1 < 1 then
         return False;
      end if;

      -- Check for 'X' form
      if Last - First + 1 = 3 then
         return S (First) = ''' and S (Last) = ''';
      end if;

      -- Check for control character names
      declare
         Name : constant String := Ada.Characters.Handling.To_Upper (S (First .. Last));
      begin
         return Name = "NUL" or Name = "SOH" or Name = "STX" or Name = "ETX" or
                Name = "EOT" or Name = "ENQ" or Name = "ACK" or Name = "BEL" or
                Name = "BS" or Name = "HT" or Name = "LF" or Name = "VT" or
                Name = "FF" or Name = "CR" or Name = "SO" or Name = "SI" or
                Name = "DLE" or Name = "DC1" or Name = "DC2" or Name = "DC3" or
                Name = "DC4" or Name = "NAK" or Name = "SYN" or Name = "ETB" or
                Name = "CAN" or Name = "EM" or Name = "SUB" or Name = "ESC" or
                Name = "FS" or Name = "GS" or Name = "RS" or Name = "US" or
                Name = "DEL";
      end;
   end Is_Character_Literal;

end System.Val_Char;
