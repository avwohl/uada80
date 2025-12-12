-- System.Val_Wchar body for Z80
-- Wide_Character value conversion implementation

package body System.Val_Wchar is

   function Hex_Value (C : Character) return Natural is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when others => raise Constraint_Error;
      end case;
   end Hex_Value;

   --------------------------
   -- Value_Wide_Character --
   --------------------------

   function Value_Wide_Character (Str : String) return Wide_Character is
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

      if Last < First then
         raise Constraint_Error;
      end if;

      -- Check for 'X' form
      if Last - First + 1 = 3 then
         if S (First) = ''' and S (Last) = ''' then
            return Wide_Character'Val (Character'Pos (S (First + 1)));
         end if;
      end if;

      -- Check for hex form: ["xxxx"]
      if Last - First + 1 = 8 then
         if S (First .. First + 1) = "[""" and
            S (Last - 1 .. Last) = """]"
         then
            declare
               Code : Natural := 0;
            begin
               for I in First + 2 .. Last - 2 loop
                  Code := Code * 16 + Hex_Value (S (I));
               end loop;
               return Wide_Character'Val (Code);
            end;
         end if;
      end if;

      raise Constraint_Error;
   end Value_Wide_Character;

   --------------------------------
   -- Is_Wide_Character_Literal --
   --------------------------------

   function Is_Wide_Character_Literal (Str : String) return Boolean is
      S     : constant String := Str;
      First : Integer := S'First;
      Last  : Integer := S'Last;
   begin
      while First <= Last and S (First) = ' ' loop
         First := First + 1;
      end loop;
      while Last >= First and S (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      if Last < First then
         return False;
      end if;

      -- 'X' form
      if Last - First + 1 = 3 then
         return S (First) = ''' and S (Last) = ''';
      end if;

      -- Hex form
      if Last - First + 1 = 8 then
         if S (First .. First + 1) /= "[""" or
            S (Last - 1 .. Last) /= """]"
         then
            return False;
         end if;

         for I in First + 2 .. Last - 2 loop
            case S (I) is
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => null;
               when others => return False;
            end case;
         end loop;
         return True;
      end if;

      return False;
   end Is_Wide_Character_Literal;

end System.Val_Wchar;
