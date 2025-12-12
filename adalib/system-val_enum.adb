-- System.Val_Enum body for Z80
-- Enumeration value conversion implementation

with Ada.Characters.Handling;

package body System.Val_Enum is

   -----------------------
   -- Value_Enumeration --
   -----------------------

   function Value_Enumeration (Str : String) return Enum is
      S     : String := Str;
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

      -- Convert to upper case
      for I in First .. Last loop
         S (I) := Ada.Characters.Handling.To_Upper (S (I));
      end loop;

      declare
         Name : constant String := S (First .. Last);
      begin
         -- Check against all enumeration literals
         for E in Enum loop
            declare
               Image : constant String := Enum'Image (E);
            begin
               if Image = Name then
                  return E;
               end if;
            end;
         end loop;

         raise Constraint_Error;
      end;
   end Value_Enumeration;

   -----------------------
   -- Scan_Enum_Literal --
   -----------------------

   function Scan_Enum_Literal
     (Str    : String;
      Ptr    : not null access Integer;
      Max    : Integer;
      Names  : String;
      Starts : access Integer) return Natural
   is
      pragma Unreferenced (Starts);

      S     : String := Str;
      First : Integer := Ptr.all;
      Last  : Integer;
      Pos   : Natural := 0;
   begin
      -- Skip leading spaces
      while First <= Max and S (First) = ' ' loop
         First := First + 1;
      end loop;

      -- Find end of identifier
      Last := First;
      while Last <= Max and
            (Ada.Characters.Handling.Is_Alphanumeric (S (Last)) or
             S (Last) = '_') loop
         Last := Last + 1;
      end loop;
      Last := Last - 1;

      if Last < First then
         raise Constraint_Error;
      end if;

      -- Convert to upper case for comparison
      for I in First .. Last loop
         S (I) := Ada.Characters.Handling.To_Upper (S (I));
      end loop;

      -- Search in Names table
      declare
         Name      : constant String := S (First .. Last);
         Name_Len  : constant Natural := Name'Length;
         Idx       : Integer := Names'First;
         Name_Start : Integer;
      begin
         while Idx <= Names'Last loop
            Name_Start := Idx;

            -- Find end of current name (names separated by NUL or space)
            while Idx <= Names'Last and
                  Names (Idx) /= ASCII.NUL and
                  Names (Idx) /= ' ' loop
               Idx := Idx + 1;
            end loop;

            -- Compare
            if Idx - Name_Start = Name_Len then
               declare
                  Match : Boolean := True;
               begin
                  for I in 0 .. Name_Len - 1 loop
                     if Ada.Characters.Handling.To_Upper
                        (Names (Name_Start + I)) /= Name (Name'First + I) then
                        Match := False;
                        exit;
                     end if;
                  end loop;

                  if Match then
                     Ptr.all := Last + 1;
                     return Pos;
                  end if;
               end;
            end if;

            -- Skip separator
            while Idx <= Names'Last and
                  (Names (Idx) = ASCII.NUL or Names (Idx) = ' ') loop
               Idx := Idx + 1;
            end loop;

            Pos := Pos + 1;
         end loop;
      end;

      raise Constraint_Error;
   end Scan_Enum_Literal;

end System.Val_Enum;
