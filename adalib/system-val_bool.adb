-- System.Val_Bool body for Z80
-- Boolean value conversion implementation

with Ada.Characters.Handling;

package body System.Val_Bool is

   -------------------
   -- Value_Boolean --
   -------------------

   function Value_Boolean (Str : String) return Boolean is
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

      if Last - First + 1 < 1 then
         raise Constraint_Error;
      end if;

      -- Convert to upper case for comparison
      for I in First .. Last loop
         S (I) := Ada.Characters.Handling.To_Upper (S (I));
      end loop;

      declare
         Name : constant String := S (First .. Last);
      begin
         if Name = "TRUE" then
            return True;
         elsif Name = "FALSE" then
            return False;
         else
            raise Constraint_Error;
         end if;
      end;
   end Value_Boolean;

   ----------------------
   -- Is_Valid_Boolean --
   ----------------------

   function Is_Valid_Boolean (Str : String) return Boolean is
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

      if Last - First + 1 < 1 then
         return False;
      end if;

      -- Convert to upper case
      for I in First .. Last loop
         S (I) := Ada.Characters.Handling.To_Upper (S (I));
      end loop;

      declare
         Name : constant String := S (First .. Last);
      begin
         return Name = "TRUE" or Name = "FALSE";
      end;
   end Is_Valid_Boolean;

end System.Val_Bool;
