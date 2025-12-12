-- GNAT.Fast_Strings body for Z80
-- Fast string operations implementation

with Ada.Characters.Handling;

package body GNAT.Fast_Strings is

   -----------
   -- Equal --
   -----------

   function Equal
     (Left  : String;
      Right : String) return Boolean
   is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in 0 .. Left'Length - 1 loop
         if Left (Left'First + I) /= Right (Right'First + I) then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   ----------------------------
   -- Equal_Case_Insensitive --
   ----------------------------

   function Equal_Case_Insensitive
     (Left  : String;
      Right : String) return Boolean
   is
      use Ada.Characters.Handling;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in 0 .. Left'Length - 1 loop
         if To_Lower (Left (Left'First + I)) /= To_Lower (Right (Right'First + I)) then
            return False;
         end if;
      end loop;
      return True;
   end Equal_Case_Insensitive;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than
     (Left  : String;
      Right : String) return Boolean
   is
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 0 .. Min_Len - 1 loop
         if Left (Left'First + I) < Right (Right'First + I) then
            return True;
         elsif Left (Left'First + I) > Right (Right'First + I) then
            return False;
         end if;
      end loop;
      return Left'Length < Right'Length;
   end Less_Than;

   ---------------------------------
   -- Less_Than_Case_Insensitive --
   ---------------------------------

   function Less_Than_Case_Insensitive
     (Left  : String;
      Right : String) return Boolean
   is
      use Ada.Characters.Handling;
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 0 .. Min_Len - 1 loop
         declare
            L : constant Character := To_Lower (Left (Left'First + I));
            R : constant Character := To_Lower (Right (Right'First + I));
         begin
            if L < R then
               return True;
            elsif L > R then
               return False;
            end if;
         end;
      end loop;
      return Left'Length < Right'Length;
   end Less_Than_Case_Insensitive;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : String;
      Pattern : Character) return Natural
   is
   begin
      for I in Source'Range loop
         if Source (I) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : String;
      Pattern : String) return Natural
   is
   begin
      if Pattern'Length = 0 or Pattern'Length > Source'Length then
         return 0;
      end if;

      for I in Source'First .. Source'Last - Pattern'Length + 1 loop
         if Source (I .. I + Pattern'Length - 1) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   -------------------
   -- Index_Reverse --
   -------------------

   function Index_Reverse
     (Source  : String;
      Pattern : Character) return Natural
   is
   begin
      for I in reverse Source'Range loop
         if Source (I) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Reverse;

end GNAT.Fast_Strings;
