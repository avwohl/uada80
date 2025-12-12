-- System.Case_Util body for Z80
-- Case conversion utilities

package body System.Case_Util is

   --------------
   -- To_Upper --
   --------------

   function To_Upper (C : Character) return Character is
   begin
      if C in 'a' .. 'z' then
         return Character'Val (Character'Pos (C) - 32);
      else
         return C;
      end if;
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (C : Character) return Character is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Character'Pos (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   procedure To_Upper (S : in Out String) is
   begin
      for I in S'Range loop
         S (I) := To_Upper (S (I));
      end loop;
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in Out String) is
   begin
      for I in S'Range loop
         S (I) := To_Lower (S (I));
      end loop;
   end To_Lower;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z' or else C in 'a' .. 'z';
   end Is_Letter;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z';
   end Is_Upper;

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (C : Character) return Boolean is
   begin
      return C in 'a' .. 'z';
   end Is_Lower;

end System.Case_Util;
