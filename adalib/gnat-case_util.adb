-- GNAT.Case_Util body for Z80
-- Character case conversion utilities implementation

with Ada.Characters.Handling;

package body GNAT.Case_Util is

   --------------
   -- To_Lower --
   --------------

   function To_Lower (A : Character) return Character is
   begin
      return Ada.Characters.Handling.To_Lower (A);
   end To_Lower;

   procedure To_Lower (A : in Out String) is
   begin
      for I in A'Range loop
         A (I) := To_Lower (A (I));
      end loop;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (A : Character) return Character is
   begin
      return Ada.Characters.Handling.To_Upper (A);
   end To_Upper;

   procedure To_Upper (A : in Out String) is
   begin
      for I in A'Range loop
         A (I) := To_Upper (A (I));
      end loop;
   end To_Upper;

   --------------
   -- To_Mixed --
   --------------

   procedure To_Mixed (A : in Out String) is
      Capital : Boolean := True;
   begin
      for I in A'Range loop
         if Capital then
            A (I) := To_Upper (A (I));
         else
            A (I) := To_Lower (A (I));
         end if;

         Capital := A (I) = '_' or else A (I) = ' ';
      end loop;
   end To_Mixed;

end GNAT.Case_Util;
