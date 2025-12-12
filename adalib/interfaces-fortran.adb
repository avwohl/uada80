-- Interfaces.Fortran body for Z80
-- Types for interfacing with Fortran implementation

package body Interfaces.Fortran is

   ----------------
   -- To_Fortran --
   ----------------

   function To_Fortran (Item : Character) return Fortran_Character is
   begin
      return Fortran_Character (Item);
   end To_Fortran;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Item : Fortran_Character) return Character is
   begin
      return Character (Item);
   end To_Ada;

   ----------------
   -- To_Fortran --
   ----------------

   function To_Fortran (Item : String) return Fortran_String is
   begin
      return Item;
   end To_Fortran;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Item : Fortran_String) return String is
   begin
      return Item;
   end To_Ada;

   ----------------
   -- To_Fortran --
   ----------------

   procedure To_Fortran
     (Item   : String;
      Target : out Fortran_String;
      Last   : out Natural)
   is
      Copy_Length : constant Natural :=
        Natural'Min (Item'Length, Target'Length);
   begin
      Target (Target'First .. Target'First + Copy_Length - 1) :=
        Item (Item'First .. Item'First + Copy_Length - 1);

      -- Pad with blanks
      if Copy_Length < Target'Length then
         Target (Target'First + Copy_Length .. Target'Last) := (others => ' ');
      end if;

      Last := Target'First + Copy_Length - 1;
   end To_Fortran;

   ------------
   -- To_Ada --
   ------------

   procedure To_Ada
     (Item   : Fortran_String;
      Target : out String;
      Last   : out Natural)
   is
      Copy_Length : constant Natural :=
        Natural'Min (Item'Length, Target'Length);
   begin
      Target (Target'First .. Target'First + Copy_Length - 1) :=
        Item (Item'First .. Item'First + Copy_Length - 1);

      Last := Target'First + Copy_Length - 1;
   end To_Ada;

end Interfaces.Fortran;
