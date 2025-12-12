-- Interfaces.Fortran for Z80
-- Types for interfacing with Fortran
--
-- Provides Ada equivalents of Fortran intrinsic types

package Interfaces.Fortran is
   pragma Pure;

   -- Character types
   type Character_Set is (ASCII_Character);  -- Only ASCII on Z80

   -- Fortran character
   type Fortran_Character is new Character;

   -- Integer types (Fortran kinds)
   type Fortran_Integer is range -32768 .. 32767;  -- 16-bit on Z80
   for Fortran_Integer'Size use 16;

   -- Logical type
   type Logical is new Boolean;
   for Logical'Size use 8;

   -- Real types (limited precision on Z80)
   -- Note: Z80 has no FPU, these are software-emulated
   type Real is digits 6;  -- Single precision
   type Double_Precision is digits 6;  -- Same as Real on Z80

   -- Complex types
   type Complex is record
      Re : Real;
      Im : Real;
   end record;

   type Double_Complex is record
      Re : Double_Precision;
      Im : Double_Precision;
   end record;

   -- Fortran string type
   -- Fortran strings are fixed-length, blank-padded
   subtype Fortran_String is String;

   -- Conversion functions between Ada and Fortran characters
   function To_Fortran (Item : Character) return Fortran_Character;
   function To_Ada (Item : Fortran_Character) return Character;

   function To_Fortran (Item : String) return Fortran_String;
   function To_Ada (Item : Fortran_String) return String;

   -- Copy procedures
   procedure To_Fortran
     (Item   : String;
      Target : out Fortran_String;
      Last   : out Natural);

   procedure To_Ada
     (Item   : Fortran_String;
      Target : out String;
      Last   : out Natural);

end Interfaces.Fortran;
