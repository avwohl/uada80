-- Report package for ACATS test harness
-- Stub implementation for semantic analysis

package Report is

   subtype File_Num is Integer range 1 .. 5;

   -- Test reporting procedures
   procedure Test (Name : String; Descr : String);
   procedure Failed (Descr : String);
   procedure Not_Applicable (Descr : String);
   procedure Special_Action (Descr : String);
   procedure Comment (Descr : String);
   procedure Result;

   -- Identity functions for dynamic values
   function Ident_Int (X : Integer) return Integer;
   function Ident_Char (X : Character) return Character;
   function Ident_Wide_Char (X : Wide_Character) return Wide_Character;
   function Ident_Bool (X : Boolean) return Boolean;
   function Ident_Str (X : String) return String;
   function Ident_Wide_Str (X : Wide_String) return Wide_String;

   -- Utility functions
   function Equal (X, Y : Integer) return Boolean;
   function Legal_File_Name (X : File_Num := 1; Nam : String := "") return String;
   function Time_Stamp return String;

   -- Event trace setting
   Generate_Event_Trace_File : constant Boolean := False;

end Report;
