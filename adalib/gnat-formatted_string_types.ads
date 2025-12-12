-- GNAT.Formatted_String_Types for Z80
-- Types for formatted strings

package GNAT.Formatted_String_Types is
   pragma Pure;

   -- Format specifier components
   type Alignment_Kind is (Left, Right, Center);
   type Sign_Kind is (Default_Sign, Plus_Sign, Space_Sign);
   type Case_Kind is (Lower_Case, Upper_Case);

   type Format_Spec is record
      Width     : Natural := 0;
      Precision : Natural := 6;
      Alignment : Alignment_Kind := Right;
      Sign      : Sign_Kind := Default_Sign;
      Fill      : Character := ' ';
      Base      : Natural := 10;
      Case_Style : Case_Kind := Lower_Case;
   end record;

   Default_Format : constant Format_Spec :=
     (Width => 0, Precision => 6, Alignment => Right,
      Sign => Default_Sign, Fill => ' ', Base => 10,
      Case_Style => Lower_Case);

end GNAT.Formatted_String_Types;
