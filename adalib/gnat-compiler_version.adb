-- GNAT.Compiler_Version body for Z80
-- Compiler version implementation

package body GNAT.Compiler_Version is

   Version_String : constant String := "UADA80 Z80 Ada Compiler 1.0";

   -------------
   -- Version --
   -------------

   function Version return String is
   begin
      return Version_String;
   end Version;

   -----------------
   -- Ver_Len_Max --
   -----------------

   function Ver_Len_Max return Natural is
   begin
      return 80;
   end Ver_Len_Max;

end GNAT.Compiler_Version;
