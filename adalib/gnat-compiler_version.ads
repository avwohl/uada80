-- GNAT.Compiler_Version for Z80
-- Compiler version information

package GNAT.Compiler_Version is
   pragma Pure;

   function Version return String;
   --  Return compiler version string

   function Ver_Len_Max return Natural;
   --  Maximum length of version string

end GNAT.Compiler_Version;
