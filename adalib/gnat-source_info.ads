-- GNAT.Source_Info for Z80
-- Source code location information

package GNAT.Source_Info is
   pragma Pure;

   -- Source location functions
   -- These are typically implemented as intrinsics by GNAT

   function File return String is ("");
   pragma Import (Intrinsic, File, "__gnat_source_file");

   function Line return Positive is (1);
   pragma Import (Intrinsic, Line, "__gnat_source_line");

   function Source_Location return String is ("");
   pragma Import (Intrinsic, Source_Location, "__gnat_source_location");

   function Enclosing_Entity return String is ("");
   pragma Import (Intrinsic, Enclosing_Entity, "__gnat_enclosing_entity");

   function Compilation_ISO_Date return String is ("2025-01-01");
   function Compilation_Time return String is ("00:00:00");

end GNAT.Source_Info;
