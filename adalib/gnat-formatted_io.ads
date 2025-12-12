-- GNAT.Formatted_IO for Z80
-- Formatted I/O utilities

package GNAT.Formatted_IO is

   -- Put with format string (printf-style subset)
   procedure Put (Format : String);
   procedure Put (Format : String; Item1 : String);
   procedure Put (Format : String; Item1, Item2 : String);
   procedure Put (Format : String; Item1, Item2, Item3 : String);

   procedure Put_Line (Format : String);
   procedure Put_Line (Format : String; Item1 : String);
   procedure Put_Line (Format : String; Item1, Item2 : String);

   -- Integer formatting
   function Format_Int
     (Value : Integer;
      Width : Natural := 0;
      Fill  : Character := ' ') return String;

   -- Float formatting (simplified)
   function Format_Float
     (Value     : Float;
      Precision : Natural := 2) return String;

end GNAT.Formatted_IO;
