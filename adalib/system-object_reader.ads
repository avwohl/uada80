-- System.Object_Reader for Z80
-- Simple object file reader utilities (stub)

package System.Object_Reader is
   pragma Pure;

   -- Object format indicator
   type Object_Format is (
      Format_Unknown,
      Format_COM,        -- CP/M COM file
      Format_Binary);    -- Raw binary

   -- Detect object format from magic bytes
   function Detect_Format (Data : String) return Object_Format;

   -- Get load address for object file
   function Get_Load_Address (Data : String) return Natural;

   -- COM files load at 0100h
   COM_Load_Address : constant := 16#0100#;

end System.Object_Reader;
