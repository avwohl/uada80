-- System.Object_Reader body for Z80
-- Simple object file reader utilities (stub)

package body System.Object_Reader is

   -------------------
   -- Detect_Format --
   -------------------

   function Detect_Format (Data : String) return Object_Format is
   begin
      -- CP/M COM files have no magic header
      -- They are raw binary loaded at 0100h
      if Data'Length > 0 then
         return Format_COM;
      else
         return Format_Unknown;
      end if;
   end Detect_Format;

   ----------------------
   -- Get_Load_Address --
   ----------------------

   function Get_Load_Address (Data : String) return Natural is
      Format : constant Object_Format := Detect_Format (Data);
   begin
      case Format is
         when Format_COM =>
            return COM_Load_Address;
         when Format_Binary =>
            return 0;
         when Format_Unknown =>
            return 0;
      end case;
   end Get_Load_Address;

end System.Object_Reader;
