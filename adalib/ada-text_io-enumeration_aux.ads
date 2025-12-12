-- Ada.Text_IO.Enumeration_Aux for Z80
-- Enumeration I/O auxiliary operations

with Ada.Text_IO;

private package Ada.Text_IO.Enumeration_Aux is

   procedure Get_Enum_Lit
     (File   : File_Type;
      Buffer : out String;
      Last   : out Natural);
   --  Get enumeration literal from file

   procedure Scan_Enum_Lit
     (Buffer : String;
      Last   : out Natural);
   --  Scan for end of enumeration literal

   procedure Put_Enum_Lit
     (File  : File_Type;
      Value : String;
      Width : Field);
   --  Put enumeration literal to file

end Ada.Text_IO.Enumeration_Aux;
