-- System.Wch_StW for Z80
-- Wide character string to string conversion

package System.Wch_StW is
   pragma Pure;

   procedure String_To_Wide_String
     (S  : String;
      WS : out Wide_String;
      WL : out Natural);
   --  Convert String to Wide_String

   procedure String_To_Wide_Wide_String
     (S   : String;
      WWS : out Wide_Wide_String;
      WWL : out Natural);
   --  Convert String to Wide_Wide_String

end System.Wch_StW;
