-- System.Wch_WtS for Z80
-- Wide string to string conversion

package System.Wch_WtS is
   pragma Pure;

   procedure Wide_String_To_String
     (WS : Wide_String;
      S  : out String;
      SL : out Natural);
   --  Convert Wide_String to String

   procedure Wide_Wide_String_To_String
     (WWS : Wide_Wide_String;
      S   : out String;
      SL  : out Natural);
   --  Convert Wide_Wide_String to String

end System.Wch_WtS;
