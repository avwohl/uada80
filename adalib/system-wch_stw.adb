-- System.Wch_StW body for Z80
-- Wide character string conversion implementation

package body System.Wch_StW is

   ----------------------------
   -- String_To_Wide_String --
   ----------------------------

   procedure String_To_Wide_String
     (S  : String;
      WS : out Wide_String;
      WL : out Natural)
   is
      Len : constant Natural := Natural'Min (S'Length, WS'Length);
   begin
      WL := Len;
      for I in 1 .. Len loop
         WS (WS'First + I - 1) := Wide_Character'Val (Character'Pos (S (S'First + I - 1)));
      end loop;
   end String_To_Wide_String;

   ---------------------------------
   -- String_To_Wide_Wide_String --
   ---------------------------------

   procedure String_To_Wide_Wide_String
     (S   : String;
      WWS : out Wide_Wide_String;
      WWL : out Natural)
   is
      Len : constant Natural := Natural'Min (S'Length, WWS'Length);
   begin
      WWL := Len;
      for I in 1 .. Len loop
         WWS (WWS'First + I - 1) := Wide_Wide_Character'Val (Character'Pos (S (S'First + I - 1)));
      end loop;
   end String_To_Wide_Wide_String;

end System.Wch_StW;
