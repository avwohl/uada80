-- System.Wch_WtS body for Z80
-- Wide string to string conversion implementation

package body System.Wch_WtS is

   ----------------------------
   -- Wide_String_To_String --
   ----------------------------

   procedure Wide_String_To_String
     (WS : Wide_String;
      S  : out String;
      SL : out Natural)
   is
      Len : constant Natural := Natural'Min (WS'Length, S'Length);
      Code : Natural;
   begin
      SL := Len;
      for I in 1 .. Len loop
         Code := Wide_Character'Pos (WS (WS'First + I - 1));
         if Code < 256 then
            S (S'First + I - 1) := Character'Val (Code);
         else
            S (S'First + I - 1) := '?';  -- Replacement for non-Latin1
         end if;
      end loop;
   end Wide_String_To_String;

   ---------------------------------
   -- Wide_Wide_String_To_String --
   ---------------------------------

   procedure Wide_Wide_String_To_String
     (WWS : Wide_Wide_String;
      S   : out String;
      SL  : out Natural)
   is
      Len : constant Natural := Natural'Min (WWS'Length, S'Length);
      Code : Natural;
   begin
      SL := Len;
      for I in 1 .. Len loop
         Code := Wide_Wide_Character'Pos (WWS (WWS'First + I - 1));
         if Code < 256 then
            S (S'First + I - 1) := Character'Val (Code);
         else
            S (S'First + I - 1) := '?';
         end if;
      end loop;
   end Wide_Wide_String_To_String;

end System.Wch_WtS;
