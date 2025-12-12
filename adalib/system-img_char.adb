-- System.Img_Char body for Z80
-- Character image implementation

package body System.Img_Char is

   ---------------------
   -- Image_Character --
   ---------------------

   procedure Image_Character
     (V : Character;
      S : in Out String;
      P : out Natural)
   is
      Pos : constant Natural := Character'Pos (V);
   begin
      if Pos < 32 then
         -- Control characters
         case V is
            when ASCII.NUL => S (S'First .. S'First + 2) := "NUL"; P := S'First + 2;
            when ASCII.SOH => S (S'First .. S'First + 2) := "SOH"; P := S'First + 2;
            when ASCII.STX => S (S'First .. S'First + 2) := "STX"; P := S'First + 2;
            when ASCII.ETX => S (S'First .. S'First + 2) := "ETX"; P := S'First + 2;
            when ASCII.EOT => S (S'First .. S'First + 2) := "EOT"; P := S'First + 2;
            when ASCII.ENQ => S (S'First .. S'First + 2) := "ENQ"; P := S'First + 2;
            when ASCII.ACK => S (S'First .. S'First + 2) := "ACK"; P := S'First + 2;
            when ASCII.BEL => S (S'First .. S'First + 2) := "BEL"; P := S'First + 2;
            when ASCII.BS  => S (S'First .. S'First + 1) := "BS"; P := S'First + 1;
            when ASCII.HT  => S (S'First .. S'First + 1) := "HT"; P := S'First + 1;
            when ASCII.LF  => S (S'First .. S'First + 1) := "LF"; P := S'First + 1;
            when ASCII.VT  => S (S'First .. S'First + 1) := "VT"; P := S'First + 1;
            when ASCII.FF  => S (S'First .. S'First + 1) := "FF"; P := S'First + 1;
            when ASCII.CR  => S (S'First .. S'First + 1) := "CR"; P := S'First + 1;
            when ASCII.SO  => S (S'First .. S'First + 1) := "SO"; P := S'First + 1;
            when ASCII.SI  => S (S'First .. S'First + 1) := "SI"; P := S'First + 1;
            when ASCII.DLE => S (S'First .. S'First + 2) := "DLE"; P := S'First + 2;
            when ASCII.DC1 => S (S'First .. S'First + 2) := "DC1"; P := S'First + 2;
            when ASCII.DC2 => S (S'First .. S'First + 2) := "DC2"; P := S'First + 2;
            when ASCII.DC3 => S (S'First .. S'First + 2) := "DC3"; P := S'First + 2;
            when ASCII.DC4 => S (S'First .. S'First + 2) := "DC4"; P := S'First + 2;
            when ASCII.NAK => S (S'First .. S'First + 2) := "NAK"; P := S'First + 2;
            when ASCII.SYN => S (S'First .. S'First + 2) := "SYN"; P := S'First + 2;
            when ASCII.ETB => S (S'First .. S'First + 2) := "ETB"; P := S'First + 2;
            when ASCII.CAN => S (S'First .. S'First + 2) := "CAN"; P := S'First + 2;
            when ASCII.EM  => S (S'First .. S'First + 1) := "EM"; P := S'First + 1;
            when ASCII.SUB => S (S'First .. S'First + 2) := "SUB"; P := S'First + 2;
            when ASCII.ESC => S (S'First .. S'First + 2) := "ESC"; P := S'First + 2;
            when ASCII.FS  => S (S'First .. S'First + 1) := "FS"; P := S'First + 1;
            when ASCII.GS  => S (S'First .. S'First + 1) := "GS"; P := S'First + 1;
            when ASCII.RS  => S (S'First .. S'First + 1) := "RS"; P := S'First + 1;
            when ASCII.US  => S (S'First .. S'First + 1) := "US"; P := S'First + 1;
            when others    => S (S'First .. S'First + 2) := "???"; P := S'First + 2;
         end case;
      elsif Pos = 127 then
         -- DEL
         S (S'First .. S'First + 2) := "DEL";
         P := S'First + 2;
      elsif Pos > 127 and Pos < 160 then
         -- Extended control characters (C1)
         S (S'First .. S'First + 3) := "C1_" & Character'Val (48 + (Pos mod 10));
         P := S'First + 3;
      else
         -- Graphic characters: 'X' form
         S (S'First) := ''';
         S (S'First + 1) := V;
         S (S'First + 2) := ''';
         P := S'First + 2;
      end if;
   end Image_Character;

end System.Img_Char;
