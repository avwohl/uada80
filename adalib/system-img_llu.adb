-- System.Img_LLU body for Z80
-- Long_Long_Unsigned image implementation

package body System.Img_LLU is

   Digits : constant String := "0123456789ABCDEF";

   --------------------------------
   -- Image_Long_Long_Unsigned --
   --------------------------------

   procedure Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in Out String;
      P : out Natural)
   is
      Pos : Natural := S'First;
   begin
      -- Leading space for standard Image format
      S (Pos) := ' ';
      Pos := Pos + 1;
      Set_Image_Long_Long_Unsigned (V, S, Pos, 10);
      P := Pos;
   end Image_Long_Long_Unsigned;

   ------------------------------------
   -- Set_Image_Long_Long_Unsigned --
   ------------------------------------

   procedure Set_Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in Out String;
      P : in Out Natural;
      Base : Natural := 10)
   is
      Buf : String (1 .. 64);
      Len : Natural := 0;
      Val : Long_Long_Unsigned := V;
      B   : constant Long_Long_Unsigned := Long_Long_Unsigned (Base);
   begin
      if Val = 0 then
         S (P) := '0';
         P := P + 1;
         return;
      end if;

      -- Build digits in reverse
      while Val > 0 loop
         Len := Len + 1;
         Buf (Len) := Digits (Natural (Val mod B) + 1);
         Val := Val / B;
      end loop;

      -- Copy in correct order
      for I in reverse 1 .. Len loop
         S (P) := Buf (I);
         P := P + 1;
      end loop;

      P := P - 1;  -- Point to last written
   end Set_Image_Long_Long_Unsigned;

end System.Img_LLU;
