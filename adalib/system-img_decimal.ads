-- System.Img_Decimal for Z80
-- Decimal fixed point image (string conversion)

package System.Img_Decimal is
   pragma Pure;

   -- Convert decimal fixed point to string
   procedure Image_Decimal
     (V     : Long_Long_Integer;
      S     : in Out String;
      P     : out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural);
   -- V is the scaled value
   -- Scale is decimal places in the type
   -- Fore, Aft, Exp control formatting

   procedure Set_Image_Decimal
     (V     : Long_Long_Integer;
      S     : in Out String;
      P     : in Out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural);

end System.Img_Decimal;
