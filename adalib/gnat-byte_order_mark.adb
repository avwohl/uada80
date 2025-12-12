-- GNAT.Byte_Order_Mark body for Z80
-- Unicode Byte Order Mark detection implementation

package body GNAT.Byte_Order_Mark is

   --------------
   -- Read_BOM --
   --------------

   procedure Read_BOM
     (Str         : String;
      Len         : out Natural;
      BOM         : out BOM_Kind;
      XML_Support : Boolean := True)
   is
      pragma Unreferenced (XML_Support);
   begin
      Len := 0;
      BOM := Unknown;

      if Str'Length < 2 then
         return;
      end if;

      -- Check for UTF-32 LE (FF FE 00 00)
      if Str'Length >= 4 and then
         Character'Pos (Str (Str'First)) = 16#FF# and then
         Character'Pos (Str (Str'First + 1)) = 16#FE# and then
         Character'Pos (Str (Str'First + 2)) = 16#00# and then
         Character'Pos (Str (Str'First + 3)) = 16#00#
      then
         BOM := UTF32_LE;
         Len := 4;
         return;
      end if;

      -- Check for UTF-32 BE (00 00 FE FF)
      if Str'Length >= 4 and then
         Character'Pos (Str (Str'First)) = 16#00# and then
         Character'Pos (Str (Str'First + 1)) = 16#00# and then
         Character'Pos (Str (Str'First + 2)) = 16#FE# and then
         Character'Pos (Str (Str'First + 3)) = 16#FF#
      then
         BOM := UTF32_BE;
         Len := 4;
         return;
      end if;

      -- Check for UTF-8 (EF BB BF)
      if Str'Length >= 3 and then
         Character'Pos (Str (Str'First)) = 16#EF# and then
         Character'Pos (Str (Str'First + 1)) = 16#BB# and then
         Character'Pos (Str (Str'First + 2)) = 16#BF#
      then
         BOM := UTF8_All;
         Len := 3;
         return;
      end if;

      -- Check for UTF-16 LE (FF FE)
      if Character'Pos (Str (Str'First)) = 16#FF# and then
         Character'Pos (Str (Str'First + 1)) = 16#FE#
      then
         BOM := UTF16_LE;
         Len := 2;
         return;
      end if;

      -- Check for UTF-16 BE (FE FF)
      if Character'Pos (Str (Str'First)) = 16#FE# and then
         Character'Pos (Str (Str'First + 1)) = 16#FF#
      then
         BOM := UTF16_BE;
         Len := 2;
         return;
      end if;
   end Read_BOM;

   ----------------
   -- BOM_Length --
   ----------------

   function BOM_Length (BOM : BOM_Kind) return Natural is
   begin
      case BOM is
         when Unknown   => return 0;
         when UTF8_All  => return 3;
         when UTF16_LE  => return 2;
         when UTF16_BE  => return 2;
         when UTF32_LE  => return 4;
         when UTF32_BE  => return 4;
      end case;
   end BOM_Length;

end GNAT.Byte_Order_Mark;
