-- GNAT.Byte_Order_Mark for Z80
-- Unicode Byte Order Mark detection

package GNAT.Byte_Order_Mark is
   pragma Pure;

   type BOM_Kind is (
      Unknown,        -- No BOM or unrecognized
      UTF8_All,       -- UTF-8 (EF BB BF)
      UTF16_LE,       -- UTF-16 Little Endian (FF FE)
      UTF16_BE,       -- UTF-16 Big Endian (FE FF)
      UTF32_LE,       -- UTF-32 Little Endian (FF FE 00 00)
      UTF32_BE);      -- UTF-32 Big Endian (00 00 FE FF)

   -- Detect BOM in string
   procedure Read_BOM
     (Str         : String;
      Len         : out Natural;
      BOM         : out BOM_Kind;
      XML_Support : Boolean := True);

   -- Get BOM length
   function BOM_Length (BOM : BOM_Kind) return Natural;

end GNAT.Byte_Order_Mark;
