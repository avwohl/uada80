-- Ada.Strings.UTF_Encoding for Z80
-- UTF encoding conversion utilities

package Ada.Strings.UTF_Encoding is
   pragma Preelaborate;

   -- Basic types
   subtype UTF_String is String;
   subtype UTF_8_String is String;
   subtype UTF_16_Wide_String is Wide_String;

   type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE);

   Encoding_Error : exception;

   -- BOM (Byte Order Mark) constants
   BOM_8    : constant UTF_8_String := Character'Val (16#EF#) &
                                       Character'Val (16#BB#) &
                                       Character'Val (16#BF#);

   BOM_16BE : constant UTF_String := Character'Val (16#FE#) &
                                     Character'Val (16#FF#);

   BOM_16LE : constant UTF_String := Character'Val (16#FF#) &
                                     Character'Val (16#FE#);

   function Encoding
     (Item    : UTF_String;
      Default : Encoding_Scheme := UTF_8) return Encoding_Scheme;

   -- Conversion functions (simplified for Z80 - ASCII subset)
   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_String;

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_16_Wide_String;

   function Convert
     (Item       : UTF_16_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False) return UTF_String;

   function Convert
     (Item       : UTF_16_Wide_String;
      Output_BOM : Boolean := False) return UTF_8_String;

end Ada.Strings.UTF_Encoding;
