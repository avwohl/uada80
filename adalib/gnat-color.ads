-- GNAT.Color for Z80
-- Color manipulation utilities

package GNAT.Color is
   pragma Pure;

   -- Basic color types
   subtype Color_Component is Natural range 0 .. 255;

   type RGB is record
      R : Color_Component;
      G : Color_Component;
      B : Color_Component;
   end record;

   type RGBA is record
      R : Color_Component;
      G : Color_Component;
      B : Color_Component;
      A : Color_Component;  -- Alpha (0=transparent, 255=opaque)
   end record;

   -- HSV representation (H=0-360, S=0-100, V=0-100)
   type HSV is record
      H : Natural range 0 .. 360;
      S : Natural range 0 .. 100;
      V : Natural range 0 .. 100;
   end record;

   -- Grayscale
   subtype Grayscale is Color_Component;

   -- Predefined colors
   Black   : constant RGB := (0, 0, 0);
   White   : constant RGB := (255, 255, 255);
   Red     : constant RGB := (255, 0, 0);
   Green   : constant RGB := (0, 255, 0);
   Blue    : constant RGB := (0, 0, 255);
   Yellow  : constant RGB := (255, 255, 0);
   Cyan    : constant RGB := (0, 255, 255);
   Magenta : constant RGB := (255, 0, 255);
   Gray    : constant RGB := (128, 128, 128);
   Orange  : constant RGB := (255, 165, 0);
   Purple  : constant RGB := (128, 0, 128);
   Brown   : constant RGB := (139, 69, 19);
   Pink    : constant RGB := (255, 192, 203);

   -- Constructors
   function Make_RGB (R, G, B : Color_Component) return RGB;
   function Make_RGBA (R, G, B, A : Color_Component) return RGBA;
   function Make_RGBA (Color : RGB; A : Color_Component := 255) return RGBA;
   function Make_Gray (Level : Grayscale) return RGB;

   -- Conversion
   function To_RGB (Color : RGBA) return RGB;
   function To_RGBA (Color : RGB) return RGBA;
   function RGB_To_HSV (Color : RGB) return HSV;
   function HSV_To_RGB (Color : HSV) return RGB;
   function To_Grayscale (Color : RGB) return Grayscale;

   -- Color manipulation
   function Invert (Color : RGB) return RGB;
   function Lighten (Color : RGB; Amount : Natural) return RGB;  -- 0-100
   function Darken (Color : RGB; Amount : Natural) return RGB;   -- 0-100
   function Saturate (Color : RGB; Amount : Natural) return RGB;
   function Desaturate (Color : RGB; Amount : Natural) return RGB;

   -- Blending (Amount is 0-100, where 100 is fully Color2)
   function Blend (Color1, Color2 : RGB; Amount : Natural) return RGB;
   function Blend_Alpha (Base, Overlay : RGBA) return RGBA;

   -- Color arithmetic
   function Add (Color1, Color2 : RGB) return RGB;  -- Clamped
   function Subtract (Color1, Color2 : RGB) return RGB;  -- Clamped
   function Multiply (Color : RGB; Factor : Natural) return RGB;  -- Factor/100
   function Screen (Color1, Color2 : RGB) return RGB;

   -- Color comparison
   function "=" (A, B : RGB) return Boolean;
   function Similar (Color1, Color2 : RGB; Tolerance : Natural := 10) return Boolean;
   function Distance (Color1, Color2 : RGB) return Natural;

   -- Component access
   function Red_Component (Color : RGB) return Color_Component;
   function Green_Component (Color : RGB) return Color_Component;
   function Blue_Component (Color : RGB) return Color_Component;
   function Luminance (Color : RGB) return Color_Component;

   -- Hue operations
   function Shift_Hue (Color : RGB; Degrees : Integer) return RGB;
   function Complement (Color : RGB) return RGB;  -- Opposite on color wheel

   -- CGA/EGA palette support (for retro systems)
   type CGA_Color is range 0 .. 15;
   type EGA_Color is range 0 .. 63;

   function CGA_To_RGB (Color : CGA_Color) return RGB;
   function RGB_To_CGA (Color : RGB) return CGA_Color;  -- Nearest match
   function EGA_To_RGB (Color : EGA_Color) return RGB;
   function RGB_To_EGA (Color : RGB) return EGA_Color;  -- Nearest match

   -- Packed color formats
   function To_RGB332 (Color : RGB) return Natural;  -- 8-bit packed
   function From_RGB332 (Packed : Natural) return RGB;
   function To_RGB565 (Color : RGB) return Natural;  -- 16-bit packed
   function From_RGB565 (Packed : Natural) return RGB;

   -- String conversion
   function To_Hex (Color : RGB) return String;  -- #RRGGBB format
   function From_Hex (S : String) return RGB;    -- Parse #RRGGBB

   -- Color names (basic)
   function Color_Name (Color : RGB) return String;
   function Named_Color (Name : String) return RGB;

end GNAT.Color;
