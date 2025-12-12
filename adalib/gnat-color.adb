-- GNAT.Color body for Z80
-- Color manipulation implementation

package body GNAT.Color is

   function Clamp (V : Integer) return Color_Component is
   begin
      if V < 0 then
         return 0;
      elsif V > 255 then
         return 255;
      else
         return Color_Component (V);
      end if;
   end Clamp;

   function Max3 (A, B, C : Natural) return Natural is
   begin
      if A >= B and A >= C then
         return A;
      elsif B >= C then
         return B;
      else
         return C;
      end if;
   end Max3;

   function Min3 (A, B, C : Natural) return Natural is
   begin
      if A <= B and A <= C then
         return A;
      elsif B <= C then
         return B;
      else
         return C;
      end if;
   end Min3;

   --------------
   -- Make_RGB --
   --------------

   function Make_RGB (R, G, B : Color_Component) return RGB is
   begin
      return (R => R, G => G, B => B);
   end Make_RGB;

   ---------------
   -- Make_RGBA --
   ---------------

   function Make_RGBA (R, G, B, A : Color_Component) return RGBA is
   begin
      return (R => R, G => G, B => B, A => A);
   end Make_RGBA;

   function Make_RGBA (Color : RGB; A : Color_Component := 255) return RGBA is
   begin
      return (R => Color.R, G => Color.G, B => Color.B, A => A);
   end Make_RGBA;

   ---------------
   -- Make_Gray --
   ---------------

   function Make_Gray (Level : Grayscale) return RGB is
   begin
      return (R => Level, G => Level, B => Level);
   end Make_Gray;

   ------------
   -- To_RGB --
   ------------

   function To_RGB (Color : RGBA) return RGB is
   begin
      return (R => Color.R, G => Color.G, B => Color.B);
   end To_RGB;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA (Color : RGB) return RGBA is
   begin
      return (R => Color.R, G => Color.G, B => Color.B, A => 255);
   end To_RGBA;

   ----------------
   -- RGB_To_HSV --
   ----------------

   function RGB_To_HSV (Color : RGB) return HSV is
      R : constant Natural := Natural (Color.R);
      G : constant Natural := Natural (Color.G);
      B : constant Natural := Natural (Color.B);
      Max_Val : constant Natural := Max3 (R, G, B);
      Min_Val : constant Natural := Min3 (R, G, B);
      Delta   : constant Natural := Max_Val - Min_Val;
      H, S, V : Natural;
   begin
      V := (Max_Val * 100) / 255;

      if Max_Val = 0 then
         S := 0;
      else
         S := (Delta * 100) / Max_Val;
      end if;

      if Delta = 0 then
         H := 0;
      elsif Max_Val = R then
         H := (60 * (Integer (G) - Integer (B))) / Integer (Delta);
         if H < 0 then
            H := H + 360;
         end if;
      elsif Max_Val = G then
         H := 120 + (60 * (Integer (B) - Integer (R))) / Integer (Delta);
      else
         H := 240 + (60 * (Integer (R) - Integer (G))) / Integer (Delta);
      end if;

      if H < 0 then
         H := H + 360;
      end if;

      return (H => H mod 360, S => S, V => V);
   end RGB_To_HSV;

   ----------------
   -- HSV_To_RGB --
   ----------------

   function HSV_To_RGB (Color : HSV) return RGB is
      H : constant Natural := Color.H;
      S : constant Natural := Color.S;
      V : constant Natural := Color.V;
      C : Natural;
      X : Natural;
      M : Natural;
      R, G, B : Natural;
   begin
      C := (V * S) / 100;
      X := C * (60 - abs ((H mod 120) - 60)) / 60;
      M := V - C;

      if H < 60 then
         R := C; G := X; B := 0;
      elsif H < 120 then
         R := X; G := C; B := 0;
      elsif H < 180 then
         R := 0; G := C; B := X;
      elsif H < 240 then
         R := 0; G := X; B := C;
      elsif H < 300 then
         R := X; G := 0; B := C;
      else
         R := C; G := 0; B := X;
      end if;

      return (R => Color_Component (((R + M) * 255) / 100),
              G => Color_Component (((G + M) * 255) / 100),
              B => Color_Component (((B + M) * 255) / 100));
   end HSV_To_RGB;

   ------------------
   -- To_Grayscale --
   ------------------

   function To_Grayscale (Color : RGB) return Grayscale is
   begin
      -- Standard luminance formula: 0.299R + 0.587G + 0.114B
      return Grayscale ((Natural (Color.R) * 299 +
                         Natural (Color.G) * 587 +
                         Natural (Color.B) * 114) / 1000);
   end To_Grayscale;

   ------------
   -- Invert --
   ------------

   function Invert (Color : RGB) return RGB is
   begin
      return (R => 255 - Color.R, G => 255 - Color.G, B => 255 - Color.B);
   end Invert;

   -------------
   -- Lighten --
   -------------

   function Lighten (Color : RGB; Amount : Natural) return RGB is
   begin
      return Blend (Color, White, Amount);
   end Lighten;

   ------------
   -- Darken --
   ------------

   function Darken (Color : RGB; Amount : Natural) return RGB is
   begin
      return Blend (Color, Black, Amount);
   end Darken;

   --------------
   -- Saturate --
   --------------

   function Saturate (Color : RGB; Amount : Natural) return RGB is
      H : HSV := RGB_To_HSV (Color);
   begin
      H.S := Natural'Min (100, H.S + Amount);
      return HSV_To_RGB (H);
   end Saturate;

   ----------------
   -- Desaturate --
   ----------------

   function Desaturate (Color : RGB; Amount : Natural) return RGB is
      H : HSV := RGB_To_HSV (Color);
   begin
      if H.S > Amount then
         H.S := H.S - Amount;
      else
         H.S := 0;
      end if;
      return HSV_To_RGB (H);
   end Desaturate;

   -----------
   -- Blend --
   -----------

   function Blend (Color1, Color2 : RGB; Amount : Natural) return RGB is
      A1 : constant Natural := 100 - Amount;
      A2 : constant Natural := Amount;
   begin
      return (R => Color_Component ((Natural (Color1.R) * A1 + Natural (Color2.R) * A2) / 100),
              G => Color_Component ((Natural (Color1.G) * A1 + Natural (Color2.G) * A2) / 100),
              B => Color_Component ((Natural (Color1.B) * A1 + Natural (Color2.B) * A2) / 100));
   end Blend;

   -----------------
   -- Blend_Alpha --
   -----------------

   function Blend_Alpha (Base, Overlay : RGBA) return RGBA is
      OA : constant Natural := Natural (Overlay.A);
      BA : constant Natural := 255 - OA;
   begin
      return (R => Color_Component ((Natural (Overlay.R) * OA + Natural (Base.R) * BA) / 255),
              G => Color_Component ((Natural (Overlay.G) * OA + Natural (Base.G) * BA) / 255),
              B => Color_Component ((Natural (Overlay.B) * OA + Natural (Base.B) * BA) / 255),
              A => Color_Component (OA + (Natural (Base.A) * BA) / 255));
   end Blend_Alpha;

   ---------
   -- Add --
   ---------

   function Add (Color1, Color2 : RGB) return RGB is
   begin
      return (R => Clamp (Integer (Color1.R) + Integer (Color2.R)),
              G => Clamp (Integer (Color1.G) + Integer (Color2.G)),
              B => Clamp (Integer (Color1.B) + Integer (Color2.B)));
   end Add;

   --------------
   -- Subtract --
   --------------

   function Subtract (Color1, Color2 : RGB) return RGB is
   begin
      return (R => Clamp (Integer (Color1.R) - Integer (Color2.R)),
              G => Clamp (Integer (Color1.G) - Integer (Color2.G)),
              B => Clamp (Integer (Color1.B) - Integer (Color2.B)));
   end Subtract;

   --------------
   -- Multiply --
   --------------

   function Multiply (Color : RGB; Factor : Natural) return RGB is
   begin
      return (R => Clamp ((Integer (Color.R) * Integer (Factor)) / 100),
              G => Clamp ((Integer (Color.G) * Integer (Factor)) / 100),
              B => Clamp ((Integer (Color.B) * Integer (Factor)) / 100));
   end Multiply;

   ------------
   -- Screen --
   ------------

   function Screen (Color1, Color2 : RGB) return RGB is
   begin
      return Invert (Add (Invert (Color1), Invert (Color2)));
   end Screen;

   ---------
   -- "=" --
   ---------

   function "=" (A, B : RGB) return Boolean is
   begin
      return A.R = B.R and A.G = B.G and A.B = B.B;
   end "=";

   -------------
   -- Similar --
   -------------

   function Similar (Color1, Color2 : RGB; Tolerance : Natural := 10) return Boolean is
   begin
      return abs (Integer (Color1.R) - Integer (Color2.R)) <= Integer (Tolerance) and
             abs (Integer (Color1.G) - Integer (Color2.G)) <= Integer (Tolerance) and
             abs (Integer (Color1.B) - Integer (Color2.B)) <= Integer (Tolerance);
   end Similar;

   --------------
   -- Distance --
   --------------

   function Distance (Color1, Color2 : RGB) return Natural is
      DR : constant Integer := Integer (Color1.R) - Integer (Color2.R);
      DG : constant Integer := Integer (Color1.G) - Integer (Color2.G);
      DB : constant Integer := Integer (Color1.B) - Integer (Color2.B);
   begin
      return Natural (abs DR + abs DG + abs DB);
   end Distance;

   -------------------
   -- Red_Component --
   -------------------

   function Red_Component (Color : RGB) return Color_Component is
   begin
      return Color.R;
   end Red_Component;

   ---------------------
   -- Green_Component --
   ---------------------

   function Green_Component (Color : RGB) return Color_Component is
   begin
      return Color.G;
   end Green_Component;

   --------------------
   -- Blue_Component --
   --------------------

   function Blue_Component (Color : RGB) return Color_Component is
   begin
      return Color.B;
   end Blue_Component;

   ---------------
   -- Luminance --
   ---------------

   function Luminance (Color : RGB) return Color_Component is
   begin
      return To_Grayscale (Color);
   end Luminance;

   ---------------
   -- Shift_Hue --
   ---------------

   function Shift_Hue (Color : RGB; Degrees : Integer) return RGB is
      H : HSV := RGB_To_HSV (Color);
   begin
      H.H := (H.H + Degrees + 360) mod 360;
      return HSV_To_RGB (H);
   end Shift_Hue;

   ----------------
   -- Complement --
   ----------------

   function Complement (Color : RGB) return RGB is
   begin
      return Shift_Hue (Color, 180);
   end Complement;

   ----------------
   -- CGA_To_RGB --
   ----------------

   function CGA_To_RGB (Color : CGA_Color) return RGB is
   begin
      case Color is
         when 0  => return (0, 0, 0);        -- Black
         when 1  => return (0, 0, 170);      -- Blue
         when 2  => return (0, 170, 0);      -- Green
         when 3  => return (0, 170, 170);    -- Cyan
         when 4  => return (170, 0, 0);      -- Red
         when 5  => return (170, 0, 170);    -- Magenta
         when 6  => return (170, 85, 0);     -- Brown
         when 7  => return (170, 170, 170);  -- Light Gray
         when 8  => return (85, 85, 85);     -- Dark Gray
         when 9  => return (85, 85, 255);    -- Light Blue
         when 10 => return (85, 255, 85);    -- Light Green
         when 11 => return (85, 255, 255);   -- Light Cyan
         when 12 => return (255, 85, 85);    -- Light Red
         when 13 => return (255, 85, 255);   -- Light Magenta
         when 14 => return (255, 255, 85);   -- Yellow
         when 15 => return (255, 255, 255);  -- White
      end case;
   end CGA_To_RGB;

   ----------------
   -- RGB_To_CGA --
   ----------------

   function RGB_To_CGA (Color : RGB) return CGA_Color is
      Best : CGA_Color := 0;
      Best_Dist : Natural := Natural'Last;
      D : Natural;
   begin
      for I in CGA_Color loop
         D := Distance (Color, CGA_To_RGB (I));
         if D < Best_Dist then
            Best_Dist := D;
            Best := I;
         end if;
      end loop;
      return Best;
   end RGB_To_CGA;

   ----------------
   -- EGA_To_RGB --
   ----------------

   function EGA_To_RGB (Color : EGA_Color) return RGB is
      R : constant Natural := ((Natural (Color) / 4) mod 4) * 85;
      G : constant Natural := ((Natural (Color) / 16) mod 4) * 85;
      B : constant Natural := (Natural (Color) mod 4) * 85;
   begin
      return (Color_Component (R), Color_Component (G), Color_Component (B));
   end EGA_To_RGB;

   ----------------
   -- RGB_To_EGA --
   ----------------

   function RGB_To_EGA (Color : RGB) return EGA_Color is
      R : constant Natural := Natural (Color.R) / 64;
      G : constant Natural := Natural (Color.G) / 64;
      B : constant Natural := Natural (Color.B) / 64;
   begin
      return EGA_Color (R * 4 + G * 16 + B);
   end RGB_To_EGA;

   ----------------
   -- To_RGB332 --
   ----------------

   function To_RGB332 (Color : RGB) return Natural is
   begin
      return (Natural (Color.R) / 32) * 32 +
             (Natural (Color.G) / 32) * 4 +
             Natural (Color.B) / 64;
   end To_RGB332;

   ------------------
   -- From_RGB332 --
   ------------------

   function From_RGB332 (Packed : Natural) return RGB is
   begin
      return (R => Color_Component (((Packed / 32) mod 8) * 36),
              G => Color_Component (((Packed / 4) mod 8) * 36),
              B => Color_Component ((Packed mod 4) * 85));
   end From_RGB332;

   ----------------
   -- To_RGB565 --
   ----------------

   function To_RGB565 (Color : RGB) return Natural is
   begin
      return (Natural (Color.R) / 8) * 2048 +
             (Natural (Color.G) / 4) * 32 +
             Natural (Color.B) / 8;
   end To_RGB565;

   ------------------
   -- From_RGB565 --
   ------------------

   function From_RGB565 (Packed : Natural) return RGB is
   begin
      return (R => Color_Component (((Packed / 2048) mod 32) * 8),
              G => Color_Component (((Packed / 32) mod 64) * 4),
              B => Color_Component ((Packed mod 32) * 8));
   end From_RGB565;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Color : RGB) return String is
      Hex : constant String := "0123456789ABCDEF";
   begin
      return '#' &
             Hex (Natural (Color.R) / 16 + 1) & Hex (Natural (Color.R) mod 16 + 1) &
             Hex (Natural (Color.G) / 16 + 1) & Hex (Natural (Color.G) mod 16 + 1) &
             Hex (Natural (Color.B) / 16 + 1) & Hex (Natural (Color.B) mod 16 + 1);
   end To_Hex;

   --------------
   -- From_Hex --
   --------------

   function From_Hex (S : String) return RGB is
      function Hex_Val (C : Character) return Natural is
      begin
         if C >= '0' and C <= '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C >= 'A' and C <= 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         elsif C >= 'a' and C <= 'f' then
            return Character'Pos (C) - Character'Pos ('a') + 10;
         else
            return 0;
         end if;
      end Hex_Val;

      Start : Natural := S'First;
   begin
      if S'Length > 0 and then S (S'First) = '#' then
         Start := S'First + 1;
      end if;

      if S'Last - Start + 1 >= 6 then
         return (R => Color_Component (Hex_Val (S (Start)) * 16 + Hex_Val (S (Start + 1))),
                 G => Color_Component (Hex_Val (S (Start + 2)) * 16 + Hex_Val (S (Start + 3))),
                 B => Color_Component (Hex_Val (S (Start + 4)) * 16 + Hex_Val (S (Start + 5))));
      else
         return Black;
      end if;
   end From_Hex;

   ----------------
   -- Color_Name --
   ----------------

   function Color_Name (Color : RGB) return String is
   begin
      if Color = Black then return "Black";
      elsif Color = White then return "White";
      elsif Color = Red then return "Red";
      elsif Color = Green then return "Green";
      elsif Color = Blue then return "Blue";
      elsif Color = Yellow then return "Yellow";
      elsif Color = Cyan then return "Cyan";
      elsif Color = Magenta then return "Magenta";
      elsif Color = Gray then return "Gray";
      else return To_Hex (Color);
      end if;
   end Color_Name;

   -----------------
   -- Named_Color --
   -----------------

   function Named_Color (Name : String) return RGB is
      function To_Upper (C : Character) return Character is
      begin
         if C >= 'a' and C <= 'z' then
            return Character'Val (Character'Pos (C) - 32);
         else
            return C;
         end if;
      end To_Upper;

      Upper : String (Name'Range);
   begin
      for I in Name'Range loop
         Upper (I) := To_Upper (Name (I));
      end loop;

      if Upper = "BLACK" then return Black;
      elsif Upper = "WHITE" then return White;
      elsif Upper = "RED" then return Red;
      elsif Upper = "GREEN" then return Green;
      elsif Upper = "BLUE" then return Blue;
      elsif Upper = "YELLOW" then return Yellow;
      elsif Upper = "CYAN" then return Cyan;
      elsif Upper = "MAGENTA" then return Magenta;
      elsif Upper = "GRAY" or Upper = "GREY" then return Gray;
      elsif Upper = "ORANGE" then return Orange;
      elsif Upper = "PURPLE" then return Purple;
      elsif Upper = "BROWN" then return Brown;
      elsif Upper = "PINK" then return Pink;
      else return Black;
      end if;
   end Named_Color;

end GNAT.Color;
