-- Ada.Strings.UTF_Encoding body for Z80
-- UTF encoding conversion implementation (simplified for ASCII subset)

package body Ada.Strings.UTF_Encoding is

   --------------
   -- Encoding --
   --------------

   function Encoding
     (Item    : UTF_String;
      Default : Encoding_Scheme := UTF_8) return Encoding_Scheme
   is
   begin
      if Item'Length >= 3 then
         if Item (Item'First .. Item'First + 2) = BOM_8 then
            return UTF_8;
         end if;
      end if;

      if Item'Length >= 2 then
         if Item (Item'First .. Item'First + 1) = BOM_16BE then
            return UTF_16BE;
         elsif Item (Item'First .. Item'First + 1) = BOM_16LE then
            return UTF_16LE;
         end if;
      end if;

      return Default;
   end Encoding;

   -------------
   -- Convert --
   -------------

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_String
   is
      Start : Natural := Item'First;
      Result : UTF_String (1 .. Item'Length + 3);
      Len : Natural := 0;
   begin
      -- Skip input BOM if present
      if Input_Scheme = UTF_8 and then Item'Length >= 3 then
         if Item (Item'First .. Item'First + 2) = BOM_8 then
            Start := Item'First + 3;
         end if;
      elsif Item'Length >= 2 then
         if (Input_Scheme = UTF_16BE and then Item (Item'First .. Item'First + 1) = BOM_16BE) or
            (Input_Scheme = UTF_16LE and then Item (Item'First .. Item'First + 1) = BOM_16LE)
         then
            Start := Item'First + 2;
         end if;
      end if;

      -- Add output BOM if requested
      if Output_BOM then
         case Output_Scheme is
            when UTF_8 =>
               Result (1 .. 3) := BOM_8;
               Len := 3;
            when UTF_16BE =>
               Result (1 .. 2) := BOM_16BE;
               Len := 2;
            when UTF_16LE =>
               Result (1 .. 2) := BOM_16LE;
               Len := 2;
         end case;
      end if;

      -- For Z80, simplified: just copy ASCII characters
      for I in Start .. Item'Last loop
         if Character'Pos (Item (I)) <= 127 then
            Len := Len + 1;
            Result (Len) := Item (I);
         end if;
      end loop;

      return Result (1 .. Len);
   end Convert;

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_16_Wide_String
   is
      pragma Unreferenced (Input_Scheme);
      Start : Natural := Item'First;
      Result : UTF_16_Wide_String (1 .. Item'Length + 1);
      Len : Natural := 0;
   begin
      -- Skip BOM if present
      if Item'Length >= 3 and then Item (Item'First .. Item'First + 2) = BOM_8 then
         Start := Item'First + 3;
      elsif Item'Length >= 2 and then
            (Item (Item'First .. Item'First + 1) = BOM_16BE or
             Item (Item'First .. Item'First + 1) = BOM_16LE)
      then
         Start := Item'First + 2;
      end if;

      -- Add output BOM if requested
      if Output_BOM then
         Len := 1;
         Result (1) := Wide_Character'Val (16#FEFF#);
      end if;

      -- Convert ASCII to Wide_Character
      for I in Start .. Item'Last loop
         if Character'Pos (Item (I)) <= 127 then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (Character'Pos (Item (I)));
         end if;
      end loop;

      return Result (1 .. Len);
   end Convert;

   function Convert
     (Item       : UTF_16_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False) return UTF_String
   is
      Start : Natural := Item'First;
      Result : UTF_String (1 .. Item'Length * 3 + 3);
      Len : Natural := 0;
   begin
      -- Skip BOM if present
      if Item'Length >= 1 and then Wide_Character'Pos (Item (Item'First)) = 16#FEFF# then
         Start := Item'First + 1;
      end if;

      -- Add output BOM if requested
      if Output_BOM then
         case Output_Scheme is
            when UTF_8 =>
               Result (1 .. 3) := BOM_8;
               Len := 3;
            when UTF_16BE =>
               Result (1 .. 2) := BOM_16BE;
               Len := 2;
            when UTF_16LE =>
               Result (1 .. 2) := BOM_16LE;
               Len := 2;
         end case;
      end if;

      -- Convert Wide_Character to ASCII (simple case)
      for I in Start .. Item'Last loop
         declare
            Code : constant Natural := Wide_Character'Pos (Item (I));
         begin
            if Code <= 127 then
               Len := Len + 1;
               Result (Len) := Character'Val (Code);
            elsif Code <= 16#7FF# then
               -- Two-byte UTF-8
               Len := Len + 1;
               Result (Len) := Character'Val (16#C0# + Code / 64);
               Len := Len + 1;
               Result (Len) := Character'Val (16#80# + (Code mod 64));
            else
               -- Three-byte UTF-8
               Len := Len + 1;
               Result (Len) := Character'Val (16#E0# + Code / 4096);
               Len := Len + 1;
               Result (Len) := Character'Val (16#80# + ((Code / 64) mod 64));
               Len := Len + 1;
               Result (Len) := Character'Val (16#80# + (Code mod 64));
            end if;
         end;
      end loop;

      return Result (1 .. Len);
   end Convert;

   function Convert
     (Item       : UTF_16_Wide_String;
      Output_BOM : Boolean := False) return UTF_8_String
   is
   begin
      return Convert (Item, UTF_8, Output_BOM);
   end Convert;

end Ada.Strings.UTF_Encoding;
