-- GNAT.UTF8 body for Z80
-- UTF-8 encoding/decoding implementation

package body GNAT.UTF8 is

   --------------------
   -- Is_Valid_UTF8 --
   --------------------

   function Is_Valid_UTF8 (S : String) return Boolean is
      I   : Positive := S'First;
      Len : Natural;
      B   : Natural;
   begin
      while I <= S'Last loop
         B := Character'Pos (S (I));

         if B < 128 then
            Len := 1;
         elsif B < 192 then
            return False;  -- Invalid start byte
         elsif B < 224 then
            Len := 2;
         elsif B < 240 then
            Len := 3;
         elsif B < 248 then
            Len := 4;
         else
            return False;  -- Invalid start byte
         end if;

         if I + Len - 1 > S'Last then
            return False;  -- Truncated sequence
         end if;

         -- Check continuation bytes
         for J in 1 .. Len - 1 loop
            B := Character'Pos (S (I + J));
            if B < 128 or B >= 192 then
               return False;
            end if;
         end loop;

         I := I + Len;
      end loop;

      return True;
   end Is_Valid_UTF8;

   ------------
   -- Length --
   ------------

   function Length (S : String) return Natural is
      Result : Natural := 0;
      I      : Positive := S'First;
      B      : Natural;
   begin
      while I <= S'Last loop
         B := Character'Pos (S (I));

         if B < 128 then
            I := I + 1;
         elsif B < 224 then
            I := I + 2;
         elsif B < 240 then
            I := I + 3;
         else
            I := I + 4;
         end if;

         Result := Result + 1;
      end loop;

      return Result;
   end Length;

   --------------
   -- Get_Char --
   --------------

   function Get_Char
     (S     : String;
      Index : Positive) return Code_Point
   is
      CP : Code_Point;
      Dummy : Natural;
   begin
      Dummy := Decode (S, Index, CP);
      return CP;
   end Get_Char;

   ---------------------
   -- Get_Char_Length --
   ---------------------

   function Get_Char_Length (C : Character) return Natural is
      B : constant Natural := Character'Pos (C);
   begin
      if B < 128 then
         return 1;
      elsif B < 224 then
         return 2;
      elsif B < 240 then
         return 3;
      else
         return 4;
      end if;
   end Get_Char_Length;

   ------------
   -- Encode --
   ------------

   function Encode (CP : Code_Point) return String is
   begin
      if CP < 16#80# then
         return (1 => Character'Val (Natural (CP)));
      elsif CP < 16#800# then
         return (Character'Val (16#C0# + Natural (CP / 64)),
                 Character'Val (16#80# + Natural (CP mod 64)));
      elsif CP < 16#10000# then
         return (Character'Val (16#E0# + Natural (CP / 4096)),
                 Character'Val (16#80# + Natural ((CP / 64) mod 64)),
                 Character'Val (16#80# + Natural (CP mod 64)));
      else
         return (Character'Val (16#F0# + Natural (CP / 262144)),
                 Character'Val (16#80# + Natural ((CP / 4096) mod 64)),
                 Character'Val (16#80# + Natural ((CP / 64) mod 64)),
                 Character'Val (16#80# + Natural (CP mod 64)));
      end if;
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode
     (S     : String;
      Index : Positive;
      CP    : out Code_Point) return Natural
   is
      B0 : constant Natural := Character'Pos (S (Index));
   begin
      if B0 < 128 then
         CP := Code_Point (B0);
         return 1;
      elsif B0 < 224 then
         CP := Code_Point ((B0 - 192) * 64 +
                           (Character'Pos (S (Index + 1)) - 128));
         return 2;
      elsif B0 < 240 then
         CP := Code_Point ((B0 - 224) * 4096 +
                           (Character'Pos (S (Index + 1)) - 128) * 64 +
                           (Character'Pos (S (Index + 2)) - 128));
         return 3;
      else
         CP := Code_Point ((B0 - 240) * 262144 +
                           (Character'Pos (S (Index + 1)) - 128) * 4096 +
                           (Character'Pos (S (Index + 2)) - 128) * 64 +
                           (Character'Pos (S (Index + 3)) - 128));
         return 4;
      end if;
   end Decode;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (S : String) return String is
      Result : String := S;
   begin
      for I in Result'Range loop
         if Result (I) in 'A' .. 'Z' then
            Result (I) := Character'Val (Character'Pos (Result (I)) + 32);
         end if;
      end loop;
      return Result;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (S : String) return String is
      Result : String := S;
   begin
      for I in Result'Range loop
         if Result (I) in 'a' .. 'z' then
            Result (I) := Character'Val (Character'Pos (Result (I)) - 32);
         end if;
      end loop;
      return Result;
   end To_Upper;

end GNAT.UTF8;
