-- GNAT.Bit_Ops body for Z80
-- Bit manipulation implementation

package body GNAT.Bit_Ops is

   -- Precomputed bit masks for 8-bit
   Bit_Masks : constant array (Bit_Index_8) of Byte :=
     (1, 2, 4, 8, 16, 32, 64, 128);

   -- Precomputed bit counts for nibbles (for fast bit counting)
   Nibble_Counts : constant array (Nibble) of Natural :=
     (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);

   ----------------
   -- 8-bit ops --
   ----------------

   function Get_Bit (Value : Byte; Bit : Bit_Index_8) return Boolean is
   begin
      return (Value / Bit_Masks (Bit)) mod 2 = 1;
   end Get_Bit;

   function Set_Bit (Value : Byte; Bit : Bit_Index_8) return Byte is
   begin
      return Value or Bit_Masks (Bit);
   end Set_Bit;

   function Clear_Bit (Value : Byte; Bit : Bit_Index_8) return Byte is
   begin
      return Value and (255 - Bit_Masks (Bit));
   end Clear_Bit;

   function Toggle_Bit (Value : Byte; Bit : Bit_Index_8) return Byte is
   begin
      return Value xor Bit_Masks (Bit);
   end Toggle_Bit;

   function Put_Bit (Value : Byte; Bit : Bit_Index_8;
                     State : Boolean) return Byte is
   begin
      if State then
         return Set_Bit (Value, Bit);
      else
         return Clear_Bit (Value, Bit);
      end if;
   end Put_Bit;

   function Bit_Count (Value : Byte) return Natural is
   begin
      return Nibble_Counts (Value mod 16) + Nibble_Counts (Value / 16);
   end Bit_Count;

   function Leading_Zeros (Value : Byte) return Natural is
   begin
      if Value = 0 then
         return 8;
      end if;

      for I in reverse Bit_Index_8 loop
         if Get_Bit (Value, I) then
            return 7 - I;
         end if;
      end loop;
      return 8;
   end Leading_Zeros;

   function Trailing_Zeros (Value : Byte) return Natural is
   begin
      if Value = 0 then
         return 8;
      end if;

      for I in Bit_Index_8 loop
         if Get_Bit (Value, I) then
            return I;
         end if;
      end loop;
      return 8;
   end Trailing_Zeros;

   function Highest_Set_Bit (Value : Byte) return Integer is
   begin
      if Value = 0 then
         return -1;
      end if;

      for I in reverse Bit_Index_8 loop
         if Get_Bit (Value, I) then
            return I;
         end if;
      end loop;
      return -1;
   end Highest_Set_Bit;

   function Lowest_Set_Bit (Value : Byte) return Integer is
   begin
      if Value = 0 then
         return -1;
      end if;

      for I in Bit_Index_8 loop
         if Get_Bit (Value, I) then
            return I;
         end if;
      end loop;
      return -1;
   end Lowest_Set_Bit;

   function Is_Power_Of_Two (Value : Byte) return Boolean is
   begin
      return Value /= 0 and then (Value and (Value - 1)) = 0;
   end Is_Power_Of_Two;

   function Next_Power_Of_Two (Value : Byte) return Byte is
      V : Byte := Value;
   begin
      if V = 0 then
         return 1;
      end if;

      V := V - 1;
      V := V or (V / 2);
      V := V or (V / 4);
      V := V or (V / 8);
      V := V or (V / 16);
      V := V or (V / 32);
      V := V or (V / 64);
      V := V or (V / 128);
      return V + 1;
   end Next_Power_Of_Two;

   function Shift_Left (Value : Byte; Count : Natural) return Byte is
   begin
      if Count >= 8 then
         return 0;
      end if;
      return (Value * (2 ** Count)) mod 256;
   end Shift_Left;

   function Shift_Right (Value : Byte; Count : Natural) return Byte is
   begin
      if Count >= 8 then
         return 0;
      end if;
      return Value / (2 ** Count);
   end Shift_Right;

   function Rotate_Left (Value : Byte; Count : Natural) return Byte is
      C : constant Natural := Count mod 8;
   begin
      return Shift_Left (Value, C) or Shift_Right (Value, 8 - C);
   end Rotate_Left;

   function Rotate_Right (Value : Byte; Count : Natural) return Byte is
      C : constant Natural := Count mod 8;
   begin
      return Shift_Right (Value, C) or Shift_Left (Value, 8 - C);
   end Rotate_Right;

   function High_Nibble (Value : Byte) return Nibble is
   begin
      return Value / 16;
   end High_Nibble;

   function Low_Nibble (Value : Byte) return Nibble is
   begin
      return Value mod 16;
   end Low_Nibble;

   function Make_Byte (High, Low : Nibble) return Byte is
   begin
      return High * 16 + Low;
   end Make_Byte;

   function Swap_Nibbles (Value : Byte) return Byte is
   begin
      return Make_Byte (Low_Nibble (Value), High_Nibble (Value));
   end Swap_Nibbles;

   -----------------
   -- 16-bit ops --
   -----------------

   function Get_Bit (Value : Word; Bit : Bit_Index_16) return Boolean is
   begin
      if Bit < 8 then
         return Get_Bit (Low_Byte (Value), Bit);
      else
         return Get_Bit (High_Byte (Value), Bit - 8);
      end if;
   end Get_Bit;

   function Set_Bit (Value : Word; Bit : Bit_Index_16) return Word is
   begin
      if Bit < 8 then
         return Make_Word (High_Byte (Value), Set_Bit (Low_Byte (Value), Bit));
      else
         return Make_Word (Set_Bit (High_Byte (Value), Bit - 8), Low_Byte (Value));
      end if;
   end Set_Bit;

   function Clear_Bit (Value : Word; Bit : Bit_Index_16) return Word is
   begin
      if Bit < 8 then
         return Make_Word (High_Byte (Value), Clear_Bit (Low_Byte (Value), Bit));
      else
         return Make_Word (Clear_Bit (High_Byte (Value), Bit - 8), Low_Byte (Value));
      end if;
   end Clear_Bit;

   function Toggle_Bit (Value : Word; Bit : Bit_Index_16) return Word is
   begin
      if Bit < 8 then
         return Make_Word (High_Byte (Value), Toggle_Bit (Low_Byte (Value), Bit));
      else
         return Make_Word (Toggle_Bit (High_Byte (Value), Bit - 8), Low_Byte (Value));
      end if;
   end Toggle_Bit;

   function Bit_Count (Value : Word) return Natural is
   begin
      return Bit_Count (High_Byte (Value)) + Bit_Count (Low_Byte (Value));
   end Bit_Count;

   function Leading_Zeros_16 (Value : Word) return Natural is
      HB : constant Byte := High_Byte (Value);
   begin
      if HB /= 0 then
         return Leading_Zeros (HB);
      else
         return 8 + Leading_Zeros (Low_Byte (Value));
      end if;
   end Leading_Zeros_16;

   function Trailing_Zeros_16 (Value : Word) return Natural is
      LB : constant Byte := Low_Byte (Value);
   begin
      if LB /= 0 then
         return Trailing_Zeros (LB);
      else
         return 8 + Trailing_Zeros (High_Byte (Value));
      end if;
   end Trailing_Zeros_16;

   function Shift_Left (Value : Word; Count : Natural) return Word is
   begin
      if Count >= 16 then
         return 0;
      end if;
      return (Value * (2 ** Count)) mod 65536;
   end Shift_Left;

   function Shift_Right (Value : Word; Count : Natural) return Word is
   begin
      if Count >= 16 then
         return 0;
      end if;
      return Value / (2 ** Count);
   end Shift_Right;

   function Rotate_Left_16 (Value : Word; Count : Natural) return Word is
      C : constant Natural := Count mod 16;
   begin
      return Shift_Left (Value, C) or Shift_Right (Value, 16 - C);
   end Rotate_Left_16;

   function Rotate_Right_16 (Value : Word; Count : Natural) return Word is
      C : constant Natural := Count mod 16;
   begin
      return Shift_Right (Value, C) or Shift_Left (Value, 16 - C);
   end Rotate_Right_16;

   function High_Byte (Value : Word) return Byte is
   begin
      return Value / 256;
   end High_Byte;

   function Low_Byte (Value : Word) return Byte is
   begin
      return Value mod 256;
   end Low_Byte;

   function Make_Word (High, Low : Byte) return Word is
   begin
      return Word (High) * 256 + Word (Low);
   end Make_Word;

   function Swap_Bytes (Value : Word) return Word is
   begin
      return Make_Word (Low_Byte (Value), High_Byte (Value));
   end Swap_Bytes;

   --------------------
   -- Bitwise (8-bit) --
   --------------------

   function Bit_And (A, B : Byte) return Byte is
   begin
      return A and B;
   end Bit_And;

   function Bit_Or (A, B : Byte) return Byte is
   begin
      return A or B;
   end Bit_Or;

   function Bit_Xor (A, B : Byte) return Byte is
   begin
      return A xor B;
   end Bit_Xor;

   function Bit_Not (Value : Byte) return Byte is
   begin
      return 255 - Value;
   end Bit_Not;

   ---------------------
   -- Bitwise (16-bit) --
   ---------------------

   function Bit_And (A, B : Word) return Word is
   begin
      return A and B;
   end Bit_And;

   function Bit_Or (A, B : Word) return Word is
   begin
      return A or B;
   end Bit_Or;

   function Bit_Xor (A, B : Word) return Word is
   begin
      return A xor B;
   end Bit_Xor;

   function Bit_Not (Value : Word) return Word is
   begin
      return 65535 - Value;
   end Bit_Not;

   -------------------
   -- Mask functions --
   -------------------

   function Mask_Low (N : Natural) return Byte is
   begin
      if N >= 8 then
         return 255;
      elsif N = 0 then
         return 0;
      else
         return (2 ** N) - 1;
      end if;
   end Mask_Low;

   function Mask_High (N : Natural) return Byte is
   begin
      return Shift_Left (Mask_Low (N), 8 - N);
   end Mask_High;

   function Mask_Range (Low, High : Bit_Index_8) return Byte is
   begin
      return Mask_Low (High - Low + 1) * (2 ** Low);
   end Mask_Range;

   function Mask_Low_16 (N : Natural) return Word is
   begin
      if N >= 16 then
         return 65535;
      elsif N = 0 then
         return 0;
      else
         return (2 ** N) - 1;
      end if;
   end Mask_Low_16;

   function Mask_High_16 (N : Natural) return Word is
   begin
      return Shift_Left (Mask_Low_16 (N), 16 - N);
   end Mask_High_16;

   function Mask_Range_16 (Low, High : Bit_Index_16) return Word is
   begin
      return Mask_Low_16 (High - Low + 1) * (2 ** Low);
   end Mask_Range_16;

   -------------------
   -- Field ops (8-bit) --
   -------------------

   function Extract_Field (Value : Byte; Low, Width : Natural) return Byte is
   begin
      return (Value / (2 ** Low)) and Mask_Low (Width);
   end Extract_Field;

   function Insert_Field (Value, Field : Byte;
                          Low, Width : Natural) return Byte is
      Mask : constant Byte := Mask_Low (Width) * (2 ** Low);
   begin
      return (Value and Bit_Not (Mask)) or ((Field * (2 ** Low)) and Mask);
   end Insert_Field;

   --------------------
   -- Field ops (16-bit) --
   --------------------

   function Extract_Field (Value : Word; Low, Width : Natural) return Word is
   begin
      return (Value / (2 ** Low)) and Mask_Low_16 (Width);
   end Extract_Field;

   function Insert_Field (Value, Field : Word;
                          Low, Width : Natural) return Word is
      Mask : constant Word := Mask_Low_16 (Width) * (2 ** Low);
   begin
      return (Value and Bit_Not (Mask)) or ((Field * (2 ** Low)) and Mask);
   end Insert_Field;

   ------------
   -- Parity --
   ------------

   function Parity (Value : Byte) return Boolean is
   begin
      return Bit_Count (Value) mod 2 = 1;
   end Parity;

   function Parity (Value : Word) return Boolean is
   begin
      return Bit_Count (Value) mod 2 = 1;
   end Parity;

   ------------------
   -- Reverse_Bits --
   ------------------

   function Reverse_Bits (Value : Byte) return Byte is
      Result : Byte := 0;
   begin
      for I in Bit_Index_8 loop
         if Get_Bit (Value, I) then
            Result := Set_Bit (Result, 7 - I);
         end if;
      end loop;
      return Result;
   end Reverse_Bits;

   function Reverse_Bits (Value : Word) return Word is
   begin
      return Make_Word (Reverse_Bits (Low_Byte (Value)),
                        Reverse_Bits (High_Byte (Value)));
   end Reverse_Bits;

   -------------
   -- To_Gray --
   -------------

   function To_Gray (Value : Byte) return Byte is
   begin
      return Value xor Shift_Right (Value, 1);
   end To_Gray;

   ---------------
   -- From_Gray --
   ---------------

   function From_Gray (Value : Byte) return Byte is
      Result : Byte := Value;
   begin
      Result := Result xor Shift_Right (Result, 4);
      Result := Result xor Shift_Right (Result, 2);
      Result := Result xor Shift_Right (Result, 1);
      return Result;
   end From_Gray;

end GNAT.Bit_Ops;
