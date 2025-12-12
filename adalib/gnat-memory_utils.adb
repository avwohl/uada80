-- GNAT.Memory_Utils body for Z80
-- Memory manipulation implementation

package body GNAT.Memory_Utils is

   -- Low-level memory access (implemented in assembly)
   procedure Mem_Write (Addr : Address; Value : Byte) is
      pragma Import (Assembler, Mem_Write, "mem_write");
   begin
      null;
   end Mem_Write;

   function Mem_Read (Addr : Address) return Byte is
      pragma Import (Assembler, Mem_Read, "mem_read");
   begin
      return 0;
   end Mem_Read;

   ----------
   -- Fill --
   ----------

   procedure Fill (Addr : Address; Size : Word; Value : Byte) is
   begin
      for I in 0 .. Size - 1 loop
         Mem_Write (Addr + I, Value);
      end loop;
   end Fill;

   ----------
   -- Zero --
   ----------

   procedure Zero (Addr : Address; Size : Word) is
   begin
      Fill (Addr, Size, 0);
   end Zero;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src, Dst : Address; Size : Word) is
   begin
      for I in 0 .. Size - 1 loop
         Mem_Write (Dst + I, Mem_Read (Src + I));
      end loop;
   end Copy;

   ----------
   -- Move --
   ----------

   procedure Move (Src, Dst : Address; Size : Word) is
   begin
      if Dst < Src or else Dst >= Src + Size then
         -- No overlap or dst before src, copy forward
         Copy (Src, Dst, Size);
      else
         -- Overlap with dst after src, copy backward
         for I in reverse 0 .. Size - 1 loop
            Mem_Write (Dst + I, Mem_Read (Src + I));
         end loop;
      end if;
   end Move;

   ----------
   -- Swap --
   ----------

   procedure Swap (Addr1, Addr2 : Address; Size : Word) is
      Tmp : Byte;
   begin
      for I in 0 .. Size - 1 loop
         Tmp := Mem_Read (Addr1 + I);
         Mem_Write (Addr1 + I, Mem_Read (Addr2 + I));
         Mem_Write (Addr2 + I, Tmp);
      end loop;
   end Swap;

   ----------
   -- Poke --
   ----------

   procedure Poke (Addr : Address; Value : Byte) is
   begin
      Mem_Write (Addr, Value);
   end Poke;

   ----------
   -- Peek --
   ----------

   function Peek (Addr : Address) return Byte is
   begin
      return Mem_Read (Addr);
   end Peek;

   ---------------
   -- Poke_Word --
   ---------------

   procedure Poke_Word (Addr : Address; Value : Word) is
   begin
      -- Z80 is little-endian
      Mem_Write (Addr, Value mod 256);
      Mem_Write (Addr + 1, Value / 256);
   end Poke_Word;

   ---------------
   -- Peek_Word --
   ---------------

   function Peek_Word (Addr : Address) return Word is
   begin
      return Mem_Read (Addr) + Mem_Read (Addr + 1) * 256;
   end Peek_Word;

   -------------
   -- Set_Bit --
   -------------

   procedure Set_Bit (Addr : Address; Bit : Natural) is
      V : Byte;
      Mask : Byte;
   begin
      if Bit < 8 then
         V := Mem_Read (Addr);
         Mask := 1;
         for I in 1 .. Bit loop
            Mask := Mask * 2;
         end loop;
         Mem_Write (Addr, V or Mask);
      end if;
   end Set_Bit;

   ---------------
   -- Clear_Bit --
   ---------------

   procedure Clear_Bit (Addr : Address; Bit : Natural) is
      V : Byte;
      Mask : Byte;
   begin
      if Bit < 8 then
         V := Mem_Read (Addr);
         Mask := 1;
         for I in 1 .. Bit loop
            Mask := Mask * 2;
         end loop;
         Mem_Write (Addr, V and (255 - Mask));
      end if;
   end Clear_Bit;

   ----------------
   -- Toggle_Bit --
   ----------------

   procedure Toggle_Bit (Addr : Address; Bit : Natural) is
      V : Byte;
      Mask : Byte;
   begin
      if Bit < 8 then
         V := Mem_Read (Addr);
         Mask := 1;
         for I in 1 .. Bit loop
            Mask := Mask * 2;
         end loop;
         Mem_Write (Addr, V xor Mask);
      end if;
   end Toggle_Bit;

   --------------
   -- Test_Bit --
   --------------

   function Test_Bit (Addr : Address; Bit : Natural) return Boolean is
      V : Byte;
      Mask : Byte;
   begin
      if Bit >= 8 then
         return False;
      end if;

      V := Mem_Read (Addr);
      Mask := 1;
      for I in 1 .. Bit loop
         Mask := Mask * 2;
      end loop;
      return (V and Mask) /= 0;
   end Test_Bit;

   -------------
   -- Compare --
   -------------

   function Compare (Addr1, Addr2 : Address; Size : Word) return Integer is
      B1, B2 : Byte;
   begin
      for I in 0 .. Size - 1 loop
         B1 := Mem_Read (Addr1 + I);
         B2 := Mem_Read (Addr2 + I);
         if B1 < B2 then
            return -1;
         elsif B1 > B2 then
            return 1;
         end if;
      end loop;
      return 0;
   end Compare;

   -----------
   -- Equal --
   -----------

   function Equal (Addr1, Addr2 : Address; Size : Word) return Boolean is
   begin
      return Compare (Addr1, Addr2, Size) = 0;
   end Equal;

   ---------------
   -- Find_Byte --
   ---------------

   function Find_Byte (Addr : Address; Size : Word;
                       Value : Byte) return Address is
   begin
      for I in 0 .. Size - 1 loop
         if Mem_Read (Addr + I) = Value then
            return Addr + I;
         end if;
      end loop;
      return 0;  -- Not found
   end Find_Byte;

   ---------------
   -- Find_Word --
   ---------------

   function Find_Word (Addr : Address; Size : Word;
                       Value : Word) return Address is
   begin
      if Size < 2 then
         return 0;
      end if;

      for I in 0 .. Size - 2 loop
         if Peek_Word (Addr + I) = Value then
            return Addr + I;
         end if;
      end loop;
      return 0;
   end Find_Word;

   ----------------
   -- Count_Byte --
   ----------------

   function Count_Byte (Addr : Address; Size : Word;
                        Value : Byte) return Natural is
      Count : Natural := 0;
   begin
      for I in 0 .. Size - 1 loop
         if Mem_Read (Addr + I) = Value then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Byte;

   ----------------
   -- Checksum_8 --
   ----------------

   function Checksum_8 (Addr : Address; Size : Word) return Byte is
      Sum : Natural := 0;
   begin
      for I in 0 .. Size - 1 loop
         Sum := Sum + Mem_Read (Addr + I);
      end loop;
      return Sum mod 256;
   end Checksum_8;

   -----------------
   -- Checksum_16 --
   -----------------

   function Checksum_16 (Addr : Address; Size : Word) return Word is
      Sum : Natural := 0;
   begin
      for I in 0 .. Size - 1 loop
         Sum := Sum + Mem_Read (Addr + I);
      end loop;
      return Sum mod 65536;
   end Checksum_16;

   -------------
   -- XOR_Sum --
   -------------

   function XOR_Sum (Addr : Address; Size : Word) return Byte is
      Result : Byte := 0;
   begin
      for I in 0 .. Size - 1 loop
         Result := Result xor Mem_Read (Addr + I);
      end loop;
      return Result;
   end XOR_Sum;

   ---------------
   -- High_Byte --
   ---------------

   function High_Byte (W : Word) return Byte is
   begin
      return W / 256;
   end High_Byte;

   --------------
   -- Low_Byte --
   --------------

   function Low_Byte (W : Word) return Byte is
   begin
      return W mod 256;
   end Low_Byte;

   ---------------
   -- Make_Word --
   ---------------

   function Make_Word (Hi, Lo : Byte) return Word is
   begin
      return Hi * 256 + Lo;
   end Make_Word;

   -----------------
   -- High_Nibble --
   -----------------

   function High_Nibble (B : Byte) return Byte is
   begin
      return B / 16;
   end High_Nibble;

   ----------------
   -- Low_Nibble --
   ----------------

   function Low_Nibble (B : Byte) return Byte is
   begin
      return B mod 16;
   end Low_Nibble;

   ---------------
   -- Make_Byte --
   ---------------

   function Make_Byte (Hi_Nib, Lo_Nib : Byte) return Byte is
   begin
      return (Hi_Nib mod 16) * 16 + (Lo_Nib mod 16);
   end Make_Byte;

   ------------
   -- To_BCD --
   ------------

   function To_BCD (Value : Byte) return Byte is
      V : Byte := Value;
   begin
      if V > 99 then
         V := 99;
      end if;
      return (V / 10) * 16 + (V mod 10);
   end To_BCD;

   --------------
   -- From_BCD --
   --------------

   function From_BCD (BCD : Byte) return Byte is
   begin
      return (BCD / 16) * 10 + (BCD mod 16);
   end From_BCD;

   ------------------
   -- Reverse_Bits --
   ------------------

   function Reverse_Bits (B : Byte) return Byte is
      Result : Byte := 0;
      V : Byte := B;
   begin
      for I in 1 .. 8 loop
         Result := Result * 2;
         if V mod 2 = 1 then
            Result := Result + 1;
         end if;
         V := V / 2;
      end loop;
      return Result;
   end Reverse_Bits;

   -------------------
   -- Reverse_Bytes --
   -------------------

   function Reverse_Bytes (W : Word) return Word is
   begin
      return Make_Word (Low_Byte (W), High_Byte (W));
   end Reverse_Bytes;

   --------------------
   -- Rotate_Left_8 --
   --------------------

   function Rotate_Left_8 (B : Byte; Count : Natural := 1) return Byte is
      Result : Byte := B;
      Carry : Byte;
   begin
      for I in 1 .. Count mod 8 loop
         Carry := Result / 128;
         Result := (Result * 2) mod 256 + Carry;
      end loop;
      return Result;
   end Rotate_Left_8;

   ---------------------
   -- Rotate_Right_8 --
   ---------------------

   function Rotate_Right_8 (B : Byte; Count : Natural := 1) return Byte is
      Result : Byte := B;
      Carry : Byte;
   begin
      for I in 1 .. Count mod 8 loop
         Carry := Result mod 2;
         Result := Result / 2 + Carry * 128;
      end loop;
      return Result;
   end Rotate_Right_8;

   ---------------------
   -- Rotate_Left_16 --
   ---------------------

   function Rotate_Left_16 (W : Word; Count : Natural := 1) return Word is
      Result : Word := W;
      Carry : Word;
   begin
      for I in 1 .. Count mod 16 loop
         Carry := Result / 32768;
         Result := (Result * 2) mod 65536 + Carry;
      end loop;
      return Result;
   end Rotate_Left_16;

   ----------------------
   -- Rotate_Right_16 --
   ----------------------

   function Rotate_Right_16 (W : Word; Count : Natural := 1) return Word is
      Result : Word := W;
      Carry : Word;
   begin
      for I in 1 .. Count mod 16 loop
         Carry := Result mod 2;
         Result := Result / 2 + Carry * 32768;
      end loop;
      return Result;
   end Rotate_Right_16;

   -------------------
   -- Shift_Left_8 --
   -------------------

   function Shift_Left_8 (B : Byte; Count : Natural := 1) return Byte is
      Result : Natural := B;
   begin
      for I in 1 .. Count loop
         Result := Result * 2;
      end loop;
      return Result mod 256;
   end Shift_Left_8;

   --------------------
   -- Shift_Right_8 --
   --------------------

   function Shift_Right_8 (B : Byte; Count : Natural := 1) return Byte is
      Result : Byte := B;
   begin
      for I in 1 .. Count loop
         Result := Result / 2;
      end loop;
      return Result;
   end Shift_Right_8;

   --------------------
   -- Shift_Left_16 --
   --------------------

   function Shift_Left_16 (W : Word; Count : Natural := 1) return Word is
      Result : Natural := W;
   begin
      for I in 1 .. Count loop
         Result := Result * 2;
      end loop;
      return Result mod 65536;
   end Shift_Left_16;

   ---------------------
   -- Shift_Right_16 --
   ---------------------

   function Shift_Right_16 (W : Word; Count : Natural := 1) return Word is
      Result : Word := W;
   begin
      for I in 1 .. Count loop
         Result := Result / 2;
      end loop;
      return Result;
   end Shift_Right_16;

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (B : Byte) return Integer is
   begin
      if B < 128 then
         return Integer (B);
      else
         return Integer (B) - 256;
      end if;
   end Sign_Extend;

   ----------------
   -- Count_Ones --
   ----------------

   function Count_Ones (B : Byte) return Natural is
      V : Byte := B;
      Count : Natural := 0;
   begin
      while V > 0 loop
         if V mod 2 = 1 then
            Count := Count + 1;
         end if;
         V := V / 2;
      end loop;
      return Count;
   end Count_Ones;

   -----------------
   -- Count_Zeros --
   -----------------

   function Count_Zeros (B : Byte) return Natural is
   begin
      return 8 - Count_Ones (B);
   end Count_Zeros;

   -------------------
   -- Leading_Zeros --
   -------------------

   function Leading_Zeros (B : Byte) return Natural is
      V : Byte := B;
      Count : Natural := 0;
      Mask : Byte := 128;
   begin
      while Count < 8 loop
         if (V and Mask) /= 0 then
            exit;
         end if;
         Count := Count + 1;
         Mask := Mask / 2;
      end loop;
      return Count;
   end Leading_Zeros;

   --------------------
   -- Trailing_Zeros --
   --------------------

   function Trailing_Zeros (B : Byte) return Natural is
      V : Byte := B;
      Count : Natural := 0;
   begin
      if V = 0 then
         return 8;
      end if;

      while V mod 2 = 0 loop
         Count := Count + 1;
         V := V / 2;
      end loop;
      return Count;
   end Trailing_Zeros;

   --------------
   -- Dump_Hex --
   --------------

   function Dump_Hex (Addr : Address; Size : Natural) return String is
      Hex : constant String := "0123456789ABCDEF";
      Max_Size : constant := 16;  -- Limit output
      Actual : Natural := Size;
      Result : String (1 .. Max_Size * 3);
      Pos : Natural := 1;
      B : Byte;
   begin
      if Actual > Max_Size then
         Actual := Max_Size;
      end if;

      for I in 0 .. Actual - 1 loop
         B := Mem_Read (Addr + Word (I));
         Result (Pos) := Hex (B / 16 + 1);
         Result (Pos + 1) := Hex (B mod 16 + 1);
         if I < Actual - 1 then
            Result (Pos + 2) := ' ';
            Pos := Pos + 3;
         else
            Pos := Pos + 2;
         end if;
      end loop;

      return Result (1 .. Pos);
   end Dump_Hex;

   -----------------------
   -- Get_Stack_Pointer --
   -----------------------

   function Get_Stack_Pointer return Address is
      pragma Import (Assembler, Get_Stack_Pointer, "get_sp");
   begin
      return 0;  -- Implemented in assembly
   end Get_Stack_Pointer;

   -----------------
   -- Free_Memory --
   -----------------

   function Free_Memory return Word is
      SP : Address;
      Heap_Top : Address;
      pragma Import (Assembler, Heap_Top, "heap_top");
   begin
      SP := Get_Stack_Pointer;
      -- Free memory is between heap top and stack pointer
      if SP > Heap_Top then
         return SP - Heap_Top;
      else
         return 0;
      end if;
   end Free_Memory;

   --------------
   -- Port_Out --
   --------------

   procedure Port_Out (Port : Byte; Value : Byte) is
      pragma Import (Assembler, Port_Out, "port_out");
   begin
      null;  -- Implemented in assembly
   end Port_Out;

   -------------
   -- Port_In --
   -------------

   function Port_In (Port : Byte) return Byte is
      pragma Import (Assembler, Port_In, "port_in");
   begin
      return 0;  -- Implemented in assembly
   end Port_In;

end GNAT.Memory_Utils;
