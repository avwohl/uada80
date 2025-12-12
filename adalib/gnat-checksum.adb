-- GNAT.Checksum body for Z80
-- Various checksum algorithm implementations

package body GNAT.Checksum is

   -----------
   -- Sum_8 --
   -----------

   function Sum_8 (S : String) return Checksum_8 is
      Sum : Checksum_8 := 0;
   begin
      for C of S loop
         Sum := Sum + Checksum_8 (Character'Pos (C));
      end loop;
      return Sum;
   end Sum_8;

   ------------
   -- Sum_16 --
   ------------

   function Sum_16 (S : String) return Checksum_16 is
      Sum : Checksum_16 := 0;
   begin
      for C of S loop
         Sum := Sum + Checksum_16 (Character'Pos (C));
      end loop;
      return Sum;
   end Sum_16;

   -----------
   -- XOR_8 --
   -----------

   function XOR_8 (S : String) return Checksum_8 is
      Result : Checksum_8 := 0;
   begin
      for C of S loop
         Result := Result xor Checksum_8 (Character'Pos (C));
      end loop;
      return Result;
   end XOR_8;

   ------------
   -- XOR_16 --
   ------------

   function XOR_16 (S : String) return Checksum_16 is
      Result : Checksum_16 := 0;
      I      : Positive := S'First;
   begin
      while I <= S'Last loop
         Result := Result xor Checksum_16 (Character'Pos (S (I)));
         if I < S'Last then
            Result := Result xor (Checksum_16 (Character'Pos (S (I + 1))) * 256);
         end if;
         I := I + 2;
      end loop;
      return Result;
   end XOR_16;

   ----------------
   -- Fletcher_16 --
   ----------------

   function Fletcher_16 (S : String) return Checksum_16 is
      Sum1 : Checksum_8 := 0;
      Sum2 : Checksum_8 := 0;
   begin
      for C of S loop
         Sum1 := Sum1 + Checksum_8 (Character'Pos (C));
         Sum2 := Sum2 + Sum1;
      end loop;
      return Checksum_16 (Sum2) * 256 + Checksum_16 (Sum1);
   end Fletcher_16;

   ------------
   -- BSD_16 --
   ------------

   function BSD_16 (S : String) return Checksum_16 is
      Checksum : Checksum_16 := 0;
   begin
      for C of S loop
         -- Rotate right
         Checksum := (Checksum / 2) + (Checksum mod 2) * 32768;
         Checksum := Checksum + Checksum_16 (Character'Pos (C));
      end loop;
      return Checksum;
   end BSD_16;

   --------------
   -- Adler_16 --
   --------------

   function Adler_16 (S : String) return Checksum_16 is
      Modulo : constant := 251;  -- Largest prime < 256
      A : Checksum_16 := 1;
      B : Checksum_16 := 0;
   begin
      for C of S loop
         A := (A + Checksum_16 (Character'Pos (C))) mod Modulo;
         B := (B + A) mod Modulo;
      end loop;
      return B * 256 + A;
   end Adler_16;

   -----------------
   -- Internet_16 --
   -----------------

   function Internet_16 (S : String) return Checksum_16 is
      Sum : Natural := 0;
      I   : Positive := S'First;
   begin
      while I <= S'Last loop
         Sum := Sum + Character'Pos (S (I)) * 256;
         if I < S'Last then
            Sum := Sum + Character'Pos (S (I + 1));
         end if;
         I := I + 2;
      end loop;

      -- Fold 32-bit sum to 16 bits
      while Sum > 65535 loop
         Sum := (Sum mod 65536) + (Sum / 65536);
      end loop;

      return Checksum_16 (65535 - Sum);  -- One's complement
   end Internet_16;

   -------------------------
   -- Ones_Complement_16 --
   -------------------------

   function Ones_Complement_16 (S : String) return Checksum_16 is
      Sum : Natural := 0;
   begin
      for C of S loop
         Sum := Sum + Character'Pos (C);
         -- Fold carry
         if Sum > 65535 then
            Sum := (Sum mod 65536) + 1;
         end if;
      end loop;
      return Checksum_16 (65535 - Sum);
   end Ones_Complement_16;

   ---------
   -- LRC --
   ---------

   function LRC (S : String) return Checksum_8 is
   begin
      return XOR_8 (S);
   end LRC;

   -------------------------
   -- Twos_Complement_8 --
   -------------------------

   function Twos_Complement_8 (S : String) return Checksum_8 is
      Sum : Checksum_8 := 0;
   begin
      for C of S loop
         Sum := Sum + Checksum_8 (Character'Pos (C));
      end loop;
      return (not Sum) + 1;
   end Twos_Complement_8;

   -- Byte array variants

   function Sum_8 (Data : Byte_Array) return Checksum_8 is
      Sum : Checksum_8 := 0;
   begin
      for B of Data loop
         Sum := Sum + Checksum_8 (B);
      end loop;
      return Sum;
   end Sum_8;

   function Sum_16 (Data : Byte_Array) return Checksum_16 is
      Sum : Checksum_16 := 0;
   begin
      for B of Data loop
         Sum := Sum + Checksum_16 (B);
      end loop;
      return Sum;
   end Sum_16;

   function XOR_8 (Data : Byte_Array) return Checksum_8 is
      Result : Checksum_8 := 0;
   begin
      for B of Data loop
         Result := Result xor Checksum_8 (B);
      end loop;
      return Result;
   end XOR_8;

   function Fletcher_16 (Data : Byte_Array) return Checksum_16 is
      Sum1 : Checksum_8 := 0;
      Sum2 : Checksum_8 := 0;
   begin
      for B of Data loop
         Sum1 := Sum1 + Checksum_8 (B);
         Sum2 := Sum2 + Sum1;
      end loop;
      return Checksum_16 (Sum2) * 256 + Checksum_16 (Sum1);
   end Fletcher_16;

   -- Verify functions

   function Verify_Sum_8 (S : String; Expected : Checksum_8) return Boolean is
   begin
      return Sum_8 (S) = Expected;
   end Verify_Sum_8;

   function Verify_Sum_16 (S : String; Expected : Checksum_16) return Boolean is
   begin
      return Sum_16 (S) = Expected;
   end Verify_Sum_16;

   function Verify_Fletcher_16 (S : String; Expected : Checksum_16) return Boolean is
   begin
      return Fletcher_16 (S) = Expected;
   end Verify_Fletcher_16;

end GNAT.Checksum;
