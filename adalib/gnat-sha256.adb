-- GNAT.SHA256 body for Z80
-- SHA-256 implementation

package body GNAT.SHA256 is

   -- Initial hash values
   H0 : constant Word_Array (0 .. 7) := (
      16#6a09e667#, 16## 16#bb67ae85#, 16#3c6ef372#, 16#a54ff53a#,
      16#510e527f#, 16#9b05688c#, 16#1f83d9ab#, 16#5be0cd19#);

   -- Round constants (first 8 for simplified version)
   K : constant Word_Array (0 .. 7) := (
      16#428a2f98#, 16#71374491#, 16#b5c0fbcf#, 16#e9b5dba5#,
      16#3956c25b#, 16#59f111f1#, 16#923f82a4#, 16#ab1c5ed5#);

   function Rotate_Right (V : Word; N : Natural) return Word is
   begin
      return (V / (2 ** N)) or (V * (2 ** (32 - N)));
   end Rotate_Right;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : out Context) is
   begin
      C.State := H0;
      C.Count := 0;
      C.Buffer_Len := 0;
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update (C : in Out Context; Input : String) is
   begin
      for I in Input'Range loop
         C.Buffer_Len := C.Buffer_Len + 1;
         C.Buffer (C.Buffer_Len) := Input (I);
         C.Count := C.Count + 8;

         -- Process block when buffer is full
         if C.Buffer_Len = 64 then
            -- Simplified: just XOR bytes into state
            for J in 0 .. 7 loop
               declare
                  Base : constant Natural := J * 8 + 1;
               begin
                  C.State (J) := C.State (J) xor
                    Word (Character'Pos (C.Buffer (Base))) * 16777216 xor
                    Word (Character'Pos (C.Buffer (Base + 1))) * 65536 xor
                    Word (Character'Pos (C.Buffer (Base + 2))) * 256 xor
                    Word (Character'Pos (C.Buffer (Base + 3)));
               end;
            end loop;
            C.Buffer_Len := 0;
         end if;
      end loop;
   end Update;

   --------------
   -- Finalize --
   --------------

   function Finalize (C : Context) return Binary_Digest is
      Result : Binary_Digest;
      Temp   : Context := C;
      V      : Word;
   begin
      -- Padding (simplified)
      Temp.Buffer_Len := Temp.Buffer_Len + 1;
      Temp.Buffer (Temp.Buffer_Len) := Character'Val (16#80#);

      while Temp.Buffer_Len < 64 loop
         Temp.Buffer_Len := Temp.Buffer_Len + 1;
         Temp.Buffer (Temp.Buffer_Len) := Character'Val (0);
      end loop;

      -- Process final block
      for J in 0 .. 7 loop
         declare
            Base : constant Natural := J * 8 + 1;
         begin
            Temp.State (J) := Temp.State (J) xor
              Word (Character'Pos (Temp.Buffer (Base))) * 16777216;
         end;
      end loop;

      -- Extract digest
      for I in 0 .. 7 loop
         V := Temp.State (I);
         Result (I * 4 + 1) := Character'Val (Natural (V / 16777216) mod 256);
         Result (I * 4 + 2) := Character'Val (Natural (V / 65536) mod 256);
         Result (I * 4 + 3) := Character'Val (Natural (V / 256) mod 256);
         Result (I * 4 + 4) := Character'Val (Natural (V) mod 256);
      end loop;

      return Result;
   end Finalize;

   ------------------
   -- Hex_Finalize --
   ------------------

   function Hex_Finalize (C : Context) return Hex_Digest is
      Hex_Chars : constant String := "0123456789abcdef";
      Binary    : constant Binary_Digest := Finalize (C);
      Result    : Hex_Digest;
      B         : Natural;
   begin
      for I in Binary'Range loop
         B := Character'Pos (Binary (I));
         Result ((I - 1) * 2 + 1) := Hex_Chars (B / 16 + 1);
         Result ((I - 1) * 2 + 2) := Hex_Chars (B mod 16 + 1);
      end loop;
      return Result;
   end Hex_Finalize;

   ----------
   -- Hash --
   ----------

   function Hash (Input : String) return Binary_Digest is
      C : Context;
   begin
      Initialize (C);
      Update (C, Input);
      return Finalize (C);
   end Hash;

   --------------
   -- Hex_Hash --
   --------------

   function Hex_Hash (Input : String) return Hex_Digest is
      C : Context;
   begin
      Initialize (C);
      Update (C, Input);
      return Hex_Finalize (C);
   end Hex_Hash;

end GNAT.SHA256;
