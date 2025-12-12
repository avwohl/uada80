-- GNAT.MD5 body for Z80
-- MD5 message digest algorithm implementation

package body GNAT.MD5 is

   use Interfaces;

   -- Initial state values
   Init_State : constant Word_Array (0 .. 3) := (
      16#67452301#, 16#EFCDAB89#, 16#98BADCFE#, 16#10325476#);

   -- Helper functions
   function F (X, Y, Z : Word) return Word is
   begin
      return (X and Y) or ((not X) and Z);
   end F;

   function G (X, Y, Z : Word) return Word is
   begin
      return (X and Z) or (Y and (not Z));
   end G;

   function H (X, Y, Z : Word) return Word is
   begin
      return X xor Y xor Z;
   end H;

   function I (X, Y, Z : Word) return Word is
   begin
      return Y xor (X or (not Z));
   end I;

   function Rotate_Left (X : Word; N : Natural) return Word is
   begin
      return Word (Shift_Left (Unsigned_32 (X), N) or
                   Shift_Right (Unsigned_32 (X), 32 - N));
   end Rotate_Left;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : out Context) is
   begin
      C.State := Init_State;
      C.Count := 0;
      C.Buf_Len := 0;
   end Initialize;

   -- Process a 64-byte block
   procedure Transform (C : in Out Context) is
      A : Word := C.State (0);
      B : Word := C.State (1);
      CC : Word := C.State (2);
      D : Word := C.State (3);
      X : Word_Array (0 .. 15);
   begin
      -- Decode input into 16 32-bit words
      for J in 0 .. 15 loop
         X (J) := Word (Character'Pos (C.Buffer (J * 4 + 1))) +
                  Word (Character'Pos (C.Buffer (J * 4 + 2))) * 256 +
                  Word (Character'Pos (C.Buffer (J * 4 + 3))) * 65536 +
                  Word (Character'Pos (C.Buffer (J * 4 + 4))) * 16777216;
      end loop;

      -- Round 1 (simplified - only first few operations shown)
      A := B + Rotate_Left (A + F (B, CC, D) + X (0) + 16#D76AA478#, 7);
      D := A + Rotate_Left (D + F (A, B, CC) + X (1) + 16#E8C7B756#, 12);
      CC := D + Rotate_Left (CC + F (D, A, B) + X (2) + 16#242070DB#, 17);
      B := CC + Rotate_Left (B + F (CC, D, A) + X (3) + 16#C1BDCEEE#, 22);

      -- Additional rounds would go here...
      -- This is a simplified implementation

      C.State (0) := C.State (0) + A;
      C.State (1) := C.State (1) + B;
      C.State (2) := C.State (2) + CC;
      C.State (3) := C.State (3) + D;
   end Transform;

   ------------
   -- Update --
   ------------

   procedure Update (C : in Out Context; Input : String) is
   begin
      for I in Input'Range loop
         C.Buf_Len := C.Buf_Len + 1;
         C.Buffer (C.Buf_Len) := Input (I);
         C.Count := C.Count + 8;

         if C.Buf_Len = 64 then
            Transform (C);
            C.Buf_Len := 0;
         end if;
      end loop;
   end Update;

   procedure Update (C : in Out Context; Input : Ada.Streams.Stream_Element_Array) is
   begin
      for I in Input'Range loop
         C.Buf_Len := C.Buf_Len + 1;
         C.Buffer (C.Buf_Len) := Character'Val (Input (I));
         C.Count := C.Count + 8;

         if C.Buf_Len = 64 then
            Transform (C);
            C.Buf_Len := 0;
         end if;
      end loop;
   end Update;

   ------------
   -- Digest --
   ------------

   function Digest (C : Context) return Message_Digest is
      Result : Message_Digest;
      Local  : Context := C;
   begin
      -- Pad message
      Local.Buf_Len := Local.Buf_Len + 1;
      Local.Buffer (Local.Buf_Len) := Character'Val (16#80#);

      while Local.Buf_Len /= 56 loop
         if Local.Buf_Len = 64 then
            Transform (Local);
            Local.Buf_Len := 0;
         end if;
         Local.Buf_Len := Local.Buf_Len + 1;
         Local.Buffer (Local.Buf_Len) := Character'Val (0);
      end loop;

      -- Append length (little-endian)
      for I in 0 .. 7 loop
         Local.Buf_Len := Local.Buf_Len + 1;
         Local.Buffer (Local.Buf_Len) := Character'Val (
           Natural (Shift_Right (Local.Count, I * 8) and 16#FF#));
      end loop;
      Transform (Local);

      -- Extract result (little-endian)
      for I in 0 .. 3 loop
         Result (I * 4 + 1) := Unsigned_8 (Local.State (I) and 16#FF#);
         Result (I * 4 + 2) := Unsigned_8 (Shift_Right (Unsigned_32 (Local.State (I)), 8) and 16#FF#);
         Result (I * 4 + 3) := Unsigned_8 (Shift_Right (Unsigned_32 (Local.State (I)), 16) and 16#FF#);
         Result (I * 4 + 4) := Unsigned_8 (Shift_Right (Unsigned_32 (Local.State (I)), 24));
      end loop;

      return Result;
   end Digest;

   function Digest (C : Context) return Digest_String is
      Hex : constant String := "0123456789abcdef";
      D   : constant Message_Digest := Digest (C);
      Result : Digest_String;
   begin
      for I in D'Range loop
         Result (I * 2 - 1) := Hex (Natural (Shift_Right (D (I), 4)) + 1);
         Result (I * 2)     := Hex (Natural (D (I) and 16#0F#) + 1);
      end loop;
      return Result;
   end Digest;

   function Digest (S : String) return Message_Digest is
      C : Context;
   begin
      Initialize (C);
      Update (C, S);
      return Digest (C);
   end Digest;

   function Digest (S : String) return Digest_String is
      C : Context;
   begin
      Initialize (C);
      Update (C, S);
      return Digest (C);
   end Digest;

end GNAT.MD5;
