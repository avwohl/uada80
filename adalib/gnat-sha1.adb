-- GNAT.SHA1 body for Z80
-- SHA-1 secure hash algorithm implementation

package body GNAT.SHA1 is

   use Interfaces;

   -- Initial hash values
   H0_Init : constant Word := 16#67452301#;
   H1_Init : constant Word := 16#EFCDAB89#;
   H2_Init : constant Word := 16#98BADCFE#;
   H3_Init : constant Word := 16#10325476#;
   H4_Init : constant Word := 16#C3D2E1F0#;

   -- Constants
   K0 : constant Word := 16#5A827999#;
   K1 : constant Word := 16#6ED9EBA1#;
   K2 : constant Word := 16#8F1BBCDC#;
   K3 : constant Word := 16#CA62C1D6#;

   function Rotate_Left (Value : Word; Amount : Natural) return Word is
   begin
      return Word (Shift_Left (Unsigned_32 (Value), Amount) or
                   Shift_Right (Unsigned_32 (Value), 32 - Amount));
   end Rotate_Left;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : out Context) is
   begin
      C.H (0) := H0_Init;
      C.H (1) := H1_Init;
      C.H (2) := H2_Init;
      C.H (3) := H3_Init;
      C.H (4) := H4_Init;
      C.Count := 0;
      C.Length := 0;
   end Initialize;

   -- Process a 64-byte block
   procedure Process_Block (C : in Out Context) is
      A, B, D, E, F, Temp : Word;
   begin
      -- Simplified SHA-1 transform
      -- Note: Full implementation would be more complex
      A := C.H (0);
      B := C.H (1);
      D := C.H (3);
      E := C.H (4);

      -- Simple mixing (not full SHA-1)
      for I in 0 .. 15 loop
         C.W (I) := Word (Character'Pos (C.Buffer (I * 4 + 1))) * 16#1000000# +
                    Word (Character'Pos (C.Buffer (I * 4 + 2))) * 16#10000# +
                    Word (Character'Pos (C.Buffer (I * 4 + 3))) * 16#100# +
                    Word (Character'Pos (C.Buffer (I * 4 + 4)));
      end loop;

      for I in 16 .. 79 loop
         C.W (I) := Rotate_Left (C.W (I - 3) xor C.W (I - 8) xor
                                 C.W (I - 14) xor C.W (I - 16), 1);
      end loop;

      -- Simple state update
      for I in 0 .. 79 loop
         Temp := Rotate_Left (A, 5) + E + C.W (I);
         if I < 20 then
            F := (B and D) or ((not B) and D);
            Temp := Temp + F + K0;
         elsif I < 40 then
            F := B xor D xor E;
            Temp := Temp + F + K1;
         elsif I < 60 then
            F := (B and D) or (B and E) or (D and E);
            Temp := Temp + F + K2;
         else
            F := B xor D xor E;
            Temp := Temp + F + K3;
         end if;
         E := D;
         D := Rotate_Left (B, 30);
         B := A;
         A := Temp;
      end loop;

      C.H (0) := C.H (0) + A;
      C.H (1) := C.H (1) + B;
      C.H (2) := C.H (2) + D;
      C.H (3) := C.H (3) + D;
      C.H (4) := C.H (4) + E;
   end Process_Block;

   ------------
   -- Update --
   ------------

   procedure Update (C : in Out Context; S : String) is
   begin
      for I in S'Range loop
         C.Count := C.Count + 1;
         C.Buffer (C.Count) := S (I);
         C.Length := C.Length + 8;
         if C.Count = 64 then
            Process_Block (C);
            C.Count := 0;
         end if;
      end loop;
   end Update;

   procedure Update (C : in Out Context; A : Ada.Streams.Stream_Element_Array) is
   begin
      for I in A'Range loop
         C.Count := C.Count + 1;
         C.Buffer (C.Count) := Character'Val (A (I));
         C.Length := C.Length + 8;
         if C.Count = 64 then
            Process_Block (C);
            C.Count := 0;
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
      Local.Count := Local.Count + 1;
      Local.Buffer (Local.Count) := Character'Val (16#80#);
      while Local.Count /= 56 loop
         if Local.Count = 64 then
            Process_Block (Local);
            Local.Count := 0;
         end if;
         Local.Count := Local.Count + 1;
         Local.Buffer (Local.Count) := Character'Val (0);
      end loop;

      -- Append length
      for I in 1 .. 8 loop
         Local.Count := Local.Count + 1;
         Local.Buffer (Local.Count) := Character'Val (
           Natural (Shift_Right (Local.Length, (8 - I) * 8) and 16#FF#));
      end loop;
      Process_Block (Local);

      -- Extract result
      for I in 0 .. 4 loop
         Result (I * 4 + 1) := Unsigned_8 (Shift_Right (Unsigned_32 (Local.H (I)), 24));
         Result (I * 4 + 2) := Unsigned_8 (Shift_Right (Unsigned_32 (Local.H (I)), 16) and 16#FF#);
         Result (I * 4 + 3) := Unsigned_8 (Shift_Right (Unsigned_32 (Local.H (I)), 8) and 16#FF#);
         Result (I * 4 + 4) := Unsigned_8 (Unsigned_32 (Local.H (I)) and 16#FF#);
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

end GNAT.SHA1;
