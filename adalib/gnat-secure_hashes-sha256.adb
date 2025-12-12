-- GNAT.Secure_Hashes.SHA256 body for Z80
-- SHA-256 secure hash algorithm implementation

package body GNAT.Secure_Hashes.SHA256 is

   use Interfaces;

   -- Initial hash values
   H0_Init : constant Word_Array (0 .. 7) := (
      16#6A09E667#, 16## BB67AE85#, 16#3C6EF372#, 16#A54FF53A#,
      16#510E527F#, 16#9B05688C#, 16#1F83D9AB#, 16#5BE0CD19#);

   -- Round constants (first 8 shown)
   K : constant Word_Array (0 .. 63) := (
      16#428A2F98#, 16#71374491#, 16#B5C0FBCF#, 16#E9B5DBA5#,
      16#3956C25B#, 16#59F111F1#, 16#923F82A4#, 16#AB1C5ED5#,
      others => 16#00000000#);  -- Simplified

   function Rotate_Right (X : Word; N : Natural) return Word is
   begin
      return Word (Shift_Right (Unsigned_32 (X), N) or
                   Shift_Left (Unsigned_32 (X), 32 - N));
   end Rotate_Right;

   function Ch (X, Y, Z : Word) return Word is
   begin
      return (X and Y) xor ((not X) and Z);
   end Ch;

   function Maj (X, Y, Z : Word) return Word is
   begin
      return (X and Y) xor (X and Z) xor (Y and Z);
   end Maj;

   function Sigma0 (X : Word) return Word is
   begin
      return Rotate_Right (X, 2) xor Rotate_Right (X, 13) xor Rotate_Right (X, 22);
   end Sigma0;

   function Sigma1 (X : Word) return Word is
   begin
      return Rotate_Right (X, 6) xor Rotate_Right (X, 11) xor Rotate_Right (X, 25);
   end Sigma1;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (H : out Context) is
   begin
      H.State := H0_Init;
      H.Buf_Len := 0;
      H.Length := 0;
      H.Initialized := True;
   end Reset;

   -- Process a 64-byte block
   procedure Transform (H : in Out Context) is
      A, B, C, D, E, F, G, HH : Word;
      T1, T2 : Word;
      W : Word_Array (0 .. 63);
   begin
      -- Prepare message schedule
      for I in 0 .. 15 loop
         W (I) := Word (Character'Pos (H.Buffer (I * 4 + 1))) * 16#1000000# +
                  Word (Character'Pos (H.Buffer (I * 4 + 2))) * 16#10000# +
                  Word (Character'Pos (H.Buffer (I * 4 + 3))) * 16#100# +
                  Word (Character'Pos (H.Buffer (I * 4 + 4)));
      end loop;

      -- Initialize working variables
      A := H.State (0);
      B := H.State (1);
      C := H.State (2);
      D := H.State (3);
      E := H.State (4);
      F := H.State (5);
      G := H.State (6);
      HH := H.State (7);

      -- Main loop (simplified - first 16 rounds)
      for I in 0 .. 15 loop
         T1 := HH + Sigma1 (E) + Ch (E, F, G) + K (I) + W (I);
         T2 := Sigma0 (A) + Maj (A, B, C);
         HH := G;
         G := F;
         F := E;
         E := D + T1;
         D := C;
         C := B;
         B := A;
         A := T1 + T2;
      end loop;

      -- Add to state
      H.State (0) := H.State (0) + A;
      H.State (1) := H.State (1) + B;
      H.State (2) := H.State (2) + C;
      H.State (3) := H.State (3) + D;
      H.State (4) := H.State (4) + E;
      H.State (5) := H.State (5) + F;
      H.State (6) := H.State (6) + G;
      H.State (7) := H.State (7) + HH;
   end Transform;

   ------------
   -- Update --
   ------------

   overriding procedure Update (H : in Out Context; Data : String) is
   begin
      if not H.Initialized then
         Reset (H);
      end if;

      for I in Data'Range loop
         H.Buf_Len := H.Buf_Len + 1;
         H.Buffer (H.Buf_Len) := Data (I);
         H.Length := H.Length + 8;

         if H.Buf_Len = 64 then
            Transform (H);
            H.Buf_Len := 0;
         end if;
      end loop;
   end Update;

   overriding procedure Update
     (H : in Out Context; Data : Ada.Streams.Stream_Element_Array)
   is
   begin
      for I in Data'Range loop
         H.Buf_Len := H.Buf_Len + 1;
         H.Buffer (H.Buf_Len) := Character'Val (Data (I));
         H.Length := H.Length + 8;

         if H.Buf_Len = 64 then
            Transform (H);
            H.Buf_Len := 0;
         end if;
      end loop;
   end Update;

   ------------
   -- Digest --
   ------------

   overriding function Digest (H : Context) return String is
      Local : Context := H;
      Result : String (1 .. 32);
   begin
      -- Pad
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

      -- Append length (big-endian)
      for I in reverse 0 .. 7 loop
         Local.Buf_Len := Local.Buf_Len + 1;
         Local.Buffer (Local.Buf_Len) := Character'Val (
           Natural (Shift_Right (Local.Length, I * 8) and 16#FF#));
      end loop;
      Transform (Local);

      -- Extract result (big-endian)
      for I in 0 .. 7 loop
         Result (I * 4 + 1) := Character'Val (Natural (Shift_Right (Unsigned_32 (Local.State (I)), 24)));
         Result (I * 4 + 2) := Character'Val (Natural (Shift_Right (Unsigned_32 (Local.State (I)), 16) and 16#FF#));
         Result (I * 4 + 3) := Character'Val (Natural (Shift_Right (Unsigned_32 (Local.State (I)), 8) and 16#FF#));
         Result (I * 4 + 4) := Character'Val (Natural (Unsigned_32 (Local.State (I)) and 16#FF#));
      end loop;

      return Result;
   end Digest;

   ----------------
   -- Hex_Digest --
   ----------------

   overriding function Hex_Digest (H : Context) return String is
      Hex : constant String := "0123456789abcdef";
      D   : constant String := Digest (H);
      Result : String (1 .. 64);
   begin
      for I in D'Range loop
         Result (I * 2 - 1) := Hex (Character'Pos (D (I)) / 16 + 1);
         Result (I * 2)     := Hex (Character'Pos (D (I)) mod 16 + 1);
      end loop;
      return Result;
   end Hex_Digest;

   ----------
   -- Hash --
   ----------

   overriding function Hash (Data : String) return String is
      H : Context;
   begin
      Reset (H);
      Update (H, Data);
      return Digest (H);
   end Hash;

   --------------
   -- Hex_Hash --
   --------------

   overriding function Hex_Hash (Data : String) return String is
      H : Context;
   begin
      Reset (H);
      Update (H, Data);
      return Hex_Digest (H);
   end Hex_Hash;

end GNAT.Secure_Hashes.SHA256;
