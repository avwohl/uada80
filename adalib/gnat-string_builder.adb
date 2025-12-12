-- GNAT.String_Builder body for Z80
-- Efficient string building implementation

package body GNAT.String_Builder is

   Hex_Digits : constant String := "0123456789ABCDEF";

   ------------
   -- Create --
   ------------

   function Create (Capacity : Positive := 256) return Builder is
      B : Builder;
   begin
      B.Cap := Positive'Min (Capacity, Max_Capacity);
      B.Len := 0;
      return B;
   end Create;

   ------------
   -- Length --
   ------------

   function Length (B : Builder) return Natural is
   begin
      return B.Len;
   end Length;

   --------------
   -- Capacity --
   --------------

   function Capacity (B : Builder) return Positive is
   begin
      return B.Cap;
   end Capacity;

   ---------------
   -- Available --
   ---------------

   function Available (B : Builder) return Natural is
   begin
      return B.Cap - B.Len;
   end Available;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (B : Builder) return Boolean is
   begin
      return B.Len = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (B : Builder) return Boolean is
   begin
      return B.Len = B.Cap;
   end Is_Full;

   -----------
   -- Clear --
   -----------

   procedure Clear (B : out Builder) is
   begin
      B.Len := 0;
   end Clear;

   ------------
   -- Append --
   ------------

   procedure Append (B : in Out Builder; S : String) is
      Space : constant Natural := B.Cap - B.Len;
      Len   : constant Natural := Natural'Min (S'Length, Space);
   begin
      B.Data (B.Len + 1 .. B.Len + Len) :=
        String_Data (S (S'First .. S'First + Len - 1));
      B.Len := B.Len + Len;
   end Append;

   procedure Append (B : in Out Builder; C : Character) is
   begin
      if B.Len < B.Cap then
         B.Len := B.Len + 1;
         B.Data (B.Len) := C;
      end if;
   end Append;

   procedure Append (B : in Out Builder; N : Integer) is
      Temp  : String (1 .. 12);
      Idx   : Natural := 0;
      Val   : Natural;
      Neg   : constant Boolean := N < 0;
   begin
      if N = 0 then
         Append (B, '0');
         return;
      end if;

      if Neg then
         Val := Natural (-N);
      else
         Val := Natural (N);
      end if;

      while Val > 0 loop
         Idx := Idx + 1;
         Temp (Idx) := Character'Val (Character'Pos ('0') + Val mod 10);
         Val := Val / 10;
      end loop;

      if Neg then
         Append (B, '-');
      end if;

      -- Reverse digits
      for I in reverse 1 .. Idx loop
         Append (B, Temp (I));
      end loop;
   end Append;

   procedure Append (B : in Out Builder; N : Natural; Width : Positive;
                     Fill : Character := '0')
   is
      Temp : String (1 .. Width);
      Val  : Natural := N;
   begin
      for I in reverse 1 .. Width loop
         Temp (I) := Character'Val (Character'Pos ('0') + Val mod 10);
         Val := Val / 10;
      end loop;

      -- Replace leading zeros with fill character
      if Fill /= '0' then
         for I in 1 .. Width - 1 loop
            exit when Temp (I) /= '0';
            Temp (I) := Fill;
         end loop;
      end if;

      Append (B, Temp);
   end Append;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line (B : in Out Builder; S : String) is
   begin
      Append (B, S);
      New_Line (B);
   end Append_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (B : in Out Builder) is
   begin
      Append (B, ASCII.CR);
      Append (B, ASCII.LF);
   end New_Line;

   ----------------
   -- Append_Hex --
   ----------------

   procedure Append_Hex (B : in Out Builder; N : Natural) is
      Temp : String (1 .. 4);
      Idx  : Natural := 0;
      Val  : Natural := N;
   begin
      if N = 0 then
         Append (B, '0');
         return;
      end if;

      while Val > 0 loop
         Idx := Idx + 1;
         Temp (Idx) := Hex_Digits (Val mod 16 + 1);
         Val := Val / 16;
      end loop;

      for I in reverse 1 .. Idx loop
         Append (B, Temp (I));
      end loop;
   end Append_Hex;

   ---------------
   -- To_String --
   ---------------

   function To_String (B : Builder) return String is
   begin
      return String (B.Data (1 .. B.Len));
   end To_String;

   ---------------
   -- Last_Char --
   ---------------

   function Last_Char (B : Builder) return Character is
   begin
      if B.Len = 0 then
         return ASCII.NUL;
      end if;
      return B.Data (B.Len);
   end Last_Char;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (B : in Out Builder; Count : Natural := 1) is
   begin
      if Count >= B.Len then
         B.Len := 0;
      else
         B.Len := B.Len - Count;
      end if;
   end Delete_Last;

   ------------
   -- Insert --
   ------------

   procedure Insert (B : in Out Builder; Before : Positive; S : String) is
      Len : constant Natural := Natural'Min (S'Length, B.Cap - B.Len);
   begin
      if Before > B.Len + 1 or Len = 0 then
         return;
      end if;

      -- Shift existing content
      for I in reverse Before .. B.Len loop
         if I + Len <= B.Cap then
            B.Data (I + Len) := B.Data (I);
         end if;
      end loop;

      -- Insert new content
      for I in 1 .. Len loop
         B.Data (Before + I - 1) := S (S'First + I - 1);
      end loop;

      B.Len := B.Len + Len;
   end Insert;

   -------------
   -- Replace --
   -------------

   procedure Replace (B : in Out Builder; Low, High : Positive; S : String) is
      Old_Len   : constant Natural := High - Low + 1;
      New_Len   : constant Natural := S'Length;
      Diff      : Integer;
   begin
      if Low > B.Len or High < Low then
         return;
      end if;

      Diff := New_Len - Old_Len;

      if Diff > 0 then
         -- Expansion - shift right
         if B.Len + Diff > B.Cap then
            return;  -- Won't fit
         end if;
         for I in reverse High + 1 .. B.Len loop
            B.Data (I + Diff) := B.Data (I);
         end loop;
      elsif Diff < 0 then
         -- Contraction - shift left
         for I in High + 1 .. B.Len loop
            B.Data (I + Diff) := B.Data (I);
         end loop;
      end if;

      -- Copy new content
      for I in 1 .. New_Len loop
         B.Data (Low + I - 1) := S (S'First + I - 1);
      end loop;

      B.Len := Natural (Integer (B.Len) + Diff);
   end Replace;

   -----------
   -- Index --
   -----------

   function Index (B : Builder; Pattern : String) return Natural is
   begin
      if Pattern'Length = 0 or Pattern'Length > B.Len then
         return 0;
      end if;

      for I in 1 .. B.Len - Pattern'Length + 1 loop
         if String (B.Data (I .. I + Pattern'Length - 1)) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   ---------------
   -- Substring --
   ---------------

   function Substring (B : Builder; Low, High : Positive) return String is
   begin
      if Low > B.Len or High < Low then
         return "";
      end if;
      declare
         Actual_High : constant Natural := Natural'Min (High, B.Len);
      begin
         return String (B.Data (Low .. Actual_High));
      end;
   end Substring;

   -- Formatted output helpers (aliases)

   procedure Put (B : in Out Builder; S : String) is
   begin
      Append (B, S);
   end Put;

   procedure Put (B : in Out Builder; C : Character) is
   begin
      Append (B, C);
   end Put;

   procedure Put (B : in Out Builder; N : Integer) is
   begin
      Append (B, N);
   end Put;

   procedure Put_Line (B : in Out Builder; S : String := "") is
   begin
      Append (B, S);
      New_Line (B);
   end Put_Line;

end GNAT.String_Builder;
