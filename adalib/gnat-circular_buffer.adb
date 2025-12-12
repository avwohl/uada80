-- GNAT.Circular_Buffer body for Z80
-- Fixed-size circular buffer implementation

package body GNAT.Circular_Buffer is

   ------------
   -- Create --
   ------------

   function Create (Capacity : Positive) return Buffer is
      B : Buffer;
   begin
      B.Cap := Positive'Min (Capacity, Max_Capacity);
      return B;
   end Create;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (B : Buffer) return Boolean is
   begin
      return B.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (B : Buffer) return Boolean is
   begin
      return B.Count = B.Cap;
   end Is_Full;

   ------------
   -- Length --
   ------------

   function Length (B : Buffer) return Natural is
   begin
      return B.Count;
   end Length;

   --------------
   -- Capacity --
   --------------

   function Capacity (B : Buffer) return Positive is
   begin
      return B.Cap;
   end Capacity;

   ---------------
   -- Available --
   ---------------

   function Available (B : Buffer) return Natural is
   begin
      return B.Cap - B.Count;
   end Available;

   ---------
   -- Put --
   ---------

   procedure Put (B : in Out Buffer; Item : Character) is
   begin
      B.Data (B.Tail) := Item;

      if B.Tail = B.Cap then
         B.Tail := 1;
      else
         B.Tail := B.Tail + 1;
      end if;

      if B.Count = B.Cap then
         -- Overwrite oldest - advance head
         if B.Head = B.Cap then
            B.Head := 1;
         else
            B.Head := B.Head + 1;
         end if;
      else
         B.Count := B.Count + 1;
      end if;
   end Put;

   procedure Put (B : in Out Buffer; Data : String) is
   begin
      for C of Data loop
         Put (B, C);
      end loop;
   end Put;

   ---------
   -- Get --
   ---------

   function Get (B : in Out Buffer) return Character is
      C : Character;
   begin
      if B.Count = 0 then
         return ASCII.NUL;
      end if;

      C := B.Data (B.Head);

      if B.Head = B.Cap then
         B.Head := 1;
      else
         B.Head := B.Head + 1;
      end if;

      B.Count := B.Count - 1;
      return C;
   end Get;

   procedure Get (B : in Out Buffer; Data : out String; Last : out Natural) is
   begin
      Last := Data'First - 1;
      for I in Data'Range loop
         exit when Is_Empty (B);
         Last := I;
         Data (I) := Get (B);
      end loop;
   end Get;

   ----------
   -- Peek --
   ----------

   function Peek (B : Buffer) return Character is
   begin
      if B.Count = 0 then
         return ASCII.NUL;
      end if;
      return B.Data (B.Head);
   end Peek;

   function Peek (B : Buffer; Index : Positive) return Character is
      Pos : Positive;
   begin
      if Index > B.Count then
         return ASCII.NUL;
      end if;

      Pos := B.Head + Index - 1;
      if Pos > B.Cap then
         Pos := Pos - B.Cap;
      end if;

      return B.Data (Pos);
   end Peek;

   -----------
   -- Clear --
   -----------

   procedure Clear (B : out Buffer) is
   begin
      B.Head := 1;
      B.Tail := 1;
      B.Count := 0;
   end Clear;

   -------------
   -- Discard --
   -------------

   procedure Discard (B : in Out Buffer; Count : Natural) is
      To_Discard : constant Natural := Natural'Min (Count, B.Count);
   begin
      for I in 1 .. To_Discard loop
         if B.Head = B.Cap then
            B.Head := 1;
         else
            B.Head := B.Head + 1;
         end if;
      end loop;
      B.Count := B.Count - To_Discard;
   end Discard;

   -- Byte buffer implementation

   function Create_Byte (Capacity : Positive) return Byte_Buffer is
      B : Byte_Buffer;
   begin
      B.Cap := Positive'Min (Capacity, Max_Capacity);
      return B;
   end Create_Byte;

   function Is_Empty (B : Byte_Buffer) return Boolean is
   begin
      return B.Count = 0;
   end Is_Empty;

   function Is_Full (B : Byte_Buffer) return Boolean is
   begin
      return B.Count = B.Cap;
   end Is_Full;

   function Length (B : Byte_Buffer) return Natural is
   begin
      return B.Count;
   end Length;

   function Capacity (B : Byte_Buffer) return Positive is
   begin
      return B.Cap;
   end Capacity;

   procedure Put (B : in Out Byte_Buffer; Item : Natural) is
   begin
      B.Data (B.Tail) := Item mod 256;

      if B.Tail = B.Cap then
         B.Tail := 1;
      else
         B.Tail := B.Tail + 1;
      end if;

      if B.Count = B.Cap then
         if B.Head = B.Cap then
            B.Head := 1;
         else
            B.Head := B.Head + 1;
         end if;
      else
         B.Count := B.Count + 1;
      end if;
   end Put;

   function Get (B : in Out Byte_Buffer) return Natural is
      V : Natural;
   begin
      if B.Count = 0 then
         return 0;
      end if;

      V := B.Data (B.Head);

      if B.Head = B.Cap then
         B.Head := 1;
      else
         B.Head := B.Head + 1;
      end if;

      B.Count := B.Count - 1;
      return V;
   end Get;

   function Peek (B : Byte_Buffer) return Natural is
   begin
      if B.Count = 0 then
         return 0;
      end if;
      return B.Data (B.Head);
   end Peek;

   procedure Clear (B : out Byte_Buffer) is
   begin
      B.Head := 1;
      B.Tail := 1;
      B.Count := 0;
   end Clear;

end GNAT.Circular_Buffer;
