-- System.Put_Images body for Z80
-- Support for 'Put_Image attribute

package body System.Put_Images is

   ----------
   -- Init --
   ----------

   procedure Init (B : out Buffer) is
   begin
      B.Last := 0;
   end Init;

   ---------
   -- Put --
   ---------

   procedure Put (B : in Out Buffer; C : Character) is
   begin
      if B.Last < Max_Buffer_Size then
         B.Last := B.Last + 1;
         B.Data (B.Last) := C;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (B : in Out Buffer; S : String) is
   begin
      for I in S'Range loop
         Put (B, S (I));
      end loop;
   end Put;

   -----------------
   -- Put_Integer --
   -----------------

   procedure Put_Integer (B : in Out Buffer; Val : Integer) is
      Buf : String (1 .. 12);
      Len : Natural := 0;
      V   : Integer := abs Val;
   begin
      -- Handle zero
      if V = 0 then
         Put (B, '0');
         return;
      end if;

      -- Build digits in reverse
      while V > 0 loop
         Len := Len + 1;
         Buf (Len) := Character'Val (Character'Pos ('0') + V mod 10);
         V := V / 10;
      end loop;

      -- Output sign
      if Val < 0 then
         Put (B, '-');
      end if;

      -- Output digits in correct order
      for I in reverse 1 .. Len loop
         Put (B, Buf (I));
      end loop;
   end Put_Integer;

   -----------------
   -- Put_Boolean --
   -----------------

   procedure Put_Boolean (B : in Out Buffer; Val : Boolean) is
   begin
      if Val then
         Put (B, "TRUE");
      else
         Put (B, "FALSE");
      end if;
   end Put_Boolean;

   ---------
   -- Get --
   ---------

   function Get (B : Buffer) return String is
   begin
      return B.Data (1 .. B.Last);
   end Get;

   ------------
   -- Length --
   ------------

   function Length (B : Buffer) return Natural is
   begin
      return B.Last;
   end Length;

end System.Put_Images;
