-- System.Memop body for Z80
-- Low-level memory operations

package body System.Memop is

   type Byte_Ptr is access all Storage_Element;
   for Byte_Ptr'Storage_Size use 0;

   ------------
   -- Memcpy --
   ------------

   procedure Memcpy
     (Dest   : System.Address;
      Source : System.Address;
      Size   : Storage_Count)
   is
      D : System.Address := Dest;
      S : System.Address := Source;
   begin
      for I in 1 .. Size loop
         declare
            D_Ptr : constant Byte_Ptr := Byte_Ptr (D);
            S_Ptr : constant Byte_Ptr := Byte_Ptr (S);
         begin
            D_Ptr.all := S_Ptr.all;
         end;
         D := D + 1;
         S := S + 1;
      end loop;
   end Memcpy;

   -------------
   -- Memmove --
   -------------

   procedure Memmove
     (Dest   : System.Address;
      Source : System.Address;
      Size   : Storage_Count)
   is
   begin
      if Dest = Source or else Size = 0 then
         return;
      end if;

      if Dest < Source then
         -- Copy forward
         Memcpy (Dest, Source, Size);
      else
         -- Copy backward to handle overlap
         declare
            D : System.Address := Dest + Storage_Offset (Size - 1);
            S : System.Address := Source + Storage_Offset (Size - 1);
         begin
            for I in 1 .. Size loop
               declare
                  D_Ptr : constant Byte_Ptr := Byte_Ptr (D);
                  S_Ptr : constant Byte_Ptr := Byte_Ptr (S);
               begin
                  D_Ptr.all := S_Ptr.all;
               end;
               D := D - 1;
               S := S - 1;
            end loop;
         end;
      end if;
   end Memmove;

   ------------
   -- Memset --
   ------------

   procedure Memset
     (Dest  : System.Address;
      Value : Storage_Element;
      Size  : Storage_Count)
   is
      D : System.Address := Dest;
   begin
      for I in 1 .. Size loop
         declare
            D_Ptr : constant Byte_Ptr := Byte_Ptr (D);
         begin
            D_Ptr.all := Value;
         end;
         D := D + 1;
      end loop;
   end Memset;

   ------------
   -- Memcmp --
   ------------

   function Memcmp
     (Left  : System.Address;
      Right : System.Address;
      Size  : Storage_Count) return Integer
   is
      L : System.Address := Left;
      R : System.Address := Right;
   begin
      for I in 1 .. Size loop
         declare
            L_Ptr : constant Byte_Ptr := Byte_Ptr (L);
            R_Ptr : constant Byte_Ptr := Byte_Ptr (R);
         begin
            if L_Ptr.all < R_Ptr.all then
               return -1;
            elsif L_Ptr.all > R_Ptr.all then
               return 1;
            end if;
         end;
         L := L + 1;
         R := R + 1;
      end loop;
      return 0;
   end Memcmp;

   ------------
   -- Memchr --
   ------------

   function Memchr
     (Source : System.Address;
      Value  : Storage_Element;
      Size   : Storage_Count) return System.Address
   is
      S : System.Address := Source;
   begin
      for I in 1 .. Size loop
         declare
            S_Ptr : constant Byte_Ptr := Byte_Ptr (S);
         begin
            if S_Ptr.all = Value then
               return S;
            end if;
         end;
         S := S + 1;
      end loop;
      return System.Null_Address;
   end Memchr;

end System.Memop;
