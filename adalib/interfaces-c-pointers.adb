-- Interfaces.C.Pointers body for Z80
-- Generic C pointer operations implementation

with System.Storage_Elements;

package body Interfaces.C.Pointers is

   use type System.Address;
   use type Interfaces.C.ptrdiff_t;

   Element_Size : constant Interfaces.C.ptrdiff_t :=
     Interfaces.C.ptrdiff_t (Element'Size / 8);

   -----------
   -- Value --
   -----------

   function Value (Ref : Pointer) return Element is
   begin
      if Ref = null then
         raise Pointer_Error;
      end if;
      return Ref.all;
   end Value;

   function Value
     (Ref    : Pointer;
      Length : Interfaces.C.ptrdiff_t) return Element_Array
   is
      Result : Element_Array (Index'First .. Index'Val (Index'Pos (Index'First) + Integer (Length) - 1));
      P      : Pointer := Ref;
   begin
      if Ref = null then
         raise Pointer_Error;
      end if;

      for I in Result'Range loop
         Result (I) := P.all;
         Increment (P);
      end loop;

      return Result;
   end Value;

   function Value
     (Ref        : Pointer;
      Terminator : Element := Default_Terminator) return Element_Array
   is
      Len : constant Interfaces.C.ptrdiff_t := Virtual_Length (Ref, Terminator);
   begin
      return Value (Ref, Len);
   end Value;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Pointer; Right : Interfaces.C.ptrdiff_t) return Pointer is
      use System.Storage_Elements;
   begin
      if Left = null then
         raise Pointer_Error;
      end if;

      declare
         Addr : constant System.Address := Left.all'Address;
         New_Addr : constant System.Address :=
           Addr + Storage_Offset (Right * Element_Size);
         Result : Pointer;
         for Result'Address use New_Addr'Address;
      begin
         return Pointer (System.Storage_Elements.To_Address (
           System.Storage_Elements.To_Integer (Addr) +
           System.Storage_Elements.Integer_Address (Right * Element_Size)));
      end;
   end "+";

   function "+" (Left : Interfaces.C.ptrdiff_t; Right : Pointer) return Pointer is
   begin
      return Right + Left;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Pointer; Right : Interfaces.C.ptrdiff_t) return Pointer is
   begin
      return Left + (-Right);
   end "-";

   function "-" (Left : Pointer; Right : Pointer) return Interfaces.C.ptrdiff_t is
      use System.Storage_Elements;
      Left_Int  : constant Integer_Address := To_Integer (Left.all'Address);
      Right_Int : constant Integer_Address := To_Integer (Right.all'Address);
   begin
      if Left = null or Right = null then
         raise Pointer_Error;
      end if;

      return Interfaces.C.ptrdiff_t ((Left_Int - Right_Int) /
        Integer_Address (Element_Size));
   end "-";

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Ref : in Out Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement (Ref : in Out Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   ---------------------------
   -- Copy_Terminated_Array --
   ---------------------------

   procedure Copy_Terminated_Array
     (Source     : Pointer;
      Target     : Pointer;
      Limit      : Interfaces.C.ptrdiff_t := Interfaces.C.ptrdiff_t'Last;
      Terminator : Element := Default_Terminator)
   is
      S : Pointer := Source;
      T : Pointer := Target;
      Count : Interfaces.C.ptrdiff_t := 0;
   begin
      if Source = null or Target = null then
         raise Pointer_Error;
      end if;

      loop
         T.all := S.all;
         exit when S.all = Terminator or Count >= Limit;
         Increment (S);
         Increment (T);
         Count := Count + 1;
      end loop;
   end Copy_Terminated_Array;

   ----------------
   -- Copy_Array --
   ----------------

   procedure Copy_Array
     (Source : Pointer;
      Target : Pointer;
      Length : Interfaces.C.ptrdiff_t)
   is
      S : Pointer := Source;
      T : Pointer := Target;
   begin
      if Source = null or Target = null then
         raise Pointer_Error;
      end if;

      for I in 1 .. Length loop
         T.all := S.all;
         Increment (S);
         Increment (T);
      end loop;
   end Copy_Array;

   --------------------
   -- Virtual_Length --
   --------------------

   function Virtual_Length
     (Ref        : Pointer;
      Terminator : Element := Default_Terminator) return Interfaces.C.ptrdiff_t
   is
      P   : Pointer := Ref;
      Len : Interfaces.C.ptrdiff_t := 0;
      Max_Len : constant Interfaces.C.ptrdiff_t := 1000;  -- Safety limit for Z80
   begin
      if Ref = null then
         raise Pointer_Error;
      end if;

      while P.all /= Terminator and Len < Max_Len loop
         Increment (P);
         Len := Len + 1;
      end loop;

      return Len;
   end Virtual_Length;

end Interfaces.C.Pointers;
