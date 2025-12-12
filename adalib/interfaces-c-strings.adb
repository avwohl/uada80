-- Interfaces.C.Strings body for Z80
-- C string pointer handling implementation

with Ada.Unchecked_Deallocation;

package body Interfaces.C.Strings is

   -- Simple memory allocation (static buffer approach for Z80)
   -- In practice, this would interface with z88dk's malloc/free

   Max_String_Length : constant := 256;

   type String_Buffer is record
      Data : Interfaces.C.char_array (0 .. Interfaces.C.size_t (Max_String_Length));
      In_Use : Boolean := False;
   end record;

   -- Small pool of string buffers for Z80
   Max_Buffers : constant := 8;
   Buffers : array (1 .. Max_Buffers) of aliased String_Buffer;

   ----------------
   -- New_String --
   ----------------

   function New_String (Str : String) return chars_ptr is
      use Interfaces.C;
   begin
      -- Find a free buffer
      for I in Buffers'Range loop
         if not Buffers (I).In_Use then
            Buffers (I).In_Use := True;

            -- Copy string with null terminator
            for J in Str'Range loop
               Buffers (I).Data (size_t (J - Str'First)) :=
                 char (Str (J));
            end loop;
            Buffers (I).Data (size_t (Str'Length)) := nul;

            return chars_ptr (Buffers (I).Data'Address);
         end if;
      end loop;

      -- No free buffers
      return Null_Ptr;
   end New_String;

   --------------------
   -- New_Char_Array --
   --------------------

   function New_Char_Array (Chars : Interfaces.C.char_array) return chars_ptr is
      use Interfaces.C;
   begin
      -- Find a free buffer
      for I in Buffers'Range loop
         if not Buffers (I).In_Use then
            Buffers (I).In_Use := True;

            -- Copy char array
            for J in Chars'Range loop
               exit when J > size_t (Max_String_Length);
               Buffers (I).Data (J - Chars'First) := Chars (J);
            end loop;

            return chars_ptr (Buffers (I).Data'Address);
         end if;
      end loop;

      return Null_Ptr;
   end New_Char_Array;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in Out chars_ptr) is
      use type System.Address;
   begin
      if Item = Null_Ptr then
         return;
      end if;

      -- Find and release the buffer
      for I in Buffers'Range loop
         if Buffers (I).Data'Address = System.Address (Item) then
            Buffers (I).In_Use := False;
            Item := Null_Ptr;
            return;
         end if;
      end loop;

      -- Not from our pool - just null it
      Item := Null_Ptr;
   end Free;

   -----------
   -- Value --
   -----------

   function Value (Item : chars_ptr) return Interfaces.C.char_array is
      use Interfaces.C;
      Len : constant size_t := Strlen (Item);
   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      declare
         Source : char_array (0 .. Len);
         for Source'Address use System.Address (Item);
      begin
         return Source;
      end;
   end Value;

   function Value (Item : chars_ptr; Length : Interfaces.C.size_t)
     return Interfaces.C.char_array
   is
      use Interfaces.C;
   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      declare
         Source : char_array (0 .. Length - 1);
         for Source'Address use System.Address (Item);
      begin
         return Source;
      end;
   end Value;

   ------------
   -- Strlen --
   ------------

   function Strlen (Item : chars_ptr) return Interfaces.C.size_t is
      use Interfaces.C;
      Len : size_t := 0;
      Source : char_array (0 .. size_t (Max_String_Length));
      for Source'Address use System.Address (Item);
   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      while Source (Len) /= nul and Len < size_t (Max_String_Length) loop
         Len := Len + 1;
      end loop;

      return Len;
   end Strlen;

   ------------
   -- Update --
   ------------

   procedure Update
     (Item   : chars_ptr;
      Offset : Interfaces.C.size_t;
      Chars  : Interfaces.C.char_array;
      Check  : Boolean := True)
   is
      use Interfaces.C;
      Dest : char_array (0 .. size_t (Max_String_Length));
      for Dest'Address use System.Address (Item);
   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      if Check then
         -- Verify we're within bounds
         declare
            Len : constant size_t := Strlen (Item);
         begin
            if Offset + Chars'Length > Len + 1 then
               raise Constraint_Error;
            end if;
         end;
      end if;

      for I in Chars'Range loop
         Dest (Offset + I - Chars'First) := Chars (I);
      end loop;
   end Update;

   procedure Update
     (Item   : chars_ptr;
      Offset : Interfaces.C.size_t;
      Str    : String;
      Check  : Boolean := True)
   is
      use Interfaces.C;
      Chars : char_array (0 .. size_t (Str'Length) - 1);
   begin
      for I in Str'Range loop
         Chars (size_t (I - Str'First)) := char (Str (I));
      end loop;

      Update (Item, Offset, Chars, Check);
   end Update;

end Interfaces.C.Strings;
