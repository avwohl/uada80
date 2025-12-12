-- Interfaces.C body for Z80
-- C interface function implementations

package body Interfaces.C is

   ----------
   -- To_C --
   ----------

   function To_C (Item : String; Append_Nul : Boolean := True) return char_array is
      Result_Len : constant size_t := Item'Length + (if Append_Nul then 1 else 0);
      Result : char_array (0 .. Result_Len - 1);
      J : size_t := 0;
   begin
      for I in Item'Range loop
         Result (J) := char (Item (I));
         J := J + 1;
      end loop;

      if Append_Nul then
         Result (J) := nul;
      end if;

      return Result;
   end To_C;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Item : char_array; Trim_Nul : Boolean := True) return String is
      Len : size_t := Item'Length;
   begin
      -- Find null terminator if trimming
      if Trim_Nul then
         Len := 0;
         for I in Item'Range loop
            exit when Item (I) = nul;
            Len := Len + 1;
         end loop;
      end if;

      declare
         Result : String (1 .. Natural (Len));
         J : Positive := 1;
      begin
         for I in Item'First .. Item'First + Len - 1 loop
            Result (J) := Character (Item (I));
            J := J + 1;
         end loop;
         return Result;
      end;
   end To_Ada;

   ----------
   -- To_C --
   ----------

   function To_C
     (Item       : String;
      Target     : out char_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   is
      J : size_t := Target'First;
   begin
      Count := 0;
      for I in Item'Range loop
         exit when J > Target'Last;
         Target (J) := char (Item (I));
         J := J + 1;
         Count := Count + 1;
      end loop;

      if Append_Nul and J <= Target'Last then
         Target (J) := nul;
         Count := Count + 1;
      end if;
   end To_C;

   -----------
   -- Value --
   -----------

   function Value (Item : chars_ptr) return String is
      Len : size_t := Strlen (Item);
      Result : String (1 .. Natural (Len));
      Ptr : chars_ptr := Item;

      -- Access character at address
      type Char_Ptr is access all char;
      function To_Char_Ptr is new Ada.Unchecked_Conversion
        (Source => chars_ptr, Target => Char_Ptr);
   begin
      for I in Result'Range loop
         declare
            C_Ptr : Char_Ptr := To_Char_Ptr (Ptr);
         begin
            Result (I) := Character (C_Ptr.all);
            Ptr := chars_ptr (System.Address (Ptr) + 1);
         end;
      end loop;
      return Result;
   end Value;

   function Value (Item : chars_ptr; Length : size_t) return String is
      Result : String (1 .. Natural (Length));
      Ptr : chars_ptr := Item;

      type Char_Ptr is access all char;
      function To_Char_Ptr is new Ada.Unchecked_Conversion
        (Source => chars_ptr, Target => Char_Ptr);
   begin
      for I in Result'Range loop
         declare
            C_Ptr : Char_Ptr := To_Char_Ptr (Ptr);
         begin
            Result (I) := Character (C_Ptr.all);
            Ptr := chars_ptr (System.Address (Ptr) + 1);
         end;
      end loop;
      return Result;
   end Value;

   ------------
   -- Strlen --
   ------------

   function Strlen (Item : chars_ptr) return size_t is
      Len : size_t := 0;
      Ptr : chars_ptr := Item;

      type Char_Ptr is access all char;
      function To_Char_Ptr is new Ada.Unchecked_Conversion
        (Source => chars_ptr, Target => Char_Ptr);
   begin
      if Item = Null_Ptr then
         return 0;
      end if;

      loop
         declare
            C_Ptr : Char_Ptr := To_Char_Ptr (Ptr);
         begin
            exit when C_Ptr.all = nul;
            Len := Len + 1;
            Ptr := chars_ptr (System.Address (Ptr) + 1);
         end;
      end loop;

      return Len;
   end Strlen;

   --------------------
   -- New_Char_Array --
   --------------------

   function New_Char_Array (Chars : char_array) return chars_ptr is
      -- Would require heap allocation
   begin
      return Null_Ptr;  -- Not implemented without heap
   end New_Char_Array;

   ----------------
   -- New_String --
   ----------------

   function New_String (Str : String) return chars_ptr is
   begin
      return Null_Ptr;  -- Not implemented without heap
   end New_String;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in Out chars_ptr) is
   begin
      Item := Null_Ptr;  -- Not implemented without heap
   end Free;

end Interfaces.C;
