-- System.Wide_String_Ops body for Z80
-- Wide string operations (limited support)

package body System.Wide_String_Ops is

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Wide_String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;

      for I in 0 .. Left'Length - 1 loop
         if Left (Left'First + I) /= Right (Right'First + I) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Wide_String) return Boolean is
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 0 .. Min_Len - 1 loop
         if Left (Left'First + I) < Right (Right'First + I) then
            return True;
         elsif Left (Left'First + I) > Right (Right'First + I) then
            return False;
         end if;
      end loop;

      return Left'Length < Right'Length;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Wide_String) return Boolean is
   begin
      return Left < Right or else Left = Right;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Wide_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Wide_String) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Wide_String) return Wide_String is
      Result : Wide_String (1 .. Left'Length + Right'Length);
   begin
      Result (1 .. Left'Length) := Left;
      Result (Left'Length + 1 .. Result'Last) := Right;
      return Result;
   end "&";

   function "&" (Left : Wide_String; Right : Wide_Character) return Wide_String is
      Result : Wide_String (1 .. Left'Length + 1);
   begin
      Result (1 .. Left'Length) := Left;
      Result (Result'Last) := Right;
      return Result;
   end "&";

   function "&" (Left : Wide_Character; Right : Wide_String) return Wide_String is
      Result : Wide_String (1 .. Right'Length + 1);
   begin
      Result (1) := Left;
      Result (2 .. Result'Last) := Right;
      return Result;
   end "&";

   ------------
   -- Length --
   ------------

   function Length (S : Wide_String) return Natural is
   begin
      return S'Length;
   end Length;

   ---------------
   -- To_String --
   ---------------

   function To_String (S : Wide_String) return String is
      Result : String (S'Range);
   begin
      for I in S'Range loop
         if Natural (S (I)) <= 127 then
            Result (I) := Character'Val (Natural (S (I)));
         else
            Result (I) := '?';  -- Non-ASCII replacement
         end if;
      end loop;
      return Result;
   end To_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (S : String) return Wide_String is
      Result : Wide_String (S'Range);
   begin
      for I in S'Range loop
         Result (I) := Wide_Character (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Wide_String;

end System.Wide_String_Ops;
