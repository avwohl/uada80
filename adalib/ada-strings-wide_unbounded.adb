-- Ada.Strings.Wide_Unbounded body for Z80
-- Unbounded wide string implementation

package body Ada.Strings.Wide_Unbounded is

   Wide_Space : constant Wide_Character := Wide_Character'Val (32);

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_Wide_String) return Natural is
   begin
      return Source.Length;
   end Length;

   -----------------------------
   -- To_Unbounded_Wide_String --
   -----------------------------

   function To_Unbounded_Wide_String (Source : Wide_String) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String;
      Len    : constant Natural := Natural'Min (Source'Length, Max_Length);
   begin
      Result.Data (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      Result.Length := Len;
      return Result;
   end To_Unbounded_Wide_String;

   function To_Unbounded_Wide_String (Length : Natural) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String;
      Len    : constant Natural := Natural'Min (Length, Max_Length);
   begin
      Result.Data (1 .. Len) := (others => Wide_Space);
      Result.Length := Len;
      return Result;
   end To_Unbounded_Wide_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Source : Unbounded_Wide_String) return Wide_String is
   begin
      return Source.Data (1 .. Source.Length);
   end To_Wide_String;

   ------------------------------
   -- Set_Unbounded_Wide_String --
   ------------------------------

   procedure Set_Unbounded_Wide_String
     (Target : out Unbounded_Wide_String;
      Source : Wide_String)
   is
      Len : constant Natural := Natural'Min (Source'Length, Max_Length);
   begin
      Target.Data (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      Target.Length := Len;
   end Set_Unbounded_Wide_String;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in Out Unbounded_Wide_String;
      New_Item : Unbounded_Wide_String)
   is
   begin
      Append (Source, To_Wide_String (New_Item));
   end Append;

   procedure Append
     (Source   : in Out Unbounded_Wide_String;
      New_Item : Wide_String)
   is
      Available : constant Natural := Max_Length - Source.Length;
      Add_Len   : constant Natural := Natural'Min (New_Item'Length, Available);
   begin
      if Add_Len > 0 then
         Source.Data (Source.Length + 1 .. Source.Length + Add_Len) :=
           New_Item (New_Item'First .. New_Item'First + Add_Len - 1);
         Source.Length := Source.Length + Add_Len;
      end if;
   end Append;

   procedure Append
     (Source   : in Out Unbounded_Wide_String;
      New_Item : Wide_Character)
   is
   begin
      if Source.Length < Max_Length then
         Source.Length := Source.Length + 1;
         Source.Data (Source.Length) := New_Item;
      end if;
   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Unbounded_Wide_String) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Unbounded_Wide_String; Right : Wide_String) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Wide_String; Right : Unbounded_Wide_String) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String := To_Unbounded_Wide_String (Left);
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Unbounded_Wide_String; Right : Wide_Character) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Wide_Character; Right : Unbounded_Wide_String) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String;
   begin
      Result.Data (1) := Left;
      Result.Length := 1;
      Append (Result, Right);
      return Result;
   end "&";

   -------------
   -- Element --
   -------------

   function Element
     (Source : Unbounded_Wide_String;
      Index  : Positive) return Wide_Character
   is
   begin
      if Index > Source.Length then
         raise Constraint_Error;
      end if;
      return Source.Data (Index);
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in Out Unbounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character)
   is
   begin
      if Index > Source.Length then
         raise Constraint_Error;
      end if;
      Source.Data (Index) := By;
   end Replace_Element;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_String
   is
   begin
      if Low > Source.Length + 1 or High > Source.Length then
         raise Constraint_Error;
      end if;
      return Source.Data (Low .. High);
   end Slice;

   ---------------------
   -- Unbounded_Slice --
   ---------------------

   function Unbounded_Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_String
   is
   begin
      return To_Unbounded_Wide_String (Slice (Source, Low, High));
   end Unbounded_Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Unbounded_Wide_String) return Boolean is
   begin
      return Left.Length = Right.Length and then
             Left.Data (1 .. Left.Length) = Right.Data (1 .. Right.Length);
   end "=";

   function "=" (Left : Unbounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return Left.Length = Right'Length and then
             Left.Data (1 .. Left.Length) = Right;
   end "=";

   function "=" (Left : Wide_String; Right : Unbounded_Wide_String) return Boolean is
   begin
      return Right = Left;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Unbounded_Wide_String) return Boolean is
   begin
      return Left.Data (1 .. Left.Length) < Right.Data (1 .. Right.Length);
   end "<";

   function "<" (Left : Unbounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return Left.Data (1 .. Left.Length) < Right;
   end "<";

   function "<" (Left : Wide_String; Right : Unbounded_Wide_String) return Boolean is
   begin
      return Left < Right.Data (1 .. Right.Length);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Unbounded_Wide_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Unbounded_Wide_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Unbounded_Wide_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward) return Natural
   is
      Str : constant Wide_String := To_Wide_String (Source);
   begin
      if Pattern'Length = 0 or Str'Length < Pattern'Length then
         return 0;
      end if;

      if Going = Forward then
         for I in Str'First .. Str'Last - Pattern'Length + 1 loop
            if Str (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      else
         for I in reverse Str'First .. Str'Last - Pattern'Length + 1 loop
            if Str (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for I in 1 .. Source.Length loop
            if Source.Data (I) /= Wide_Space then
               return I;
            end if;
         end loop;
      else
         for I in reverse 1 .. Source.Length loop
            if Source.Data (I) /= Wide_Space then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index_Non_Blank;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String) return Natural
   is
      Str    : constant Wide_String := To_Wide_String (Source);
      Result : Natural := 0;
      Pos    : Natural;
   begin
      if Pattern'Length = 0 or Str'Length < Pattern'Length then
         return 0;
      end if;

      Pos := Str'First;
      while Pos <= Str'Last - Pattern'Length + 1 loop
         if Str (Pos .. Pos + Pattern'Length - 1) = Pattern then
            Result := Result + 1;
            Pos := Pos + Pattern'Length;
         else
            Pos := Pos + 1;
         end if;
      end loop;

      return Result;
   end Count;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Unbounded_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_String
   is
      First : Natural := 1;
      Last  : Natural := Source.Length;
   begin
      if Source.Length = 0 then
         return Null_Unbounded_Wide_String;
      end if;

      if Side = Left or Side = Both then
         while First <= Last and then Source.Data (First) = Wide_Space loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while Last >= First and then Source.Data (Last) = Wide_Space loop
            Last := Last - 1;
         end loop;
      end if;

      if First > Last then
         return Null_Unbounded_Wide_String;
      else
         return To_Unbounded_Wide_String (Source.Data (First .. Last));
      end if;
   end Trim;

   procedure Trim
     (Source : in Out Unbounded_Wide_String;
      Side   : Trim_End)
   is
   begin
      Source := Trim (Source, Side);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Unbounded_Wide_String
   is
      Result : Unbounded_Wide_String;
      Len    : constant Natural := Natural'Min (Count, Max_Length);
   begin
      if Len <= Source.Length then
         Result.Data (1 .. Len) := Source.Data (1 .. Len);
      else
         Result.Data (1 .. Source.Length) := Source.Data (1 .. Source.Length);
         Result.Data (Source.Length + 1 .. Len) := (others => Pad);
      end if;
      Result.Length := Len;
      return Result;
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Unbounded_Wide_String
   is
      Result : Unbounded_Wide_String;
      Len    : constant Natural := Natural'Min (Count, Max_Length);
   begin
      if Len <= Source.Length then
         Result.Data (1 .. Len) :=
           Source.Data (Source.Length - Len + 1 .. Source.Length);
      else
         declare
            Pad_Count : constant Natural := Len - Source.Length;
         begin
            Result.Data (1 .. Pad_Count) := (others => Pad);
            Result.Data (Pad_Count + 1 .. Len) := Source.Data (1 .. Source.Length);
         end;
      end if;
      Result.Length := Len;
      return Result;
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Wide_Character) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String;
      Len    : constant Natural := Natural'Min (Left, Max_Length);
   begin
      Result.Data (1 .. Len) := (others => Right);
      Result.Length := Len;
      return Result;
   end "*";

   function "*" (Left : Natural; Right : Wide_String) return Unbounded_Wide_String is
      Result : Unbounded_Wide_String;
   begin
      if Left = 0 or Right'Length = 0 then
         return Null_Unbounded_Wide_String;
      end if;

      for I in 1 .. Left loop
         exit when Result.Length + Right'Length > Max_Length;
         Append (Result, Right);
      end loop;

      return Result;
   end "*";

   function "*" (Left : Natural; Right : Unbounded_Wide_String) return Unbounded_Wide_String is
   begin
      return Left * To_Wide_String (Right);
   end "*";

end Ada.Strings.Wide_Unbounded;
