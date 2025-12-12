-- Ada.Strings.Bounded body for Z80
-- Bounded-length string handling implementation

package body Ada.Strings.Bounded is

   ------------
   -- Length --
   ------------

   function Length (Source : Bounded_String) return Length_Range is
   begin
      return Source.Length;
   end Length;

   -------------------------
   -- To_Bounded_String --
   -------------------------

   function To_Bounded_String
     (Source : String;
      Drop   : Truncation := Error) return Bounded_String
   is
      Result : Bounded_String;
   begin
      if Source'Length > Max_Length then
         case Drop is
            when Error =>
               raise Length_Error;
            when Left =>
               Result.Length := Max_Length;
               for I in 1 .. Max_Length loop
                  Result.Data (I) := Source (Source'Last - Max_Length + I);
               end loop;
            when Right =>
               Result.Length := Max_Length;
               for I in 1 .. Max_Length loop
                  Result.Data (I) := Source (Source'First + I - 1);
               end loop;
         end case;
      else
         Result.Length := Source'Length;
         for I in 1 .. Source'Length loop
            Result.Data (I) := Source (Source'First + I - 1);
         end loop;
      end if;
      return Result;
   end To_Bounded_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Source : Bounded_String) return String is
   begin
      return Source.Data (1 .. Source.Length);
   end To_String;

   ------------------------
   -- Set_Bounded_String --
   ------------------------

   procedure Set_Bounded_String
     (Target : out Bounded_String;
      Source : String;
      Drop   : Truncation := Error)
   is
   begin
      Target := To_Bounded_String (Source, Drop);
   end Set_Bounded_String;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in Out Bounded_String;
      New_Item : Bounded_String;
      Drop     : Truncation := Error)
   is
      New_Len : Natural := Source.Length + New_Item.Length;
   begin
      if New_Len > Max_Length then
         case Drop is
            when Error =>
               raise Length_Error;
            when Left =>
               -- Keep rightmost characters
               declare
                  Combined : String (1 .. New_Len);
               begin
                  Combined (1 .. Source.Length) := Source.Data (1 .. Source.Length);
                  Combined (Source.Length + 1 .. New_Len) := New_Item.Data (1 .. New_Item.Length);
                  Source.Length := Max_Length;
                  for I in 1 .. Max_Length loop
                     Source.Data (I) := Combined (New_Len - Max_Length + I);
                  end loop;
               end;
            when Right =>
               -- Keep leftmost characters
               declare
                  Space_Left : Natural := Max_Length - Source.Length;
               begin
                  for I in 1 .. Space_Left loop
                     Source.Data (Source.Length + I) := New_Item.Data (I);
                  end loop;
                  Source.Length := Max_Length;
               end;
         end case;
      else
         for I in 1 .. New_Item.Length loop
            Source.Data (Source.Length + I) := New_Item.Data (I);
         end loop;
         Source.Length := New_Len;
      end if;
   end Append;

   procedure Append
     (Source   : in Out Bounded_String;
      New_Item : String;
      Drop     : Truncation := Error)
   is
   begin
      Append (Source, To_Bounded_String (New_Item, Drop), Drop);
   end Append;

   procedure Append
     (Source   : in Out Bounded_String;
      New_Item : Character;
      Drop     : Truncation := Error)
   is
   begin
      if Source.Length >= Max_Length then
         case Drop is
            when Error =>
               raise Length_Error;
            when Left =>
               for I in 1 .. Max_Length - 1 loop
                  Source.Data (I) := Source.Data (I + 1);
               end loop;
               Source.Data (Max_Length) := New_Item;
            when Right =>
               null;  -- Drop the new character
         end case;
      else
         Source.Length := Source.Length + 1;
         Source.Data (Source.Length) := New_Item;
      end if;
   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Bounded_String) return Bounded_String is
      Result : Bounded_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Bounded_String; Right : String) return Bounded_String is
      Result : Bounded_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : String; Right : Bounded_String) return Bounded_String is
      Result : Bounded_String := To_Bounded_String (Left);
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Bounded_String; Right : Character) return Bounded_String is
      Result : Bounded_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Character; Right : Bounded_String) return Bounded_String is
      Result : Bounded_String;
   begin
      Result.Length := 1;
      Result.Data (1) := Left;
      Append (Result, Right);
      return Result;
   end "&";

   -------------
   -- Element --
   -------------

   function Element
     (Source : Bounded_String;
      Index  : Positive) return Character
   is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;
      return Source.Data (Index);
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in Out Bounded_String;
      Index  : Positive;
      By     : Character)
   is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;
      Source.Data (Index) := By;
   end Replace_Element;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return String
   is
   begin
      if Low > Source.Length + 1 or High > Source.Length then
         raise Index_Error;
      end if;
      return Source.Data (Low .. High);
   end Slice;

   -------------------
   -- Bounded_Slice --
   -------------------

   function Bounded_Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return Bounded_String
   is
   begin
      return To_Bounded_String (Slice (Source, Low, High));
   end Bounded_Slice;

   procedure Bounded_Slice
     (Source : Bounded_String;
      Target : out Bounded_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      Target := Bounded_Slice (Source, Low, High);
   end Bounded_Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Bounded_String) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;
      for I in 1 .. Left.Length loop
         if Left.Data (I) /= Right.Data (I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   function "=" (Left : Bounded_String; Right : String) return Boolean is
   begin
      return To_String (Left) = Right;
   end "=";

   function "=" (Left : String; Right : Bounded_String) return Boolean is
   begin
      return Right = Left;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Bounded_String) return Boolean is
   begin
      return To_String (Left) < To_String (Right);
   end "<";

   function "<" (Left : Bounded_String; Right : String) return Boolean is
   begin
      return To_String (Left) < Right;
   end "<";

   function "<" (Left : String; Right : Bounded_String) return Boolean is
   begin
      return Left < To_String (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Bounded_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left : Bounded_String; Right : String) return Boolean is
   begin
      return not (Right < To_String (Left));
   end "<=";

   function "<=" (Left : String; Right : Bounded_String) return Boolean is
   begin
      return not (To_String (Right) < Left);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Bounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Bounded_String; Right : String) return Boolean is
   begin
      return Right < To_String (Left);
   end ">";

   function ">" (Left : String; Right : Bounded_String) return Boolean is
   begin
      return To_String (Right) < Left;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Bounded_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : Bounded_String; Right : String) return Boolean is
   begin
      return not (To_String (Left) < Right);
   end ">=";

   function ">=" (Left : String; Right : Bounded_String) return Boolean is
   begin
      return not (Left < To_String (Right));
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Bounded_String;
      Pattern : String;
      Going   : Direction := Forward) return Natural
   is
      Pat_Len : Natural := Pattern'Length;
   begin
      if Pat_Len = 0 then
         raise Pattern_Error;
      end if;

      if Pat_Len > Source.Length then
         return 0;
      end if;

      if Going = Forward then
         for I in 1 .. Source.Length - Pat_Len + 1 loop
            declare
               Match : Boolean := True;
            begin
               for J in 1 .. Pat_Len loop
                  if Source.Data (I + J - 1) /= Pattern (Pattern'First + J - 1) then
                     Match := False;
                     exit;
                  end if;
               end loop;
               if Match then
                  return I;
               end if;
            end;
         end loop;
      else
         for I in reverse 1 .. Source.Length - Pat_Len + 1 loop
            declare
               Match : Boolean := True;
            begin
               for J in 1 .. Pat_Len loop
                  if Source.Data (I + J - 1) /= Pattern (Pattern'First + J - 1) then
                     Match := False;
                     exit;
                  end if;
               end loop;
               if Match then
                  return I;
               end if;
            end;
         end loop;
      end if;

      return 0;
   end Index;

   function Index
     (Source : Bounded_String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
      function In_Set (C : Character) return Boolean is
      begin
         for I in Set'Range loop
            if C = Set (I) then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;
   begin
      if Going = Forward then
         for I in 1 .. Source.Length loop
            if (Test = Inside and In_Set (Source.Data (I))) or
               (Test = Outside and not In_Set (Source.Data (I)))
            then
               return I;
            end if;
         end loop;
      else
         for I in reverse 1 .. Source.Length loop
            if (Test = Inside and In_Set (Source.Data (I))) or
               (Test = Outside and not In_Set (Source.Data (I)))
            then
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
     (Source : Bounded_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for I in 1 .. Source.Length loop
            if Source.Data (I) /= ' ' then
               return I;
            end if;
         end loop;
      else
         for I in reverse 1 .. Source.Length loop
            if Source.Data (I) /= ' ' then
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
     (Source  : Bounded_String;
      Pattern : String) return Natural
   is
      Result  : Natural := 0;
      Pat_Len : Natural := Pattern'Length;
      I       : Natural := 1;
   begin
      if Pat_Len = 0 then
         raise Pattern_Error;
      end if;

      while I <= Source.Length - Pat_Len + 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 1 .. Pat_Len loop
               if Source.Data (I + J - 1) /= Pattern (Pattern'First + J - 1) then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               Result := Result + 1;
               I := I + Pat_Len;
            else
               I := I + 1;
            end if;
         end;
      end loop;

      return Result;
   end Count;

   function Count
     (Source : Bounded_String;
      Set    : String) return Natural
   is
      Result : Natural := 0;

      function In_Set (C : Character) return Boolean is
      begin
         for I in Set'Range loop
            if C = Set (I) then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;
   begin
      for I in 1 .. Source.Length loop
         if In_Set (Source.Data (I)) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String;
      Drop   : Truncation := Error) return Bounded_String
   is
   begin
      return To_Bounded_String (
        Source.Data (1 .. Low - 1) & By & Source.Data (High + 1 .. Source.Length),
        Drop);
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in Out Bounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String;
      Drop   : Truncation := Error)
   is
   begin
      Source := Replace_Slice (Source, Low, High, By, Drop);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Bounded_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error) return Bounded_String
   is
   begin
      return Replace_Slice (Source, Before, Before - 1, New_Item, Drop);
   end Insert;

   procedure Insert
     (Source   : in Out Bounded_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   is
   begin
      Source := Insert (Source, Before, New_Item, Drop);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Bounded_String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Error) return Bounded_String
   is
      End_Pos : Natural := Position + New_Item'Length - 1;
   begin
      if Position > Source.Length + 1 then
         raise Index_Error;
      end if;

      if End_Pos <= Source.Length then
         return Replace_Slice (Source, Position, End_Pos, New_Item, Drop);
      else
         return To_Bounded_String (
           Source.Data (1 .. Position - 1) & New_Item,
           Drop);
      end if;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out Bounded_String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   is
   begin
      Source := Overwrite (Source, Position, New_Item, Drop);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Bounded_String;
      From    : Positive;
      Through : Natural) return Bounded_String
   is
   begin
      if Through < From then
         return Source;
      end if;
      return To_Bounded_String (
        Source.Data (1 .. From - 1) & Source.Data (Through + 1 .. Source.Length));
   end Delete;

   procedure Delete
     (Source  : in Out Bounded_String;
      From    : Positive;
      Through : Natural)
   is
   begin
      Source := Delete (Source, From, Through);
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Bounded_String;
      Side   : Trim_End) return Bounded_String
   is
      First : Positive := 1;
      Last  : Natural := Source.Length;
   begin
      if Source.Length = 0 then
         return Null_Bounded_String;
      end if;

      if Side = Left or Side = Both then
         while First <= Source.Length and then Source.Data (First) = ' ' loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while Last >= First and then Source.Data (Last) = ' ' loop
            Last := Last - 1;
         end loop;
      end if;

      if First > Last then
         return Null_Bounded_String;
      end if;

      return Bounded_Slice (Source, First, Last);
   end Trim;

   procedure Trim
     (Source : in Out Bounded_String;
      Side   : Trim_End)
   is
   begin
      Source := Trim (Source, Side);
   end Trim;

   function Trim
     (Source : Bounded_String;
      Left   : String;
      Right  : String) return Bounded_String
   is
      First : Positive := 1;
      Last  : Natural := Source.Length;

      function In_Left (C : Character) return Boolean is
      begin
         for I in Left'Range loop
            if C = Left (I) then
               return True;
            end if;
         end loop;
         return False;
      end In_Left;

      function In_Right (C : Character) return Boolean is
      begin
         for I in Right'Range loop
            if C = Right (I) then
               return True;
            end if;
         end loop;
         return False;
      end In_Right;
   begin
      if Source.Length = 0 then
         return Null_Bounded_String;
      end if;

      while First <= Source.Length and then In_Left (Source.Data (First)) loop
         First := First + 1;
      end loop;

      while Last >= First and then In_Right (Source.Data (Last)) loop
         Last := Last - 1;
      end loop;

      if First > Last then
         return Null_Bounded_String;
      end if;

      return Bounded_Slice (Source, First, Last);
   end Trim;

   procedure Trim
     (Source : in Out Bounded_String;
      Left   : String;
      Right  : String)
   is
   begin
      Source := Trim (Source, Left, Right);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error) return Bounded_String
   is
      Result : Bounded_String;
   begin
      if Count > Max_Length then
         case Drop is
            when Error =>
               raise Length_Error;
            when others =>
               -- Will be truncated below
               null;
         end case;
      end if;

      if Count <= Source.Length then
         Result.Length := Count;
         for I in 1 .. Count loop
            Result.Data (I) := Source.Data (I);
         end loop;
      else
         declare
            Final_Len : Natural := Natural'Min (Count, Max_Length);
         begin
            Result.Length := Final_Len;
            for I in 1 .. Source.Length loop
               Result.Data (I) := Source.Data (I);
            end loop;
            for I in Source.Length + 1 .. Final_Len loop
               Result.Data (I) := Pad;
            end loop;
         end;
      end if;

      return Result;
   end Head;

   procedure Head
     (Source : in Out Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error)
   is
   begin
      Source := Head (Source, Count, Pad, Drop);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error) return Bounded_String
   is
      Result : Bounded_String;
   begin
      if Count > Max_Length then
         case Drop is
            when Error =>
               raise Length_Error;
            when others =>
               null;
         end case;
      end if;

      if Count <= Source.Length then
         Result.Length := Count;
         for I in 1 .. Count loop
            Result.Data (I) := Source.Data (Source.Length - Count + I);
         end loop;
      else
         declare
            Final_Len : Natural := Natural'Min (Count, Max_Length);
            Pad_Count : Natural := Final_Len - Source.Length;
         begin
            Result.Length := Final_Len;
            for I in 1 .. Pad_Count loop
               Result.Data (I) := Pad;
            end loop;
            for I in 1 .. Source.Length loop
               Result.Data (Pad_Count + I) := Source.Data (I);
            end loop;
         end;
      end if;

      return Result;
   end Tail;

   procedure Tail
     (Source : in Out Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error)
   is
   begin
      Source := Tail (Source, Count, Pad, Drop);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character) return Bounded_String
   is
      Result : Bounded_String;
      Len    : Natural := Natural'Min (Left, Max_Length);
   begin
      Result.Length := Len;
      for I in 1 .. Len loop
         Result.Data (I) := Right;
      end loop;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : String) return Bounded_String
   is
      Result  : Bounded_String;
      New_Len : Natural := Left * Right'Length;
      Len     : Natural := Natural'Min (New_Len, Max_Length);
   begin
      Result.Length := Len;
      for I in 1 .. Left loop
         exit when (I - 1) * Right'Length >= Len;
         for J in 1 .. Right'Length loop
            declare
               Pos : Natural := (I - 1) * Right'Length + J;
            begin
               exit when Pos > Len;
               Result.Data (Pos) := Right (Right'First + J - 1);
            end;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Bounded_String) return Bounded_String
   is
   begin
      return Left * To_String (Right);
   end "*";

end Ada.Strings.Bounded;
