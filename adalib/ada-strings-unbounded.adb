-- Ada.Strings.Unbounded body for Z80
-- Unbounded-length string handling implementation

package body Ada.Strings.Unbounded is

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_String) return Natural is
   begin
      return Source.Length;
   end Length;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String (Source : String) return Unbounded_String is
      Result : Unbounded_String;
      Len    : Natural := Source'Length;
   begin
      if Len > Max_Length then
         raise Length_Error;
      end if;
      Result.Length := Len;
      for I in 1 .. Len loop
         Result.Data (I) := Source (Source'First + I - 1);
      end loop;
      return Result;
   end To_Unbounded_String;

   function To_Unbounded_String (Length : Natural) return Unbounded_String is
      Result : Unbounded_String;
   begin
      if Length > Max_Length then
         raise Ada.Strings.Length_Error;
      end if;
      Result.Length := Length;
      for I in 1 .. Length loop
         Result.Data (I) := ' ';
      end loop;
      return Result;
   end To_Unbounded_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Source : Unbounded_String) return String is
   begin
      return Source.Data (1 .. Source.Length);
   end To_String;

   --------------------------
   -- Set_Unbounded_String --
   --------------------------

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String)
   is
      Len : Natural := Source'Length;
   begin
      if Len > Max_Length then
         raise Length_Error;
      end if;
      Target.Length := Len;
      for I in 1 .. Len loop
         Target.Data (I) := Source (Source'First + I - 1);
      end loop;
   end Set_Unbounded_String;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in Out Unbounded_String;
      New_Item : Unbounded_String)
   is
      New_Len : Natural := Source.Length + New_Item.Length;
   begin
      if New_Len > Max_Length then
         raise Length_Error;
      end if;
      for I in 1 .. New_Item.Length loop
         Source.Data (Source.Length + I) := New_Item.Data (I);
      end loop;
      Source.Length := New_Len;
   end Append;

   procedure Append
     (Source   : in Out Unbounded_String;
      New_Item : String)
   is
      New_Len : Natural := Source.Length + New_Item'Length;
   begin
      if New_Len > Max_Length then
         raise Length_Error;
      end if;
      for I in 1 .. New_Item'Length loop
         Source.Data (Source.Length + I) := New_Item (New_Item'First + I - 1);
      end loop;
      Source.Length := New_Len;
   end Append;

   procedure Append
     (Source   : in Out Unbounded_String;
      New_Item : Character)
   is
   begin
      if Source.Length >= Max_Length then
         raise Length_Error;
      end if;
      Source.Length := Source.Length + 1;
      Source.Data (Source.Length) := New_Item;
   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Unbounded_String; Right : String) return Unbounded_String is
      Result : Unbounded_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : String; Right : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String (Left);
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Unbounded_String; Right : Character) return Unbounded_String is
      Result : Unbounded_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&" (Left : Character; Right : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String;
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
     (Source : Unbounded_String;
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
     (Source : in Out Unbounded_String;
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
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String
   is
   begin
      if Low > Source.Length + 1 or High > Source.Length then
         raise Index_Error;
      end if;
      return Source.Data (Low .. High);
   end Slice;

   --------------------
   -- Unbounded_Slice --
   --------------------

   function Unbounded_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return Unbounded_String
   is
   begin
      return To_Unbounded_String (Slice (Source, Low, High));
   end Unbounded_Slice;

   procedure Unbounded_Slice
     (Source : Unbounded_String;
      Target : out Unbounded_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      Target := Unbounded_Slice (Source, Low, High);
   end Unbounded_Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Unbounded_String) return Boolean is
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

   function "=" (Left : Unbounded_String; Right : String) return Boolean is
   begin
      if Left.Length /= Right'Length then
         return False;
      end if;
      for I in 1 .. Left.Length loop
         if Left.Data (I) /= Right (Right'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   function "=" (Left : String; Right : Unbounded_String) return Boolean is
   begin
      return Right = Left;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Unbounded_String) return Boolean is
      Min_Len : Natural := Natural'Min (Left.Length, Right.Length);
   begin
      for I in 1 .. Min_Len loop
         if Left.Data (I) < Right.Data (I) then
            return True;
         elsif Left.Data (I) > Right.Data (I) then
            return False;
         end if;
      end loop;
      return Left.Length < Right.Length;
   end "<";

   function "<" (Left : Unbounded_String; Right : String) return Boolean is
   begin
      return Left < To_Unbounded_String (Right);
   end "<";

   function "<" (Left : String; Right : Unbounded_String) return Boolean is
   begin
      return To_Unbounded_String (Left) < Right;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Unbounded_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left : Unbounded_String; Right : String) return Boolean is
   begin
      return not (To_Unbounded_String (Right) < Left);
   end "<=";

   function "<=" (Left : String; Right : Unbounded_String) return Boolean is
   begin
      return not (Right < To_Unbounded_String (Left));
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Unbounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Unbounded_String; Right : String) return Boolean is
   begin
      return To_Unbounded_String (Right) < Left;
   end ">";

   function ">" (Left : String; Right : Unbounded_String) return Boolean is
   begin
      return Right < To_Unbounded_String (Left);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Unbounded_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : Unbounded_String; Right : String) return Boolean is
   begin
      return not (Left < To_Unbounded_String (Right));
   end ">=";

   function ">=" (Left : String; Right : Unbounded_String) return Boolean is
   begin
      return not (To_Unbounded_String (Left) < Right);
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Unbounded_String;
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
     (Source : Unbounded_String;
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
     (Source : Unbounded_String;
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
     (Source  : Unbounded_String;
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
               I := I + Pat_Len;  -- Non-overlapping
            else
               I := I + 1;
            end if;
         end;
      end loop;

      return Result;
   end Count;

   function Count
     (Source : Unbounded_String;
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
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String) return Unbounded_String
   is
      Result  : Unbounded_String;
      New_Len : Natural;
      Old_Len : Natural;
   begin
      if Low > Source.Length + 1 then
         raise Index_Error;
      end if;

      if High < Low then
         -- Insert mode
         Old_Len := 0;
      else
         Old_Len := High - Low + 1;
      end if;

      New_Len := Source.Length - Old_Len + By'Length;
      if New_Len > Max_Length then
         raise Length_Error;
      end if;

      -- Copy before Low
      for I in 1 .. Low - 1 loop
         Result.Data (I) := Source.Data (I);
      end loop;

      -- Copy replacement
      for I in 1 .. By'Length loop
         Result.Data (Low - 1 + I) := By (By'First + I - 1);
      end loop;

      -- Copy after High
      if High < Source.Length then
         for I in High + 1 .. Source.Length loop
            Result.Data (Low + By'Length + I - High - 1) := Source.Data (I);
         end loop;
      end if;

      Result.Length := New_Len;
      return Result;
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in Out Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String)
   is
   begin
      Source := Replace_Slice (Source, Low, High, By);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String) return Unbounded_String
   is
   begin
      return Replace_Slice (Source, Before, Before - 1, New_Item);
   end Insert;

   procedure Insert
     (Source   : in Out Unbounded_String;
      Before   : Positive;
      New_Item : String)
   is
   begin
      Source := Insert (Source, Before, New_Item);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Unbounded_String;
      Position : Positive;
      New_Item : String) return Unbounded_String
   is
      Result  : Unbounded_String := Source;
      End_Pos : Natural := Position + New_Item'Length - 1;
   begin
      if Position > Source.Length + 1 then
         raise Index_Error;
      end if;

      if End_Pos > Max_Length then
         raise Length_Error;
      end if;

      for I in 1 .. New_Item'Length loop
         Result.Data (Position + I - 1) := New_Item (New_Item'First + I - 1);
      end loop;

      if End_Pos > Result.Length then
         Result.Length := End_Pos;
      end if;

      return Result;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out Unbounded_String;
      Position : Positive;
      New_Item : String)
   is
   begin
      Source := Overwrite (Source, Position, New_Item);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      if From > Source.Length then
         raise Index_Error;
      end if;

      if Through < From then
         return Source;  -- Nothing to delete
      end if;

      -- Copy before From
      for I in 1 .. From - 1 loop
         Result.Data (I) := Source.Data (I);
      end loop;

      -- Copy after Through
      for I in Through + 1 .. Source.Length loop
         Result.Data (From + I - Through - 1) := Source.Data (I);
      end loop;

      Result.Length := Source.Length - (Through - From + 1);
      return Result;
   end Delete;

   procedure Delete
     (Source  : in Out Unbounded_String;
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
     (Source : Unbounded_String;
      Side   : Trim_End) return Unbounded_String
   is
      First : Positive := 1;
      Last  : Natural := Source.Length;
   begin
      if Source.Length = 0 then
         return Null_Unbounded_String;
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
         return Null_Unbounded_String;
      end if;

      return Unbounded_Slice (Source, First, Last);
   end Trim;

   procedure Trim
     (Source : in Out Unbounded_String;
      Side   : Trim_End)
   is
   begin
      Source := Trim (Source, Side);
   end Trim;

   function Trim
     (Source : Unbounded_String;
      Left   : String;
      Right  : String) return Unbounded_String
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
         return Null_Unbounded_String;
      end if;

      while First <= Source.Length and then In_Left (Source.Data (First)) loop
         First := First + 1;
      end loop;

      while Last >= First and then In_Right (Source.Data (Last)) loop
         Last := Last - 1;
      end loop;

      if First > Last then
         return Null_Unbounded_String;
      end if;

      return Unbounded_Slice (Source, First, Last);
   end Trim;

   procedure Trim
     (Source : in Out Unbounded_String;
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
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      if Count > Max_Length then
         raise Length_Error;
      end if;

      if Count <= Source.Length then
         Result.Length := Count;
         for I in 1 .. Count loop
            Result.Data (I) := Source.Data (I);
         end loop;
      else
         Result.Length := Count;
         for I in 1 .. Source.Length loop
            Result.Data (I) := Source.Data (I);
         end loop;
         for I in Source.Length + 1 .. Count loop
            Result.Data (I) := Pad;
         end loop;
      end if;

      return Result;
   end Head;

   procedure Head
     (Source : in Out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   is
   begin
      Source := Head (Source, Count, Pad);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      if Count > Max_Length then
         raise Length_Error;
      end if;

      if Count <= Source.Length then
         Result.Length := Count;
         for I in 1 .. Count loop
            Result.Data (I) := Source.Data (Source.Length - Count + I);
         end loop;
      else
         Result.Length := Count;
         -- Pad at the beginning
         for I in 1 .. Count - Source.Length loop
            Result.Data (I) := Pad;
         end loop;
         -- Copy source at the end
         for I in 1 .. Source.Length loop
            Result.Data (Count - Source.Length + I) := Source.Data (I);
         end loop;
      end if;

      return Result;
   end Tail;

   procedure Tail
     (Source : in Out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   is
   begin
      Source := Tail (Source, Count, Pad);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Character) return Unbounded_String is
      Result : Unbounded_String;
   begin
      if Left > Max_Length then
         raise Length_Error;
      end if;

      Result.Length := Left;
      for I in 1 .. Left loop
         Result.Data (I) := Right;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Natural; Right : String) return Unbounded_String is
      Result  : Unbounded_String;
      New_Len : Natural := Left * Right'Length;
   begin
      if New_Len > Max_Length then
         raise Length_Error;
      end if;

      Result.Length := New_Len;
      for I in 1 .. Left loop
         for J in 1 .. Right'Length loop
            Result.Data ((I - 1) * Right'Length + J) := Right (Right'First + J - 1);
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Natural; Right : Unbounded_String) return Unbounded_String is
   begin
      return Left * To_String (Right);
   end "*";

end Ada.Strings.Unbounded;
