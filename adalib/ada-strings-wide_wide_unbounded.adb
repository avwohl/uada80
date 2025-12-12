-- Ada.Strings.Wide_Wide_Unbounded body for Z80
-- Unbounded Wide_Wide_String handling implementation

with Ada.Unchecked_Deallocation;

package body Ada.Strings.Wide_Wide_Unbounded is

   Wide_Wide_Space : constant Wide_Wide_Character := Wide_Wide_Character'Val (32);

   procedure Free_String is new Ada.Unchecked_Deallocation
     (Wide_Wide_String, Wide_Wide_String_Access);

   ----------
   -- Free --
   ----------

   procedure Free (X : in Out Wide_Wide_String_Access) is
   begin
      Free_String (X);
   end Free;

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_Wide_Wide_String) return Natural is
   begin
      return Source.Length;
   end Length;

   ------------------------------------
   -- To_Unbounded_Wide_Wide_String --
   ------------------------------------

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
   begin
      Result.Length := Natural'Min (Source'Length, Max_Length);
      for I in 1 .. Result.Length loop
         Result.Data (I) := Source (Source'First + I - 1);
      end loop;
      return Result;
   end To_Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Length : Natural) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
   begin
      Result.Length := Natural'Min (Length, Max_Length);
      for I in 1 .. Result.Length loop
         Result.Data (I) := Wide_Wide_Space;
      end loop;
      return Result;
   end To_Unbounded_Wide_Wide_String;

   --------------------------
   -- To_Wide_Wide_String --
   --------------------------

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Source.Length);
   begin
      for I in 1 .. Source.Length loop
         Result (I) := Source.Data (I);
      end loop;
      return Result;
   end To_Wide_Wide_String;

   ------------------------------------
   -- Set_Unbounded_Wide_Wide_String --
   ------------------------------------

   procedure Set_Unbounded_Wide_Wide_String
     (Target : out Unbounded_Wide_Wide_String;
      Source : Wide_Wide_String)
   is
   begin
      Target := To_Unbounded_Wide_Wide_String (Source);
   end Set_Unbounded_Wide_Wide_String;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in Out Unbounded_Wide_Wide_String;
      New_Item : Unbounded_Wide_Wide_String)
   is
   begin
      Append (Source, To_Wide_Wide_String (New_Item));
   end Append;

   procedure Append
     (Source   : in Out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_String)
   is
      New_Len : constant Natural := Natural'Min (Source.Length + New_Item'Length, Max_Length);
   begin
      for I in Source.Length + 1 .. New_Len loop
         Source.Data (I) := New_Item (New_Item'First + I - Source.Length - 1);
      end loop;
      Source.Length := New_Len;
   end Append;

   procedure Append
     (Source   : in Out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_Character)
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

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return To_Unbounded_Wide_Wide_String (Left) & Right;
   end "&";

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String := Left;
   begin
      Append (Result, Right);
      return Result;
   end "&";

   function "&"
     (Left  : Wide_Wide_Character;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return To_Unbounded_Wide_Wide_String ((1 => Left)) & Right;
   end "&";

   -------------
   -- Element --
   -------------

   function Element
     (Source : Unbounded_Wide_Wide_String;
      Index  : Positive) return Wide_Wide_Character
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
     (Source : in Out Unbounded_Wide_Wide_String;
      Index  : Positive;
      By     : Wide_Wide_Character)
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
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_Wide_String
   is
   begin
      if Low > Source.Length + 1 or High > Source.Length then
         raise Index_Error;
      end if;
      if High < Low then
         return "";
      end if;
      declare
         Result : Wide_Wide_String (1 .. High - Low + 1);
      begin
         for I in Low .. High loop
            Result (I - Low + 1) := Source.Data (I);
         end loop;
         return Result;
      end;
   end Slice;

   --------------------
   -- Unbounded_Slice --
   --------------------

   function Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_Wide_String
   is
   begin
      return To_Unbounded_Wide_Wide_String (Slice (Source, Low, High));
   end Unbounded_Slice;

   procedure Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Target : out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      Target := Unbounded_Slice (Source, Low, High);
   end Unbounded_Slice;

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return To_Wide_Wide_String (Left) = To_Wide_Wide_String (Right);
   end "=";

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return To_Wide_Wide_String (Left) = Right;
   end "=";

   function "="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left = To_Wide_Wide_String (Right);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return To_Wide_Wide_String (Left) < To_Wide_Wide_String (Right);
   end "<";

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return To_Wide_Wide_String (Left) < Right;
   end "<";

   function "<"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left < To_Wide_Wide_String (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return not (Right < Left);
   end "<=";

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return not (Right < To_Wide_Wide_String (Left));
   end "<=";

   function "<="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return not (To_Wide_Wide_String (Right) < Left);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Right < Left;
   end ">";

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return Right < To_Wide_Wide_String (Left);
   end ">";

   function ">"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return To_Wide_Wide_String (Right) < Left;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return not (Left < Right);
   end ">=";

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return not (To_Wide_Wide_String (Left) < Right);
   end ">=";

   function ">="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return not (Left < To_Wide_Wide_String (Right));
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural
   is
      S : constant Wide_Wide_String := To_Wide_Wide_String (Source);
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      case Going is
         when Forward =>
            for I in S'First .. S'Last - Pattern'Length + 1 loop
               declare
                  Match : Boolean := True;
               begin
                  for J in 0 .. Pattern'Length - 1 loop
                     if Wide_Wide_Maps.Value (Mapping, S (I + J)) /=
                        Pattern (Pattern'First + J)
                     then
                        Match := False;
                        exit;
                     end if;
                  end loop;
                  if Match then
                     return I;
                  end if;
               end;
            end loop;
         when Backward =>
            for I in reverse S'First .. S'Last - Pattern'Length + 1 loop
               declare
                  Match : Boolean := True;
               begin
                  for J in 0 .. Pattern'Length - 1 loop
                     if Wide_Wide_Maps.Value (Mapping, S (I + J)) /=
                        Pattern (Pattern'First + J)
                     then
                        Match := False;
                        exit;
                     end if;
                  end loop;
                  if Match then
                     return I;
                  end if;
               end;
            end loop;
      end case;

      return 0;
   end Index;

   function Index
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
      S : constant Wide_Wide_String := To_Wide_Wide_String (Source);
   begin
      case Going is
         when Forward =>
            for I in S'Range loop
               if (Test = Inside) = Wide_Wide_Maps.Is_In (S (I), Set) then
                  return I;
               end if;
            end loop;
         when Backward =>
            for I in reverse S'Range loop
               if (Test = Inside) = Wide_Wide_Maps.Is_In (S (I), Set) then
                  return I;
               end if;
            end loop;
      end case;
      return 0;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : Unbounded_Wide_Wide_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      case Going is
         when Forward =>
            for I in 1 .. Source.Length loop
               if Source.Data (I) /= Wide_Wide_Space then
                  return I;
               end if;
            end loop;
         when Backward =>
            for I in reverse 1 .. Source.Length loop
               if Source.Data (I) /= Wide_Wide_Space then
                  return I;
               end if;
            end loop;
      end case;
      return 0;
   end Index_Non_Blank;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural
   is
      S      : constant Wide_Wide_String := To_Wide_Wide_String (Source);
      Result : Natural := 0;
      I      : Natural := S'First;
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      while I <= S'Last - Pattern'Length + 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. Pattern'Length - 1 loop
               if Wide_Wide_Maps.Value (Mapping, S (I + J)) /=
                  Pattern (Pattern'First + J)
               then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               Result := Result + 1;
               I := I + Pattern'Length;
            else
               I := I + 1;
            end if;
         end;
      end loop;

      return Result;
   end Count;

   function Count
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural
   is
      Result : Natural := 0;
   begin
      for I in 1 .. Source.Length loop
         if Wide_Wide_Maps.Is_In (Source.Data (I), Set) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
     return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String := Source;
   begin
      for I in 1 .. Result.Length loop
         Result.Data (I) := Wide_Wide_Maps.Value (Mapping, Result.Data (I));
      end loop;
      return Result;
   end Translate;

   procedure Translate
     (Source  : in Out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
   is
   begin
      for I in 1 .. Source.Length loop
         Source.Data (I) := Wide_Wide_Maps.Value (Mapping, Source.Data (I));
      end loop;
   end Translate;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      S : constant Wide_Wide_String := To_Wide_Wide_String (Source);
   begin
      if Low > S'Length + 1 then
         raise Index_Error;
      end if;

      if High < Low then
         return To_Unbounded_Wide_Wide_String (S (S'First .. Low - 1) & By & S (Low .. S'Last));
      else
         return To_Unbounded_Wide_Wide_String (S (S'First .. Low - 1) & By & S (High + 1 .. S'Last));
      end if;
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in Out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String)
   is
   begin
      Source := Replace_Slice (Source, Low, High, By);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return Replace_Slice (Source, Before, Before - 1, New_Item);
   end Insert;

   procedure Insert
     (Source   : in Out Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String)
   is
   begin
      Source := Insert (Source, Before, New_Item);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      S : constant Wide_Wide_String := To_Wide_Wide_String (Source);
   begin
      if Position > S'Length + 1 then
         raise Index_Error;
      end if;

      declare
         End_Pos : constant Natural := Position + New_Item'Length - 1;
      begin
         if End_Pos <= S'Length then
            declare
               Result : Wide_Wide_String := S;
            begin
               Result (Position .. End_Pos) := New_Item;
               return To_Unbounded_Wide_Wide_String (Result);
            end;
         else
            return To_Unbounded_Wide_Wide_String (S (S'First .. Position - 1) & New_Item);
         end if;
      end;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String)
   is
   begin
      Source := Overwrite (Source, Position, New_Item);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Unbounded_Wide_Wide_String
   is
      S : constant Wide_Wide_String := To_Wide_Wide_String (Source);
   begin
      if From > Through then
         return Source;
      end if;
      if From < S'First or Through > S'Last then
         raise Index_Error;
      end if;
      return To_Unbounded_Wide_Wide_String (S (S'First .. From - 1) & S (Through + 1 .. S'Last));
   end Delete;

   procedure Delete
     (Source  : in Out Unbounded_Wide_Wide_String;
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
     (Source : Unbounded_Wide_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_Wide_String
   is
      First : Positive := 1;
      Last  : Natural := Source.Length;
   begin
      if Side = Left or Side = Both then
         while First <= Last and then Source.Data (First) = Wide_Wide_Space loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while Last >= First and then Source.Data (Last) = Wide_Wide_Space loop
            Last := Last - 1;
         end loop;
      end if;

      return Unbounded_Slice (Source, First, Last);
   end Trim;

   procedure Trim
     (Source : in Out Unbounded_Wide_Wide_String;
      Side   : Trim_End)
   is
   begin
      Source := Trim (Source, Side);
   end Trim;

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
     return Unbounded_Wide_Wide_String
   is
      First : Positive := 1;
      Last  : Natural := Source.Length;
   begin
      while First <= Last and then Wide_Wide_Maps.Is_In (Source.Data (First), Left) loop
         First := First + 1;
      end loop;

      while Last >= First and then Wide_Wide_Maps.Is_In (Source.Data (Last), Right) loop
         Last := Last - 1;
      end loop;

      return Unbounded_Slice (Source, First, Last);
   end Trim;

   procedure Trim
     (Source : in Out Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
   is
   begin
      Source := Trim (Source, Left, Right);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32))
     return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
      Len    : constant Natural := Natural'Min (Count, Max_Length);
   begin
      Result.Length := Len;
      for I in 1 .. Len loop
         if I <= Source.Length then
            Result.Data (I) := Source.Data (I);
         else
            Result.Data (I) := Pad;
         end if;
      end loop;
      return Result;
   end Head;

   procedure Head
     (Source : in Out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32))
   is
   begin
      Source := Head (Source, Count, Pad);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32))
     return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
      Len    : constant Natural := Natural'Min (Count, Max_Length);
   begin
      Result.Length := Len;
      if Count <= Source.Length then
         for I in 1 .. Len loop
            Result.Data (I) := Source.Data (Source.Length - Count + I);
         end loop;
      else
         declare
            Pad_Count : constant Natural := Count - Source.Length;
         begin
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
     (Source : in Out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32))
   is
   begin
      Source := Tail (Source, Count, Pad);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
   begin
      Result.Length := Natural'Min (Left, Max_Length);
      for I in 1 .. Result.Length loop
         Result.Data (I) := Right;
      end loop;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
      Total  : constant Natural := Left * Right'Length;
      Len    : constant Natural := Natural'Min (Total, Max_Length);
      Pos    : Natural := 1;
   begin
      Result.Length := Len;
      for I in 1 .. Left loop
         exit when Pos > Len;
         for J in Right'Range loop
            exit when Pos > Len;
            Result.Data (Pos) := Right (J);
            Pos := Pos + 1;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return Left * To_Wide_Wide_String (Right);
   end "*";

end Ada.Strings.Wide_Wide_Unbounded;
