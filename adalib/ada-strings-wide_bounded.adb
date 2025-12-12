-- Ada.Strings.Wide_Bounded body for Z80
-- Bounded-length wide string handling implementation

package body Ada.Strings.Wide_Bounded is

   Wide_Space : constant Wide_Character := Wide_Character'Val (32);

   ------------
   -- Length --
   ------------

   function Length (Source : Bounded_Wide_String) return Length_Range is
   begin
      return Source.Length;
   end Length;

   ---------------------------
   -- To_Bounded_Wide_String --
   ---------------------------

   function To_Bounded_Wide_String
     (Source : Wide_String;
      Drop   : Truncation := Error) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String;
   begin
      Set_Bounded_Wide_String (Result, Source, Drop);
      return Result;
   end To_Bounded_Wide_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Source : Bounded_Wide_String) return Wide_String is
   begin
      return Source.Data (1 .. Source.Length);
   end To_Wide_String;

   ----------------------------
   -- Set_Bounded_Wide_String --
   ----------------------------

   procedure Set_Bounded_Wide_String
     (Target : out Bounded_Wide_String;
      Source : Wide_String;
      Drop   : Truncation := Error)
   is
   begin
      if Source'Length <= Max_Length then
         Target.Data (1 .. Source'Length) := Source;
         Target.Length := Source'Length;
      else
         case Drop is
            when Left =>
               Target.Data := Source (Source'Last - Max_Length + 1 .. Source'Last);
               Target.Length := Max_Length;
            when Right =>
               Target.Data := Source (Source'First .. Source'First + Max_Length - 1);
               Target.Length := Max_Length;
            when Error =>
               raise Length_Error;
         end case;
      end if;
   end Set_Bounded_Wide_String;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in Out Bounded_Wide_String;
      New_Item : Bounded_Wide_String;
      Drop     : Truncation := Error)
   is
   begin
      Append (Source, To_Wide_String (New_Item), Drop);
   end Append;

   procedure Append
     (Source   : in Out Bounded_Wide_String;
      New_Item : Wide_String;
      Drop     : Truncation := Error)
   is
      New_Length : constant Natural := Source.Length + New_Item'Length;
   begin
      if New_Length <= Max_Length then
         Source.Data (Source.Length + 1 .. New_Length) := New_Item;
         Source.Length := New_Length;
      else
         case Drop is
            when Left =>
               declare
                  Combined : Wide_String (1 .. New_Length);
               begin
                  Combined (1 .. Source.Length) := Source.Data (1 .. Source.Length);
                  Combined (Source.Length + 1 .. New_Length) := New_Item;
                  Source.Data := Combined (New_Length - Max_Length + 1 .. New_Length);
                  Source.Length := Max_Length;
               end;
            when Right =>
               declare
                  Space : constant Natural := Max_Length - Source.Length;
               begin
                  if Space > 0 then
                     Source.Data (Source.Length + 1 .. Max_Length) :=
                       New_Item (New_Item'First .. New_Item'First + Space - 1);
                  end if;
                  Source.Length := Max_Length;
               end;
            when Error =>
               raise Length_Error;
         end case;
      end if;
   end Append;

   procedure Append
     (Source   : in Out Bounded_Wide_String;
      New_Item : Wide_Character;
      Drop     : Truncation := Error)
   is
   begin
      Append (Source, (1 => New_Item), Drop);
   end Append;

   ---------
   -- "&" --
   ---------

   function "&"
     (Left, Right : Bounded_Wide_String) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String := Left;
   begin
      Append (Result, Right, Error);
      return Result;
   end "&";

   function "&"
     (Left : Bounded_Wide_String; Right : Wide_String) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String := Left;
   begin
      Append (Result, Right, Error);
      return Result;
   end "&";

   function "&"
     (Left : Wide_String; Right : Bounded_Wide_String) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String := To_Bounded_Wide_String (Left);
   begin
      Append (Result, Right, Error);
      return Result;
   end "&";

   function "&"
     (Left : Bounded_Wide_String; Right : Wide_Character) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String := Left;
   begin
      Append (Result, Right, Error);
      return Result;
   end "&";

   function "&"
     (Left : Wide_Character; Right : Bounded_Wide_String) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String := To_Bounded_Wide_String ((1 => Left));
   begin
      Append (Result, Right, Error);
      return Result;
   end "&";

   -------------
   -- Element --
   -------------

   function Element
     (Source : Bounded_Wide_String;
      Index  : Positive) return Wide_Character
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
     (Source : in Out Bounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character)
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
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_String
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
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Bounded_Wide_String
   is
   begin
      return To_Bounded_Wide_String (Slice (Source, Low, High));
   end Bounded_Slice;

   procedure Bounded_Slice
     (Source : Bounded_Wide_String;
      Target : out Bounded_Wide_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      Target := Bounded_Slice (Source, Low, High);
   end Bounded_Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Bounded_Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) = To_Wide_String (Right);
   end "=";

   function "=" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) = Right;
   end "=";

   function "=" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean is
   begin
      return Left = To_Wide_String (Right);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Bounded_Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) < To_Wide_String (Right);
   end "<";

   function "<" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) < Right;
   end "<";

   function "<" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean is
   begin
      return Left < To_Wide_String (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Bounded_Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) <= To_Wide_String (Right);
   end "<=";

   function "<=" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) <= Right;
   end "<=";

   function "<=" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean is
   begin
      return Left <= To_Wide_String (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Bounded_Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) > To_Wide_String (Right);
   end ">";

   function ">" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) > Right;
   end ">";

   function ">" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean is
   begin
      return Left > To_Wide_String (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Bounded_Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) >= To_Wide_String (Right);
   end ">=";

   function ">=" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean is
   begin
      return To_Wide_String (Left) >= Right;
   end ">=";

   function ">=" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean is
   begin
      return Left >= To_Wide_String (Right);
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Bounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward) return Natural
   is
      Src : constant Wide_String := To_Wide_String (Source);
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      if Pattern'Length > Src'Length then
         return 0;
      end if;

      if Going = Forward then
         for I in Src'First .. Src'Last - Pattern'Length + 1 loop
            if Src (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      else
         for I in reverse Src'First .. Src'Last - Pattern'Length + 1 loop
            if Src (I .. I + Pattern'Length - 1) = Pattern then
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
     (Source : Bounded_Wide_String;
      Going  : Direction := Forward) return Natural
   is
      Src : constant Wide_String := To_Wide_String (Source);
   begin
      if Going = Forward then
         for I in Src'Range loop
            if Src (I) /= Wide_Space then
               return I;
            end if;
         end loop;
      else
         for I in reverse Src'Range loop
            if Src (I) /= Wide_Space then
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
     (Source  : Bounded_Wide_String;
      Pattern : Wide_String) return Natural
   is
      Src   : constant Wide_String := To_Wide_String (Source);
      N     : Natural := 0;
      Index : Natural := Src'First;
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      while Index <= Src'Last - Pattern'Length + 1 loop
         if Src (Index .. Index + Pattern'Length - 1) = Pattern then
            N := N + 1;
            Index := Index + Pattern'Length;
         else
            Index := Index + 1;
         end if;
      end loop;
      return N;
   end Count;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error) return Bounded_Wide_String
   is
      Src : constant Wide_String := To_Wide_String (Source);
   begin
      if Low > Source.Length + 1 then
         raise Index_Error;
      end if;

      if High < Low then
         return To_Bounded_Wide_String (Src (1 .. Low - 1) & By & Src (Low .. Source.Length), Drop);
      else
         return To_Bounded_Wide_String (Src (1 .. Low - 1) & By & Src (High + 1 .. Source.Length), Drop);
      end if;
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in Out Bounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error)
   is
   begin
      Source := Replace_Slice (Source, Low, High, By, Drop);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Bounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error) return Bounded_Wide_String
   is
   begin
      return Replace_Slice (Source, Before, Before - 1, New_Item, Drop);
   end Insert;

   procedure Insert
     (Source   : in Out Bounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error)
   is
   begin
      Source := Insert (Source, Before, New_Item, Drop);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Bounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error) return Bounded_Wide_String
   is
      End_Pos : constant Natural := Position + New_Item'Length - 1;
   begin
      if Position > Source.Length + 1 then
         raise Index_Error;
      end if;

      if End_Pos <= Source.Length then
         return Replace_Slice (Source, Position, End_Pos, New_Item, Drop);
      else
         return Replace_Slice (Source, Position, Source.Length, New_Item, Drop);
      end if;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out Bounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error)
   is
   begin
      Source := Overwrite (Source, Position, New_Item, Drop);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Bounded_Wide_String;
      From    : Positive;
      Through : Natural) return Bounded_Wide_String
   is
      Src : constant Wide_String := To_Wide_String (Source);
   begin
      if From > Source.Length + 1 then
         raise Index_Error;
      end if;

      if Through < From then
         return Source;
      end if;

      return To_Bounded_Wide_String (Src (1 .. From - 1) & Src (Through + 1 .. Source.Length));
   end Delete;

   procedure Delete
     (Source  : in Out Bounded_Wide_String;
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
     (Source : Bounded_Wide_String;
      Side   : Trim_End) return Bounded_Wide_String
   is
      Src   : constant Wide_String := To_Wide_String (Source);
      First : Positive := 1;
      Last  : Natural := Source.Length;
   begin
      if Source.Length = 0 then
         return Source;
      end if;

      if Side = Left or Side = Both then
         while First <= Last and then Src (First) = Wide_Space loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while Last >= First and then Src (Last) = Wide_Space loop
            Last := Last - 1;
         end loop;
      end if;

      if First > Last then
         return Null_Bounded_Wide_String;
      end if;

      return To_Bounded_Wide_String (Src (First .. Last));
   end Trim;

   procedure Trim
     (Source : in Out Bounded_Wide_String;
      Side   : Trim_End)
   is
   begin
      Source := Trim (Source, Side);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String;
   begin
      if Count <= Source.Length then
         Result.Data (1 .. Count) := Source.Data (1 .. Count);
         Result.Length := Count;
      elsif Count <= Max_Length then
         Result.Data (1 .. Source.Length) := Source.Data (1 .. Source.Length);
         Result.Data (Source.Length + 1 .. Count) := (others => Pad);
         Result.Length := Count;
      else
         case Drop is
            when Left =>
               Result.Data := (others => Pad);
               declare
                  Start : constant Positive := Count - Max_Length + 1;
               begin
                  if Start <= Source.Length then
                     Result.Data (1 .. Source.Length - Start + 1) :=
                       Source.Data (Start .. Source.Length);
                  end if;
               end;
               Result.Length := Max_Length;
            when Right =>
               Result.Data (1 .. Source.Length) := Source.Data (1 .. Source.Length);
               Result.Data (Source.Length + 1 .. Max_Length) := (others => Pad);
               Result.Length := Max_Length;
            when Error =>
               raise Length_Error;
         end case;
      end if;
      return Result;
   end Head;

   procedure Head
     (Source : in Out Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error)
   is
   begin
      Source := Head (Source, Count, Pad, Drop);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String;
   begin
      if Count <= Source.Length then
         Result.Data (1 .. Count) :=
           Source.Data (Source.Length - Count + 1 .. Source.Length);
         Result.Length := Count;
      elsif Count <= Max_Length then
         Result.Data (1 .. Count - Source.Length) := (others => Pad);
         Result.Data (Count - Source.Length + 1 .. Count) :=
           Source.Data (1 .. Source.Length);
         Result.Length := Count;
      else
         case Drop is
            when Left =>
               Result.Data (1 .. Max_Length - Source.Length) := (others => Pad);
               Result.Data (Max_Length - Source.Length + 1 .. Max_Length) :=
                 Source.Data (1 .. Source.Length);
               Result.Length := Max_Length;
            when Right =>
               Result.Data := (others => Pad);
               Result.Length := Max_Length;
            when Error =>
               raise Length_Error;
         end case;
      end if;
      return Result;
   end Tail;

   procedure Tail
     (Source : in Out Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
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
      Right : Wide_Character) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String;
   begin
      if Left > Max_Length then
         raise Length_Error;
      end if;
      Result.Data (1 .. Left) := (others => Right);
      Result.Length := Left;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Wide_String) return Bounded_Wide_String
   is
      Result : Bounded_Wide_String;
      Len    : constant Natural := Left * Right'Length;
      Pos    : Positive := 1;
   begin
      if Len > Max_Length then
         raise Length_Error;
      end if;
      for I in 1 .. Left loop
         Result.Data (Pos .. Pos + Right'Length - 1) := Right;
         Pos := Pos + Right'Length;
      end loop;
      Result.Length := Len;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Bounded_Wide_String) return Bounded_Wide_String
   is
   begin
      return Left * To_Wide_String (Right);
   end "*";

end Ada.Strings.Wide_Bounded;
