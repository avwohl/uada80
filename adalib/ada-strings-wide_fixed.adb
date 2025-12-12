-- Ada.Strings.Wide_Fixed body for Z80
-- Fixed-length wide string handling implementation

package body Ada.Strings.Wide_Fixed is

   Wide_Space : constant Wide_Character := Wide_Character'Val (32);

   ----------
   -- Move --
   ----------

   procedure Move
     (Source  : Wide_String;
      Target  : out Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Space)
   is
      Src_Len : constant Natural := Source'Length;
      Tgt_Len : constant Natural := Target'Length;
   begin
      if Src_Len = Tgt_Len then
         Target := Source;
      elsif Src_Len < Tgt_Len then
         -- Source fits, need to justify and pad
         case Justify is
            when Left =>
               Target (Target'First .. Target'First + Src_Len - 1) := Source;
               Target (Target'First + Src_Len .. Target'Last) := (others => Pad);
            when Right =>
               Target (Target'First .. Target'Last - Src_Len) := (others => Pad);
               Target (Target'Last - Src_Len + 1 .. Target'Last) := Source;
            when Center =>
               declare
                  Left_Pad : constant Natural := (Tgt_Len - Src_Len) / 2;
               begin
                  Target (Target'First .. Target'First + Left_Pad - 1) := (others => Pad);
                  Target (Target'First + Left_Pad .. Target'First + Left_Pad + Src_Len - 1) := Source;
                  Target (Target'First + Left_Pad + Src_Len .. Target'Last) := (others => Pad);
               end;
         end case;
      else
         -- Source too long, need to truncate
         case Drop is
            when Left =>
               Target := Source (Source'Last - Tgt_Len + 1 .. Source'Last);
            when Right =>
               Target := Source (Source'First .. Source'First + Tgt_Len - 1);
            when Error =>
               raise Length_Error;
         end case;
      end if;
   end Move;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward) return Natural
   is
   begin
      if Pattern'Length = 0 or Source'Length < Pattern'Length then
         return 0;
      end if;

      if Going = Forward then
         for I in Source'First .. Source'Last - Pattern'Length + 1 loop
            if Source (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'First .. Source'Last - Pattern'Length + 1 loop
            if Source (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index;

   function Index
     (Source : Wide_String;
      Set    : Wide_String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
      function In_Set (Ch : Wide_Character) return Boolean is
      begin
         for I in Set'Range loop
            if Set (I) = Ch then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;

      In_S : Boolean;
   begin
      if Going = Forward then
         for I in Source'Range loop
            In_S := In_Set (Source (I));
            if (Test = Inside and In_S) or (Test = Outside and not In_S) then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'Range loop
            In_S := In_Set (Source (I));
            if (Test = Inside and In_S) or (Test = Outside and not In_S) then
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
     (Source : Wide_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for I in Source'Range loop
            if Source (I) /= Wide_Space then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'Range loop
            if Source (I) /= Wide_Space then
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
     (Source  : Wide_String;
      Pattern : Wide_String) return Natural
   is
      Result : Natural := 0;
      Pos    : Natural;
   begin
      if Pattern'Length = 0 or Source'Length < Pattern'Length then
         return 0;
      end if;

      Pos := Source'First;
      while Pos <= Source'Last - Pattern'Length + 1 loop
         if Source (Pos .. Pos + Pattern'Length - 1) = Pattern then
            Result := Result + 1;
            Pos := Pos + Pattern'Length;
         else
            Pos := Pos + 1;
         end if;
      end loop;

      return Result;
   end Count;

   function Count
     (Source : Wide_String;
      Set    : Wide_String) return Natural
   is
      Result : Natural := 0;

      function In_Set (Ch : Wide_Character) return Boolean is
      begin
         for I in Set'Range loop
            if Set (I) = Ch then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;
   begin
      for I in Source'Range loop
         if In_Set (Source (I)) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String) return Wide_String
   is
      Prefix_Len : constant Natural := Low - Source'First;
      Suffix_Len : constant Natural := Source'Last - High;
   begin
      return Source (Source'First .. Low - 1) & By & Source (High + 1 .. Source'Last);
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in Out Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error;
      Justify : Alignment := Left;
      Pad    : Wide_Character := Wide_Space)
   is
      Result : constant Wide_String := Replace_Slice (Source, Low, High, By);
   begin
      Move (Result, Source, Drop, Justify, Pad);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Wide_String;
      Before   : Positive;
      New_Item : Wide_String) return Wide_String
   is
   begin
      return Source (Source'First .. Before - 1) & New_Item & Source (Before .. Source'Last);
   end Insert;

   procedure Insert
     (Source   : in Out Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error)
   is
      Result : constant Wide_String := Insert (Source, Before, New_Item);
   begin
      Move (Result, Source, Drop);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Wide_String;
      Position : Positive;
      New_Item : Wide_String) return Wide_String
   is
      End_Pos : constant Natural := Position + New_Item'Length - 1;
   begin
      if End_Pos > Source'Last then
         return Source (Source'First .. Position - 1) & New_Item;
      else
         return Source (Source'First .. Position - 1) &
                New_Item &
                Source (End_Pos + 1 .. Source'Last);
      end if;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Right)
   is
      Result : constant Wide_String := Overwrite (Source, Position, New_Item);
   begin
      Move (Result, Source, Drop);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Wide_String;
      From    : Positive;
      Through : Natural) return Wide_String
   is
   begin
      if From > Through then
         return Source;
      else
         return Source (Source'First .. From - 1) & Source (Through + 1 .. Source'Last);
      end if;
   end Delete;

   procedure Delete
     (Source  : in Out Wide_String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Space)
   is
      Result : constant Wide_String := Delete (Source, From, Through);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Wide_String;
      Side   : Trim_End) return Wide_String
   is
      First : Natural := Source'First;
      Last  : Natural := Source'Last;
   begin
      if Side = Left or Side = Both then
         while First <= Last and then Source (First) = Wide_Space loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while Last >= First and then Source (Last) = Wide_Space loop
            Last := Last - 1;
         end loop;
      end if;

      if First > Last then
         return "";
      else
         return Source (First .. Last);
      end if;
   end Trim;

   procedure Trim
     (Source  : in Out Wide_String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Space)
   is
      Result : constant Wide_String := Trim (Source, Side);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Trim;

   function Trim
     (Source : Wide_String;
      Left   : Wide_String;
      Right  : Wide_String) return Wide_String
   is
      function In_Set (Ch : Wide_Character; Set : Wide_String) return Boolean is
      begin
         for I in Set'Range loop
            if Set (I) = Ch then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;

      First : Natural := Source'First;
      Last  : Natural := Source'Last;
   begin
      while First <= Last and then In_Set (Source (First), Left) loop
         First := First + 1;
      end loop;

      while Last >= First and then In_Set (Source (Last), Right) loop
         Last := Last - 1;
      end loop;

      if First > Last then
         return "";
      else
         return Source (First .. Last);
      end if;
   end Trim;

   procedure Trim
     (Source  : in Out Wide_String;
      Left    : Wide_String;
      Right   : Wide_String;
      Justify : Alignment := Ada.Strings.Left;
      Pad     : Wide_Character := Wide_Space)
   is
      Result : constant Wide_String := Trim (Source, Left, Right);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Wide_String
   is
   begin
      if Count <= Source'Length then
         return Source (Source'First .. Source'First + Count - 1);
      else
         declare
            Result : Wide_String (1 .. Count);
         begin
            Result (1 .. Source'Length) := Source;
            Result (Source'Length + 1 .. Count) := (others => Pad);
            return Result;
         end;
      end if;
   end Head;

   procedure Head
     (Source  : in Out Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Space)
   is
      Result : constant Wide_String := Head (Source, Count, Pad);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Wide_String
   is
   begin
      if Count <= Source'Length then
         return Source (Source'Last - Count + 1 .. Source'Last);
      else
         declare
            Result : Wide_String (1 .. Count);
         begin
            Result (1 .. Count - Source'Length) := (others => Pad);
            Result (Count - Source'Length + 1 .. Count) := Source;
            return Result;
         end;
      end if;
   end Tail;

   procedure Tail
     (Source  : in Out Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Space)
   is
      Result : constant Wide_String := Tail (Source, Count, Pad);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Wide_Character) return Wide_String is
      Result : Wide_String (1 .. Left);
   begin
      for I in Result'Range loop
         Result (I) := Right;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Natural; Right : Wide_String) return Wide_String is
   begin
      if Left = 0 or Right'Length = 0 then
         return "";
      end if;

      declare
         Result : Wide_String (1 .. Left * Right'Length);
         Pos    : Positive := 1;
      begin
         for I in 1 .. Left loop
            Result (Pos .. Pos + Right'Length - 1) := Right;
            Pos := Pos + Right'Length;
         end loop;
         return Result;
      end;
   end "*";

end Ada.Strings.Wide_Fixed;
