-- Ada.Strings.Wide_Wide_Fixed body for Z80
-- Wide_Wide_String fixed-length string handling implementation

package body Ada.Strings.Wide_Wide_Fixed is

   ----------
   -- Move --
   ----------

   procedure Move
     (Source  : Wide_Wide_String;
      Target  : out Wide_Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Src_Len : constant Natural := Source'Length;
      Tgt_Len : constant Natural := Target'Length;
   begin
      if Src_Len = Tgt_Len then
         Target := Source;
      elsif Src_Len < Tgt_Len then
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural
   is
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      if Pattern'Length > Source'Length then
         return 0;
      end if;

      case Going is
         when Forward =>
            for I in Source'First .. Source'Last - Pattern'Length + 1 loop
               declare
                  Match : Boolean := True;
               begin
                  for J in 0 .. Pattern'Length - 1 loop
                     if Wide_Wide_Maps.Value (Mapping, Source (I + J)) /=
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
            for I in reverse Source'First .. Source'Last - Pattern'Length + 1 loop
               declare
                  Match : Boolean := True;
               begin
                  for J in 0 .. Pattern'Length - 1 loop
                     if Wide_Wide_Maps.Value (Mapping, Source (I + J)) /=
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      if Pattern'Length > Source'Length then
         return 0;
      end if;

      case Going is
         when Forward =>
            for I in Source'First .. Source'Last - Pattern'Length + 1 loop
               declare
                  Match : Boolean := True;
               begin
                  for J in 0 .. Pattern'Length - 1 loop
                     if Mapping (Source (I + J)) /= Pattern (Pattern'First + J) then
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
            for I in reverse Source'First .. Source'Last - Pattern'Length + 1 loop
               declare
                  Match : Boolean := True;
               begin
                  for J in 0 .. Pattern'Length - 1 loop
                     if Mapping (Source (I + J)) /= Pattern (Pattern'First + J) then
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
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      case Going is
         when Forward =>
            for I in Source'Range loop
               if (Test = Inside) = Wide_Wide_Maps.Is_In (Source (I), Set) then
                  return I;
               end if;
            end loop;
         when Backward =>
            for I in reverse Source'Range loop
               if (Test = Inside) = Wide_Wide_Maps.Is_In (Source (I), Set) then
                  return I;
               end if;
            end loop;
      end case;
      return 0;
   end Index;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural
   is
   begin
      case Going is
         when Forward =>
            return Index (Source (From .. Source'Last), Pattern, Forward, Mapping);
         when Backward =>
            return Index (Source (Source'First .. From), Pattern, Backward, Mapping);
      end case;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : Wide_Wide_String;
      Going  : Direction := Forward) return Natural
   is
   begin
      case Going is
         when Forward =>
            for I in Source'Range loop
               if Source (I) /= Wide_Wide_Space then
                  return I;
               end if;
            end loop;
         when Backward =>
            for I in reverse Source'Range loop
               if Source (I) /= Wide_Wide_Space then
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural
   is
      Result : Natural := 0;
      I      : Natural := Source'First;
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      while I <= Source'Last - Pattern'Length + 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. Pattern'Length - 1 loop
               if Wide_Wide_Maps.Value (Mapping, Source (I + J)) /=
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
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function) return Natural
   is
      Result : Natural := 0;
      I      : Natural := Source'First;
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      while I <= Source'Last - Pattern'Length + 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. Pattern'Length - 1 loop
               if Mapping (Source (I + J)) /= Pattern (Pattern'First + J) then
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
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural
   is
      Result : Natural := 0;
   begin
      for I in Source'Range loop
         if Wide_Wide_Maps.Is_In (Source (I), Set) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Wide_Wide_String
   is
   begin
      if Low > Source'Last + 1 or High < Source'First - 1 then
         raise Index_Error;
      end if;

      declare
         Result_Len : constant Natural :=
           Source'Length - Natural'Max (0, High - Low + 1) + By'Length;
         Result : Wide_Wide_String (1 .. Result_Len);
         Front  : constant Natural := Low - Source'First;
      begin
         Result (1 .. Front) := Source (Source'First .. Low - 1);
         Result (Front + 1 .. Front + By'Length) := By;
         Result (Front + By'Length + 1 .. Result_Len) := Source (High + 1 .. Source'Last);
         return Result;
      end;
   end Replace_Slice;

   procedure Replace_Slice
     (Source  : in Out Wide_Wide_String;
      Low     : Positive;
      High    : Natural;
      By      : Wide_Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Result : constant Wide_Wide_String := Replace_Slice (Source, Low, High, By);
   begin
      Move (Result, Source, Drop, Justify, Pad);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Wide_Wide_String
   is
   begin
      return Replace_Slice (Source, Before, Before - 1, New_Item);
   end Insert;

   procedure Insert
     (Source   : in Out Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String;
      Drop     : Truncation := Error)
   is
      Result : constant Wide_Wide_String := Insert (Source, Before, New_Item);
   begin
      Move (Result, Source, Drop);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Wide_Wide_String
   is
   begin
      if Position > Source'Last + 1 then
         raise Index_Error;
      end if;

      declare
         End_Pos : constant Natural := Position + New_Item'Length - 1;
      begin
         if End_Pos <= Source'Last then
            declare
               Result : Wide_Wide_String := Source;
            begin
               Result (Position .. End_Pos) := New_Item;
               return Result;
            end;
         else
            return Source (Source'First .. Position - 1) & New_Item;
         end if;
      end;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String;
      Drop     : Truncation := Right)
   is
      Result : constant Wide_Wide_String := Overwrite (Source, Position, New_Item);
   begin
      Move (Result, Source, Drop);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Wide_Wide_String
   is
   begin
      if From > Through then
         return Source;
      end if;

      if From < Source'First or Through > Source'Last then
         raise Index_Error;
      end if;

      return Source (Source'First .. From - 1) & Source (Through + 1 .. Source'Last);
   end Delete;

   procedure Delete
     (Source  : in Out Wide_Wide_String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Result : constant Wide_Wide_String := Delete (Source, From, Through);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Wide_Wide_String;
      Side   : Trim_End) return Wide_Wide_String
   is
      First : Positive := Source'First;
      Last  : Natural := Source'Last;
   begin
      if Side = Left or Side = Both then
         while First <= Last and then Source (First) = Wide_Wide_Space loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while Last >= First and then Source (Last) = Wide_Wide_Space loop
            Last := Last - 1;
         end loop;
      end if;

      return Source (First .. Last);
   end Trim;

   procedure Trim
     (Source  : in Out Wide_Wide_String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Result : constant Wide_Wide_String := Trim (Source, Side);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Trim;

   function Trim
     (Source : Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set) return Wide_Wide_String
   is
      First : Positive := Source'First;
      Last  : Natural := Source'Last;
   begin
      while First <= Last and then Wide_Wide_Maps.Is_In (Source (First), Left) loop
         First := First + 1;
      end loop;

      while Last >= First and then Wide_Wide_Maps.Is_In (Source (Last), Right) loop
         Last := Last - 1;
      end loop;

      return Source (First .. Last);
   end Trim;

   procedure Trim
     (Source  : in Out Wide_Wide_String;
      Left    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Result : constant Wide_Wide_String := Trim (Source, Left, Right);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Count);
   begin
      if Count <= Source'Length then
         Result := Source (Source'First .. Source'First + Count - 1);
      else
         Result (1 .. Source'Length) := Source;
         Result (Source'Length + 1 .. Count) := (others => Pad);
      end if;
      return Result;
   end Head;

   procedure Head
     (Source  : in Out Wide_Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Result : constant Wide_Wide_String := Head (Source, Count, Pad);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Count);
   begin
      if Count <= Source'Length then
         Result := Source (Source'Last - Count + 1 .. Source'Last);
      else
         Result (1 .. Count - Source'Length) := (others => Pad);
         Result (Count - Source'Length + 1 .. Count) := Source;
      end if;
      return Result;
   end Tail;

   procedure Tail
     (Source  : in Out Wide_Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space)
   is
      Result : constant Wide_Wide_String := Tail (Source, Count, Pad);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_Character) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Left);
   begin
      for I in Result'Range loop
         Result (I) := Right;
      end loop;
      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Left * Right'Length);
      Pos    : Positive := 1;
   begin
      for I in 1 .. Left loop
         Result (Pos .. Pos + Right'Length - 1) := Right;
         Pos := Pos + Right'Length;
      end loop;
      return Result;
   end "*";

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping) return Wide_Wide_String
   is
      Result : Wide_Wide_String (Source'Range);
   begin
      for I in Source'Range loop
         Result (I) := Wide_Wide_Maps.Value (Mapping, Source (I));
      end loop;
      return Result;
   end Translate;

   procedure Translate
     (Source  : in Out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
   is
   begin
      for I in Source'Range loop
         Source (I) := Wide_Wide_Maps.Value (Mapping, Source (I));
      end loop;
   end Translate;

   function Translate
     (Source  : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function) return Wide_Wide_String
   is
      Result : Wide_Wide_String (Source'Range);
   begin
      for I in Source'Range loop
         Result (I) := Mapping (Source (I));
      end loop;
      return Result;
   end Translate;

   procedure Translate
     (Source  : in Out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
   is
   begin
      for I in Source'Range loop
         Source (I) := Mapping (Source (I));
      end loop;
   end Translate;

end Ada.Strings.Wide_Wide_Fixed;
