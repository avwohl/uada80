-- Ada.Strings.Fixed body for Z80
-- String manipulation implementations

package body Ada.Strings.Fixed is

   ---------
   -- Move --
   ---------

   procedure Move
     (Source  : String;
      Target  : out String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
      Source_Len : constant Natural := Source'Length;
      Target_Len : constant Natural := Target'Length;
      Pad_Count  : Natural;
   begin
      if Source_Len = Target_Len then
         Target := Source;
      elsif Source_Len > Target_Len then
         case Drop is
            when Left =>
               Target := Source (Source'Last - Target_Len + 1 .. Source'Last);
            when Right =>
               Target := Source (Source'First .. Source'First + Target_Len - 1);
            when Error =>
               raise Length_Error;
         end case;
      else
         -- Source is shorter, need padding
         Pad_Count := Target_Len - Source_Len;
         case Justify is
            when Left =>
               Target (Target'First .. Target'First + Source_Len - 1) := Source;
               for I in Target'First + Source_Len .. Target'Last loop
                  Target (I) := Pad;
               end loop;
            when Right =>
               for I in Target'First .. Target'First + Pad_Count - 1 loop
                  Target (I) := Pad;
               end loop;
               Target (Target'First + Pad_Count .. Target'Last) := Source;
            when Center =>
               declare
                  Left_Pad : Natural := Pad_Count / 2;
                  Right_Pad : Natural := Pad_Count - Left_Pad;
               begin
                  for I in Target'First .. Target'First + Left_Pad - 1 loop
                     Target (I) := Pad;
                  end loop;
                  Target (Target'First + Left_Pad .. Target'First + Left_Pad + Source_Len - 1) := Source;
                  for I in Target'Last - Right_Pad + 1 .. Target'Last loop
                     Target (I) := Pad;
                  end loop;
               end;
         end case;
      end if;
   end Move;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward) return Natural
   is
      Pattern_Len : constant Natural := Pattern'Length;
   begin
      if Pattern_Len = 0 then
         raise Pattern_Error;
      end if;

      if Pattern_Len > Source'Length then
         return 0;
      end if;

      if Going = Forward then
         for I in Source'First .. Source'Last - Pattern_Len + 1 loop
            if Source (I .. I + Pattern_Len - 1) = Pattern then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'First .. Source'Last - Pattern_Len + 1 loop
            if Source (I .. I + Pattern_Len - 1) = Pattern then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index;

   function Index
     (Source : String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
      function In_Set (C : Character) return Boolean is
      begin
         for I in Set'Range loop
            if Set (I) = C then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;
   begin
      if Going = Forward then
         for I in Source'Range loop
            if (Test = Inside) = In_Set (Source (I)) then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'Range loop
            if (Test = Inside) = In_Set (Source (I)) then
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
     (Source : String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for I in Source'Range loop
            if Source (I) /= ' ' then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'Range loop
            if Source (I) /= ' ' then
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
     (Source  : String;
      Pattern : String) return Natural
   is
      N : Natural := 0;
      J : Natural;
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      J := Source'First;
      while J <= Source'Last - Pattern'Length + 1 loop
         if Source (J .. J + Pattern'Length - 1) = Pattern then
            N := N + 1;
            J := J + Pattern'Length;
         else
            J := J + 1;
         end if;
      end loop;

      return N;
   end Count;

   function Count
     (Source : String;
      Set    : String) return Natural
   is
      N : Natural := 0;
   begin
      for I in Source'Range loop
         for J in Set'Range loop
            if Source (I) = Set (J) then
               N := N + 1;
               exit;
            end if;
         end loop;
      end loop;
      return N;
   end Count;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String
   is
      Front : String := Source (Source'First .. Low - 1);
      Back  : String := Source (High + 1 .. Source'Last);
   begin
      return Front & By & Back;
   end Replace_Slice;

   procedure Replace_Slice
     (Source  : in Out String;
      Low     : Positive;
      High    : Natural;
      By      : String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
      Result : String := Replace_Slice (Source, Low, High, By);
   begin
      Move (Result, Source, Drop, Justify, Pad);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String
   is
   begin
      return Source (Source'First .. Before - 1) &
             New_Item &
             Source (Before .. Source'Last);
   end Insert;

   procedure Insert
     (Source   : in Out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   is
      Result : String := Insert (Source, Before, New_Item);
   begin
      Move (Result, Source, Drop);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String
   is
      Result : String := Source;
      End_Pos : Natural := Position + New_Item'Length - 1;
   begin
      if End_Pos > Source'Last then
         End_Pos := Source'Last;
      end if;
      Result (Position .. End_Pos) := New_Item (New_Item'First .. New_Item'First + End_Pos - Position);
      return Result;
   end Overwrite;

   procedure Overwrite
     (Source   : in Out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right)
   is
      End_Pos : Natural := Position + New_Item'Length - 1;
   begin
      if End_Pos > Source'Last then
         End_Pos := Source'Last;
      end if;
      Source (Position .. End_Pos) := New_Item (New_Item'First .. New_Item'First + End_Pos - Position);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String
   is
   begin
      if From > Through then
         return Source;
      end if;
      return Source (Source'First .. From - 1) &
             Source (Through + 1 .. Source'Last);
   end Delete;

   procedure Delete
     (Source  : in Out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
      Result : String := Delete (Source, From, Through);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : String;
      Side   : Trim_End) return String
   is
      Low  : Positive := Source'First;
      High : Natural := Source'Last;
   begin
      if Side = Left or Side = Both then
         while Low <= High and then Source (Low) = ' ' loop
            Low := Low + 1;
         end loop;
      end if;

      if Side = Right or Side = Both then
         while High >= Low and then Source (High) = ' ' loop
            High := High - 1;
         end loop;
      end if;

      return Source (Low .. High);
   end Trim;

   procedure Trim
     (Source  : in Out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
      Result : String := Trim (Source, Side);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Trim;

   function Trim
     (Source : String;
      Left   : String;
      Right  : String) return String
   is
      Low  : Positive := Source'First;
      High : Natural := Source'Last;

      function In_Set (C : Character; Set : String) return Boolean is
      begin
         for I in Set'Range loop
            if Set (I) = C then
               return True;
            end if;
         end loop;
         return False;
      end In_Set;
   begin
      while Low <= High and then In_Set (Source (Low), Left) loop
         Low := Low + 1;
      end loop;

      while High >= Low and then In_Set (Source (High), Right) loop
         High := High - 1;
      end loop;

      return Source (Low .. High);
   end Trim;

   procedure Trim
     (Source  : in Out String;
      Left    : String;
      Right   : String;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space)
   is
      Result : String := Trim (Source, Left, Right);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      Result : String (1 .. Count);
   begin
      if Count <= Source'Length then
         Result := Source (Source'First .. Source'First + Count - 1);
      else
         Result (1 .. Source'Length) := Source;
         for I in Source'Length + 1 .. Count loop
            Result (I) := Pad;
         end loop;
      end if;
      return Result;
   end Head;

   procedure Head
     (Source  : in Out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
      Result : String := Head (Source, Count, Pad);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      Result : String (1 .. Count);
   begin
      if Count <= Source'Length then
         Result := Source (Source'Last - Count + 1 .. Source'Last);
      else
         for I in 1 .. Count - Source'Length loop
            Result (I) := Pad;
         end loop;
         Result (Count - Source'Length + 1 .. Count) := Source;
      end if;
      return Result;
   end Tail;

   procedure Tail
     (Source  : in Out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
      Result : String := Tail (Source, Count, Pad);
   begin
      Move (Result, Source, Error, Justify, Pad);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Character) return String is
      Result : String (1 .. Left);
   begin
      for I in Result'Range loop
         Result (I) := Right;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Natural; Right : String) return String is
      Len : constant Natural := Right'Length;
      Result : String (1 .. Left * Len);
   begin
      for I in 0 .. Left - 1 loop
         Result (I * Len + 1 .. (I + 1) * Len) := Right;
      end loop;
      return Result;
   end "*";

end Ada.Strings.Fixed;
