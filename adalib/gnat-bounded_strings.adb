-- GNAT.Bounded_Strings body for Z80
-- Fixed-capacity bounded strings implementation

package body GNAT.Bounded_Strings is

   ------------
   -- Length --
   ------------

   function Length (Source : Bounded_String) return Natural is
   begin
      return Source.Length;
   end Length;

   --------------
   -- Max_Size --
   --------------

   function Max_Size (Source : Bounded_String) return Natural is
      pragma Unreferenced (Source);
   begin
      return Max_Length;
   end Max_Size;

   ---------------
   -- To_String --
   ---------------

   function To_String (Source : Bounded_String) return String is
   begin
      return Source.Data (1 .. Source.Length);
   end To_String;

   -----------------------
   -- To_Bounded_String --
   -----------------------

   function To_Bounded_String (Source : String) return Bounded_String is
      Result : Bounded_String;
      Len    : constant Natural := Natural'Min (Source'Length, Max_Length);
   begin
      Result.Data (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      Result.Length := Len;
      return Result;
   end To_Bounded_String;

   ---------
   -- Set --
   ---------

   procedure Set (Target : out Bounded_String; Source : String) is
      Len : constant Natural := Natural'Min (Source'Length, Max_Length);
   begin
      Target.Data (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      Target.Length := Len;
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in Out Bounded_String; New_Item : String) is
      Space : constant Natural := Max_Length - Source.Length;
      Len   : constant Natural := Natural'Min (New_Item'Length, Space);
   begin
      Source.Data (Source.Length + 1 .. Source.Length + Len) :=
        New_Item (New_Item'First .. New_Item'First + Len - 1);
      Source.Length := Source.Length + Len;
   end Append;

   procedure Append (Source : in Out Bounded_String; New_Item : Character) is
   begin
      if Source.Length < Max_Length then
         Source.Length := Source.Length + 1;
         Source.Data (Source.Length) := New_Item;
      end if;
   end Append;

   procedure Append (Source : in Out Bounded_String; New_Item : Bounded_String) is
   begin
      Append (Source, To_String (New_Item));
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

   -------------
   -- Element --
   -------------

   function Element (Source : Bounded_String; Index : Positive) return Character is
   begin
      if Index <= Source.Length then
         return Source.Data (Index);
      else
         return ASCII.NUL;
      end if;
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
      if Index <= Source.Length then
         Source.Data (Index) := By;
      end if;
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
      if High < Low then
         return "";
      elsif Low > Source.Length then
         return "";
      else
         declare
            Actual_High : constant Natural :=
              Natural'Min (High, Source.Length);
         begin
            return Source.Data (Low .. Actual_High);
         end;
      end if;
   end Slice;

   -----------
   -- Clear --
   -----------

   procedure Clear (Source : out Bounded_String) is
   begin
      Source.Length := 0;
   end Clear;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Bounded_String) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;
      return Left.Data (1 .. Left.Length) = Right.Data (1 .. Right.Length);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Bounded_String) return Boolean is
   begin
      return Left.Data (1 .. Left.Length) < Right.Data (1 .. Right.Length);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Bounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Bounded_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Bounded_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Bounded_String;
      Pattern : String) return Natural
   is
   begin
      if Pattern'Length = 0 or Pattern'Length > Source.Length then
         return 0;
      end if;

      for I in 1 .. Source.Length - Pattern'Length + 1 loop
         if Source.Data (I .. I + Pattern'Length - 1) = Pattern then
            return I;
         end if;
      end loop;

      return 0;
   end Index;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Bounded_String;
      Pattern : String) return Natural
   is
      Result : Natural := 0;
      I      : Positive := 1;
   begin
      if Pattern'Length = 0 or Pattern'Length > Source.Length then
         return 0;
      end if;

      while I <= Source.Length - Pattern'Length + 1 loop
         if Source.Data (I .. I + Pattern'Length - 1) = Pattern then
            Result := Result + 1;
            I := I + Pattern'Length;
         else
            I := I + 1;
         end if;
      end loop;

      return Result;
   end Count;

   ----------
   -- Trim --
   ----------

   procedure Trim
     (Source : in Out Bounded_String;
      Side   : Ada.Strings.Trim_End := Ada.Strings.Both)
   is
      First : Natural := 1;
      Last  : Natural := Source.Length;
   begin
      if Source.Length = 0 then
         return;
      end if;

      -- Trim left
      if Side = Ada.Strings.Left or Side = Ada.Strings.Both then
         while First <= Last and then
           (Source.Data (First) = ' ' or Source.Data (First) = ASCII.HT)
         loop
            First := First + 1;
         end loop;
      end if;

      -- Trim right
      if Side = Ada.Strings.Right or Side = Ada.Strings.Both then
         while Last >= First and then
           (Source.Data (Last) = ' ' or Source.Data (Last) = ASCII.HT)
         loop
            Last := Last - 1;
         end loop;
      end if;

      if First > Last then
         Source.Length := 0;
      elsif First > 1 then
         Source.Data (1 .. Last - First + 1) := Source.Data (First .. Last);
         Source.Length := Last - First + 1;
      else
         Source.Length := Last;
      end if;
   end Trim;

   ----------
   -- Head --
   ----------

   procedure Head
     (Source : in Out Bounded_String;
      Count  : Natural)
   is
   begin
      if Count < Source.Length then
         Source.Length := Count;
      end if;
   end Head;

   ----------
   -- Tail --
   ----------

   procedure Tail
     (Source : in Out Bounded_String;
      Count  : Natural)
   is
   begin
      if Count >= Source.Length then
         return;
      end if;

      declare
         Start : constant Positive := Source.Length - Count + 1;
      begin
         Source.Data (1 .. Count) := Source.Data (Start .. Source.Length);
         Source.Length := Count;
      end;
   end Tail;

end GNAT.Bounded_Strings;
