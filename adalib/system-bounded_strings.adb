-- System.Bounded_Strings body for Z80
-- Low-level bounded string operations implementation

package body System.Bounded_Strings is

   ----------------
   -- To_Bounded --
   ----------------

   function To_Bounded (Source : String) return Bounded_String is
      Result : Bounded_String;
      Len    : constant Natural := Natural'Min (Source'Length, Max_Length);
   begin
      Result.Length := Len;
      for I in 1 .. Len loop
         Result.Data (I) := Source (Source'First + I - 1);
      end loop;
      return Result;
   end To_Bounded;

   ---------------
   -- To_String --
   ---------------

   function To_String (Source : Bounded_String) return String is
   begin
      return Source.Data (1 .. Source.Length);
   end To_String;

   ------------
   -- Length --
   ------------

   function Length (Source : Bounded_String) return Natural is
   begin
      return Source.Length;
   end Length;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in Out Bounded_String;
      New_Item : String)
   is
      Space : constant Natural := Max_Length - Source.Length;
      Len   : constant Natural := Natural'Min (New_Item'Length, Space);
   begin
      for I in 1 .. Len loop
         Source.Data (Source.Length + I) := New_Item (New_Item'First + I - 1);
      end loop;
      Source.Length := Source.Length + Len;
   end Append;

   procedure Append
     (Source : in Out Bounded_String;
      New_Item : Character)
   is
   begin
      if Source.Length < Max_Length then
         Source.Length := Source.Length + 1;
         Source.Data (Source.Length) := New_Item;
      end if;
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Bounded_String;
      Index  : Positive) return Character
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
     (Source : in Out Bounded_String;
      Index  : Positive;
      By     : Character)
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
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return String
   is
   begin
      if Low > Source.Length + 1 or High > Source.Length then
         raise Constraint_Error;
      end if;
      return Source.Data (Low .. High);
   end Slice;

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

end System.Bounded_Strings;
