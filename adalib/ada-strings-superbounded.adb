-- Ada.Strings.Superbounded body for Z80
-- Super bounded string support implementation

package body Ada.Strings.Superbounded is

   ------------
   -- Length --
   ------------

   function Length (Source : Super_String) return Natural is
   begin
      return Source.Cur_Length;
   end Length;

   ----------------------
   -- Set_Super_String --
   ----------------------

   procedure Set_Super_String
     (Target   : in Out Super_String;
      Source   : String;
      Max      : Positive;
      Drop     : Truncation := Error)
   is
   begin
      Target.Max_Length := Max;
      if Source'Length <= Max then
         Target.Cur_Length := Source'Length;
         Target.Data (1 .. Source'Length) := Source;
      else
         case Drop is
            when Left =>
               Target.Cur_Length := Max;
               Target.Data (1 .. Max) :=
                 Source (Source'Last - Max + 1 .. Source'Last);
            when Right =>
               Target.Cur_Length := Max;
               Target.Data (1 .. Max) :=
                 Source (Source'First .. Source'First + Max - 1);
            when Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Set_Super_String;

   ---------------------
   -- Super_To_String --
   ---------------------

   function Super_To_String (Source : Super_String) return String is
   begin
      return Source.Data (1 .. Source.Cur_Length);
   end Super_To_String;

   -------------------
   -- Super_Element --
   -------------------

   function Super_Element
     (Source : Super_String;
      Index  : Positive) return Character
   is
   begin
      if Index > Source.Cur_Length then
         raise Ada.Strings.Index_Error;
      end if;
      return Source.Data (Index);
   end Super_Element;

   ---------------------------
   -- Super_Replace_Element --
   ---------------------------

   procedure Super_Replace_Element
     (Source : in Out Super_String;
      Index  : Positive;
      By     : Character)
   is
   begin
      if Index > Source.Cur_Length then
         raise Ada.Strings.Index_Error;
      end if;
      Source.Data (Index) := By;
   end Super_Replace_Element;

   -----------------
   -- Super_Slice --
   -----------------

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return String
   is
   begin
      if Low > Source.Cur_Length + 1 or High > Source.Cur_Length then
         raise Ada.Strings.Index_Error;
      end if;
      return Source.Data (Low .. High);
   end Super_Slice;

   ------------------
   -- Super_Append --
   ------------------

   function Super_Append
     (Left  : Super_String;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String
   is
   begin
      return Super_Append (Left, Super_To_String (Right), Drop);
   end Super_Append;

   ------------------
   -- Super_Append --
   ------------------

   function Super_Append
     (Left  : Super_String;
      Right : String;
      Drop  : Truncation := Error) return Super_String
   is
      Result : Super_String;
   begin
      Result.Max_Length := Left.Max_Length;

      if Left.Cur_Length + Right'Length <= Left.Max_Length then
         Result.Cur_Length := Left.Cur_Length + Right'Length;
         Result.Data (1 .. Left.Cur_Length) := Left.Data (1 .. Left.Cur_Length);
         Result.Data (Left.Cur_Length + 1 .. Result.Cur_Length) := Right;
      else
         case Drop is
            when Left =>
               if Right'Length >= Left.Max_Length then
                  Result.Cur_Length := Left.Max_Length;
                  Result.Data (1 .. Left.Max_Length) :=
                    Right (Right'Last - Left.Max_Length + 1 .. Right'Last);
               else
                  declare
                     Keep : constant Natural := Left.Max_Length - Right'Length;
                  begin
                     Result.Cur_Length := Left.Max_Length;
                     Result.Data (1 .. Keep) :=
                       Left.Data (Left.Cur_Length - Keep + 1 .. Left.Cur_Length);
                     Result.Data (Keep + 1 .. Left.Max_Length) := Right;
                  end;
               end if;
            when Right =>
               Result.Cur_Length := Left.Max_Length;
               Result.Data (1 .. Left.Cur_Length) :=
                 Left.Data (1 .. Left.Cur_Length);
               declare
                  Space : constant Natural := Left.Max_Length - Left.Cur_Length;
               begin
                  Result.Data (Left.Cur_Length + 1 .. Left.Max_Length) :=
                    Right (Right'First .. Right'First + Space - 1);
               end;
            when Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Append;

   -----------------
   -- Super_Index --
   -----------------

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      pragma Unreferenced (Mapping);
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      if Pattern'Length > Source.Cur_Length then
         return 0;
      end if;

      if Going = Forward then
         for I in 1 .. Source.Cur_Length - Pattern'Length + 1 loop
            if Source.Data (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      else
         for I in reverse 1 .. Source.Cur_Length - Pattern'Length + 1 loop
            if Source.Data (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Super_Index;

end Ada.Strings.Superbounded;
