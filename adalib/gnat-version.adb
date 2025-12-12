-- GNAT.Version body for Z80
-- Version utilities implementation

package body GNAT.Version is

   function Int_To_String (N : Natural) return String is
      Buf : String (1 .. 4);
      Pos : Natural := 4;
      V   : Natural := N;
   begin
      if V = 0 then
         return "0";
      end if;

      while V > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + (V mod 10));
         V := V / 10;
         Pos := Pos - 1;
      end loop;

      return Buf (Pos + 1 .. 4);
   end Int_To_String;

   ------------------
   -- Make_Version --
   ------------------

   function Make_Version (Major, Minor : Natural;
                          Patch : Natural := 0) return Version_Number is
   begin
      return (Major => Major, Minor => Minor, Patch => Patch);
   end Make_Version;

   -------------------
   -- Parse_Version --
   -------------------

   function Parse_Version (S : String) return Version_Number is
      V       : Version_Number := (0, 0, 0);
      Current : Natural := 0;
      Part    : Natural := 1;  -- 1=major, 2=minor, 3=patch
   begin
      for I in S'Range loop
         if S (I) = '.' then
            if Part = 1 then
               V.Major := Current;
            elsif Part = 2 then
               V.Minor := Current;
            end if;
            Current := 0;
            Part := Part + 1;
         elsif S (I) >= '0' and S (I) <= '9' then
            Current := Current * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
         end if;
      end loop;

      -- Store last part
      if Part = 1 then
         V.Major := Current;
      elsif Part = 2 then
         V.Minor := Current;
      elsif Part = 3 then
         V.Patch := Current;
      end if;

      return V;
   end Parse_Version;

   ---------------
   -- To_String --
   ---------------

   function To_String (V : Version_Number) return String is
   begin
      return Int_To_String (V.Major) & "." &
             Int_To_String (V.Minor) & "." &
             Int_To_String (V.Patch);
   end To_String;

   ---------------------
   -- To_String_Short --
   ---------------------

   function To_String_Short (V : Version_Number) return String is
   begin
      return Int_To_String (V.Major) & "." & Int_To_String (V.Minor);
   end To_String_Short;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Version_Number) return Boolean is
   begin
      return Left.Major = Right.Major and
             Left.Minor = Right.Minor and
             Left.Patch = Right.Patch;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Version_Number) return Boolean is
   begin
      if Left.Major /= Right.Major then
         return Left.Major < Right.Major;
      elsif Left.Minor /= Right.Minor then
         return Left.Minor < Right.Minor;
      else
         return Left.Patch < Right.Patch;
      end if;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Version_Number) return Boolean is
   begin
      return Left = Right or Left < Right;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Version_Number) return Boolean is
   begin
      return Right < Left;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Version_Number) return Boolean is
   begin
      return Left = Right or Left > Right;
   end ">=";

   -------------------
   -- Is_Compatible --
   -------------------

   function Is_Compatible (Required, Actual : Version_Number) return Boolean is
   begin
      -- Compatible if same major version and actual >= required
      return Actual.Major = Required.Major and Actual >= Required;
   end Is_Compatible;

   --------------
   -- Is_Newer --
   --------------

   function Is_Newer (V1, V2 : Version_Number) return Boolean is
   begin
      return V1 > V2;
   end Is_Newer;

   -------------------
   -- Is_Same_Major --
   -------------------

   function Is_Same_Major (V1, V2 : Version_Number) return Boolean is
   begin
      return V1.Major = V2.Major;
   end Is_Same_Major;

   ---------------------
   -- Increment_Major --
   ---------------------

   function Increment_Major (V : Version_Number) return Version_Number is
   begin
      return (Major => V.Major + 1, Minor => 0, Patch => 0);
   end Increment_Major;

   ---------------------
   -- Increment_Minor --
   ---------------------

   function Increment_Minor (V : Version_Number) return Version_Number is
   begin
      return (Major => V.Major, Minor => V.Minor + 1, Patch => 0);
   end Increment_Minor;

   ---------------------
   -- Increment_Patch --
   ---------------------

   function Increment_Patch (V : Version_Number) return Version_Number is
   begin
      return (Major => V.Major, Minor => V.Minor, Patch => V.Patch + 1);
   end Increment_Patch;

   ------------------
   -- Build_String --
   ------------------

   function Build_String return String is
   begin
      return Compiler_Name & " " &
             To_String (Current_Build.Version) & " (" &
             Current_Build.Build_Date & ") for " &
             Current_Build.Target;
   end Build_String;

   --------------
   -- In_Range --
   --------------

   function In_Range (V, Min_Version, Max_Version : Version_Number) return Boolean is
   begin
      return V >= Min_Version and V <= Max_Version;
   end In_Range;

   ------------------------
   -- Is_Breaking_Change --
   ------------------------

   function Is_Breaking_Change (Old_V, New_V : Version_Number) return Boolean is
   begin
      return New_V.Major > Old_V.Major;
   end Is_Breaking_Change;

   ------------------------
   -- Is_Feature_Release --
   ------------------------

   function Is_Feature_Release (Old_V, New_V : Version_Number) return Boolean is
   begin
      return New_V.Major = Old_V.Major and New_V.Minor > Old_V.Minor;
   end Is_Feature_Release;

   ----------------
   -- Is_Bug_Fix --
   ----------------

   function Is_Bug_Fix (Old_V, New_V : Version_Number) return Boolean is
   begin
      return New_V.Major = Old_V.Major and
             New_V.Minor = Old_V.Minor and
             New_V.Patch > Old_V.Patch;
   end Is_Bug_Fix;

   ------------------
   -- Major_String --
   ------------------

   function Major_String (V : Version_Number) return String is
   begin
      return Int_To_String (V.Major);
   end Major_String;

   ------------------
   -- Minor_String --
   ------------------

   function Minor_String (V : Version_Number) return String is
   begin
      return Int_To_String (V.Minor);
   end Minor_String;

   ------------------
   -- Patch_String --
   ------------------

   function Patch_String (V : Version_Number) return String is
   begin
      return Int_To_String (V.Patch);
   end Patch_String;

end GNAT.Version;
