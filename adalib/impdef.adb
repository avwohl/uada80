-- ImpDef package body for ACATS test harness
-- Z80/CP/M implementation

package body ImpDef is

   procedure Exceed_Time_Slice is
      -- Burn some CPU time to exceed a time slice
      Dummy : Integer := 0;
   begin
      for I in 1 .. 1000 loop
         Dummy := Dummy + 1;
      end loop;
   end Exceed_Time_Slice;

   function Equivalent_File_Names (Left, Right : String) return Boolean is
      -- CP/M is case-insensitive for file names
      function To_Upper (C : Character) return Character is
      begin
         if C in 'a' .. 'z' then
            return Character'Val(Character'Pos(C) - 32);
         else
            return C;
         end if;
      end To_Upper;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in 0 .. Left'Length - 1 loop
         if To_Upper(Left(Left'First + I)) /= To_Upper(Right(Right'First + I)) then
            return False;
         end if;
      end loop;
      return True;
   end Equivalent_File_Names;

end ImpDef;
