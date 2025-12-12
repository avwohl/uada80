-- GNAT.String_Split body for Z80
-- String splitting utilities implementation

package body GNAT.String_Split is

   function Is_Separator (C : Character; Separators : String) return Boolean is
   begin
      for I in Separators'Range loop
         if C = Separators (I) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Separator;

   ------------
   -- Create --
   ------------

   procedure Create
     (S          : out Slice_Set;
      From       : String;
      Separators : String;
      Mode       : Separator_Mode := Single)
   is
      Pos : Natural := From'First;
      Start : Natural;
   begin
      -- Initialize
      S.Count := 0;
      S.Length := Natural'Min (From'Length, S.Source'Length);

      -- Copy source
      for I in 1 .. S.Length loop
         S.Source (I) := From (From'First + I - 1);
      end loop;

      Pos := 1;

      while Pos <= S.Length and then S.Count < Max_Slices loop
         -- Skip separators if in Multiple mode
         if Mode = Multiple then
            while Pos <= S.Length
              and then Is_Separator (S.Source (Pos), Separators)
            loop
               Pos := Pos + 1;
            end loop;
         end if;

         exit when Pos > S.Length;

         -- Find start of slice
         Start := Pos;

         -- Find end of slice
         while Pos <= S.Length
           and then not Is_Separator (S.Source (Pos), Separators)
         loop
            Pos := Pos + 1;
         end loop;

         -- Record slice
         S.Count := S.Count + 1;
         S.Slices (Natural (S.Count)).First := Start;
         S.Slices (Natural (S.Count)).Last := Pos - 1;

         -- Skip separator
         if Pos <= S.Length then
            Pos := Pos + 1;
         end if;
      end loop;
   end Create;

   -----------------
   -- Slice_Count --
   -----------------

   function Slice_Count (S : Slice_Set) return Slice_Number is
   begin
      return S.Count;
   end Slice_Count;

   -----------
   -- Slice --
   -----------

   function Slice (S : Slice_Set; Index : Slice_Number) return String is
   begin
      if Index = 0 or Index > S.Count then
         return "";
      end if;

      declare
         Info : constant Slice_Info := S.Slices (Natural (Index));
      begin
         if Info.First > Info.Last then
            return "";
         end if;
         return S.Source (Info.First .. Info.Last);
      end;
   end Slice;

end GNAT.String_Split;
