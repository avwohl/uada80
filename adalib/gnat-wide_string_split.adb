-- GNAT.Wide_String_Split body for Z80
-- Wide string splitting implementation

package body GNAT.Wide_String_Split is

   function Is_Separator (C : Wide_Character; Seps : Wide_String) return Boolean is
   begin
      for S of Seps loop
         if C = S then
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
      From       : Wide_String;
      Separators : Wide_String;
      Mode       : Separator_Mode := Single)
   is
      In_Sep : Boolean := False;
      Start  : Natural := 1;
   begin
      S.Num_Slices := 0;
      S.Data_Last := Natural'Min (From'Length, Max_Length);

      -- Copy data
      for I in 1 .. S.Data_Last loop
         S.Data (I) := From (From'First + I - 1);
      end loop;

      -- Find slices
      for I in 1 .. S.Data_Last loop
         if Is_Separator (S.Data (I), Separators) then
            if not In_Sep then
               if I > Start then
                  S.Num_Slices := S.Num_Slices + 1;
                  S.Slices (Positive (S.Num_Slices)).First := Start;
                  S.Slices (Positive (S.Num_Slices)).Last := I - 1;
               end if;
               In_Sep := True;
            end if;

            if Mode = Single then
               In_Sep := False;
               Start := I + 1;
            end if;
         else
            if In_Sep or I = 1 then
               Start := I;
               In_Sep := False;
            end if;
         end if;
      end loop;

      -- Handle last slice
      if Start <= S.Data_Last and not In_Sep then
         S.Num_Slices := S.Num_Slices + 1;
         S.Slices (Positive (S.Num_Slices)).First := Start;
         S.Slices (Positive (S.Num_Slices)).Last := S.Data_Last;
      end if;
   end Create;

   -----------------
   -- Slice_Count --
   -----------------

   function Slice_Count (S : Slice_Set) return Slice_Number is
   begin
      return S.Num_Slices;
   end Slice_Count;

   -----------
   -- Slice --
   -----------

   function Slice
     (S     : Slice_Set;
      Index : Slice_Number) return Wide_String
   is
      Info : Slice_Info;
   begin
      if Index = 0 or else Index > S.Num_Slices then
         return "";
      end if;

      Info := S.Slices (Positive (Index));

      declare
         Result : Wide_String (1 .. Info.Last - Info.First + 1);
      begin
         for I in Result'Range loop
            Result (I) := S.Data (Info.First + I - 1);
         end loop;
         return Result;
      end;
   end Slice;

end GNAT.Wide_String_Split;
