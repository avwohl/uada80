-- GNAT.Array_Split body for Z80
-- Array splitting utilities implementation

package body GNAT.Array_Split is

   ------------
   -- Create --
   ------------

   procedure Create
     (S          : out Slice_Set;
      From       : Element_Sequence;
      Separators : Element_Set;
      Mode       : Separator_Mode := Single)
   is
      In_Sep : Boolean := False;
      Start  : Natural := 1;
   begin
      S.Num_Slices := 0;
      S.Data_Last := From'Length;

      -- Copy data
      for I in From'Range loop
         S.Data (I - From'First + 1) := From (I);
      end loop;

      -- Find slices
      for I in 1 .. S.Data_Last loop
         if Is_In (S.Data (I), Separators) then
            if not In_Sep then
               -- End of slice
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
      Index : Slice_Number) return Element_Sequence
   is
      Info : Slice_Info;
   begin
      if Index = 0 or else Index > S.Num_Slices then
         declare
            Empty : Element_Sequence (1 .. 0);
         begin
            return Empty;
         end;
      end if;

      Info := S.Slices (Positive (Index));

      declare
         Result : Element_Sequence (1 .. Info.Last - Info.First + 1);
      begin
         for I in Result'Range loop
            Result (I) := S.Data (Info.First + I - 1);
         end loop;
         return Result;
      end;
   end Slice;

   ----------------
   -- Separators --
   ----------------

   function Separators
     (S     : Slice_Set;
      Index : Slice_Number) return Slice_Separators
   is
      Empty : Slice_Separators (1 .. 0);
   begin
      -- Simplified: return empty for now
      return Empty;
   end Separators;

end GNAT.Array_Split;
