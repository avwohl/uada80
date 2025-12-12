-- GNAT.Rewrite_Data body for Z80
-- Data rewriting implementation

package body GNAT.Rewrite_Data is

   ------------
   -- Create --
   ------------

   procedure Create
     (B           : out Buffer;
      Pattern     : String;
      Replacement : String)
   is
      PLen : constant Natural := Natural'Min (Pattern'Length, Max_Pattern);
      RLen : constant Natural := Natural'Min (Replacement'Length, Max_Pattern);
   begin
      B.Pattern (1 .. PLen) := Pattern (Pattern'First .. Pattern'First + PLen - 1);
      B.Pat_Len := PLen;
      B.Replacement (1 .. RLen) := Replacement (Replacement'First .. Replacement'First + RLen - 1);
      B.Rep_Len := RLen;
      B.Pend_Len := 0;
   end Create;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite
     (B      : in Out Buffer;
      Input  : String;
      Output : out String;
      Last   : out Natural)
   is
      Out_Idx : Natural := Output'First - 1;
      I       : Natural := Input'First;
      Match   : Boolean;
   begin
      while I <= Input'Last loop
         -- Check for pattern match
         Match := False;
         if I + B.Pat_Len - 1 <= Input'Last then
            Match := Input (I .. I + B.Pat_Len - 1) = B.Pattern (1 .. B.Pat_Len);
         end if;

         if Match then
            -- Output replacement
            for J in 1 .. B.Rep_Len loop
               if Out_Idx < Output'Last then
                  Out_Idx := Out_Idx + 1;
                  Output (Out_Idx) := B.Replacement (J);
               end if;
            end loop;
            I := I + B.Pat_Len;
         else
            -- Copy input character
            if Out_Idx < Output'Last then
               Out_Idx := Out_Idx + 1;
               Output (Out_Idx) := Input (I);
            end if;
            I := I + 1;
         end if;
      end loop;

      Last := Out_Idx;
   end Rewrite;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (B      : in Out Buffer;
      Output : out String;
      Last   : out Natural)
   is
   begin
      -- Copy pending data to output
      for I in 1 .. B.Pend_Len loop
         if I <= Output'Length then
            Output (Output'First + I - 1) := B.Pending (I);
         end if;
      end loop;
      Last := Output'First + B.Pend_Len - 1;
      B.Pend_Len := 0;
   end Flush;

   -----------
   -- Reset --
   -----------

   procedure Reset (B : in Out Buffer) is
   begin
      B.Pend_Len := 0;
   end Reset;

end GNAT.Rewrite_Data;
