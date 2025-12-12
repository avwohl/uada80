-- System.File_Control_Block body for Z80/CP/M
-- CP/M File Control Block implementation

package body System.File_Control_Block is

   --------------
   -- Init_FCB --
   --------------

   procedure Init_FCB (F : out FCB; Filename : String) is
   begin
      Clear_FCB (F);
      Parse_Filename (Filename, F.Name, F.Extension);
   end Init_FCB;

   --------------------
   -- Parse_Filename --
   --------------------

   procedure Parse_Filename
     (Filename : String;
      Name     : out String;
      Ext      : out String)
   is
      Dot_Pos : Natural := 0;
      N_Idx   : Natural := Name'First;
      E_Idx   : Natural := Ext'First;
   begin
      -- Initialize with spaces
      Name := (others => ' ');
      Ext  := (others => ' ');

      -- Find dot position
      for I in Filename'Range loop
         if Filename (I) = '.' then
            Dot_Pos := I;
            exit;
         end if;
      end loop;

      -- Copy name part (before dot or all if no dot)
      for I in Filename'Range loop
         if Dot_Pos > 0 and then I >= Dot_Pos then
            exit;
         end if;

         if N_Idx <= Name'Last then
            -- Convert to uppercase
            if Filename (I) in 'a' .. 'z' then
               Name (N_Idx) := Character'Val (Character'Pos (Filename (I)) - 32);
            else
               Name (N_Idx) := Filename (I);
            end if;
            N_Idx := N_Idx + 1;
         end if;
      end loop;

      -- Copy extension part (after dot)
      if Dot_Pos > 0 then
         for I in Dot_Pos + 1 .. Filename'Last loop
            if E_Idx <= Ext'Last then
               -- Convert to uppercase
               if Filename (I) in 'a' .. 'z' then
                  Ext (E_Idx) := Character'Val (Character'Pos (Filename (I)) - 32);
               else
                  Ext (E_Idx) := Filename (I);
               end if;
               E_Idx := E_Idx + 1;
            end if;
         end loop;
      end if;
   end Parse_Filename;

   ---------------
   -- Clear_FCB --
   ---------------

   procedure Clear_FCB (F : out FCB) is
   begin
      F.Drive     := 0;
      F.Name      := (others => ' ');
      F.Extension := (others => ' ');
      F.Extent    := 0;
      F.S1        := 0;
      F.S2        := 0;
      F.RC        := 0;
      F.D         := (others => ' ');
      F.CR        := 0;
      F.R0        := 0;
      F.R1        := 0;
      F.R2        := 0;
   end Clear_FCB;

end System.File_Control_Block;
