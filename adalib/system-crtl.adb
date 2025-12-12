-- System.CRTL body for Z80
-- C runtime library interface (CP/M BDOS calls)

with System.Storage_Elements;
with System.File_Control_Block;

package body System.CRTL is

   use System.Storage_Elements;

   -- DMA buffer (128 bytes)
   DMA_Buffer : String (1 .. 128);
   pragma Volatile_Components (DMA_Buffer);

   -- Simple file descriptor table (8 entries)
   type FD_Entry is record
      In_Use : Boolean := False;
      FCB    : System.File_Control_Block.FCB;
      Pos    : Natural := 0;
   end record;

   FD_Table : array (0 .. 7) of FD_Entry;

   -- Convert filename to FCB format
   procedure Setup_FCB
     (FCB  : out System.File_Control_Block.FCB;
      Name : String)
   is
      use System.File_Control_Block;
      Dot_Pos : Natural := 0;
      J : Natural;
   begin
      -- Initialize FCB
      FCB := (Drive_Code => 0,
              File_Name  => (others => ' '),
              File_Type  => (others => ' '),
              Extent     => 0,
              S1         => 0,
              S2         => 0,
              Record_Count => 0,
              Alloc_Map  => (others => 0),
              Current_Record => 0,
              Random_Record  => (others => 0));

      -- Find dot position
      for I in Name'Range loop
         if Name (I) = '.' then
            Dot_Pos := I;
            exit;
         end if;
      end loop;

      -- Copy filename (up to 8 chars)
      J := 1;
      for I in Name'First .. (if Dot_Pos > 0 then Dot_Pos - 1 else Name'Last) loop
         exit when J > 8;
         if Name (I) in 'a' .. 'z' then
            FCB.File_Name (J) := Character'Val (Character'Pos (Name (I)) - 32);
         else
            FCB.File_Name (J) := Name (I);
         end if;
         J := J + 1;
      end loop;

      -- Copy extension (up to 3 chars)
      if Dot_Pos > 0 then
         J := 1;
         for I in Dot_Pos + 1 .. Name'Last loop
            exit when J > 3;
            if Name (I) in 'a' .. 'z' then
               FCB.File_Type (J) := Character'Val (Character'Pos (Name (I)) - 32);
            else
               FCB.File_Type (J) := Name (I);
            end if;
            J := J + 1;
         end loop;
      end if;
   end Setup_FCB;

   -- Allocate file descriptor
   function Alloc_FD return Integer is
   begin
      for I in FD_Table'Range loop
         if not FD_Table (I).In_Use then
            FD_Table (I).In_Use := True;
            FD_Table (I).Pos := 0;
            return I;
         end if;
      end loop;
      return -1;
   end Alloc_FD;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read (Name : String) return Integer is
      FD : Integer;
   begin
      FD := Alloc_FD;
      if FD < 0 then
         return -1;
      end if;

      Setup_FCB (FD_Table (FD).FCB, Name);

      -- Set DMA and try to open
      null;  -- BDOS (Set_DMA, Natural (To_Integer (FD_Table (FD).FCB'Address)));
      if BDOS (Open_File, Natural (To_Integer (FD_Table (FD).FCB'Address))) = 255 then
         FD_Table (FD).In_Use := False;
         return -1;
      end if;

      return FD;
   end Open_Read;

   ---------------------
   -- Open_Read_Write --
   ---------------------

   function Open_Read_Write (Name : String) return Integer is
   begin
      return Open_Read (Name);  -- CP/M doesn't distinguish
   end Open_Read_Write;

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Integer is
      FD : Integer;
   begin
      FD := Alloc_FD;
      if FD < 0 then
         return -1;
      end if;

      Setup_FCB (FD_Table (FD).FCB, Name);

      -- Delete existing file first
      null;  -- BDOS (Delete_File, ...);

      if BDOS (Make_File, Natural (To_Integer (FD_Table (FD).FCB'Address))) = 255 then
         FD_Table (FD).In_Use := False;
         return -1;
      end if;

      return FD;
   end Create;

   -----------
   -- Close --
   -----------

   procedure Close (FD : Integer) is
   begin
      if FD in FD_Table'Range and then FD_Table (FD).In_Use then
         null;  -- BDOS (Close_File, ...);
         FD_Table (FD).In_Use := False;
      end if;
   end Close;

   ----------
   -- Read --
   ----------

   function Read (FD : Integer; Buf : System.Address; Len : Integer) return Integer is
      Total : Integer := 0;
      Chunk : Integer;
   begin
      if FD not in FD_Table'Range or else not FD_Table (FD).In_Use then
         return -1;
      end if;

      -- Read in 128-byte chunks
      while Total < Len loop
         -- Set DMA to our buffer
         null;  -- BDOS (Set_DMA, DMA_Buffer'Address);

         if BDOS (Read_Sequential, Natural (To_Integer (FD_Table (FD).FCB'Address))) /= 0 then
            exit;  -- EOF or error
         end if;

         Chunk := Integer'Min (128, Len - Total);
         -- Copy from DMA buffer to user buffer
         Memcpy (Buf + Storage_Offset (Total), DMA_Buffer'Address, Natural (Chunk));
         Total := Total + Chunk;
      end loop;

      return Total;
   end Read;

   -----------
   -- Write --
   -----------

   function Write (FD : Integer; Buf : System.Address; Len : Integer) return Integer is
      Total : Integer := 0;
      Chunk : Integer;
   begin
      if FD not in FD_Table'Range or else not FD_Table (FD).In_Use then
         return -1;
      end if;

      while Total < Len loop
         Chunk := Integer'Min (128, Len - Total);

         -- Copy to DMA buffer
         Memcpy (DMA_Buffer'Address, Buf + Storage_Offset (Total), Natural (Chunk));

         -- Pad with Ctrl-Z if partial
         for I in Chunk + 1 .. 128 loop
            DMA_Buffer (I) := Character'Val (26);
         end loop;

         -- Set DMA
         null;  -- BDOS (Set_DMA, DMA_Buffer'Address);

         if BDOS (Write_Sequential, Natural (To_Integer (FD_Table (FD).FCB'Address))) /= 0 then
            return -1;
         end if;

         Total := Total + Chunk;
      end loop;

      return Total;
   end Write;

   ------------
   -- Unlink --
   ------------

   function Unlink (Name : String) return Integer is
      FCB : System.File_Control_Block.FCB;
   begin
      Setup_FCB (FCB, Name);
      if BDOS (Delete_File, Natural (To_Integer (FCB'Address))) = 255 then
         return -1;
      end if;
      return 0;
   end Unlink;

   ------------
   -- Rename --
   ------------

   function Rename (Old_Name, New_Name : String) return Integer is
      pragma Unreferenced (Old_Name, New_Name);
   begin
      -- Would need special FCB format for rename
      return -1;
   end Rename;

   -------------
   -- Getchar --
   -------------

   function Getchar return Integer is
   begin
      return Integer (BDOS (Console_Input));
   end Getchar;

   -------------
   -- Putchar --
   -------------

   procedure Putchar (C : Integer) is
      Dummy : Natural;
   begin
      Dummy := BDOS (Console_Output, C);
   end Putchar;

   -----------
   -- Kbhit --
   -----------

   function Kbhit return Boolean is
   begin
      return BDOS (Console_Status) /= 0;
   end Kbhit;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Status : Integer) is
      pragma Unreferenced (Status);
      Dummy : Natural;
   begin
      Dummy := BDOS (System_Reset);
      -- Never returns
      loop
         null;
      end loop;
   end Exit_Program;

   ------------
   -- Memcpy --
   ------------

   procedure Memcpy (Dest, Src : System.Address; N : Natural) is
      D : Storage_Array (1 .. Storage_Offset (N));
      for D'Address use Dest;
      S : Storage_Array (1 .. Storage_Offset (N));
      for S'Address use Src;
   begin
      for I in 1 .. Storage_Offset (N) loop
         D (I) := S (I);
      end loop;
   end Memcpy;

   ------------
   -- Memset --
   ------------

   procedure Memset (Dest : System.Address; C : Integer; N : Natural) is
      D : Storage_Array (1 .. Storage_Offset (N));
      for D'Address use Dest;
   begin
      for I in 1 .. Storage_Offset (N) loop
         D (I) := Storage_Element (C mod 256);
      end loop;
   end Memset;

   ------------
   -- Memcmp --
   ------------

   function Memcmp (S1, S2 : System.Address; N : Natural) return Integer is
      A1 : Storage_Array (1 .. Storage_Offset (N));
      for A1'Address use S1;
      A2 : Storage_Array (1 .. Storage_Offset (N));
      for A2'Address use S2;
   begin
      for I in 1 .. Storage_Offset (N) loop
         if A1 (I) < A2 (I) then
            return -1;
         elsif A1 (I) > A2 (I) then
            return 1;
         end if;
      end loop;
      return 0;
   end Memcmp;

   ------------
   -- Strlen --
   ------------

   function Strlen (S : System.Address) return Natural is
      Str : String (1 .. 256);
      for Str'Address use S;
   begin
      for I in Str'Range loop
         if Str (I) = Character'Val (0) then
            return I - 1;
         end if;
      end loop;
      return 256;
   end Strlen;

end System.CRTL;
