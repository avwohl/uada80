-- GNAT.Memory_Pool body for Z80
-- Simple fixed-size memory pool allocator

package body GNAT.Memory_Pool is

   -- Small pool
   type Small_Block is array (1 .. Small_Block_Size) of Natural range 0 .. 255;
   type Small_Block_Array is array (1 .. Small_Pool_Blocks) of Small_Block;
   type Small_Free_Array is array (1 .. Small_Pool_Blocks) of Boolean;

   Small_Blocks : Small_Block_Array;
   Small_Free   : Small_Free_Array := (others => True);
   Small_Stats  : Pool_Stats := (Small_Pool_Blocks, Small_Pool_Blocks, 0, 0, 0);

   -- Medium pool
   type Medium_Block is array (1 .. Medium_Block_Size) of Natural range 0 .. 255;
   type Medium_Block_Array is array (1 .. Medium_Pool_Blocks) of Medium_Block;
   type Medium_Free_Array is array (1 .. Medium_Pool_Blocks) of Boolean;

   Medium_Blocks : Medium_Block_Array;
   Medium_Free   : Medium_Free_Array := (others => True);
   Medium_Stats  : Pool_Stats := (Medium_Pool_Blocks, Medium_Pool_Blocks, 0, 0, 0);

   -- Large pool
   type Large_Block is array (1 .. Large_Block_Size) of Natural range 0 .. 255;
   type Large_Block_Array is array (1 .. Large_Pool_Blocks) of Large_Block;
   type Large_Free_Array is array (1 .. Large_Pool_Blocks) of Boolean;

   Large_Blocks : Large_Block_Array;
   Large_Free   : Large_Free_Array := (others => True);
   Large_Stats  : Pool_Stats := (Large_Pool_Blocks, Large_Pool_Blocks, 0, 0, 0);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Small_Free := (others => True);
      Small_Stats := (Small_Pool_Blocks, Small_Pool_Blocks, 0, 0, 0);
      for I in Small_Blocks'Range loop
         Small_Blocks (I) := (others => 0);
      end loop;

      Medium_Free := (others => True);
      Medium_Stats := (Medium_Pool_Blocks, Medium_Pool_Blocks, 0, 0, 0);
      for I in Medium_Blocks'Range loop
         Medium_Blocks (I) := (others => 0);
      end loop;

      Large_Free := (others => True);
      Large_Stats := (Large_Pool_Blocks, Large_Pool_Blocks, 0, 0, 0);
      for I in Large_Blocks'Range loop
         Large_Blocks (I) := (others => 0);
      end loop;
   end Initialize;

   --------------
   -- Allocate --
   --------------

   function Allocate (Pool : Pool_Type) return Block_Address is
   begin
      case Pool is
         when Small =>
            for I in Small_Free'Range loop
               if Small_Free (I) then
                  Small_Free (I) := False;
                  Small_Stats.Free_Blocks := Small_Stats.Free_Blocks - 1;
                  Small_Stats.Allocations := Small_Stats.Allocations + 1;
                  return Block_Address (I);
               end if;
            end loop;
            Small_Stats.Allocation_Fails := Small_Stats.Allocation_Fails + 1;

         when Medium =>
            for I in Medium_Free'Range loop
               if Medium_Free (I) then
                  Medium_Free (I) := False;
                  Medium_Stats.Free_Blocks := Medium_Stats.Free_Blocks - 1;
                  Medium_Stats.Allocations := Medium_Stats.Allocations + 1;
                  return Block_Address (I);
               end if;
            end loop;
            Medium_Stats.Allocation_Fails := Medium_Stats.Allocation_Fails + 1;

         when Large =>
            for I in Large_Free'Range loop
               if Large_Free (I) then
                  Large_Free (I) := False;
                  Large_Stats.Free_Blocks := Large_Stats.Free_Blocks - 1;
                  Large_Stats.Allocations := Large_Stats.Allocations + 1;
                  return Block_Address (I);
               end if;
            end loop;
            Large_Stats.Allocation_Fails := Large_Stats.Allocation_Fails + 1;
      end case;
      return Null_Block;
   end Allocate;

   ----------
   -- Free --
   ----------

   procedure Free (Pool : Pool_Type; Addr : Block_Address) is
      Idx : constant Positive := Positive (Addr);
   begin
      if Addr = Null_Block then
         return;
      end if;

      case Pool is
         when Small =>
            if Idx in Small_Free'Range and then not Small_Free (Idx) then
               Small_Free (Idx) := True;
               Small_Stats.Free_Blocks := Small_Stats.Free_Blocks + 1;
               Small_Stats.Deallocations := Small_Stats.Deallocations + 1;
            end if;

         when Medium =>
            if Idx in Medium_Free'Range and then not Medium_Free (Idx) then
               Medium_Free (Idx) := True;
               Medium_Stats.Free_Blocks := Medium_Stats.Free_Blocks + 1;
               Medium_Stats.Deallocations := Medium_Stats.Deallocations + 1;
            end if;

         when Large =>
            if Idx in Large_Free'Range and then not Large_Free (Idx) then
               Large_Free (Idx) := True;
               Large_Stats.Free_Blocks := Large_Stats.Free_Blocks + 1;
               Large_Stats.Deallocations := Large_Stats.Deallocations + 1;
            end if;
      end case;
   end Free;

   ---------------
   -- Available --
   ---------------

   function Available (Pool : Pool_Type) return Natural is
   begin
      case Pool is
         when Small  => return Small_Stats.Free_Blocks;
         when Medium => return Medium_Stats.Free_Blocks;
         when Large  => return Large_Stats.Free_Blocks;
      end case;
   end Available;

   -----------
   -- Total --
   -----------

   function Total (Pool : Pool_Type) return Natural is
   begin
      case Pool is
         when Small  => return Small_Pool_Blocks;
         when Medium => return Medium_Pool_Blocks;
         when Large  => return Large_Pool_Blocks;
      end case;
   end Total;

   ----------
   -- Used --
   ----------

   function Used (Pool : Pool_Type) return Natural is
   begin
      return Total (Pool) - Available (Pool);
   end Used;

   ----------------
   -- Block_Size --
   ----------------

   function Block_Size (Pool : Pool_Type) return Natural is
   begin
      case Pool is
         when Small  => return Small_Block_Size;
         when Medium => return Medium_Block_Size;
         when Large  => return Large_Block_Size;
      end case;
   end Block_Size;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte (Pool : Pool_Type; Addr : Block_Address;
                         Offset : Natural; Value : Natural)
   is
      Idx : constant Positive := Positive (Addr);
   begin
      if Addr = Null_Block then
         return;
      end if;

      case Pool is
         when Small =>
            if Idx in Small_Blocks'Range and Offset < Small_Block_Size then
               Small_Blocks (Idx) (Offset + 1) := Value mod 256;
            end if;
         when Medium =>
            if Idx in Medium_Blocks'Range and Offset < Medium_Block_Size then
               Medium_Blocks (Idx) (Offset + 1) := Value mod 256;
            end if;
         when Large =>
            if Idx in Large_Blocks'Range and Offset < Large_Block_Size then
               Large_Blocks (Idx) (Offset + 1) := Value mod 256;
            end if;
      end case;
   end Write_Byte;

   ---------------
   -- Read_Byte --
   ---------------

   function Read_Byte (Pool : Pool_Type; Addr : Block_Address;
                       Offset : Natural) return Natural
   is
      Idx : constant Positive := Positive (Addr);
   begin
      if Addr = Null_Block then
         return 0;
      end if;

      case Pool is
         when Small =>
            if Idx in Small_Blocks'Range and Offset < Small_Block_Size then
               return Small_Blocks (Idx) (Offset + 1);
            end if;
         when Medium =>
            if Idx in Medium_Blocks'Range and Offset < Medium_Block_Size then
               return Medium_Blocks (Idx) (Offset + 1);
            end if;
         when Large =>
            if Idx in Large_Blocks'Range and Offset < Large_Block_Size then
               return Large_Blocks (Idx) (Offset + 1);
            end if;
      end case;
      return 0;
   end Read_Byte;

   ----------------
   -- Write_Word --
   ----------------

   procedure Write_Word (Pool : Pool_Type; Addr : Block_Address;
                         Offset : Natural; Value : Natural)
   is
   begin
      Write_Byte (Pool, Addr, Offset, Value mod 256);
      Write_Byte (Pool, Addr, Offset + 1, Value / 256);
   end Write_Word;

   ---------------
   -- Read_Word --
   ---------------

   function Read_Word (Pool : Pool_Type; Addr : Block_Address;
                       Offset : Natural) return Natural
   is
   begin
      return Read_Byte (Pool, Addr, Offset) +
             Read_Byte (Pool, Addr, Offset + 1) * 256;
   end Read_Word;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Pool : Pool_Type; Addr : Block_Address;
                           Offset : Natural; S : String)
   is
      Max_Len : Natural;
   begin
      Max_Len := Block_Size (Pool) - Offset;
      for I in 1 .. Natural'Min (S'Length, Max_Len) loop
         Write_Byte (Pool, Addr, Offset + I - 1,
                     Character'Pos (S (S'First + I - 1)));
      end loop;
   end Write_String;

   -----------------
   -- Read_String --
   -----------------

   procedure Read_String (Pool : Pool_Type; Addr : Block_Address;
                          Offset : Natural; S : out String; Last : out Natural)
   is
      Max_Len : Natural;
      B       : Natural;
   begin
      Last := S'First - 1;
      Max_Len := Block_Size (Pool) - Offset;

      for I in 1 .. Natural'Min (S'Length, Max_Len) loop
         B := Read_Byte (Pool, Addr, Offset + I - 1);
         exit when B = 0;  -- NUL terminator
         Last := S'First + I - 1;
         S (Last) := Character'Val (B);
      end loop;
   end Read_String;

   ---------------
   -- Get_Stats --
   ---------------

   function Get_Stats (Pool : Pool_Type) return Pool_Stats is
   begin
      case Pool is
         when Small  => return Small_Stats;
         when Medium => return Medium_Stats;
         when Large  => return Large_Stats;
      end case;
   end Get_Stats;

   -----------------
   -- Reset_Stats --
   -----------------

   procedure Reset_Stats is
   begin
      Small_Stats.Allocations := 0;
      Small_Stats.Deallocations := 0;
      Small_Stats.Allocation_Fails := 0;

      Medium_Stats.Allocations := 0;
      Medium_Stats.Deallocations := 0;
      Medium_Stats.Allocation_Fails := 0;

      Large_Stats.Allocations := 0;
      Large_Stats.Deallocations := 0;
      Large_Stats.Allocation_Fails := 0;
   end Reset_Stats;

end GNAT.Memory_Pool;
