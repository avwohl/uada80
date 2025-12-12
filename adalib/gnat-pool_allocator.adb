-- GNAT.Pool_Allocator body for Z80
-- Fixed-size memory pool implementation

package body GNAT.Pool_Allocator is

   ----------------
   -- Small Pool --
   ----------------

   procedure Init_Small (P : out Small_Pool) is
   begin
      P.Count := 0;
      for I in 1 .. Max_Small_Blocks loop
         P.Blocks (I).Used := False;
         P.Blocks (I).Data := (others => 0);
      end loop;
   end Init_Small;

   function Alloc_Small (P : in Out Small_Pool) return Natural is
   begin
      for I in 1 .. Max_Small_Blocks loop
         if not P.Blocks (I).Used then
            P.Blocks (I).Used := True;
            P.Count := P.Count + 1;
            return I;
         end if;
      end loop;
      return 0;  -- No free blocks
   end Alloc_Small;

   procedure Free_Small (P : in Out Small_Pool; Block : Natural) is
   begin
      if Block in 1 .. Max_Small_Blocks then
         if P.Blocks (Block).Used then
            P.Blocks (Block).Used := False;
            P.Count := P.Count - 1;
         end if;
      end if;
   end Free_Small;

   function Small_Available (P : Small_Pool) return Natural is
   begin
      return Max_Small_Blocks - P.Count;
   end Small_Available;

   function Small_Used (P : Small_Pool) return Natural is
   begin
      return P.Count;
   end Small_Used;

   function Small_Is_Valid (P : Small_Pool; Block : Natural) return Boolean is
   begin
      if Block not in 1 .. Max_Small_Blocks then
         return False;
      end if;
      return P.Blocks (Block).Used;
   end Small_Is_Valid;

   procedure Small_Write_Byte (P : in Out Small_Pool;
                               Block : Natural; Offset : Natural; Value : Natural) is
   begin
      if Block in 1 .. Max_Small_Blocks and then
         Offset < Small_Block_Size and then
         P.Blocks (Block).Used then
         P.Blocks (Block).Data (Offset + 1) := Value mod 256;
      end if;
   end Small_Write_Byte;

   function Small_Read_Byte (P : Small_Pool;
                             Block : Natural; Offset : Natural) return Natural is
   begin
      if Block in 1 .. Max_Small_Blocks and then
         Offset < Small_Block_Size and then
         P.Blocks (Block).Used then
         return P.Blocks (Block).Data (Offset + 1);
      end if;
      return 0;
   end Small_Read_Byte;

   procedure Reset_Small (P : out Small_Pool) is
   begin
      Init_Small (P);
   end Reset_Small;

   function Total_Small_Memory return Natural is
   begin
      return Max_Small_Blocks * Small_Block_Size;
   end Total_Small_Memory;

   procedure Copy_Small_Block (P : in Out Small_Pool; From, To : Natural) is
   begin
      if From in 1 .. Max_Small_Blocks and then
         To in 1 .. Max_Small_Blocks and then
         P.Blocks (From).Used and then
         P.Blocks (To).Used then
         P.Blocks (To).Data := P.Blocks (From).Data;
      end if;
   end Copy_Small_Block;

   procedure Zero_Small_Block (P : in Out Small_Pool; Block : Natural) is
   begin
      if Block in 1 .. Max_Small_Blocks and then P.Blocks (Block).Used then
         P.Blocks (Block).Data := (others => 0);
      end if;
   end Zero_Small_Block;

   procedure Fill_Small_Block (P : in Out Small_Pool;
                               Block : Natural; Value : Natural) is
   begin
      if Block in 1 .. Max_Small_Blocks and then P.Blocks (Block).Used then
         P.Blocks (Block).Data := (others => Value mod 256);
      end if;
   end Fill_Small_Block;

   -----------------
   -- Medium Pool --
   -----------------

   procedure Init_Medium (P : out Medium_Pool) is
   begin
      P.Count := 0;
      for I in 1 .. Max_Medium_Blocks loop
         P.Blocks (I).Used := False;
         P.Blocks (I).Data := (others => 0);
      end loop;
   end Init_Medium;

   function Alloc_Medium (P : in Out Medium_Pool) return Natural is
   begin
      for I in 1 .. Max_Medium_Blocks loop
         if not P.Blocks (I).Used then
            P.Blocks (I).Used := True;
            P.Count := P.Count + 1;
            return I;
         end if;
      end loop;
      return 0;
   end Alloc_Medium;

   procedure Free_Medium (P : in Out Medium_Pool; Block : Natural) is
   begin
      if Block in 1 .. Max_Medium_Blocks then
         if P.Blocks (Block).Used then
            P.Blocks (Block).Used := False;
            P.Count := P.Count - 1;
         end if;
      end if;
   end Free_Medium;

   function Medium_Available (P : Medium_Pool) return Natural is
   begin
      return Max_Medium_Blocks - P.Count;
   end Medium_Available;

   function Medium_Used (P : Medium_Pool) return Natural is
   begin
      return P.Count;
   end Medium_Used;

   function Medium_Is_Valid (P : Medium_Pool; Block : Natural) return Boolean is
   begin
      if Block not in 1 .. Max_Medium_Blocks then
         return False;
      end if;
      return P.Blocks (Block).Used;
   end Medium_Is_Valid;

   procedure Medium_Write_Byte (P : in Out Medium_Pool;
                                Block : Natural; Offset : Natural; Value : Natural) is
   begin
      if Block in 1 .. Max_Medium_Blocks and then
         Offset < Medium_Block_Size and then
         P.Blocks (Block).Used then
         P.Blocks (Block).Data (Offset + 1) := Value mod 256;
      end if;
   end Medium_Write_Byte;

   function Medium_Read_Byte (P : Medium_Pool;
                              Block : Natural; Offset : Natural) return Natural is
   begin
      if Block in 1 .. Max_Medium_Blocks and then
         Offset < Medium_Block_Size and then
         P.Blocks (Block).Used then
         return P.Blocks (Block).Data (Offset + 1);
      end if;
      return 0;
   end Medium_Read_Byte;

   procedure Reset_Medium (P : out Medium_Pool) is
   begin
      Init_Medium (P);
   end Reset_Medium;

   function Total_Medium_Memory return Natural is
   begin
      return Max_Medium_Blocks * Medium_Block_Size;
   end Total_Medium_Memory;

   procedure Copy_Medium_Block (P : in Out Medium_Pool; From, To : Natural) is
   begin
      if From in 1 .. Max_Medium_Blocks and then
         To in 1 .. Max_Medium_Blocks and then
         P.Blocks (From).Used and then
         P.Blocks (To).Used then
         P.Blocks (To).Data := P.Blocks (From).Data;
      end if;
   end Copy_Medium_Block;

   procedure Zero_Medium_Block (P : in Out Medium_Pool; Block : Natural) is
   begin
      if Block in 1 .. Max_Medium_Blocks and then P.Blocks (Block).Used then
         P.Blocks (Block).Data := (others => 0);
      end if;
   end Zero_Medium_Block;

   procedure Fill_Medium_Block (P : in Out Medium_Pool;
                                Block : Natural; Value : Natural) is
   begin
      if Block in 1 .. Max_Medium_Blocks and then P.Blocks (Block).Used then
         P.Blocks (Block).Data := (others => Value mod 256);
      end if;
   end Fill_Medium_Block;

   ----------------
   -- Large Pool --
   ----------------

   procedure Init_Large (P : out Large_Pool) is
   begin
      P.Count := 0;
      for I in 1 .. Max_Large_Blocks loop
         P.Blocks (I).Used := False;
         P.Blocks (I).Data := (others => 0);
      end loop;
   end Init_Large;

   function Alloc_Large (P : in Out Large_Pool) return Natural is
   begin
      for I in 1 .. Max_Large_Blocks loop
         if not P.Blocks (I).Used then
            P.Blocks (I).Used := True;
            P.Count := P.Count + 1;
            return I;
         end if;
      end loop;
      return 0;
   end Alloc_Large;

   procedure Free_Large (P : in Out Large_Pool; Block : Natural) is
   begin
      if Block in 1 .. Max_Large_Blocks then
         if P.Blocks (Block).Used then
            P.Blocks (Block).Used := False;
            P.Count := P.Count - 1;
         end if;
      end if;
   end Free_Large;

   function Large_Available (P : Large_Pool) return Natural is
   begin
      return Max_Large_Blocks - P.Count;
   end Large_Available;

   function Large_Used (P : Large_Pool) return Natural is
   begin
      return P.Count;
   end Large_Used;

   function Large_Is_Valid (P : Large_Pool; Block : Natural) return Boolean is
   begin
      if Block not in 1 .. Max_Large_Blocks then
         return False;
      end if;
      return P.Blocks (Block).Used;
   end Large_Is_Valid;

   procedure Large_Write_Byte (P : in Out Large_Pool;
                               Block : Natural; Offset : Natural; Value : Natural) is
   begin
      if Block in 1 .. Max_Large_Blocks and then
         Offset < Large_Block_Size and then
         P.Blocks (Block).Used then
         P.Blocks (Block).Data (Offset + 1) := Value mod 256;
      end if;
   end Large_Write_Byte;

   function Large_Read_Byte (P : Large_Pool;
                             Block : Natural; Offset : Natural) return Natural is
   begin
      if Block in 1 .. Max_Large_Blocks and then
         Offset < Large_Block_Size and then
         P.Blocks (Block).Used then
         return P.Blocks (Block).Data (Offset + 1);
      end if;
      return 0;
   end Large_Read_Byte;

   procedure Reset_Large (P : out Large_Pool) is
   begin
      Init_Large (P);
   end Reset_Large;

   function Total_Large_Memory return Natural is
   begin
      return Max_Large_Blocks * Large_Block_Size;
   end Total_Large_Memory;

   procedure Copy_Large_Block (P : in Out Large_Pool; From, To : Natural) is
   begin
      if From in 1 .. Max_Large_Blocks and then
         To in 1 .. Max_Large_Blocks and then
         P.Blocks (From).Used and then
         P.Blocks (To).Used then
         P.Blocks (To).Data := P.Blocks (From).Data;
      end if;
   end Copy_Large_Block;

   procedure Zero_Large_Block (P : in Out Large_Pool; Block : Natural) is
   begin
      if Block in 1 .. Max_Large_Blocks and then P.Blocks (Block).Used then
         P.Blocks (Block).Data := (others => 0);
      end if;
   end Zero_Large_Block;

   procedure Fill_Large_Block (P : in Out Large_Pool;
                               Block : Natural; Value : Natural) is
   begin
      if Block in 1 .. Max_Large_Blocks and then P.Blocks (Block).Used then
         P.Blocks (Block).Data := (others => Value mod 256);
      end if;
   end Fill_Large_Block;

end GNAT.Pool_Allocator;
