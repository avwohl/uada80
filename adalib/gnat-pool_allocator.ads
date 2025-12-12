-- GNAT.Pool_Allocator for Z80
-- Fixed-size memory pool allocator

package GNAT.Pool_Allocator is
   pragma Preelaborate;

   -- Small block pool (8 bytes each)
   Max_Small_Blocks : constant := 32;
   Small_Block_Size : constant := 8;

   type Small_Pool is limited private;

   procedure Init_Small (P : out Small_Pool);
   function Alloc_Small (P : in Out Small_Pool) return Natural;
   procedure Free_Small (P : in Out Small_Pool; Block : Natural);
   function Small_Available (P : Small_Pool) return Natural;
   function Small_Used (P : Small_Pool) return Natural;
   function Small_Is_Valid (P : Small_Pool; Block : Natural) return Boolean;
   procedure Small_Write_Byte (P : in Out Small_Pool;
                               Block : Natural; Offset : Natural; Value : Natural);
   function Small_Read_Byte (P : Small_Pool;
                             Block : Natural; Offset : Natural) return Natural;

   -- Medium block pool (32 bytes each)
   Max_Medium_Blocks : constant := 16;
   Medium_Block_Size : constant := 32;

   type Medium_Pool is limited private;

   procedure Init_Medium (P : out Medium_Pool);
   function Alloc_Medium (P : in Out Medium_Pool) return Natural;
   procedure Free_Medium (P : in Out Medium_Pool; Block : Natural);
   function Medium_Available (P : Medium_Pool) return Natural;
   function Medium_Used (P : Medium_Pool) return Natural;
   function Medium_Is_Valid (P : Medium_Pool; Block : Natural) return Boolean;
   procedure Medium_Write_Byte (P : in Out Medium_Pool;
                                Block : Natural; Offset : Natural; Value : Natural);
   function Medium_Read_Byte (P : Medium_Pool;
                              Block : Natural; Offset : Natural) return Natural;

   -- Large block pool (128 bytes each)
   Max_Large_Blocks : constant := 8;
   Large_Block_Size : constant := 128;

   type Large_Pool is limited private;

   procedure Init_Large (P : out Large_Pool);
   function Alloc_Large (P : in Out Large_Pool) return Natural;
   procedure Free_Large (P : in Out Large_Pool; Block : Natural);
   function Large_Available (P : Large_Pool) return Natural;
   function Large_Used (P : Large_Pool) return Natural;
   function Large_Is_Valid (P : Large_Pool; Block : Natural) return Boolean;
   procedure Large_Write_Byte (P : in Out Large_Pool;
                               Block : Natural; Offset : Natural; Value : Natural);
   function Large_Read_Byte (P : Large_Pool;
                             Block : Natural; Offset : Natural) return Natural;

   -- Generic pool utilities
   procedure Reset_Small (P : out Small_Pool);
   procedure Reset_Medium (P : out Medium_Pool);
   procedure Reset_Large (P : out Large_Pool);

   -- Statistics
   function Total_Small_Memory return Natural;
   function Total_Medium_Memory return Natural;
   function Total_Large_Memory return Natural;

   -- Block copying within pools
   procedure Copy_Small_Block (P : in Out Small_Pool; From, To : Natural);
   procedure Copy_Medium_Block (P : in Out Medium_Pool; From, To : Natural);
   procedure Copy_Large_Block (P : in Out Large_Pool; From, To : Natural);

   -- Zero a block
   procedure Zero_Small_Block (P : in Out Small_Pool; Block : Natural);
   procedure Zero_Medium_Block (P : in Out Medium_Pool; Block : Natural);
   procedure Zero_Large_Block (P : in Out Large_Pool; Block : Natural);

   -- Fill a block with value
   procedure Fill_Small_Block (P : in Out Small_Pool;
                               Block : Natural; Value : Natural);
   procedure Fill_Medium_Block (P : in Out Medium_Pool;
                                Block : Natural; Value : Natural);
   procedure Fill_Large_Block (P : in Out Large_Pool;
                               Block : Natural; Value : Natural);

private

   subtype Byte is Natural range 0 .. 255;

   type Small_Data is array (1 .. Small_Block_Size) of Byte;
   type Small_Block_Record is record
      Data : Small_Data;
      Used : Boolean;
   end record;
   type Small_Block_Array is array (1 .. Max_Small_Blocks) of Small_Block_Record;

   type Small_Pool is limited record
      Blocks : Small_Block_Array;
      Count  : Natural := 0;
   end record;

   type Medium_Data is array (1 .. Medium_Block_Size) of Byte;
   type Medium_Block_Record is record
      Data : Medium_Data;
      Used : Boolean;
   end record;
   type Medium_Block_Array is array (1 .. Max_Medium_Blocks) of Medium_Block_Record;

   type Medium_Pool is limited record
      Blocks : Medium_Block_Array;
      Count  : Natural := 0;
   end record;

   type Large_Data is array (1 .. Large_Block_Size) of Byte;
   type Large_Block_Record is record
      Data : Large_Data;
      Used : Boolean;
   end record;
   type Large_Block_Array is array (1 .. Max_Large_Blocks) of Large_Block_Record;

   type Large_Pool is limited record
      Blocks : Large_Block_Array;
      Count  : Natural := 0;
   end record;

end GNAT.Pool_Allocator;
