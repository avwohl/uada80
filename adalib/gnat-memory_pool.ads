-- GNAT.Memory_Pool for Z80
-- Simple fixed-size memory pool allocator

package GNAT.Memory_Pool is
   pragma Preelaborate;

   -- Small block pool (8 bytes each)
   Small_Block_Size : constant := 8;
   Small_Pool_Blocks : constant := 32;

   -- Medium block pool (32 bytes each)
   Medium_Block_Size : constant := 32;
   Medium_Pool_Blocks : constant := 16;

   -- Large block pool (128 bytes each)
   Large_Block_Size : constant := 128;
   Large_Pool_Blocks : constant := 4;

   type Pool_Type is (Small, Medium, Large);

   type Block_Address is new Natural;
   Null_Block : constant Block_Address := 0;

   procedure Initialize;
   --  Initialize all memory pools

   function Allocate (Pool : Pool_Type) return Block_Address;
   --  Allocate block from specified pool

   procedure Free (Pool : Pool_Type; Addr : Block_Address);
   --  Free block back to pool

   function Available (Pool : Pool_Type) return Natural;
   --  Return number of available blocks

   function Total (Pool : Pool_Type) return Natural;
   --  Return total blocks in pool

   function Used (Pool : Pool_Type) return Natural;
   --  Return number of used blocks

   function Block_Size (Pool : Pool_Type) return Natural;
   --  Return size of blocks in pool

   -- Read/Write block data
   procedure Write_Byte (Pool : Pool_Type; Addr : Block_Address;
                         Offset : Natural; Value : Natural);
   function Read_Byte (Pool : Pool_Type; Addr : Block_Address;
                       Offset : Natural) return Natural;

   procedure Write_Word (Pool : Pool_Type; Addr : Block_Address;
                         Offset : Natural; Value : Natural);
   function Read_Word (Pool : Pool_Type; Addr : Block_Address;
                       Offset : Natural) return Natural;

   procedure Write_String (Pool : Pool_Type; Addr : Block_Address;
                           Offset : Natural; S : String);
   procedure Read_String (Pool : Pool_Type; Addr : Block_Address;
                          Offset : Natural; S : out String; Last : out Natural);

   -- Statistics
   type Pool_Stats is record
      Total_Blocks      : Natural;
      Free_Blocks       : Natural;
      Allocations       : Natural;
      Deallocations     : Natural;
      Allocation_Fails  : Natural;
   end record;

   function Get_Stats (Pool : Pool_Type) return Pool_Stats;
   procedure Reset_Stats;

end GNAT.Memory_Pool;
