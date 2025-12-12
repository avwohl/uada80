-- System.Memory body for Z80
-- Low-level memory allocation implementation

package body System.Memory is

   use System.Storage_Elements;

   -- Simple heap for Z80
   -- Heap starts after program code and stack
   Heap_Start : constant Address := System'To_Address (16#8000#);  -- 32K boundary
   Heap_End   : constant Address := System'To_Address (16#FEFF#);  -- Top of RAM - 256

   type Heap_Block;
   type Block_Ptr is access all Heap_Block;

   type Heap_Block is record
      Size    : Storage_Count := 0;
      In_Use  : Boolean := False;
      Next    : Block_Ptr := null;
   end record;

   -- Block header size
   Header_Size : constant Storage_Count := Heap_Block'Size / Storage_Unit;

   -- Free list head
   Free_List : Block_Ptr := null;
   Heap_Initialized : Boolean := False;

   procedure Initialize_Heap is
   begin
      if not Heap_Initialized then
         -- Create initial free block spanning entire heap
         declare
            Initial_Block : Block_Ptr;
            for Initial_Block'Address use Heap_Start;
         begin
            Initial_Block.Size := Storage_Count (Heap_End - Heap_Start) - Header_Size;
            Initial_Block.In_Use := False;
            Initial_Block.Next := null;
            Free_List := Initial_Block;
         end;
         Heap_Initialized := True;
      end if;
   end Initialize_Heap;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : Storage_Count) return Address is
      Current  : Block_Ptr;
      Previous : Block_Ptr := null;
      Aligned_Size : Storage_Count;
   begin
      Initialize_Heap;

      -- Align to 2-byte boundary
      Aligned_Size := ((Size + 1) / 2) * 2;

      -- First-fit search
      Current := Free_List;
      while Current /= null loop
         if not Current.In_Use and Current.Size >= Aligned_Size then
            -- Found a suitable block
            Current.In_Use := True;

            -- Return address after header
            return Current.all'Address + Header_Size;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop;

      -- No suitable block found
      return Null_Address;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Addr : Address) is
      Block : Block_Ptr;
   begin
      if Addr = Null_Address then
         return;
      end if;

      -- Get block header (before the user data)
      declare
         Block_Addr : constant Address := Addr - Header_Size;
      begin
         for Block'Address use Block_Addr;
         pragma Unreferenced (Block);
      end;

      -- For simplicity, just mark as not in use
      -- A proper implementation would coalesce adjacent free blocks
      declare
         B : Block_Ptr;
         for B'Address use Addr - Header_Size;
      begin
         B.In_Use := False;
      end;
   end Free;

   ------------
   -- Realloc --
   ------------

   function Realloc
     (Addr : Address;
      Size : Storage_Count) return Address
   is
      New_Addr : Address;
      Old_Block : Block_Ptr;
      Copy_Size : Storage_Count;
   begin
      if Addr = Null_Address then
         return Alloc (Size);
      end if;

      if Size = 0 then
         Free (Addr);
         return Null_Address;
      end if;

      -- Get old block size
      declare
         B : Block_Ptr;
         for B'Address use Addr - Header_Size;
      begin
         Old_Block := B;
      end;

      -- Allocate new block
      New_Addr := Alloc (Size);
      if New_Addr = Null_Address then
         return Null_Address;
      end if;

      -- Copy data
      Copy_Size := Storage_Count'Min (Size, Old_Block.Size);
      declare
         Src : Storage_Array (1 .. Copy_Size);
         for Src'Address use Addr;
         Dst : Storage_Array (1 .. Copy_Size);
         for Dst'Address use New_Addr;
      begin
         Dst := Src;
      end;

      -- Free old block
      Free (Addr);

      return New_Addr;
   end Realloc;

   ---------------
   -- Available --
   ---------------

   function Available return Storage_Count is
      Total   : Storage_Count := 0;
      Current : Block_Ptr;
   begin
      Initialize_Heap;

      Current := Free_List;
      while Current /= null loop
         if not Current.In_Use then
            Total := Total + Current.Size;
         end if;
         Current := Current.Next;
      end loop;

      return Total;
   end Available;

end System.Memory;
