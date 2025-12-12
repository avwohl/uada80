-- GNAT.Debug_Pools body for Z80
-- Debug memory pool implementation

with System.Memory;

package body GNAT.Debug_Pools is

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool                     : in Out Debug_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
   begin
      Storage_Address := System.Memory.Alloc (System.Memory.Size_T (Size_In_Storage_Elements));

      Pool.Current_Water := Pool.Current_Water + Byte_Count (Size_In_Storage_Elements);
      if Pool.Current_Water > Pool.High_Water then
         Pool.High_Water := Pool.Current_Water;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool                     : in Out Debug_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
   begin
      if Pool.Reset_On_Free then
         -- Fill with pattern (0xDEADBEEF style, but 8-bit)
         declare
            use System.Storage_Elements;
            Mem : Storage_Array (1 .. Size_In_Storage_Elements);
            for Mem'Address use Storage_Address;
         begin
            Mem := (others => 16#DE#);
         end;
      end if;

      System.Memory.Free (Storage_Address);
      Pool.Current_Water := Pool.Current_Water - Byte_Count (Size_In_Storage_Elements);
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size
     (Pool : Debug_Pool) return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return System.Storage_Elements.Storage_Count'Last;
   end Storage_Size;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Pool                           : in Out Debug_Pool;
      Stack_Trace_Depth              : Natural := 10;
      Maximum_Logically_Freed_Memory : System.Storage_Elements.Storage_Count := 0;
      Minimum_To_Free                : System.Storage_Elements.Storage_Count := 0;
      Reset_Content_On_Free          : Boolean := False;
      Raise_Exceptions               : Boolean := True;
      Advanced_Scanning              : Boolean := False;
      Errors_To_Stdout               : Boolean := True;
      Low_Level_Traces               : Boolean := False)
   is
   begin
      Pool.Stack_Trace_Depth := Stack_Trace_Depth;
      Pool.Max_Freed := Maximum_Logically_Freed_Memory;
      Pool.Min_To_Free := Minimum_To_Free;
      Pool.Reset_On_Free := Reset_Content_On_Free;
      Pool.Raise_Except := Raise_Exceptions;
      Pool.Advanced_Scan := Advanced_Scanning;
      Pool.To_Stdout := Errors_To_Stdout;
      Pool.Low_Traces := Low_Level_Traces;
   end Configure;

   ----------------
   -- Print_Info --
   ----------------

   procedure Print_Info
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False)
   is
      pragma Unreferenced (Pool, Cumulate, Display_Slots, Display_Leaks);
   begin
      -- Simplified for Z80: no printing in basic implementation
      null;
   end Print_Info;

   -----------------------
   -- Print_Info_Stdout --
   -----------------------

   procedure Print_Info_Stdout
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False)
   is
   begin
      Print_Info (Pool, Cumulate, Display_Slots, Display_Leaks);
   end Print_Info_Stdout;

   ---------------------
   -- High_Water_Mark --
   ---------------------

   function High_Water_Mark
     (Pool : Debug_Pool) return Byte_Count
   is
   begin
      return Pool.High_Water;
   end High_Water_Mark;

   ------------------------
   -- Current_Water_Mark --
   ------------------------

   function Current_Water_Mark
     (Pool : Debug_Pool) return Byte_Count
   is
   begin
      return Pool.Current_Water;
   end Current_Water_Mark;

   -----------
   -- Reset --
   -----------

   procedure Reset (Pool : in Out Debug_Pool) is
   begin
      Pool.High_Water := Pool.Current_Water;
   end Reset;

end GNAT.Debug_Pools;
