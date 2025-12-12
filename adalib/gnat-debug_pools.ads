-- GNAT.Debug_Pools for Z80
-- Debug memory pool for detecting memory problems

with System.Storage_Pools;
with System.Storage_Elements;

package GNAT.Debug_Pools is

   type Debug_Pool is new System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool                     : in Out Debug_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   function Storage_Size
     (Pool : Debug_Pool) return System.Storage_Elements.Storage_Count;

   -- Configuration
   procedure Configure
     (Pool                           : in out Debug_Pool;
      Stack_Trace_Depth              : Natural := 10;
      Maximum_Logically_Freed_Memory : System.Storage_Elements.Storage_Count := 0;
      Minimum_To_Free                : System.Storage_Elements.Storage_Count := 0;
      Reset_Content_On_Free          : Boolean := False;
      Raise_Exceptions               : Boolean := True;
      Advanced_Scanning              : Boolean := False;
      Errors_To_Stdout               : Boolean := True;
      Low_Level_Traces               : Boolean := False);

   -- Statistics
   type Byte_Count is new System.Storage_Elements.Storage_Count;

   procedure Print_Info
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False);

   procedure Print_Info_Stdout
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False);

   function High_Water_Mark
     (Pool : Debug_Pool) return Byte_Count;

   function Current_Water_Mark
     (Pool : Debug_Pool) return Byte_Count;

   -- Reset statistics
   procedure Reset (Pool : in Out Debug_Pool);

   -- Exceptions
   Accessing_Not_Allocated_Storage : exception;
   Accessing_Deallocated_Storage   : exception;
   Freeing_Not_Allocated_Storage   : exception;
   Freeing_Deallocated_Storage     : exception;

private
   type Debug_Pool is new System.Storage_Pools.Root_Storage_Pool with record
      Stack_Trace_Depth    : Natural := 10;
      Max_Freed            : System.Storage_Elements.Storage_Count := 0;
      Min_To_Free          : System.Storage_Elements.Storage_Count := 0;
      Reset_On_Free        : Boolean := False;
      Raise_Except         : Boolean := True;
      Advanced_Scan        : Boolean := False;
      To_Stdout            : Boolean := True;
      Low_Traces           : Boolean := False;
      Allocated            : Byte_Count := 0;
      High_Water           : Byte_Count := 0;
      Current_Water        : Byte_Count := 0;
   end record;

end GNAT.Debug_Pools;
