-- GNAT.Object_Pool for Z80
-- Reusable object pool for reducing allocations

package GNAT.Object_Pool is
   pragma Preelaborate;

   Max_Objects : constant := 32;  -- Maximum pooled objects for Z80
   Max_Object_Size : constant := 64;  -- Maximum object size in bytes

   type Pool is private;

   type Object_Handle is private;
   Null_Handle : constant Object_Handle;

   procedure Initialize (P : out Pool; Object_Size : Positive);
   --  Initialize pool with objects of given size

   function Acquire (P : in Out Pool) return Object_Handle;
   --  Get object from pool (returns Null_Handle if pool empty)

   procedure Release (P : in Out Pool; H : in Out Object_Handle);
   --  Return object to pool

   function Is_Valid (H : Object_Handle) return Boolean;
   --  Check if handle is valid

   function Available (P : Pool) return Natural;
   --  Number of available objects

   function In_Use (P : Pool) return Natural;
   --  Number of objects currently in use

   function Object_Size (P : Pool) return Positive;
   --  Size of objects in this pool

   procedure Clear_Object (P : in Out Pool; H : Object_Handle);
   --  Zero out object data

   -- Read/Write object data
   procedure Write_Byte (P : in Out Pool; H : Object_Handle;
                         Offset : Natural; Value : Natural);
   function Read_Byte (P : Pool; H : Object_Handle;
                       Offset : Natural) return Natural;

   procedure Write_Word (P : in Out Pool; H : Object_Handle;
                         Offset : Natural; Value : Natural);
   function Read_Word (P : Pool; H : Object_Handle;
                       Offset : Natural) return Natural;

private

   type Object_Handle is new Natural range 0 .. Max_Objects;
   Null_Handle : constant Object_Handle := 0;

   type Object_Data is array (1 .. Max_Object_Size) of Natural range 0 .. 255;
   type Object_Array is array (1 .. Max_Objects) of Object_Data;
   type In_Use_Array is array (1 .. Max_Objects) of Boolean;

   type Pool is record
      Objects    : Object_Array;
      In_Use     : In_Use_Array := (others => False);
      Obj_Size   : Positive := 1;
      Count      : Natural := 0;  -- Objects in use
   end record;

end GNAT.Object_Pool;
