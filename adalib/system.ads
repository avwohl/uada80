-- System package for Z80
-- Provides low-level system information and types

package System is
   pragma Pure;

   -- Z80 is a little-endian 8-bit processor
   -- with 16-bit address space

   type Name is (Z80);
   System_Name : constant Name := Z80;

   -- Storage unit is 8 bits (1 byte)
   Storage_Unit : constant := 8;

   -- Word size is 16 bits on Z80
   Word_Size : constant := 16;

   -- Memory size (64KB address space)
   Memory_Size : constant := 65536;

   -- Address is a 16-bit value
   type Address is mod Memory_Size;
   Null_Address : constant Address := 0;

   -- Storage elements
   type Storage_Element is mod 2**Storage_Unit;
   type Storage_Offset is range -(2**15) .. (2**15) - 1;
   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;

   type Storage_Array is array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use Storage_Unit;

   -- Bit ordering (Z80 is little-endian)
   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order := Low_Order_First;

   -- Address arithmetic
   function "+" (Left : Address; Right : Storage_Offset) return Address;
   function "+" (Left : Storage_Offset; Right : Address) return Address;
   function "-" (Left : Address; Right : Storage_Offset) return Address;
   function "-" (Left : Address; Right : Address) return Storage_Offset;

   function "mod" (Left : Address; Right : Storage_Offset) return Storage_Offset;

   -- Address attributes
   Max_Int : constant := 2**15 - 1;
   Min_Int : constant := -(2**15);

   Max_Binary_Modulus : constant := 2**16;
   Max_Nonbinary_Modulus : constant := 2**16 - 1;
   Max_Base_Digits : constant := 4;
   Max_Digits : constant := 15;

   Max_Mantissa : constant := 31;
   Fine_Delta : constant := 2.0**(-Max_Mantissa);

   Tick : constant := 0.02;  -- 50Hz typical Z80 interrupt rate

   -- Sizes for standard types
   subtype Any_Priority is Integer range 0 .. 31;
   subtype Priority is Any_Priority range 0 .. 30;
   subtype Interrupt_Priority is Any_Priority range 31 .. 31;

   Default_Priority : constant Priority := 15;

   -- Address size for access types
   Address_Size : constant := 16;

private

   type Address is mod Memory_Size;
   pragma Convention (C, Address);

end System;
