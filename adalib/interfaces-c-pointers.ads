-- Interfaces.C.Pointers for Z80
-- Generic C pointer operations
--
-- Provides pointer arithmetic for interfacing with C code

with Interfaces.C;
with System;

generic
   type Index is (<>);
   type Element is private;
   type Element_Array is array (Index range <>) of aliased Element;
   Default_Terminator : Element;
package Interfaces.C.Pointers is
   pragma Preelaborate;

   type Pointer is access all Element;
   for Pointer'Size use System.Address'Size;

   pragma No_Strict_Aliasing (Pointer);

   function Value (Ref : Pointer) return Element;

   function Value
     (Ref    : Pointer;
      Length : Interfaces.C.ptrdiff_t) return Element_Array;

   function Value
     (Ref        : Pointer;
      Terminator : Element := Default_Terminator) return Element_Array;

   -- Pointer arithmetic
   Pointer_Error : exception;

   function "+" (Left : Pointer; Right : Interfaces.C.ptrdiff_t) return Pointer;
   function "+" (Left : Interfaces.C.ptrdiff_t; Right : Pointer) return Pointer;
   function "-" (Left : Pointer; Right : Interfaces.C.ptrdiff_t) return Pointer;
   function "-" (Left : Pointer; Right : Pointer) return Interfaces.C.ptrdiff_t;

   procedure Increment (Ref : in Out Pointer);
   procedure Decrement (Ref : in Out Pointer);

   -- Copy functions
   procedure Copy_Terminated_Array
     (Source     : Pointer;
      Target     : Pointer;
      Limit      : Interfaces.C.ptrdiff_t := Interfaces.C.ptrdiff_t'Last;
      Terminator : Element := Default_Terminator);

   procedure Copy_Array
     (Source : Pointer;
      Target : Pointer;
      Length : Interfaces.C.ptrdiff_t);

   -- Virtual array
   function Virtual_Length
     (Ref        : Pointer;
      Terminator : Element := Default_Terminator) return Interfaces.C.ptrdiff_t;

end Interfaces.C.Pointers;
