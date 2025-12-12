-- GNAT.Altivec for Z80
-- AltiVec vector operations (stub for Z80)

package GNAT.Altivec is
   pragma Pure;

   -- AltiVec is PowerPC-specific, provide stub types for compatibility

   type Vector_Unsigned_Char is array (0 .. 15) of Unsigned_8;
   type Vector_Signed_Char is array (0 .. 15) of Integer_8;
   type Vector_Bool_Char is array (0 .. 15) of Boolean;

   type Vector_Unsigned_Short is array (0 .. 7) of Unsigned_16;
   type Vector_Signed_Short is array (0 .. 7) of Integer_16;
   type Vector_Bool_Short is array (0 .. 7) of Boolean;

   type Vector_Unsigned_Int is array (0 .. 3) of Unsigned_32;
   type Vector_Signed_Int is array (0 .. 3) of Integer_32;
   type Vector_Bool_Int is array (0 .. 3) of Boolean;

   type Vector_Float is array (0 .. 3) of Float;

private
   type Unsigned_8 is mod 2**8;
   type Integer_8 is range -128 .. 127;
   type Unsigned_16 is mod 2**16;
   type Integer_16 is range -32768 .. 32767;
   type Unsigned_32 is mod 2**32;
   type Integer_32 is range -2**31 .. 2**31 - 1;

end GNAT.Altivec;
