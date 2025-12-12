-- GNAT.Traceback for Z80
-- Stack traceback support

with System.Traceback_Entries;

package GNAT.Traceback is
   pragma Preelaborate;

   subtype Tracebacks_Array is System.Traceback_Entries.Tracebacks_Array;

   -- Get current call stack
   procedure Call_Chain
     (Traceback : out Tracebacks_Array;
      Len       : out Natural);

   -- Get call chain starting from specific address
   procedure Call_Chain
     (Traceback   : out Tracebacks_Array;
      Len         : out Natural;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address);

end GNAT.Traceback;
