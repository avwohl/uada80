-- GNAT.Traceback body for Z80
-- Stack traceback support implementation

package body GNAT.Traceback is

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback : out Tracebacks_Array;
      Len       : out Natural)
   is
   begin
      -- Z80 doesn't have frame pointers in the traditional sense
      -- Return empty traceback
      Len := 0;
      Traceback := (others => System.Null_Address);
   end Call_Chain;

   procedure Call_Chain
     (Traceback   : out Tracebacks_Array;
      Len         : out Natural;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address)
   is
      pragma Unreferenced (Exclude_Min, Exclude_Max);
   begin
      Call_Chain (Traceback, Len);
   end Call_Chain;

end GNAT.Traceback;
