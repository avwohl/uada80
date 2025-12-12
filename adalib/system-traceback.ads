-- System.Traceback for Z80
-- Stack traceback support

package System.Traceback is
   pragma Preelaborate;

   -- Maximum traceback depth
   Max_Traceback_Depth : constant := 16;

   -- Traceback array type
   type Traceback_Array is array (1 .. Max_Traceback_Depth) of System.Address;

   -- Null traceback
   Null_Traceback : constant Traceback_Array := (others => System.Null_Address);

   -- Call chain record for Z80
   -- Z80 doesn't have frame pointers by default, so this is limited
   type Call_Chain is record
      Addresses : Traceback_Array;
      Length    : Natural := 0;
   end record;

   -- Get current call chain (limited on Z80)
   procedure Get_Call_Chain
     (Chain : out Call_Chain;
      Max_Depth : Natural := Max_Traceback_Depth);

   -- Get symbolic information for address (stub on Z80)
   function Symbolic_Traceback (Address : System.Address) return String;

   -- Format traceback as string
   function Format_Traceback (Chain : Call_Chain) return String;

end System.Traceback;
