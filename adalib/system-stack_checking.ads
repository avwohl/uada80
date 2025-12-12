-- System.Stack_Checking for Z80
-- Stack overflow checking support

package System.Stack_Checking is
   pragma Preelaborate;

   -- Stack limit for current task
   Stack_Limit : System.Address := System.Null_Address;
   pragma Export (C, Stack_Limit, "__gnat_stack_limit");

   -- Check if stack overflow occurred
   procedure Stack_Check (Stack_Address : System.Address);
   pragma Export (C, Stack_Check, "__gnat_stack_check");

   -- Update stack limit for task
   procedure Update_Stack_Limit (Limit : System.Address);

   -- Exception for stack overflow
   Stack_Overflow : exception;

end System.Stack_Checking;
