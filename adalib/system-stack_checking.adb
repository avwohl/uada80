-- System.Stack_Checking body for Z80
-- Stack overflow checking implementation

package body System.Stack_Checking is

   -----------------
   -- Stack_Check --
   -----------------

   procedure Stack_Check (Stack_Address : System.Address) is
      use type System.Address;
   begin
      if Stack_Limit /= System.Null_Address then
         if Stack_Address < Stack_Limit then
            raise Stack_Overflow;
         end if;
      end if;
   end Stack_Check;

   ------------------------
   -- Update_Stack_Limit --
   ------------------------

   procedure Update_Stack_Limit (Limit : System.Address) is
   begin
      Stack_Limit := Limit;
   end Update_Stack_Limit;

end System.Stack_Checking;
