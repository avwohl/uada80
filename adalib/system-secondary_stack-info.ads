-- System.Secondary_Stack.Info for Z80
-- Secondary stack information

with System.Secondary_Stack;

package System.Secondary_Stack.Info is
   pragma Preelaborate;

   function Get_Current_Size return Natural;
   --  Return current secondary stack usage

   function Get_Max_Size return Natural;
   --  Return maximum secondary stack size

   function Get_High_Water_Mark return Natural;
   --  Return high water mark of usage

   procedure Reset_High_Water_Mark;
   --  Reset the high water mark

end System.Secondary_Stack.Info;
