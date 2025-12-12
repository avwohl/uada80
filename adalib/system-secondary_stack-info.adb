-- System.Secondary_Stack.Info body for Z80
-- Secondary stack information implementation

package body System.Secondary_Stack.Info is

   Current_Usage   : Natural := 0;
   Max_Usage       : Natural := 0;
   High_Water      : Natural := 0;

   ----------------------
   -- Get_Current_Size --
   ----------------------

   function Get_Current_Size return Natural is
   begin
      return Current_Usage;
   end Get_Current_Size;

   ------------------
   -- Get_Max_Size --
   ------------------

   function Get_Max_Size return Natural is
   begin
      return Max_Usage;
   end Get_Max_Size;

   --------------------------
   -- Get_High_Water_Mark --
   --------------------------

   function Get_High_Water_Mark return Natural is
   begin
      return High_Water;
   end Get_High_Water_Mark;

   ----------------------------
   -- Reset_High_Water_Mark --
   ----------------------------

   procedure Reset_High_Water_Mark is
   begin
      High_Water := Current_Usage;
   end Reset_High_Water_Mark;

end System.Secondary_Stack.Info;
