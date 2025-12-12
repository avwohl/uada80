-- Ada.Command_Line.Remove for Z80
-- Command line argument removal

package Ada.Command_Line.Remove is

   procedure Remove_Argument (Number : Positive);
   --  Remove argument at Number from command line

   procedure Remove_Arguments (From, To : Positive);
   --  Remove arguments in range From .. To

end Ada.Command_Line.Remove;
