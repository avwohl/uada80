-- System.Partition_Interface body for Z80
-- Distributed systems partition interface (stub implementation)

package body System.Partition_Interface is

   ----------------------------
   -- Get_Local_Partition_Id --
   ----------------------------

   function Get_Local_Partition_Id return System.RPC.Partition_Id is
   begin
      return 0;  -- Z80 is always partition 0
   end Get_Local_Partition_Id;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Name    : Unit_Name;
      Version : String := "")
   is
      pragma Unreferenced (Name, Version);
   begin
      null;  -- No-op on single partition
   end Register_Passive_Package;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name    : Unit_Name;
      Receiver : RCI_Subp_Info_Access;
      Version : String := "")
   is
      pragma Unreferenced (Name, Receiver, Version);
   begin
      null;  -- No-op on single partition
   end Register_Receiving_Stub;

end System.Partition_Interface;
