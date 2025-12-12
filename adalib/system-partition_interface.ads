-- System.Partition_Interface for Z80
-- Distributed systems partition interface (stub)

with Ada.Streams;
with System.RPC;

package System.Partition_Interface is
   pragma Preelaborate;

   type DSA_Implementation_Name is (No_DSA, GLADE, PolyORB);
   DSA_Implementation : constant DSA_Implementation_Name := No_DSA;

   type RCI_Subp_Info is record
      Addr : System.Address;
   end record;

   type RCI_Subp_Info_Access is access all RCI_Subp_Info;

   procedure Check
     (Name    : Unit_Name;
      Version : String;
      RCI     : Boolean := True);
   pragma Import (Ada, Check, "__gnat_check_unit");

   subtype Unit_Name is String;

   -- Get partition ID
   function Get_Local_Partition_Id return System.RPC.Partition_Id;

   -- RCI registration (stubs)
   procedure Register_Passive_Package
     (Name    : Unit_Name;
      Version : String := "");

   procedure Register_Receiving_Stub
     (Name    : Unit_Name;
      Receiver : RCI_Subp_Info_Access;
      Version : String := "");

end System.Partition_Interface;
