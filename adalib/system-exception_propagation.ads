-- System.Exception_Propagation for Z80
-- Exception propagation support

with Ada.Exceptions;

package System.Exception_Propagation is
   pragma Preelaborate;

   -- Exception propagation data
   type Propagation_Data is limited private;

   -- Save/restore exception state
   procedure Save_State
     (Data : out Propagation_Data;
      E    : Ada.Exceptions.Exception_Occurrence);

   procedure Restore_State
     (Data : Propagation_Data;
      E    : out Ada.Exceptions.Exception_Occurrence);

   -- Propagate exception
   procedure Propagate_Exception
     (E : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Propagate_Exception);

   -- Setup propagation for current frame
   procedure Setup_Frame;
   procedure Cleanup_Frame;

private
   type Propagation_Data is record
      PC       : System.Address := System.Null_Address;
      SP       : System.Address := System.Null_Address;
      Handler  : System.Address := System.Null_Address;
   end record;

end System.Exception_Propagation;
