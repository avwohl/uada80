-- System.Exception_Propagation body for Z80
-- Exception propagation implementation

with System.Machine_Code;

package body System.Exception_Propagation is

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State
     (Data : out Propagation_Data;
      E    : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (E);
   begin
      -- Save current PC and SP
      Data.PC := System.Null_Address;  -- Would use inline assembly
      Data.SP := System.Null_Address;
      Data.Handler := System.Null_Address;
   end Save_State;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State
     (Data : Propagation_Data;
      E    : out Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Data);
   begin
      Ada.Exceptions.Save_Occurrence (E, Ada.Exceptions.Null_Occurrence);
   end Restore_State;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   procedure Propagate_Exception
     (E : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      -- Re-raise the exception
      Ada.Exceptions.Reraise_Occurrence (E);
   end Propagate_Exception;

   -----------------
   -- Setup_Frame --
   -----------------

   procedure Setup_Frame is
   begin
      -- Push exception frame onto stack
      null;
   end Setup_Frame;

   -------------------
   -- Cleanup_Frame --
   -------------------

   procedure Cleanup_Frame is
   begin
      -- Pop exception frame from stack
      null;
   end Cleanup_Frame;

end System.Exception_Propagation;
