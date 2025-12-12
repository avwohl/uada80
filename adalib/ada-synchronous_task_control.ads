-- Ada.Synchronous_Task_Control for Z80
-- Task synchronization primitive
--
-- Provides a simple suspension object for task synchronization

package Ada.Synchronous_Task_Control is
   pragma Preelaborate;

   type Suspension_Object is limited private;

   -- Set the suspension object to True, releasing any waiting task
   procedure Set_True (S : in Out Suspension_Object);

   -- Set the suspension object to False
   procedure Set_False (S : in Out Suspension_Object);

   -- Return current state
   function Current_State (S : Suspension_Object) return Boolean;

   -- Suspend until the object becomes True, then set to False
   procedure Suspend_Until_True (S : in Out Suspension_Object);

private

   type Suspension_Object is record
      State : Boolean := False;
   end record;

end Ada.Synchronous_Task_Control;
