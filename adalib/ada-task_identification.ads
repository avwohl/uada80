-- Ada.Task_Identification for Z80
-- Task identification support

package Ada.Task_Identification is
   pragma Preelaborate;

   type Task_Id is private;

   Null_Task_Id : constant Task_Id;

   function "=" (Left, Right : Task_Id) return Boolean;

   function Image (T : Task_Id) return String;

   function Current_Task return Task_Id;

   function Environment_Task return Task_Id;

   procedure Abort_Task (T : Task_Id);

   function Is_Terminated (T : Task_Id) return Boolean;
   function Is_Callable (T : Task_Id) return Boolean;

   function Activation_Is_Complete (T : Task_Id) return Boolean;

private

   type Task_Id is new Natural;

   Null_Task_Id : constant Task_Id := 0;

end Ada.Task_Identification;
