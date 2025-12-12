-- Interfaces.VxWorks for Z80
-- VxWorks compatibility (stub)

package Interfaces.VxWorks is
   pragma Pure;

   -- VxWorks types (stubs for source compatibility)
   type Int is new Integer;
   type Short is range -32768 .. 32767;
   type Long is new Integer;

   type STATUS is new Int;
   OK    : constant STATUS := 0;
   ERROR : constant STATUS := -1;

   type BOOL is new Int;
   FALSE : constant BOOL := 0;
   TRUE  : constant BOOL := 1;

   -- Task priorities
   type Task_Priority is range 0 .. 255;

   -- Semaphore types
   type SEM_ID is private;
   Null_SEM_ID : constant SEM_ID;

   type SEM_B_STATE is (SEM_EMPTY, SEM_FULL);
   type SEM_OPTIONS is new Int;

   SEM_Q_FIFO     : constant SEM_OPTIONS := 0;
   SEM_Q_PRIORITY : constant SEM_OPTIONS := 1;

private
   type SEM_ID is new Natural;
   Null_SEM_ID : constant SEM_ID := 0;

end Interfaces.VxWorks;
