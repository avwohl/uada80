-- Ada.Finalization for Z80
-- Provides controlled types with automatic initialization/adjustment/finalization

package Ada.Finalization is
   pragma Preelaborate;

   -- Controlled types call Initialize on creation, Adjust on assignment,
   -- and Finalize on destruction

   type Controlled is abstract tagged private;

   -- Initialize is called after object creation
   procedure Initialize (Object : in Out Controlled) is null;

   -- Adjust is called after assignment
   procedure Adjust (Object : in Out Controlled) is null;

   -- Finalize is called before object destruction
   procedure Finalize (Object : in Out Controlled) is null;

   -- Limited_Controlled types don't support assignment
   type Limited_Controlled is abstract tagged limited private;

   procedure Initialize (Object : in Out Limited_Controlled) is null;
   procedure Finalize (Object : in Out Limited_Controlled) is null;

private

   type Controlled is abstract tagged record
      -- Internal finalization chain pointer (managed by runtime)
      Finalization_Link : Integer := 0;
   end record;

   type Limited_Controlled is abstract tagged limited record
      Finalization_Link : Integer := 0;
   end record;

end Ada.Finalization;
