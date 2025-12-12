-- System.Elaboration for Z80
-- Elaboration control and tracking

package System.Elaboration is
   pragma Preelaborate;

   -- Maximum number of units that can be tracked
   Max_Units : constant := 64;

   -- Unit elaboration state
   type Elaboration_Flag is (Not_Elaborated, In_Progress, Elaborated);

   -- Register a unit for elaboration tracking
   procedure Register_Unit
     (Unit_Name : String;
      Unit_Id   : out Natural);

   -- Mark unit as being elaborated
   procedure Start_Elaboration (Unit_Id : Natural);

   -- Mark unit as elaborated
   procedure Finish_Elaboration (Unit_Id : Natural);

   -- Check if unit is elaborated
   function Is_Elaborated (Unit_Id : Natural) return Boolean;

   -- Check for circular elaboration (returns True if in progress)
   function Check_Circular (Unit_Id : Natural) return Boolean;

   -- Get count of registered units
   function Unit_Count return Natural;

end System.Elaboration;
