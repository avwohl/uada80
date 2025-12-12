-- System.Elaboration body for Z80
-- Elaboration control and tracking

package body System.Elaboration is

   -- Elaboration flags for all units
   Unit_Flags : array (1 .. Max_Units) of Elaboration_Flag :=
     (others => Not_Elaborated);

   -- Count of registered units
   Registered_Units : Natural := 0;

   -------------------
   -- Register_Unit --
   -------------------

   procedure Register_Unit
     (Unit_Name : String;
      Unit_Id   : out Natural)
   is
      pragma Unreferenced (Unit_Name);
   begin
      if Registered_Units >= Max_Units then
         -- Too many units
         Unit_Id := 0;
         return;
      end if;

      Registered_Units := Registered_Units + 1;
      Unit_Id := Registered_Units;
   end Register_Unit;

   -----------------------
   -- Start_Elaboration --
   -----------------------

   procedure Start_Elaboration (Unit_Id : Natural) is
   begin
      if Unit_Id in 1 .. Max_Units then
         Unit_Flags (Unit_Id) := In_Progress;
      end if;
   end Start_Elaboration;

   ------------------------
   -- Finish_Elaboration --
   ------------------------

   procedure Finish_Elaboration (Unit_Id : Natural) is
   begin
      if Unit_Id in 1 .. Max_Units then
         Unit_Flags (Unit_Id) := Elaborated;
      end if;
   end Finish_Elaboration;

   -------------------
   -- Is_Elaborated --
   -------------------

   function Is_Elaborated (Unit_Id : Natural) return Boolean is
   begin
      if Unit_Id in 1 .. Max_Units then
         return Unit_Flags (Unit_Id) = Elaborated;
      else
         return False;
      end if;
   end Is_Elaborated;

   --------------------
   -- Check_Circular --
   --------------------

   function Check_Circular (Unit_Id : Natural) return Boolean is
   begin
      if Unit_Id in 1 .. Max_Units then
         return Unit_Flags (Unit_Id) = In_Progress;
      else
         return False;
      end if;
   end Check_Circular;

   ----------------
   -- Unit_Count --
   ----------------

   function Unit_Count return Natural is
   begin
      return Registered_Units;
   end Unit_Count;

end System.Elaboration;
