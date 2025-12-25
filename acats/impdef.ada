-- IMPDEF.ADA
-- Implementation-defined values for UADA80 (Z80/CP/M Ada compiler)
--
-- This package provides tailorable entities for the Z80/CP/M target.

with Report;
with Ada.Text_IO;

package ImpDef is

   -- Annex validation flags (all enabled for full compliance target)

   Validating_Annex_C : constant Boolean := False;  -- Systems Programming
   Validating_Annex_D : constant Boolean := False;  -- Real-Time
   Validating_Annex_E : constant Boolean := False;  -- Distributed
   Validating_Annex_F : constant Boolean := False;  -- Information Systems
   Validating_Annex_G : constant Boolean := False;  -- Numerics
   Validating_Annex_H : constant Boolean := False;  -- Safety/Security

   -- Task switching times (conservative values for Z80)

   Minimum_Task_Switch : constant Duration := 0.100;
   Switch_To_New_Task  : constant Duration := 0.100;
   Clear_Ready_Queue   : constant Duration := 0.500;

   -- Time-related delays

   Delay_For_Time_Past    : constant Duration := 0.001;
   Time_Dependent_Reset   : constant Duration := 0.001;
   Delay_Per_Random_Test  : constant Duration := 0.001;

   -- Time slice procedure
   procedure Exceed_Time_Slice;

   -- Random number state string (invalid state)
   Non_State_String : constant String := "INVALID_STATE";

   -- External tag value
   External_Tag_Value : constant String := "uada80_tagged";

   -- Alignment constants (Z80 is byte-aligned)
   Max_Default_Alignment : constant := 1;
   Max_Linker_Alignment  : constant := 1;

   -- Storage size adjustment
   Maximum_Adjustment_To_Specified_Storage_Size : constant := 256;

   -- Directory names (CP/M style)
   Directory_To_Create     : constant String := "TESTDIR";
   Parent_Directory_Name   : constant String := "..";
   Current_Directory_Name  : constant String := ".";

   -- File name comparison
   function Equivalent_File_Names (Left, Right : String) return Boolean;

   -- Environment variables (not supported on CP/M)
   Existing_Environment_Variable_To_Read   : constant String := "";
   Unused_Environment_Variable_To_Modify   : constant String := "";

   -- C interface external names
   CXB30040_External_Name : constant String := "CXB30040";
   CXB30060_External_Name : constant String := "CXB30060";
   CXB30130_External_Name : constant String := "CXB30130";
   CXB30131_External_Name : constant String := "CXB30131";
   CXB30170_External_Name : constant String := "CXB30170";
   CXB30230_External_Name : constant String := "CXB30230";
   CXB30180_External_Name : constant String := "CXB30180";
   CXB30240_External_Name : constant String := "CXB30240";

   CXB30171_Global_External_Name : constant String := "CXB30171_Global";
   CXB30171_Proc_External_Name   : constant String := "CXB30171_Proc";
   CXB30181_Global_External_Name : constant String := "CXB30181_Global";
   CXB30181_Proc_External_Name   : constant String := "CXB30181_Proc";

   -- COBOL interface external names
   CXB40090_External_Name : constant String := "CXB40090";
   CXB40091_External_Name : constant String := "CXB40091";
   CXB40092_External_Name : constant String := "CXB40092";

   -- Fortran interface external names
   CXB50040_External_Name : constant String := "args";
   CXB50041_External_Name : constant String := "tax";
   CXB50050_External_Name : constant String := "align";
   CXB50051_External_Name : constant String := "modify";

   -- Record representation constants
   Char_Bits         : constant := 8;
   Next_Storage_Slot : constant := 6;

   -- Address I/O (Z80 uses 16-bit addresses)
   -- Note: Ada.Text_IO.Modular_IO not yet available in uada80
   -- package Address_Value_IO is
   --    new Ada.Text_IO.Modular_IO (System.Storage_Elements.Integer_Address);

end ImpDef;


package body ImpDef is

   procedure Exceed_Time_Slice is
      T : Integer := 0;
      Loop_Max : constant Integer := 1000;  -- Smaller for Z80
   begin
      for I in 1..Loop_Max loop
         T := Report.Ident_Int (1) * Report.Ident_Int (2);
      end loop;
   end Exceed_Time_Slice;

   function Equivalent_File_Names (Left, Right : String) return Boolean is
      -- CP/M is case-insensitive for file names
      function To_Upper (C : Character) return Character is
      begin
         if C in 'a'..'z' then
            return Character'Val (Character'Pos (C) - 32);
         else
            return C;
         end if;
      end To_Upper;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in Left'Range loop
         if To_Upper (Left (I)) /= To_Upper (Right (Left'First + I - Left'First)) then
            return False;
         end if;
      end loop;
      return True;
   end Equivalent_File_Names;

end ImpDef;
