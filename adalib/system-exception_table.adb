-- System.Exception_Table body for Z80
-- Exception name table management

package body System.Exception_Table is

   -- Exception name table
   type Name_Entry is record
      Name : String (1 .. Max_Name_Length);
      Len  : Natural := 0;
   end record;

   Names : array (1 .. Max_Exceptions) of Name_Entry;
   Registered : Natural := 12;  -- Pre-registered standard exceptions

   -- Initialize standard exception names
   procedure Initialize_Standard is
   begin
      Names (1) := (Name => "CONSTRAINT_ERROR" & (17 .. Max_Name_Length => ' '), Len => 16);
      Names (2) := (Name => "PROGRAM_ERROR" & (14 .. Max_Name_Length => ' '), Len => 13);
      Names (3) := (Name => "STORAGE_ERROR" & (14 .. Max_Name_Length => ' '), Len => 13);
      Names (4) := (Name => "TASKING_ERROR" & (14 .. Max_Name_Length => ' '), Len => 13);
      Names (5) := (Name => "DATA_ERROR" & (11 .. Max_Name_Length => ' '), Len => 10);
      Names (6) := (Name => "STATUS_ERROR" & (13 .. Max_Name_Length => ' '), Len => 12);
      Names (7) := (Name => "MODE_ERROR" & (11 .. Max_Name_Length => ' '), Len => 10);
      Names (8) := (Name => "NAME_ERROR" & (11 .. Max_Name_Length => ' '), Len => 10);
      Names (9) := (Name => "USE_ERROR" & (10 .. Max_Name_Length => ' '), Len => 9);
      Names (10) := (Name => "DEVICE_ERROR" & (13 .. Max_Name_Length => ' '), Len => 12);
      Names (11) := (Name => "END_ERROR" & (10 .. Max_Name_Length => ' '), Len => 9);
      Names (12) := (Name => "LAYOUT_ERROR" & (13 .. Max_Name_Length => ' '), Len => 12);
   end Initialize_Standard;

   Initialized : Boolean := False;

   ------------------------
   -- Register_Exception --
   ------------------------

   procedure Register_Exception
     (Name : String;
      Id   : out Exception_Id)
   is
      Len : constant Natural := Natural'Min (Name'Length, Max_Name_Length);
   begin
      if not Initialized then
         Initialize_Standard;
         Initialized := True;
      end if;

      if Registered >= Max_Exceptions then
         Id := Null_Exception;
         return;
      end if;

      Registered := Registered + 1;
      Names (Registered).Name (1 .. Len) := Name (Name'First .. Name'First + Len - 1);
      Names (Registered).Len := Len;
      Id := Exception_Id (Registered);
   end Register_Exception;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (Id : Exception_Id) return String is
   begin
      if not Initialized then
         Initialize_Standard;
         Initialized := True;
      end if;

      if Id = Null_Exception or else Natural (Id) > Registered then
         return "UNKNOWN_EXCEPTION";
      end if;

      return Names (Natural (Id)).Name (1 .. Names (Natural (Id)).Len);
   end Exception_Name;

   ----------------------------
   -- Exception_Id_From_Name --
   ----------------------------

   function Exception_Id_From_Name (Name : String) return Exception_Id is
   begin
      if not Initialized then
         Initialize_Standard;
         Initialized := True;
      end if;

      for I in 1 .. Registered loop
         if Names (I).Len = Name'Length then
            declare
               Match : Boolean := True;
            begin
               for J in 1 .. Names (I).Len loop
                  if Names (I).Name (J) /= Name (Name'First + J - 1) then
                     Match := False;
                     exit;
                  end if;
               end loop;

               if Match then
                  return Exception_Id (I);
               end if;
            end;
         end if;
      end loop;

      return Null_Exception;
   end Exception_Id_From_Name;

end System.Exception_Table;
