-- System.Exception_Table for Z80
-- Exception name table management

package System.Exception_Table is
   pragma Preelaborate;

   -- Maximum exception name length
   Max_Name_Length : constant := 32;

   -- Maximum number of exceptions
   Max_Exceptions : constant := 32;

   -- Exception ID type
   type Exception_Id is range 0 .. Max_Exceptions;

   -- Null exception
   Null_Exception : constant Exception_Id := 0;

   -- Standard exception IDs
   Constraint_Error_Id : constant Exception_Id := 1;
   Program_Error_Id    : constant Exception_Id := 2;
   Storage_Error_Id    : constant Exception_Id := 3;
   Tasking_Error_Id    : constant Exception_Id := 4;
   Data_Error_Id       : constant Exception_Id := 5;
   Status_Error_Id     : constant Exception_Id := 6;
   Mode_Error_Id       : constant Exception_Id := 7;
   Name_Error_Id       : constant Exception_Id := 8;
   Use_Error_Id        : constant Exception_Id := 9;
   Device_Error_Id     : constant Exception_Id := 10;
   End_Error_Id        : constant Exception_Id := 11;
   Layout_Error_Id     : constant Exception_Id := 12;

   -- Register an exception
   procedure Register_Exception
     (Name : String;
      Id   : out Exception_Id);

   -- Get exception name from ID
   function Exception_Name (Id : Exception_Id) return String;

   -- Get exception ID from name
   function Exception_Id_From_Name (Name : String) return Exception_Id;

end System.Exception_Table;
