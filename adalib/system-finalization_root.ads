-- System.Finalization_Root for Z80
-- Root type for controlled objects

package System.Finalization_Root is
   pragma Pure;

   -- Root type for all controlled objects
   type Root_Controlled is abstract tagged limited private;

   -- Finalization operations
   procedure Initialize (Object : in Out Root_Controlled) is null;
   procedure Adjust (Object : in Out Root_Controlled) is null;
   procedure Finalize (Object : in Out Root_Controlled) is null;

private
   type Root_Controlled is abstract tagged limited record
      Prev : System.Address := System.Null_Address;
      Next : System.Address := System.Null_Address;
   end record;

end System.Finalization_Root;
