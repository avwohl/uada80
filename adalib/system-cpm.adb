-- System.CPM body for Z80
-- CP/M specific implementation

with System.CRTL;

package body System.CPM is

   -----------------
   -- CPM_Version --
   -----------------

   function CPM_Version return Natural is
   begin
      return System.CRTL.BDOS_Get_Version;
   end CPM_Version;

   -------------------
   -- Current_Drive --
   -------------------

   function Current_Drive return Natural is
   begin
      return System.CRTL.BDOS_Get_Current_Disk;
   end Current_Drive;

   ------------------
   -- Select_Drive --
   ------------------

   procedure Select_Drive (Drive : Natural) is
   begin
      System.CRTL.BDOS_Select_Disk (Drive);
   end Select_Drive;

   ------------------
   -- Current_User --
   ------------------

   function Current_User return Natural is
   begin
      return System.CRTL.BDOS_Get_Set_User (16#FF#);
   end Current_User;

   --------------
   -- Set_User --
   --------------

   procedure Set_User (User : Natural) is
      Dummy : Natural;
   begin
      Dummy := System.CRTL.BDOS_Get_Set_User (User);
   end Set_User;

   ---------------
   -- Warm_Boot --
   ---------------

   procedure Warm_Boot is
   begin
      System.CRTL.BDOS_System_Reset;
   end Warm_Boot;

   ----------------------
   -- Get_Alloc_Vector --
   ----------------------

   function Get_Alloc_Vector return System.Address is
   begin
      return System.CRTL.BDOS_Get_Addr_Alloc_Vec;
   end Get_Alloc_Vector;

   -------------
   -- Get_DPB --
   -------------

   function Get_DPB return System.Address is
   begin
      return System.CRTL.BDOS_Get_Addr_DPB;
   end Get_DPB;

   ------------------
   -- Is_Read_Only --
   ------------------

   function Is_Read_Only (FCB : System.Address) return Boolean is
      pragma Unreferenced (FCB);
   begin
      -- Check T1 attribute bit (read-only)
      -- Simplified - would need to check FCB byte 9 bit 7
      return False;
   end Is_Read_Only;

end System.CPM;
