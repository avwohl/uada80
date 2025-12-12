-- System.CPM for Z80
-- CP/M specific interfaces and utilities

package System.CPM is
   pragma Preelaborate;

   -- CP/M memory map constants
   TPA_Start : constant := 16#0100#;  -- Transient Program Area start
   BDOS_Entry : constant := 16#0005#; -- BDOS entry point
   FCB_Default : constant := 16#005C#; -- Default FCB location
   DMA_Buffer : constant := 16#0080#; -- Default DMA buffer

   -- CP/M version info
   function CPM_Version return Natural;
   --  Return CP/M version (22 = 2.2, 30 = 3.0)

   -- Disk operations
   function Current_Drive return Natural;
   procedure Select_Drive (Drive : Natural);

   -- User number
   function Current_User return Natural;
   procedure Set_User (User : Natural);

   -- System reset
   procedure Warm_Boot;
   pragma No_Return (Warm_Boot);

   -- Allocation vector
   function Get_Alloc_Vector return System.Address;

   -- Disk parameter block
   function Get_DPB return System.Address;

   -- Check for read-only file
   function Is_Read_Only (FCB : System.Address) return Boolean;

end System.CPM;
