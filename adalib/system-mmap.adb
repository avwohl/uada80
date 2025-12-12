-- System.Mmap body for Z80
-- Memory mapping stub implementation

package body System.Mmap is

   use System.Storage_Elements;

   --------------
   -- Map_File --
   --------------

   function Map_File
     (File_Name : String;
      Offset    : Storage_Offset := 0;
      Length    : Storage_Count := 0)
     return Mapped_Region
   is
      pragma Unreferenced (File_Name, Offset, Length);
   begin
      -- Z80 has no memory mapping
      -- Return invalid region
      return Invalid_Region;
   end Map_File;

   -----------
   -- Unmap --
   -----------

   procedure Unmap (Region : in Out Mapped_Region) is
   begin
      Region := Invalid_Region;
   end Unmap;

   -------------
   -- Address --
   -------------

   function Address (Region : Mapped_Region) return System.Address is
   begin
      return Region.Base;
   end Address;

   ------------
   -- Length --
   ------------

   function Length (Region : Mapped_Region) return Storage_Count is
   begin
      return Region.Size;
   end Length;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Region : Mapped_Region) return Boolean is
   begin
      return Region.Valid;
   end Is_Valid;

end System.Mmap;
