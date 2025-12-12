-- System.Mmap for Z80
-- Memory mapping abstraction
-- Note: Z80 has no MMU, this is a compatibility stub

with System.Storage_Elements;

package System.Mmap is
   pragma Preelaborate;

   -- Mapped memory region
   type Mapped_Region is private;

   Invalid_Region : constant Mapped_Region;

   -- Page size (Z80: single 64KB address space)
   Page_Size : constant := 256;

   -- Map file into memory (stub - returns direct memory)
   function Map_File
     (File_Name : String;
      Offset    : System.Storage_Elements.Storage_Offset := 0;
      Length    : System.Storage_Elements.Storage_Count := 0)
     return Mapped_Region;

   -- Unmap region
   procedure Unmap (Region : in Out Mapped_Region);

   -- Get base address of mapped region
   function Address (Region : Mapped_Region) return System.Address;

   -- Get length of mapped region
   function Length (Region : Mapped_Region)
     return System.Storage_Elements.Storage_Count;

   -- Check if region is valid
   function Is_Valid (Region : Mapped_Region) return Boolean;

private

   type Mapped_Region is record
      Base   : System.Address := System.Null_Address;
      Size   : System.Storage_Elements.Storage_Count := 0;
      Valid  : Boolean := False;
   end record;

   Invalid_Region : constant Mapped_Region :=
     (Base => System.Null_Address, Size => 0, Valid => False);

end System.Mmap;
