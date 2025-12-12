-- System.Memory_Copy for Z80
-- Memory copy operations

package System.Memory_Copy is
   pragma Pure;

   procedure Copy
     (Target : System.Address;
      Source : System.Address;
      Size   : Natural);
   --  Copy Size bytes from Source to Target

   procedure Move
     (Target : System.Address;
      Source : System.Address;
      Size   : Natural);
   --  Move Size bytes (handles overlapping regions)

   procedure Fill
     (Target : System.Address;
      Value  : Natural;
      Size   : Natural);
   --  Fill Size bytes with Value (low 8 bits)

   procedure Zero
     (Target : System.Address;
      Size   : Natural);
   --  Zero Size bytes

   function Compare
     (Left   : System.Address;
      Right  : System.Address;
      Size   : Natural) return Integer;
   --  Compare Size bytes, return <0, 0, or >0

end System.Memory_Copy;
