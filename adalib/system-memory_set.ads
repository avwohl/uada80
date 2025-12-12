-- System.Memory_Set for Z80
-- Memory set operations

package System.Memory_Set is
   pragma Pure;

   procedure Set
     (Target : System.Address;
      Value  : Integer;
      Size   : Natural);
   --  Set Size bytes to Value (like C memset)

   procedure Set_Word
     (Target : System.Address;
      Value  : Natural;
      Count  : Natural);
   --  Set Count 16-bit words to Value

end System.Memory_Set;
