-- System.Traceback_Entries for Z80
-- Traceback entry representation

package System.Traceback_Entries is
   pragma Preelaborate;

   -- Traceback entry - represents one call frame
   type Traceback_Entry is private;

   -- Null entry
   Null_TB_Entry : constant Traceback_Entry;

   -- Get program counter from entry
   function PC_For (TB_Entry : Traceback_Entry) return System.Address;

   -- Create entry from address
   function TB_Entry_For (PC : System.Address) return Traceback_Entry;

   -- Check if entry is null
   function Is_Null (TB_Entry : Traceback_Entry) return Boolean;

private
   -- On Z80, traceback entry is just an address (16-bit)
   type Traceback_Entry is record
      PC : System.Address;
   end record;

   Null_TB_Entry : constant Traceback_Entry :=
     (PC => System.Null_Address);

end System.Traceback_Entries;
