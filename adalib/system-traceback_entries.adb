-- System.Traceback_Entries body for Z80
-- Traceback entry representation

package body System.Traceback_Entries is

   ------------
   -- PC_For --
   ------------

   function PC_For (TB_Entry : Traceback_Entry) return System.Address is
   begin
      return TB_Entry.PC;
   end PC_For;

   ------------------
   -- TB_Entry_For --
   ------------------

   function TB_Entry_For (PC : System.Address) return Traceback_Entry is
   begin
      return (PC => PC);
   end TB_Entry_For;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (TB_Entry : Traceback_Entry) return Boolean is
   begin
      return TB_Entry.PC = System.Null_Address;
   end Is_Null;

end System.Traceback_Entries;
