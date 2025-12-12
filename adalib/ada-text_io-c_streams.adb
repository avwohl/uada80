-- Ada.Text_IO.C_Streams body for Z80
-- Interface to C-style text file streams implementation

package body Ada.Text_IO.C_Streams is

   --------------
   -- C_Stream --
   --------------

   function C_Stream (F : File_Type) return FILEs is
      pragma Unreferenced (F);
   begin
      -- On Z80/CP/M, return null
      return FILEs (System.Null_Address);
   end C_Stream;

   ----------
   -- Open --
   ----------

   procedure Open
     (File     : in Out File_Type;
      Mode     : File_Mode;
      C_Stream : FILEs;
      Form     : String := "")
   is
      pragma Unreferenced (C_Stream, Form);
   begin
      -- Stub: CP/M doesn't support C streams directly
      Reset (File, Mode);
   end Open;

end Ada.Text_IO.C_Streams;
