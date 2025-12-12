-- Ada.Streams.Stream_IO.C_Streams body for Z80
-- Interface to C-style file streams implementation

package body Ada.Streams.Stream_IO.C_Streams is

   --------------
   -- C_Stream --
   --------------

   function C_Stream (F : File_Type) return FILEs is
      pragma Unreferenced (F);
   begin
      -- On Z80/CP/M, return null - no direct C stream support
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
      -- Just reset the file state
      Reset (File, Mode);
   end Open;

end Ada.Streams.Stream_IO.C_Streams;
