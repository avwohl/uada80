-- Ada.Streams.Stream_IO.C_Streams for Z80
-- Interface to C-style file streams
--
-- Provides conversion between Ada Stream_IO and C FILE streams

with System;

package Ada.Streams.Stream_IO.C_Streams is
   pragma Preelaborate;

   -- C FILE pointer type (for CP/M, this is simplified)
   type FILEs is new System.Address;

   -- Convert between Ada and C file representations
   function C_Stream (F : File_Type) return FILEs;
   -- Returns the C FILE* associated with the Ada file

   procedure Open
     (File     : in Out File_Type;
      Mode     : File_Mode;
      C_Stream : FILEs;
      Form     : String := "");
   -- Open an Ada file using an existing C stream

end Ada.Streams.Stream_IO.C_Streams;
