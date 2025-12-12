-- Ada.Text_IO.C_Streams for Z80
-- Interface to C-style text file streams
--
-- Provides conversion between Ada Text_IO and C FILE streams

with System;

package Ada.Text_IO.C_Streams is
   pragma Preelaborate;

   -- C FILE pointer type
   type FILEs is new System.Address;

   -- Convert between Ada and C file representations
   function C_Stream (F : File_Type) return FILEs;
   -- Returns the C FILE* associated with the Ada text file

   procedure Open
     (File     : in Out File_Type;
      Mode     : File_Mode;
      C_Stream : FILEs;
      Form     : String := "");
   -- Open an Ada text file using an existing C stream

end Ada.Text_IO.C_Streams;
