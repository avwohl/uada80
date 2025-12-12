-- System.Val_Uns for Z80
-- Unsigned integer value conversion from strings

with Interfaces;

package System.Val_Uns is
   pragma Pure;

   -- Scan Unsigned value from string
   function Scan_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Interfaces.Unsigned_32;
   -- Scans Str starting at Ptr.all for an unsigned value.
   -- Updates Ptr to point past the last character scanned.

   -- Get Unsigned value from string
   function Value_Unsigned (Str : String) return Interfaces.Unsigned_32;
   -- Returns the Unsigned value of the string Str

end System.Val_Uns;
