-- System.Val_Real for Z80
-- Real (floating-point) value conversion from strings

package System.Val_Real is
   pragma Pure;

   -- Scan Float value from string
   function Scan_Real
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Float;
   -- Scans Str starting at Ptr.all for a floating-point value.
   -- Updates Ptr to point past the last character scanned.

   -- Get Float value from string
   function Value_Real (Str : String) return Long_Long_Float;
   -- Returns the Float value of the string Str

end System.Val_Real;
