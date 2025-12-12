-- System.Val_Int for Z80
-- Integer value conversion from strings

package System.Val_Int is
   pragma Pure;

   -- Scan integer value from string
   function Scan_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Integer;
   -- Scans Str starting at Ptr.all for an integer value.
   -- Updates Ptr to point past the last character scanned.
   -- Max is the maximum index to scan.

   -- Get integer value from string
   function Value_Integer (Str : String) return Integer;
   -- Returns the Integer value of the string Str

end System.Val_Int;
