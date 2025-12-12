-- System.Val_LLI for Z80
-- Long_Long_Integer value conversion from strings

package System.Val_LLI is
   pragma Pure;

   -- Scan Long_Long_Integer value from string
   function Scan_Long_Long_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Integer;
   -- Scans Str starting at Ptr.all for a Long_Long_Integer value.
   -- Updates Ptr to point past the last character scanned.

   -- Get Long_Long_Integer value from string
   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer;
   -- Returns the Long_Long_Integer value of the string Str

end System.Val_LLI;
