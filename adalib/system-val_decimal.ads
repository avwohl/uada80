-- System.Val_Decimal for Z80
-- Decimal fixed point value conversion from string

package System.Val_Decimal is
   pragma Pure;

   -- Scan decimal fixed point value
   function Scan_Decimal
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Scale : Integer) return Long_Long_Integer;
   -- Scans decimal literal, returns value scaled by 10**Scale
   -- Scale is typically positive for decimal fixed point

   -- Convert string to decimal value
   function Value_Decimal
     (Str   : String;
      Scale : Integer) return Long_Long_Integer;

end System.Val_Decimal;
