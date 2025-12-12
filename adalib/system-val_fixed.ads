-- System.Val_Fixed for Z80
-- Fixed point value conversion from string

package System.Val_Fixed is
   pragma Pure;

   -- Scan fixed point value from string
   function Scan_Fixed
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Num   : Long_Long_Integer;
      Den   : Long_Long_Integer) return Long_Long_Integer;
   -- Scans a fixed-point literal from Str starting at Ptr.all
   -- Num/Den represent the small of the fixed type
   -- Returns scaled result

   -- Convert string to fixed point value
   function Value_Fixed
     (Str : String;
      Num : Long_Long_Integer;
      Den : Long_Long_Integer) return Long_Long_Integer;
   -- Converts entire string to fixed-point value
   -- Raises Constraint_Error on invalid input

end System.Val_Fixed;
