-- System.Val_LLU for Z80
-- Long_Long_Unsigned value conversion from string

package System.Val_LLU is
   pragma Pure;

   type Long_Long_Unsigned is mod 2 ** 64;

   -- Scan Long_Long_Unsigned from string
   function Scan_Long_Long_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Unsigned;
   -- Scans from Str starting at Ptr.all, up to Max
   -- Updates Ptr to position after scanned number

   -- Convert entire string to Long_Long_Unsigned
   function Value_Long_Long_Unsigned (Str : String) return Long_Long_Unsigned;
   -- Raises Constraint_Error for invalid input

end System.Val_LLU;
