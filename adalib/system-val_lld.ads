-- System.Val_LLD for Z80
-- Value conversion for Long_Long_Integer (32-bit on Z80)

package System.Val_LLD is
   pragma Pure;

   type Long_Long_Integer is range -2 ** 31 .. 2 ** 31 - 1;

   -- Scan integer from string
   procedure Scan_Long_Long_Integer
     (Str  : String;
      Ptr  : in Out Integer;
      Max  : Integer;
      Res  : out Long_Long_Integer);

   -- Value function
   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer;

end System.Val_LLD;
