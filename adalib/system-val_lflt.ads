-- System.Val_LFlt for Z80
-- Long_Float value conversion

package System.Val_LFlt is
   pragma Pure;

   -- Scan Long_Float from string
   procedure Scan_Long_Float
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Res : out Long_Float);

   -- Value function
   function Value_Long_Float (Str : String) return Long_Float;

end System.Val_LFlt;
