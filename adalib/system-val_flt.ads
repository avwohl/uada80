-- System.Val_Flt for Z80
-- Float value conversion

package System.Val_Flt is
   pragma Pure;

   -- Scan Float from string
   procedure Scan_Float
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Res : out Float);

   -- Value function
   function Value_Float (Str : String) return Float;

end System.Val_Flt;
