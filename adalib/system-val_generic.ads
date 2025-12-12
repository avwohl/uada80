-- System.Val_Generic for Z80
-- Generic value conversion utilities

generic
   type T is range <>;
package System.Val_Generic is
   pragma Pure;

   -- Scan value from string
   procedure Scan
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Res : out T);

   -- Value function
   function Value (Str : String) return T;

   -- Checked conversion with range check
   function Checked_Value (Str : String; Lo, Hi : T) return T;

end System.Val_Generic;
