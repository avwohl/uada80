-- System.Wid_Util for Z80
-- Width calculation utilities

package System.Wid_Util is
   pragma Pure;

   -- Calculate width of integer image
   function Width_Integer (Lo, Hi : Integer) return Natural;

   -- Calculate width of unsigned image
   function Width_Unsigned (Lo, Hi : Natural) return Natural;

   -- Calculate width of enumeration
   function Width_Enumeration
     (Lo, Hi     : Integer;
      Width_Func : access function (Index : Integer) return Natural)
      return Natural;

end System.Wid_Util;
