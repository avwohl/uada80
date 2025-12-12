-- System.Wid_Uns for Z80
-- Unsigned integer width computation for 'Width attribute

with Interfaces;

package System.Wid_Uns is
   pragma Pure;

   -- Width of Unsigned_32 type
   function Width_Unsigned
     (Lo, Hi : Interfaces.Unsigned_32) return Natural;
   -- Returns the maximum width of Unsigned_32'Image(X)
   -- for all X in Lo .. Hi

   -- Width of Unsigned_16 type
   function Width_Unsigned_16
     (Lo, Hi : Interfaces.Unsigned_16) return Natural;

   -- Width of Unsigned_8 type
   function Width_Unsigned_8
     (Lo, Hi : Interfaces.Unsigned_8) return Natural;

end System.Wid_Uns;
