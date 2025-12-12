-- System.Wid_Enum for Z80
-- Enumeration width computation for 'Width attribute

package System.Wid_Enum is
   pragma Pure;

   -- Generic enumeration width
   generic
      type Enum is (<>);
   function Width_Enumeration (Lo, Hi : Enum) return Natural;
   -- Returns the maximum width of Enum'Image(X)
   -- for all X in Lo .. Hi

   -- Width using name table (8-bit indices)
   function Width_Enumeration_8
     (Names  : String;
      Starts : String;
      Lo, Hi : Natural) return Natural;
   -- Lo, Hi are enumeration positions
   -- Names contains concatenated literal names
   -- Starts contains starting indices (8-bit)

   -- Width using name table (16-bit indices)
   function Width_Enumeration_16
     (Names  : String;
      Starts : String;
      Lo, Hi : Natural) return Natural;

end System.Wid_Enum;
