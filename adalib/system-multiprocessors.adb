-- System.Multiprocessors body for Z80
-- Multiprocessor support implementation

package body System.Multiprocessors is

   --------------------
   -- Number_Of_CPUs --
   --------------------

   function Number_Of_CPUs return CPU is
   begin
      -- Z80 is a single-processor system
      return 1;
   end Number_Of_CPUs;

end System.Multiprocessors;
