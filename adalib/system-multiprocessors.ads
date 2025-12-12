-- System.Multiprocessors for Z80
-- Multiprocessor support
--
-- Note: Z80 is a single-processor system. This package provides stubs
-- for compatibility with programs that query processor information.

package System.Multiprocessors is
   pragma Preelaborate;

   -- CPU identifier type
   type CPU is range 0 .. 1;
   -- Z80 has only one CPU, so valid range is 1..1

   Not_A_Specific_CPU : constant CPU := 0;
   -- Special value meaning no specific CPU

   -- Number of CPUs in the system
   function Number_Of_CPUs return CPU;
   -- Always returns 1 for Z80

end System.Multiprocessors;
