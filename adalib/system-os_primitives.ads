-- System.OS_Primitives for Z80/CP/M
-- OS-level primitive operations

package System.OS_Primitives is
   pragma Preelaborate;

   -- Time type (in seconds with fractional part)
   subtype Time_Type is Duration;

   -- Clock resolution
   Clock_Resolution : constant Duration := 0.02;  -- 50Hz typical

   -- Monotonic clock
   function Clock return Time_Type;
   --  Return current time

   -- Delay operations
   procedure Timed_Delay
     (Time : Time_Type;
      Mode : Integer);
   --  Delay for specified time
   --  Mode: 0 = relative, 1 = absolute

   -- Constants for Mode parameter
   Relative : constant Integer := 0;
   Absolute : constant Integer := 1;

   -- Initialize OS primitives
   procedure Initialize;

end System.OS_Primitives;
