-- Ada.Real_Time body for Z80
-- High-resolution time implementation

package body Ada.Real_Time is

   -- Tick rate: 50Hz = 20ms per tick
   Ticks_Per_Second : constant := 50;
   MS_Per_Tick      : constant := 20;
   US_Per_Tick      : constant := 20000;
   NS_Per_Tick      : constant := 20000000;

   -- External timer functions from runtime
   function Rt_Get_Ticks return Integer;
   pragma Import (C, Rt_Get_Ticks, "_get_ticks");

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return (Ticks => Rt_Get_Ticks);
   end Clock;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return (Ticks => NS / NS_Per_Tick);
   end Nanoseconds;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return (Ticks => US / US_Per_Tick);
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return (Ticks => MS / MS_Per_Tick);
   end Milliseconds;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return (Ticks => S * Ticks_Per_Second);
   end Seconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return (Ticks => M * 60 * Ticks_Per_Second);
   end Minutes;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      -- Duration in seconds (approximate)
      return Duration (TS.Ticks) / Duration (Ticks_Per_Second);
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return (Ticks => Integer (D * Duration (Ticks_Per_Second)));
   end To_Time_Span;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return (Ticks => Left.Ticks + Right.Ticks);
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      return Right + Left;
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
   begin
      return (Ticks => Left.Ticks + Right.Ticks);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      return (Ticks => Left.Ticks - Right.Ticks);
   end "-";

   function "-" (Left : Time; Right : Time) return Time_Span is
   begin
      return (Ticks => Left.Ticks - Right.Ticks);
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
   begin
      return (Ticks => Left.Ticks - Right.Ticks);
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
   begin
      return (Ticks => -Right.Ticks);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return (Ticks => Left.Ticks * Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Right * Left;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
   begin
      return Left.Ticks / Right.Ticks;
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return (Ticks => Left.Ticks / Right);
   end "/";

   -----------
   -- "abs" --
   -----------

   function "abs" (Right : Time_Span) return Time_Span is
   begin
      if Right.Ticks < 0 then
         return (Ticks => -Right.Ticks);
      else
         return Right;
      end if;
   end "abs";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Left.Ticks < Right.Ticks;
   end "<";

   function "<" (Left, Right : Time_Span) return Boolean is
   begin
      return Left.Ticks < Right.Ticks;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Left.Ticks <= Right.Ticks;
   end "<=";

   function "<=" (Left, Right : Time_Span) return Boolean is
   begin
      return Left.Ticks <= Right.Ticks;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Left.Ticks > Right.Ticks;
   end ">";

   function ">" (Left, Right : Time_Span) return Boolean is
   begin
      return Left.Ticks > Right.Ticks;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Left.Ticks >= Right.Ticks;
   end ">=";

   function ">=" (Left, Right : Time_Span) return Boolean is
   begin
      return Left.Ticks >= Right.Ticks;
   end ">=";

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Integer; TS : out Time_Span) is
   begin
      SC := T.Ticks / Ticks_Per_Second;
      TS := (Ticks => T.Ticks mod Ticks_Per_Second);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Integer; TS : Time_Span) return Time is
   begin
      return (Ticks => SC * Ticks_Per_Second + TS.Ticks);
   end Time_Of;

end Ada.Real_Time;
