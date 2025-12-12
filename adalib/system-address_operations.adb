-- System.Address_Operations body for Z80
-- Address arithmetic operations

package body System.Address_Operations is

   ---------
   -- Add --
   ---------

   function Add (Left : System.Address; Right : Integer) return System.Address is
   begin
      return System.Address (Integer (Left) + Right);
   end Add;

   ---------
   -- Sub --
   ---------

   function Sub (Left : System.Address; Right : Integer) return System.Address is
   begin
      return System.Address (Integer (Left) - Right);
   end Sub;

   ----------
   -- Diff --
   ----------

   function Diff (Left, Right : System.Address) return Integer is
   begin
      return Integer (Left) - Integer (Right);
   end Diff;

   --------
   -- Lt --
   --------

   function Lt (Left, Right : System.Address) return Boolean is
   begin
      return Left < Right;
   end Lt;

   --------
   -- Le --
   --------

   function Le (Left, Right : System.Address) return Boolean is
   begin
      return Left <= Right;
   end Le;

   --------
   -- Gt --
   --------

   function Gt (Left, Right : System.Address) return Boolean is
   begin
      return Left > Right;
   end Gt;

   --------
   -- Ge --
   --------

   function Ge (Left, Right : System.Address) return Boolean is
   begin
      return Left >= Right;
   end Ge;

   --------
   -- Eq --
   --------

   function Eq (Left, Right : System.Address) return Boolean is
   begin
      return Left = Right;
   end Eq;

   --------
   -- Ne --
   --------

   function Ne (Left, Right : System.Address) return Boolean is
   begin
      return Left /= Right;
   end Ne;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (A : System.Address) return Integer is
   begin
      return Integer (A);
   end To_Integer;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (I : Integer) return System.Address is
   begin
      return System.Address (I);
   end To_Address;

end System.Address_Operations;
