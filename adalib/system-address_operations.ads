-- System.Address_Operations for Z80
-- Address arithmetic operations

package System.Address_Operations is
   pragma Pure;

   -- Address addition
   function Add (Left : System.Address; Right : Integer) return System.Address;

   -- Address subtraction
   function Sub (Left : System.Address; Right : Integer) return System.Address;

   -- Address difference
   function Diff (Left, Right : System.Address) return Integer;

   -- Address comparison
   function Lt (Left, Right : System.Address) return Boolean;
   function Le (Left, Right : System.Address) return Boolean;
   function Gt (Left, Right : System.Address) return Boolean;
   function Ge (Left, Right : System.Address) return Boolean;
   function Eq (Left, Right : System.Address) return Boolean;
   function Ne (Left, Right : System.Address) return Boolean;

   -- Convert address to integer
   function To_Integer (A : System.Address) return Integer;

   -- Convert integer to address
   function To_Address (I : Integer) return System.Address;

end System.Address_Operations;
