-- Ada.Numerics root package for Z80
-- Provides numeric constants and exceptions

package Ada.Numerics is
   pragma Pure;

   -- Mathematical constant for argument errors
   Argument_Error : exception;

   -- Pi constant (approximation suitable for 48-bit float)
   Pi : constant := 3.14159_26535_89793;

   -- e constant
   e : constant := 2.71828_18284_59045;

end Ada.Numerics;
