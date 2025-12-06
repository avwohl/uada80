-- Fibonacci sequence calculator
-- Tests loops, arithmetic, and variables

procedure Fibonacci is
   A : Integer := 0;
   B : Integer := 1;
   Temp : Integer;
   N : constant Integer := 10;
begin
   -- Print first N Fibonacci numbers
   for I in 1 .. N loop
      -- Print current value
      Put(A);
      New_Line;

      -- Calculate next Fibonacci number
      Temp := A + B;
      A := B;
      B := Temp;
   end loop;
end Fibonacci;
