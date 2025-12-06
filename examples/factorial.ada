-- Factorial calculator using recursion
-- Tests function calls and recursion

function Factorial(N : Integer) return Integer is
begin
   if N <= 1 then
      return 1;
   else
      return N * Factorial(N - 1);
   end if;
end Factorial;

-- Main program
procedure Test_Factorial is
   Result : Integer;
begin
   Result := Factorial(5);
   Put(Result);  -- Should print 120
   New_Line;
end Test_Factorial;
