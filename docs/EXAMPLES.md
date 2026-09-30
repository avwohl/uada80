# Example programs

## Hello World

```ada
with Ada.Text_IO;

procedure Hello is
begin
   Ada.Text_IO.Put_Line("Hello from Ada on Z80!");
end Hello;
```

## Fibonacci

```ada
procedure Fibonacci is
   A, B, Temp : Integer;
   N : Integer := 10;
begin
   A := 0;
   B := 1;

   for I in 1 .. N loop
      Temp := A + B;
      A := B;
      B := Temp;
   end loop;
end Fibonacci;
```

See [learn-ada-z80](https://github.com/avwohl/learn-ada-z80) for 99 example programs covering basics through tasking, generics, and applications.
