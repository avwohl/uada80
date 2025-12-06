-- Simple Hello World program for uada80
-- Tests basic I/O and string handling

procedure Hello is
   Message : constant String := "Hello from Ada on Z80!";
begin
   -- Print each character
   for I in Message'Range loop
      Put(Message(I));
   end loop;
   New_Line;
end Hello;
