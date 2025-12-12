-- System.Put_Images for Z80
-- Support for 'Put_Image attribute

package System.Put_Images is
   pragma Pure;

   -- Simple output buffer for Put_Image operations
   type Buffer is limited private;

   -- Initialize buffer
   procedure Init (B : out Buffer);

   -- Put a character
   procedure Put (B : in out Buffer; C : Character);

   -- Put a string
   procedure Put (B : in Out Buffer; S : String);

   -- Put an integer
   procedure Put_Integer (B : in Out Buffer; Val : Integer);

   -- Put a Boolean
   procedure Put_Boolean (B : in Out Buffer; Val : Boolean);

   -- Get the current contents
   function Get (B : Buffer) return String;

   -- Get the current length
   function Length (B : Buffer) return Natural;

   -- Buffer capacity
   Max_Buffer_Size : constant := 256;

private
   type Buffer is record
      Data : String (1 .. Max_Buffer_Size);
      Last : Natural := 0;
   end record;

end System.Put_Images;
