-- Ada.Strings root package for Z80
-- Defines common string manipulation constants and types

package Ada.Strings is
   pragma Pure;

   -- Character to use for padding strings
   Space : constant Character := ' ';

   -- Direction for string operations
   type Alignment  is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction  is (Forward, Backward);
   type Trim_End   is (Left, Right, Both);

   -- Exception for string length errors
   Length_Error : exception;
   Pattern_Error : exception;
   Index_Error : exception;
   Translation_Error : exception;

end Ada.Strings;
