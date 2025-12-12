-- System.Scalar_Values for Z80
-- Invalid scalar value checking support

package System.Scalar_Values is
   pragma Pure;

   -- Type for invalid value markers
   type Invalid_Value_Type is mod 2 ** 8;

   -- Invalid values for various scalar sizes
   Invalid_8_Value  : constant Invalid_Value_Type := 16#81#;
   Invalid_16_Value : constant := 16#8181#;
   Invalid_32_Value : constant := 16#8181_8181#;

   -- Check if value might be invalid
   function Is_Invalid_8 (Val : Invalid_Value_Type) return Boolean;

   -- Mark for invalid enumeration (typically first invalid value)
   Invalid_Enum_Value : constant := -1;

   -- Initialize scalar to invalid value
   procedure Set_Invalid_8 (Addr : System.Address);
   procedure Set_Invalid_16 (Addr : System.Address);

end System.Scalar_Values;
