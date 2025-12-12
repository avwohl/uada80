-- System.Memory_Set body for Z80
-- Memory set implementation

with System.Storage_Elements;

package body System.Memory_Set is

   use System.Storage_Elements;

   ---------
   -- Set --
   ---------

   procedure Set
     (Target : System.Address;
      Value  : Integer;
      Size   : Natural)
   is
      Dst : Storage_Array (1 .. Storage_Offset (Size));
      for Dst'Address use Target;
      V : constant Storage_Element := Storage_Element (Value mod 256);
   begin
      Dst := (others => V);
   end Set;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Target : System.Address;
      Value  : Natural;
      Count  : Natural)
   is
      type Word is mod 2**16;
      type Word_Array is array (1 .. Count) of Word;

      Dst : Word_Array;
      for Dst'Address use Target;
      V : constant Word := Word (Value mod 65536);
   begin
      Dst := (others => V);
   end Set_Word;

end System.Memory_Set;
