-- System.Memory_Copy body for Z80
-- Memory copy implementation

with System.Storage_Elements;

package body System.Memory_Copy is

   use System.Storage_Elements;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Target : System.Address;
      Source : System.Address;
      Size   : Natural)
   is
      Src : Storage_Array (1 .. Storage_Offset (Size));
      for Src'Address use Source;

      Dst : Storage_Array (1 .. Storage_Offset (Size));
      for Dst'Address use Target;
   begin
      Dst := Src;
   end Copy;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : System.Address;
      Source : System.Address;
      Size   : Natural)
   is
      use type System.Address;

      Src : Storage_Array (1 .. Storage_Offset (Size));
      for Src'Address use Source;

      Dst : Storage_Array (1 .. Storage_Offset (Size));
      for Dst'Address use Target;
   begin
      if Size = 0 then
         return;
      end if;

      -- Handle overlap
      if Target > Source and Target < Source + Storage_Offset (Size) then
         -- Copy backwards
         for I in reverse 1 .. Storage_Offset (Size) loop
            Dst (I) := Src (I);
         end loop;
      else
         -- Copy forwards
         Dst := Src;
      end if;
   end Move;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Target : System.Address;
      Value  : Natural;
      Size   : Natural)
   is
      Dst : Storage_Array (1 .. Storage_Offset (Size));
      for Dst'Address use Target;
   begin
      Dst := (others => Storage_Element (Value mod 256));
   end Fill;

   ----------
   -- Zero --
   ----------

   procedure Zero
     (Target : System.Address;
      Size   : Natural)
   is
   begin
      Fill (Target, 0, Size);
   end Zero;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left   : System.Address;
      Right  : System.Address;
      Size   : Natural) return Integer
   is
      L : Storage_Array (1 .. Storage_Offset (Size));
      for L'Address use Left;

      R : Storage_Array (1 .. Storage_Offset (Size));
      for R'Address use Right;
   begin
      for I in 1 .. Storage_Offset (Size) loop
         if L (I) < R (I) then
            return -1;
         elsif L (I) > R (I) then
            return 1;
         end if;
      end loop;
      return 0;
   end Compare;

end System.Memory_Copy;
