-- GNAT.Object_Pool body for Z80
-- Reusable object pool implementation

package body GNAT.Object_Pool is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (P : out Pool; Object_Size : Positive) is
   begin
      P.Objects := (others => (others => 0));
      P.In_Use := (others => False);
      P.Obj_Size := Positive'Min (Object_Size, Max_Object_Size);
      P.Count := 0;
   end Initialize;

   -------------
   -- Acquire --
   -------------

   function Acquire (P : in Out Pool) return Object_Handle is
   begin
      for I in 1 .. Max_Objects loop
         if not P.In_Use (I) then
            P.In_Use (I) := True;
            P.Count := P.Count + 1;
            return Object_Handle (I);
         end if;
      end loop;
      return Null_Handle;
   end Acquire;

   -------------
   -- Release --
   -------------

   procedure Release (P : in Out Pool; H : in Out Object_Handle) is
   begin
      if H /= Null_Handle and then P.In_Use (Positive (H)) then
         P.In_Use (Positive (H)) := False;
         P.Count := P.Count - 1;
      end if;
      H := Null_Handle;
   end Release;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (H : Object_Handle) return Boolean is
   begin
      return H /= Null_Handle;
   end Is_Valid;

   ---------------
   -- Available --
   ---------------

   function Available (P : Pool) return Natural is
   begin
      return Max_Objects - P.Count;
   end Available;

   ------------
   -- In_Use --
   ------------

   function In_Use (P : Pool) return Natural is
   begin
      return P.Count;
   end In_Use;

   -----------------
   -- Object_Size --
   -----------------

   function Object_Size (P : Pool) return Positive is
   begin
      return P.Obj_Size;
   end Object_Size;

   ------------------
   -- Clear_Object --
   ------------------

   procedure Clear_Object (P : in Out Pool; H : Object_Handle) is
   begin
      if H /= Null_Handle then
         for I in 1 .. P.Obj_Size loop
            P.Objects (Positive (H)) (I) := 0;
         end loop;
      end if;
   end Clear_Object;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte (P : in Out Pool; H : Object_Handle;
                         Offset : Natural; Value : Natural)
   is
   begin
      if H /= Null_Handle and then Offset < P.Obj_Size then
         P.Objects (Positive (H)) (Offset + 1) := Value mod 256;
      end if;
   end Write_Byte;

   ---------------
   -- Read_Byte --
   ---------------

   function Read_Byte (P : Pool; H : Object_Handle;
                       Offset : Natural) return Natural
   is
   begin
      if H = Null_Handle or else Offset >= P.Obj_Size then
         return 0;
      end if;
      return P.Objects (Positive (H)) (Offset + 1);
   end Read_Byte;

   ----------------
   -- Write_Word --
   ----------------

   procedure Write_Word (P : in Out Pool; H : Object_Handle;
                         Offset : Natural; Value : Natural)
   is
   begin
      Write_Byte (P, H, Offset, Value mod 256);
      Write_Byte (P, H, Offset + 1, Value / 256);
   end Write_Word;

   ---------------
   -- Read_Word --
   ---------------

   function Read_Word (P : Pool; H : Object_Handle;
                       Offset : Natural) return Natural
   is
   begin
      return Read_Byte (P, H, Offset) + Read_Byte (P, H, Offset + 1) * 256;
   end Read_Word;

end GNAT.Object_Pool;
