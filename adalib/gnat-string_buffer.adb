-- GNAT.String_Buffer body for Z80
-- Efficient string buffer implementation

package body GNAT.String_Buffer is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (B : out Buffer; Capacity : Positive := Default_Capacity) is
   begin
      B.Length := 0;
      if Capacity > Max_Buffer then
         B.Capacity := Max_Buffer;
      else
         B.Capacity := Capacity;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (B : in Out Buffer) is
   begin
      B.Length := 0;
      B.Capacity := 0;
   end Finalize;

   ------------
   -- Append --
   ------------

   procedure Append (B : in Out Buffer; S : String) is
   begin
      if B.Length + S'Length <= B.Capacity then
         B.Data (B.Length + 1 .. B.Length + S'Length) := S;
         B.Length := B.Length + S'Length;
      else
         -- Truncate if buffer full
         declare
            Space : constant Natural := B.Capacity - B.Length;
         begin
            if Space > 0 then
               B.Data (B.Length + 1 .. B.Capacity) := S (S'First .. S'First + Space - 1);
               B.Length := B.Capacity;
            end if;
         end;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (B : in Out Buffer; C : Character) is
   begin
      if B.Length < B.Capacity then
         B.Length := B.Length + 1;
         B.Data (B.Length) := C;
      end if;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (B : in Out Buffer) is
   begin
      B.Length := 0;
   end Clear;

   ------------
   -- Length --
   ------------

   function Length (B : Buffer) return Natural is
   begin
      return B.Length;
   end Length;

   --------------
   -- Capacity --
   --------------

   function Capacity (B : Buffer) return Natural is
   begin
      return B.Capacity;
   end Capacity;

   ---------------
   -- To_String --
   ---------------

   function To_String (B : Buffer) return String is
   begin
      return B.Data (1 .. B.Length);
   end To_String;

   -------------
   -- Element --
   -------------

   function Element (B : Buffer; Index : Positive) return Character is
   begin
      if Index <= B.Length then
         return B.Data (Index);
      else
         raise Constraint_Error;
      end if;
   end Element;

end GNAT.String_Buffer;
