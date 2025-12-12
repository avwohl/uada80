-- GNAT.Ring_Buffer body for Z80
-- Generic circular buffer implementation

package body GNAT.Ring_Buffer is

   package body Bounded is

      -----------
      -- Clear --
      -----------

      procedure Clear (B : out Ring_Buffer) is
      begin
         B.Read_Idx := 0;
         B.Write_Idx := 0;
         B.Count := 0;
      end Clear;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (B : Ring_Buffer) return Boolean is
      begin
         return B.Count = 0;
      end Is_Empty;

      -------------
      -- Is_Full --
      -------------

      function Is_Full (B : Ring_Buffer) return Boolean is
      begin
         return B.Count >= Max_Size;
      end Is_Full;

      ----------
      -- Size --
      ----------

      function Size (B : Ring_Buffer) return Natural is
      begin
         return B.Count;
      end Size;

      --------------
      -- Capacity --
      --------------

      function Capacity (B : Ring_Buffer) return Natural is
         pragma Unreferenced (B);
      begin
         return Max_Size;
      end Capacity;

      -----------
      -- Write --
      -----------

      procedure Write (B : in Out Ring_Buffer; Item : Element_Type) is
      begin
         B.Data (B.Write_Idx) := Item;
         B.Write_Idx := (B.Write_Idx + 1) mod Max_Size;

         if B.Count < Max_Size then
            B.Count := B.Count + 1;
         else
            -- Overwrite oldest - move read pointer
            B.Read_Idx := (B.Read_Idx + 1) mod Max_Size;
         end if;
      end Write;

      ----------
      -- Read --
      ----------

      procedure Read (B : in Out Ring_Buffer; Item : out Element_Type) is
      begin
         if B.Count = 0 then
            raise Constraint_Error with "buffer is empty";
         end if;

         Item := B.Data (B.Read_Idx);
         B.Read_Idx := (B.Read_Idx + 1) mod Max_Size;
         B.Count := B.Count - 1;
      end Read;

      ----------
      -- Peek --
      ----------

      function Peek (B : Ring_Buffer; Index : Positive := 1) return Element_Type is
      begin
         if Index > B.Count then
            raise Constraint_Error with "index out of range";
         end if;
         return B.Data ((B.Read_Idx + Index - 1) mod Max_Size);
      end Peek;

   end Bounded;

end GNAT.Ring_Buffer;
