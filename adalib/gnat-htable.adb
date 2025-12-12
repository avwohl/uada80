-- GNAT.HTable body for Z80
-- Generic hash table implementation

package body GNAT.HTable is

   package body Static_HTable is
      Table : array (Header_Num) of Elmt_Ptr := (others => Null_Ptr);
      Iterator_Index : Header_Num;
      Iterator_Ptr : Elmt_Ptr;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Table := (others => Null_Ptr);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (E : Elmt_Ptr) is
         Index : constant Header_Num := Hash (Get_Key (E));
      begin
         Set_Next (E, Table (Index));
         Table (Index) := E;
      end Set;

      ---------
      -- Get --
      ---------

      function Get (K : String) return Elmt_Ptr is
         Index : constant Header_Num := Hash (K);
         E : Elmt_Ptr := Table (Index);
      begin
         while E /= Null_Ptr loop
            if Equal (Get_Key (E), K) then
               return E;
            end if;
            E := Next (E);
         end loop;
         return Null_Ptr;
      end Get;

      ------------
      -- Remove --
      ------------

      procedure Remove (K : String) is
         Index : constant Header_Num := Hash (K);
         E : Elmt_Ptr := Table (Index);
         Prev : Elmt_Ptr := Null_Ptr;
      begin
         while E /= Null_Ptr loop
            if Equal (Get_Key (E), K) then
               if Prev = Null_Ptr then
                  Table (Index) := Next (E);
               else
                  Set_Next (Prev, Next (E));
               end if;
               return;
            end if;
            Prev := E;
            E := Next (E);
         end loop;
      end Remove;

      ---------------
      -- Get_First --
      ---------------

      function Get_First return Elmt_Ptr is
      begin
         Iterator_Index := Header_Num'First;
         while Iterator_Index <= Header_Num'Last loop
            if Table (Iterator_Index) /= Null_Ptr then
               Iterator_Ptr := Table (Iterator_Index);
               return Iterator_Ptr;
            end if;
            Iterator_Index := Iterator_Index + 1;
         end loop;
         return Null_Ptr;
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next return Elmt_Ptr is
      begin
         if Iterator_Ptr /= Null_Ptr then
            Iterator_Ptr := Next (Iterator_Ptr);
            if Iterator_Ptr /= Null_Ptr then
               return Iterator_Ptr;
            end if;
         end if;

         Iterator_Index := Iterator_Index + 1;
         while Iterator_Index <= Header_Num'Last loop
            if Table (Iterator_Index) /= Null_Ptr then
               Iterator_Ptr := Table (Iterator_Index);
               return Iterator_Ptr;
            end if;
            Iterator_Index := Iterator_Index + 1;
         end loop;
         return Null_Ptr;
      end Get_Next;

   end Static_HTable;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Natural is
      Result : Natural := 0;
   begin
      for I in Key'Range loop
         Result := Result * 31 + Character'Pos (Key (I));
      end loop;
      return Result;
   end Hash;

end GNAT.HTable;
