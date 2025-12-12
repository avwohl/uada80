-- GNAT.FIFO body for Z80
-- FIFO queue implementations

package body GNAT.FIFO is

   ---------------
   -- Byte FIFO --
   ---------------

   procedure Init_Byte_FIFO (Q : out Byte_FIFO) is
   begin
      Q.Head := 0;
      Q.Tail := 0;
      Q.Count := 0;
   end Init_Byte_FIFO;

   procedure Enqueue_Byte (Q : in Out Byte_FIFO; Value : Natural) is
   begin
      if Q.Count < Max_Byte_FIFO then
         Q.Data (Q.Tail) := Value mod 256;
         Q.Tail := (Q.Tail + 1) mod Max_Byte_FIFO;
         Q.Count := Q.Count + 1;
      end if;
   end Enqueue_Byte;

   function Dequeue_Byte (Q : in Out Byte_FIFO) return Natural is
      Result : Natural;
   begin
      if Q.Count = 0 then
         return 0;
      end if;

      Result := Q.Data (Q.Head);
      Q.Head := (Q.Head + 1) mod Max_Byte_FIFO;
      Q.Count := Q.Count - 1;
      return Result;
   end Dequeue_Byte;

   function Peek_Byte (Q : Byte_FIFO) return Natural is
   begin
      if Q.Count = 0 then
         return 0;
      end if;
      return Q.Data (Q.Head);
   end Peek_Byte;

   function Byte_FIFO_Count (Q : Byte_FIFO) return Natural is
   begin
      return Q.Count;
   end Byte_FIFO_Count;

   function Byte_FIFO_Is_Empty (Q : Byte_FIFO) return Boolean is
   begin
      return Q.Count = 0;
   end Byte_FIFO_Is_Empty;

   function Byte_FIFO_Is_Full (Q : Byte_FIFO) return Boolean is
   begin
      return Q.Count = Max_Byte_FIFO;
   end Byte_FIFO_Is_Full;

   procedure Clear_Byte_FIFO (Q : out Byte_FIFO) is
   begin
      Init_Byte_FIFO (Q);
   end Clear_Byte_FIFO;

   function Byte_FIFO_Free (Q : Byte_FIFO) return Natural is
   begin
      return Max_Byte_FIFO - Q.Count;
   end Byte_FIFO_Free;

   --------------
   -- Int FIFO --
   --------------

   procedure Init_Int_FIFO (Q : out Int_FIFO) is
   begin
      Q.Head := 0;
      Q.Tail := 0;
      Q.Count := 0;
   end Init_Int_FIFO;

   procedure Enqueue_Int (Q : in Out Int_FIFO; Value : Integer) is
   begin
      if Q.Count < Max_Int_FIFO then
         Q.Data (Q.Tail) := Value;
         Q.Tail := (Q.Tail + 1) mod Max_Int_FIFO;
         Q.Count := Q.Count + 1;
      end if;
   end Enqueue_Int;

   function Dequeue_Int (Q : in Out Int_FIFO) return Integer is
      Result : Integer;
   begin
      if Q.Count = 0 then
         return 0;
      end if;

      Result := Q.Data (Q.Head);
      Q.Head := (Q.Head + 1) mod Max_Int_FIFO;
      Q.Count := Q.Count - 1;
      return Result;
   end Dequeue_Int;

   function Peek_Int (Q : Int_FIFO) return Integer is
   begin
      if Q.Count = 0 then
         return 0;
      end if;
      return Q.Data (Q.Head);
   end Peek_Int;

   function Int_FIFO_Count (Q : Int_FIFO) return Natural is
   begin
      return Q.Count;
   end Int_FIFO_Count;

   function Int_FIFO_Is_Empty (Q : Int_FIFO) return Boolean is
   begin
      return Q.Count = 0;
   end Int_FIFO_Is_Empty;

   function Int_FIFO_Is_Full (Q : Int_FIFO) return Boolean is
   begin
      return Q.Count = Max_Int_FIFO;
   end Int_FIFO_Is_Full;

   procedure Clear_Int_FIFO (Q : out Int_FIFO) is
   begin
      Init_Int_FIFO (Q);
   end Clear_Int_FIFO;

   ---------------
   -- Char FIFO --
   ---------------

   procedure Init_Char_FIFO (Q : out Char_FIFO) is
   begin
      Q.Head := 0;
      Q.Tail := 0;
      Q.Count := 0;
   end Init_Char_FIFO;

   procedure Enqueue_Char (Q : in Out Char_FIFO; C : Character) is
   begin
      if Q.Count < Max_Char_FIFO then
         Q.Data (Q.Tail) := C;
         Q.Tail := (Q.Tail + 1) mod Max_Char_FIFO;
         Q.Count := Q.Count + 1;
      end if;
   end Enqueue_Char;

   function Dequeue_Char (Q : in Out Char_FIFO) return Character is
      Result : Character;
   begin
      if Q.Count = 0 then
         return Character'Val (0);
      end if;

      Result := Q.Data (Q.Head);
      Q.Head := (Q.Head + 1) mod Max_Char_FIFO;
      Q.Count := Q.Count - 1;
      return Result;
   end Dequeue_Char;

   function Peek_Char (Q : Char_FIFO) return Character is
   begin
      if Q.Count = 0 then
         return Character'Val (0);
      end if;
      return Q.Data (Q.Head);
   end Peek_Char;

   function Char_FIFO_Count (Q : Char_FIFO) return Natural is
   begin
      return Q.Count;
   end Char_FIFO_Count;

   function Char_FIFO_Is_Empty (Q : Char_FIFO) return Boolean is
   begin
      return Q.Count = 0;
   end Char_FIFO_Is_Empty;

   function Char_FIFO_Is_Full (Q : Char_FIFO) return Boolean is
   begin
      return Q.Count = Max_Char_FIFO;
   end Char_FIFO_Is_Full;

   procedure Clear_Char_FIFO (Q : out Char_FIFO) is
   begin
      Init_Char_FIFO (Q);
   end Clear_Char_FIFO;

   procedure Enqueue_String (Q : in Out Char_FIFO; S : String) is
   begin
      for C of S loop
         exit when Char_FIFO_Is_Full (Q);
         Enqueue_Char (Q, C);
      end loop;
   end Enqueue_String;

   function Dequeue_String (Q : in Out Char_FIFO; Max_Len : Positive) return String is
      Result : String (1 .. Max_Len);
      Len    : Natural := 0;
   begin
      while Len < Max_Len and not Char_FIFO_Is_Empty (Q) loop
         Len := Len + 1;
         Result (Len) := Dequeue_Char (Q);
      end loop;
      return Result (1 .. Len);
   end Dequeue_String;

   function Dequeue_Line (Q : in Out Char_FIFO) return String is
      Result : String (1 .. 128);
      Len    : Natural := 0;
      C      : Character;
   begin
      while Len < 128 and not Char_FIFO_Is_Empty (Q) loop
         C := Dequeue_Char (Q);
         if C = Character'Val (10) or C = Character'Val (13) then
            exit;
         end if;
         Len := Len + 1;
         Result (Len) := C;
      end loop;
      return Result (1 .. Len);
   end Dequeue_Line;

   function Has_Line (Q : Char_FIFO) return Boolean is
      Pos : Natural := Q.Head;
   begin
      for I in 1 .. Q.Count loop
         if Q.Data (Pos) = Character'Val (10) or
            Q.Data (Pos) = Character'Val (13) then
            return True;
         end if;
         Pos := (Pos + 1) mod Max_Char_FIFO;
      end loop;
      return False;
   end Has_Line;

   ------------------
   -- Message FIFO --
   ------------------

   procedure Init_Message_FIFO (Q : out Message_FIFO) is
   begin
      Q.Head := 0;
      Q.Tail := 0;
      Q.Count := 0;
   end Init_Message_FIFO;

   procedure Enqueue_Message (Q : in Out Message_FIFO; Msg : String) is
      Len : Natural;
   begin
      if Q.Count >= Max_Messages then
         return;
      end if;

      Q.Data (Q.Tail).Text := (others => ' ');
      Len := Msg'Length;
      if Len > Max_Msg_Length then
         Len := Max_Msg_Length;
      end if;
      Q.Data (Q.Tail).Length := Len;

      for I in 1 .. Len loop
         Q.Data (Q.Tail).Text (I) := Msg (Msg'First + I - 1);
      end loop;

      Q.Tail := (Q.Tail + 1) mod Max_Messages;
      Q.Count := Q.Count + 1;
   end Enqueue_Message;

   function Dequeue_Message (Q : in Out Message_FIFO) return String is
      Len : Natural;
   begin
      if Q.Count = 0 then
         return "";
      end if;

      Len := Q.Data (Q.Head).Length;
      declare
         Result : constant String := Q.Data (Q.Head).Text (1 .. Len);
      begin
         Q.Head := (Q.Head + 1) mod Max_Messages;
         Q.Count := Q.Count - 1;
         return Result;
      end;
   end Dequeue_Message;

   function Peek_Message (Q : Message_FIFO) return String is
   begin
      if Q.Count = 0 then
         return "";
      end if;
      return Q.Data (Q.Head).Text (1 .. Q.Data (Q.Head).Length);
   end Peek_Message;

   function Message_FIFO_Count (Q : Message_FIFO) return Natural is
   begin
      return Q.Count;
   end Message_FIFO_Count;

   function Message_FIFO_Is_Empty (Q : Message_FIFO) return Boolean is
   begin
      return Q.Count = 0;
   end Message_FIFO_Is_Empty;

   function Message_FIFO_Is_Full (Q : Message_FIFO) return Boolean is
   begin
      return Q.Count = Max_Messages;
   end Message_FIFO_Is_Full;

   procedure Clear_Message_FIFO (Q : out Message_FIFO) is
   begin
      Init_Message_FIFO (Q);
   end Clear_Message_FIFO;

   -------------------
   -- Priority FIFO --
   -------------------

   procedure Init_Priority_FIFO (Q : out Priority_FIFO) is
   begin
      Q.Count := 0;
      for I in 1 .. Max_Priority_Items loop
         Q.Data (I).Used := False;
      end loop;
   end Init_Priority_FIFO;

   procedure Enqueue_Priority (Q : in Out Priority_FIFO;
                               Value : Integer; Priority : Natural) is
   begin
      if Q.Count >= Max_Priority_Items then
         return;
      end if;

      -- Find free slot
      for I in 1 .. Max_Priority_Items loop
         if not Q.Data (I).Used then
            Q.Data (I).Value := Value;
            Q.Data (I).Priority := Priority;
            Q.Data (I).Used := True;
            Q.Count := Q.Count + 1;
            return;
         end if;
      end loop;
   end Enqueue_Priority;

   function Dequeue_Priority (Q : in Out Priority_FIFO) return Integer is
      Best_Idx : Natural := 0;
      Best_Pri : Natural := 0;
      Result   : Integer;
   begin
      if Q.Count = 0 then
         return 0;
      end if;

      -- Find highest priority
      for I in 1 .. Max_Priority_Items loop
         if Q.Data (I).Used then
            if Best_Idx = 0 or Q.Data (I).Priority > Best_Pri then
               Best_Idx := I;
               Best_Pri := Q.Data (I).Priority;
            end if;
         end if;
      end loop;

      if Best_Idx > 0 then
         Result := Q.Data (Best_Idx).Value;
         Q.Data (Best_Idx).Used := False;
         Q.Count := Q.Count - 1;
         return Result;
      end if;

      return 0;
   end Dequeue_Priority;

   function Peek_Priority (Q : Priority_FIFO) return Integer is
      Best_Idx : Natural := 0;
      Best_Pri : Natural := 0;
   begin
      if Q.Count = 0 then
         return 0;
      end if;

      for I in 1 .. Max_Priority_Items loop
         if Q.Data (I).Used then
            if Best_Idx = 0 or Q.Data (I).Priority > Best_Pri then
               Best_Idx := I;
               Best_Pri := Q.Data (I).Priority;
            end if;
         end if;
      end loop;

      if Best_Idx > 0 then
         return Q.Data (Best_Idx).Value;
      end if;
      return 0;
   end Peek_Priority;

   function Peek_Top_Priority (Q : Priority_FIFO) return Natural is
      Best_Pri : Natural := 0;
   begin
      for I in 1 .. Max_Priority_Items loop
         if Q.Data (I).Used and Q.Data (I).Priority > Best_Pri then
            Best_Pri := Q.Data (I).Priority;
         end if;
      end loop;
      return Best_Pri;
   end Peek_Top_Priority;

   function Priority_FIFO_Count (Q : Priority_FIFO) return Natural is
   begin
      return Q.Count;
   end Priority_FIFO_Count;

   function Priority_FIFO_Is_Empty (Q : Priority_FIFO) return Boolean is
   begin
      return Q.Count = 0;
   end Priority_FIFO_Is_Empty;

   function Priority_FIFO_Is_Full (Q : Priority_FIFO) return Boolean is
   begin
      return Q.Count = Max_Priority_Items;
   end Priority_FIFO_Is_Full;

   procedure Clear_Priority_FIFO (Q : out Priority_FIFO) is
   begin
      Init_Priority_FIFO (Q);
   end Clear_Priority_FIFO;

   ---------------------
   -- Bulk Operations --
   ---------------------

   procedure Enqueue_Bytes (Q : in Out Byte_FIFO; Data : String) is
   begin
      for C of Data loop
         exit when Byte_FIFO_Is_Full (Q);
         Enqueue_Byte (Q, Character'Pos (C));
      end loop;
   end Enqueue_Bytes;

   function Dequeue_Bytes (Q : in Out Byte_FIFO; Count : Positive) return String is
      Result : String (1 .. Count);
      Len    : Natural := 0;
   begin
      while Len < Count and not Byte_FIFO_Is_Empty (Q) loop
         Len := Len + 1;
         Result (Len) := Character'Val (Dequeue_Byte (Q));
      end loop;
      return Result (1 .. Len);
   end Dequeue_Bytes;

end GNAT.FIFO;
