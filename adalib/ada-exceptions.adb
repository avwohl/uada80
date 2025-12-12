-- Ada.Exceptions implementation for Z80
-- Interfaces with runtime exception handling

with System;

package body Ada.Exceptions is

   -- Runtime imports (implemented in Z80 assembly)
   function Rt_Exc_Get_Name (Id : Integer) return System.Address;
   pragma Import (C, Rt_Exc_Get_Name, "_exc_get_name");

   function Rt_Exc_Get_Message return System.Address;
   pragma Import (C, Rt_Exc_Get_Message, "_exc_get_message");

   function Rt_Exc_Get_Info return System.Address;
   pragma Import (C, Rt_Exc_Get_Info, "_exc_get_info");

   procedure Rt_Exc_Raise (Id : Integer; Msg : System.Address);
   pragma Import (C, Rt_Exc_Raise, "_exc_raise");
   pragma No_Return (Rt_Exc_Raise);

   function Rt_Exc_Current return Integer;
   pragma Import (C, Rt_Exc_Current, "_exc_get_current");

   -- Helper: Convert System.Address to String
   function Addr_To_String (Addr : System.Address; Max_Len : Natural := 80) return String is
      Result : String (1 .. Max_Len);
      Len    : Natural := 0;
   begin
      if Addr = System.Null_Address then
         return "";
      end if;
      -- Read length byte at address (Ada string format)
      declare
         Len_Byte : Character;
         for Len_Byte'Address use Addr;
      begin
         Len := Character'Pos (Len_Byte);
         if Len > Max_Len then
            Len := Max_Len;
         end if;
      end;
      -- Copy characters
      for I in 1 .. Len loop
         declare
            C : Character;
            for C'Address use Addr + System.Storage_Elements.Storage_Offset (I);
         begin
            Result (I) := C;
         end;
      end loop;
      return Result (1 .. Len);
   end Addr_To_String;

   -- Exception_Identity
   function Exception_Identity (X : Exception_Occurrence) return Exception_Id is
   begin
      return X.Id;
   end Exception_Identity;

   -- Exception_Name (Id)
   function Exception_Name (Id : Exception_Id) return String is
      Addr : System.Address;
   begin
      if Id = Null_Id then
         return "";
      end if;
      Addr := Rt_Exc_Get_Name (Integer (Id));
      return Addr_To_String (Addr);
   end Exception_Name;

   -- Exception_Name (Occurrence)
   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (X.Id);
   end Exception_Name;

   -- Wide versions (same as String on Z80 - no Unicode support)
   function Wide_Exception_Name (Id : Exception_Id) return Wide_String is
      S : constant String := Exception_Name (Id);
      Result : Wide_String (1 .. S'Length);
   begin
      for I in S'Range loop
         Result (I - S'First + 1) := Wide_Character'Val (Character'Pos (S (I)));
      end loop;
      return Result;
   end Wide_Exception_Name;

   function Wide_Exception_Name (X : Exception_Occurrence) return Wide_String is
   begin
      return Wide_Exception_Name (X.Id);
   end Wide_Exception_Name;

   function Wide_Wide_Exception_Name (Id : Exception_Id) return Wide_Wide_String is
      S : constant String := Exception_Name (Id);
      Result : Wide_Wide_String (1 .. S'Length);
   begin
      for I in S'Range loop
         Result (I - S'First + 1) := Wide_Wide_Character'Val (Character'Pos (S (I)));
      end loop;
      return Result;
   end Wide_Wide_Exception_Name;

   function Wide_Wide_Exception_Name (X : Exception_Occurrence) return Wide_Wide_String is
   begin
      return Wide_Wide_Exception_Name (X.Id);
   end Wide_Wide_Exception_Name;

   -- Exception_Message
   function Exception_Message (X : Exception_Occurrence) return String is
   begin
      if X.Msg_Ptr = System.Null_Address then
         return "";
      end if;
      return Addr_To_String (X.Msg_Ptr, X.Msg_Len);
   end Exception_Message;

   -- Exception_Information
   function Exception_Information (X : Exception_Occurrence) return String is
      Name : constant String := Exception_Name (X);
      Msg  : constant String := Exception_Message (X);
   begin
      if Msg'Length = 0 then
         return Name;
      else
         return Name & ": " & Msg;
      end if;
   end Exception_Information;

   -- Raise_Exception
   procedure Raise_Exception (E : Exception_Id; Message : String := "") is
   begin
      if E /= Null_Id then
         Rt_Exc_Raise (Integer (E), Message'Address);
      end if;
      -- If E is Null_Id, do nothing (as per Ada RM)
   end Raise_Exception;

   procedure Raise_Exception_Always (E : Exception_Id; Message : String := "") is
   begin
      Rt_Exc_Raise (Integer (E), Message'Address);
   end Raise_Exception_Always;

   -- Reraise_Occurrence
   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id /= Null_Id then
         Rt_Exc_Raise (Integer (X.Id), X.Msg_Ptr);
      end if;
   end Reraise_Occurrence;

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence) is
   begin
      Rt_Exc_Raise (Integer (X.Id), X.Msg_Ptr);
   end Reraise_Occurrence_Always;

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence) is
   begin
      Rt_Exc_Raise (Integer (X.Id), X.Msg_Ptr);
   end Reraise_Occurrence_No_Defer;

   -- Save_Occurrence
   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
   is
   begin
      Target := Source;
   end Save_Occurrence;

   function Save_Occurrence
     (Source : Exception_Occurrence) return Exception_Occurrence_Access
   is
      Result : Exception_Occurrence_Access;
   begin
      Result := new Exception_Occurrence;
      Result.all := Source;
      return Result;
   end Save_Occurrence;

end Ada.Exceptions;
