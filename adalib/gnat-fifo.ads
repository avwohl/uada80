-- GNAT.FIFO for Z80
-- First-In-First-Out queue implementations

package GNAT.FIFO is
   pragma Preelaborate;

   -- Byte FIFO (64 bytes)
   Max_Byte_FIFO : constant := 64;

   type Byte_FIFO is limited private;

   procedure Init_Byte_FIFO (Q : out Byte_FIFO);
   procedure Enqueue_Byte (Q : in Out Byte_FIFO; Value : Natural);
   function Dequeue_Byte (Q : in Out Byte_FIFO) return Natural;
   function Peek_Byte (Q : Byte_FIFO) return Natural;
   function Byte_FIFO_Count (Q : Byte_FIFO) return Natural;
   function Byte_FIFO_Is_Empty (Q : Byte_FIFO) return Boolean;
   function Byte_FIFO_Is_Full (Q : Byte_FIFO) return Boolean;
   procedure Clear_Byte_FIFO (Q : out Byte_FIFO);
   function Byte_FIFO_Free (Q : Byte_FIFO) return Natural;

   -- Integer FIFO (32 integers)
   Max_Int_FIFO : constant := 32;

   type Int_FIFO is limited private;

   procedure Init_Int_FIFO (Q : out Int_FIFO);
   procedure Enqueue_Int (Q : in Out Int_FIFO; Value : Integer);
   function Dequeue_Int (Q : in Out Int_FIFO) return Integer;
   function Peek_Int (Q : Int_FIFO) return Integer;
   function Int_FIFO_Count (Q : Int_FIFO) return Natural;
   function Int_FIFO_Is_Empty (Q : Int_FIFO) return Boolean;
   function Int_FIFO_Is_Full (Q : Int_FIFO) return Boolean;
   procedure Clear_Int_FIFO (Q : out Int_FIFO);

   -- Character FIFO (128 characters)
   Max_Char_FIFO : constant := 128;

   type Char_FIFO is limited private;

   procedure Init_Char_FIFO (Q : out Char_FIFO);
   procedure Enqueue_Char (Q : in Out Char_FIFO; C : Character);
   function Dequeue_Char (Q : in Out Char_FIFO) return Character;
   function Peek_Char (Q : Char_FIFO) return Character;
   function Char_FIFO_Count (Q : Char_FIFO) return Natural;
   function Char_FIFO_Is_Empty (Q : Char_FIFO) return Boolean;
   function Char_FIFO_Is_Full (Q : Char_FIFO) return Boolean;
   procedure Clear_Char_FIFO (Q : out Char_FIFO);

   -- String helpers for Char FIFO
   procedure Enqueue_String (Q : in Out Char_FIFO; S : String);
   function Dequeue_String (Q : in Out Char_FIFO; Max_Len : Positive) return String;
   function Dequeue_Line (Q : in Out Char_FIFO) return String;
   function Has_Line (Q : Char_FIFO) return Boolean;

   -- Message FIFO (8 messages, 32 bytes each)
   Max_Messages : constant := 8;
   Max_Msg_Length : constant := 32;

   type Message_FIFO is limited private;

   procedure Init_Message_FIFO (Q : out Message_FIFO);
   procedure Enqueue_Message (Q : in Out Message_FIFO; Msg : String);
   function Dequeue_Message (Q : in Out Message_FIFO) return String;
   function Peek_Message (Q : Message_FIFO) return String;
   function Message_FIFO_Count (Q : Message_FIFO) return Natural;
   function Message_FIFO_Is_Empty (Q : Message_FIFO) return Boolean;
   function Message_FIFO_Is_Full (Q : Message_FIFO) return Boolean;
   procedure Clear_Message_FIFO (Q : out Message_FIFO);

   -- Priority FIFO (items with priority, higher = first)
   Max_Priority_Items : constant := 16;

   type Priority_FIFO is limited private;

   procedure Init_Priority_FIFO (Q : out Priority_FIFO);
   procedure Enqueue_Priority (Q : in Out Priority_FIFO;
                               Value : Integer; Priority : Natural);
   function Dequeue_Priority (Q : in Out Priority_FIFO) return Integer;
   function Peek_Priority (Q : Priority_FIFO) return Integer;
   function Peek_Top_Priority (Q : Priority_FIFO) return Natural;
   function Priority_FIFO_Count (Q : Priority_FIFO) return Natural;
   function Priority_FIFO_Is_Empty (Q : Priority_FIFO) return Boolean;
   function Priority_FIFO_Is_Full (Q : Priority_FIFO) return Boolean;
   procedure Clear_Priority_FIFO (Q : out Priority_FIFO);

   -- Bulk operations
   procedure Enqueue_Bytes (Q : in Out Byte_FIFO; Data : String);
   function Dequeue_Bytes (Q : in Out Byte_FIFO; Count : Positive) return String;

private

   subtype Byte is Natural range 0 .. 255;

   type Byte_Array is array (0 .. Max_Byte_FIFO - 1) of Byte;
   type Byte_FIFO is limited record
      Data  : Byte_Array;
      Head  : Natural := 0;
      Tail  : Natural := 0;
      Count : Natural := 0;
   end record;

   type Int_Array is array (0 .. Max_Int_FIFO - 1) of Integer;
   type Int_FIFO is limited record
      Data  : Int_Array;
      Head  : Natural := 0;
      Tail  : Natural := 0;
      Count : Natural := 0;
   end record;

   type Char_Array is array (0 .. Max_Char_FIFO - 1) of Character;
   type Char_FIFO is limited record
      Data  : Char_Array;
      Head  : Natural := 0;
      Tail  : Natural := 0;
      Count : Natural := 0;
   end record;

   type Message_Record is record
      Text   : String (1 .. Max_Msg_Length);
      Length : Natural;
   end record;
   type Message_Array is array (0 .. Max_Messages - 1) of Message_Record;
   type Message_FIFO is limited record
      Data  : Message_Array;
      Head  : Natural := 0;
      Tail  : Natural := 0;
      Count : Natural := 0;
   end record;

   type Priority_Record is record
      Value    : Integer;
      Priority : Natural;
      Used     : Boolean;
   end record;
   type Priority_Array is array (1 .. Max_Priority_Items) of Priority_Record;
   type Priority_FIFO is limited record
      Data  : Priority_Array;
      Count : Natural := 0;
   end record;

end GNAT.FIFO;
