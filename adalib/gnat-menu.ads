-- GNAT.Menu for Z80
-- Simple text-based menu system

package GNAT.Menu is
   pragma Preelaborate;

   Max_Items     : constant := 16;
   Max_Item_Len  : constant := 32;
   Max_Title_Len : constant := 40;

   type Menu_Item is record
      Label   : String (1 .. Max_Item_Len);
      Len     : Natural;
      Key     : Character;  -- Shortcut key (or ' ' for none)
      Enabled : Boolean;
   end record;

   type Menu_Array is array (1 .. Max_Items) of Menu_Item;

   type Menu is limited private;

   -- Initialize menu with title
   procedure Initialize (M : out Menu; Title : String := "");

   -- Set menu title
   procedure Set_Title (M : in Out Menu; Title : String);

   -- Add menu item
   procedure Add_Item (M : in Out Menu; Label : String; Key : Character := ' ');

   -- Add item with enabled flag
   procedure Add_Item (M : in Out Menu; Label : String; Key : Character;
                       Enabled : Boolean);

   -- Clear all items
   procedure Clear_Items (M : out Menu);

   -- Enable/disable item
   procedure Set_Enabled (M : in Out Menu; Index : Positive; Enabled : Boolean);

   -- Get item count
   function Item_Count (M : Menu) return Natural;

   -- Get menu title
   function Get_Title (M : Menu) return String;

   -- Get item label
   function Get_Label (M : Menu; Index : Positive) return String;

   -- Get item key
   function Get_Key (M : Menu; Index : Positive) return Character;

   -- Check if item is enabled
   function Is_Enabled (M : Menu; Index : Positive) return Boolean;

   -- Find item by key (returns 0 if not found)
   function Find_By_Key (M : Menu; Key : Character) return Natural;

   -- Rendering functions (return strings for display)

   -- Get border line
   function Top_Border (M : Menu) return String;
   function Bottom_Border (M : Menu) return String;

   -- Get title line (centered)
   function Title_Line (M : Menu) return String;

   -- Get separator line
   function Separator (M : Menu) return String;

   -- Get formatted menu item line
   function Item_Line (M : Menu; Index : Positive) return String;

   -- Get prompt line
   function Prompt_Line (M : Menu; Prompt : String := "Choice: ") return String;

   -- Calculate menu width
   function Menu_Width (M : Menu) return Natural;

   -- Full menu as multi-line string (lines separated by LF)
   function Render (M : Menu) return String;

   -- Selection result type
   type Selection_Result is record
      Index   : Natural;  -- 0 if cancelled/invalid
      Key     : Character;
      Valid   : Boolean;
   end record;

   -- Parse user input to get selection
   function Parse_Selection (M : Menu; Input : String) return Selection_Result;

   -- Check if input is valid for this menu
   function Is_Valid_Selection (M : Menu; Input : String) return Boolean;

private

   type Menu is limited record
      Items     : Menu_Array;
      Count     : Natural := 0;
      Title     : String (1 .. Max_Title_Len);
      Title_Len : Natural := 0;
   end record;

end GNAT.Menu;
