-- Ada.Wide_Characters.Unicode_Names for Z80
-- Unicode character names (stub)

package Ada.Wide_Characters.Unicode_Names is
   pragma Pure;

   -- This package would normally contain constants for Unicode character names
   -- Stub version for Z80 with just a few examples

   Nul                  : constant Wide_Character := Wide_Character'Val (0);
   Start_Of_Heading     : constant Wide_Character := Wide_Character'Val (1);
   Start_Of_Text        : constant Wide_Character := Wide_Character'Val (2);
   End_Of_Text          : constant Wide_Character := Wide_Character'Val (3);
   End_Of_Transmission  : constant Wide_Character := Wide_Character'Val (4);
   Enquiry              : constant Wide_Character := Wide_Character'Val (5);
   Acknowledge          : constant Wide_Character := Wide_Character'Val (6);
   Bell                 : constant Wide_Character := Wide_Character'Val (7);
   Backspace            : constant Wide_Character := Wide_Character'Val (8);
   Character_Tabulation : constant Wide_Character := Wide_Character'Val (9);
   Line_Feed            : constant Wide_Character := Wide_Character'Val (10);
   Line_Tabulation      : constant Wide_Character := Wide_Character'Val (11);
   Form_Feed            : constant Wide_Character := Wide_Character'Val (12);
   Carriage_Return      : constant Wide_Character := Wide_Character'Val (13);
   Space                : constant Wide_Character := Wide_Character'Val (32);
   Delete               : constant Wide_Character := Wide_Character'Val (127);

end Ada.Wide_Characters.Unicode_Names;
