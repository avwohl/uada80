-- Standard.ASCII - Character constants package
-- This is an obsolescent package from Ada 83, retained for compatibility

package ASCII is
   pragma Pure;

   -- Control characters
   NUL   : constant Character := Character'Val(0);
   SOH   : constant Character := Character'Val(1);
   STX   : constant Character := Character'Val(2);
   ETX   : constant Character := Character'Val(3);
   EOT   : constant Character := Character'Val(4);
   ENQ   : constant Character := Character'Val(5);
   ACK   : constant Character := Character'Val(6);
   BEL   : constant Character := Character'Val(7);
   BS    : constant Character := Character'Val(8);
   HT    : constant Character := Character'Val(9);
   LF    : constant Character := Character'Val(10);
   VT    : constant Character := Character'Val(11);
   FF    : constant Character := Character'Val(12);
   CR    : constant Character := Character'Val(13);
   SO    : constant Character := Character'Val(14);
   SI    : constant Character := Character'Val(15);
   DLE   : constant Character := Character'Val(16);
   DC1   : constant Character := Character'Val(17);
   DC2   : constant Character := Character'Val(18);
   DC3   : constant Character := Character'Val(19);
   DC4   : constant Character := Character'Val(20);
   NAK   : constant Character := Character'Val(21);
   SYN   : constant Character := Character'Val(22);
   ETB   : constant Character := Character'Val(23);
   CAN   : constant Character := Character'Val(24);
   EM    : constant Character := Character'Val(25);
   SUB   : constant Character := Character'Val(26);
   ESC   : constant Character := Character'Val(27);
   FS    : constant Character := Character'Val(28);
   GS    : constant Character := Character'Val(29);
   RS    : constant Character := Character'Val(30);
   US    : constant Character := Character'Val(31);

   -- Special graphic characters
   Exclam     : constant Character := '!';
   Quotation  : constant Character := '"';
   Sharp      : constant Character := '#';
   Dollar     : constant Character := '$';
   Percent    : constant Character := '%';
   Ampersand  : constant Character := '&';
   Colon      : constant Character := ':';
   Semicolon  : constant Character := ';';
   Query      : constant Character := '?';
   At_Sign    : constant Character := '@';
   L_Bracket  : constant Character := '[';
   Back_Slash : constant Character := '\';
   R_Bracket  : constant Character := ']';
   Circumflex : constant Character := '^';
   Underline  : constant Character := '_';
   Grave      : constant Character := '`';
   L_Brace    : constant Character := '{';
   Bar        : constant Character := '|';
   R_Brace    : constant Character := '}';
   Tilde      : constant Character := '~';

   -- Delete character
   DEL : constant Character := Character'Val(127);

   -- Alternate names
   LC_A : constant Character := 'a';
   LC_B : constant Character := 'b';
   LC_C : constant Character := 'c';
   LC_D : constant Character := 'd';
   LC_E : constant Character := 'e';
   LC_F : constant Character := 'f';
   LC_G : constant Character := 'g';
   LC_H : constant Character := 'h';
   LC_I : constant Character := 'i';
   LC_J : constant Character := 'j';
   LC_K : constant Character := 'k';
   LC_L : constant Character := 'l';
   LC_M : constant Character := 'm';
   LC_N : constant Character := 'n';
   LC_O : constant Character := 'o';
   LC_P : constant Character := 'p';
   LC_Q : constant Character := 'q';
   LC_R : constant Character := 'r';
   LC_S : constant Character := 's';
   LC_T : constant Character := 't';
   LC_U : constant Character := 'u';
   LC_V : constant Character := 'v';
   LC_W : constant Character := 'w';
   LC_X : constant Character := 'x';
   LC_Y : constant Character := 'y';
   LC_Z : constant Character := 'z';

end ASCII;
