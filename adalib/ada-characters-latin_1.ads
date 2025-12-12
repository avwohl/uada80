-- Ada.Characters.Latin_1 for Z80
-- Character constants for Latin-1 character set
--
-- Provides named constants for all Latin-1 characters

package Ada.Characters.Latin_1 is
   pragma Pure;

   ------------------------
   -- Control Characters --
   ------------------------

   NUL : constant Character := Character'Val (0);
   SOH : constant Character := Character'Val (1);
   STX : constant Character := Character'Val (2);
   ETX : constant Character := Character'Val (3);
   EOT : constant Character := Character'Val (4);
   ENQ : constant Character := Character'Val (5);
   ACK : constant Character := Character'Val (6);
   BEL : constant Character := Character'Val (7);
   BS  : constant Character := Character'Val (8);
   HT  : constant Character := Character'Val (9);
   LF  : constant Character := Character'Val (10);
   VT  : constant Character := Character'Val (11);
   FF  : constant Character := Character'Val (12);
   CR  : constant Character := Character'Val (13);
   SO  : constant Character := Character'Val (14);
   SI  : constant Character := Character'Val (15);

   DLE : constant Character := Character'Val (16);
   DC1 : constant Character := Character'Val (17);
   DC2 : constant Character := Character'Val (18);
   DC3 : constant Character := Character'Val (19);
   DC4 : constant Character := Character'Val (20);
   NAK : constant Character := Character'Val (21);
   SYN : constant Character := Character'Val (22);
   ETB : constant Character := Character'Val (23);
   CAN : constant Character := Character'Val (24);
   EM  : constant Character := Character'Val (25);
   SUB : constant Character := Character'Val (26);
   ESC : constant Character := Character'Val (27);
   FS  : constant Character := Character'Val (28);
   GS  : constant Character := Character'Val (29);
   RS  : constant Character := Character'Val (30);
   US  : constant Character := Character'Val (31);

   -------------------------
   -- ISO 646 Characters --
   -------------------------

   Space              : constant Character := ' ';
   Exclamation        : constant Character := '!';
   Quotation          : constant Character := '"';
   Number_Sign        : constant Character := '#';
   Dollar_Sign        : constant Character := '$';
   Percent_Sign       : constant Character := '%';
   Ampersand          : constant Character := '&';
   Apostrophe         : constant Character := ''';
   Left_Parenthesis   : constant Character := '(';
   Right_Parenthesis  : constant Character := ')';
   Asterisk           : constant Character := '*';
   Plus_Sign          : constant Character := '+';
   Comma              : constant Character := ',';
   Hyphen             : constant Character := '-';
   Minus_Sign         : Character renames Hyphen;
   Full_Stop          : constant Character := '.';
   Solidus            : constant Character := '/';

   -- Decimal digits
   Digit_Zero  : constant Character := '0';
   Digit_One   : constant Character := '1';
   Digit_Two   : constant Character := '2';
   Digit_Three : constant Character := '3';
   Digit_Four  : constant Character := '4';
   Digit_Five  : constant Character := '5';
   Digit_Six   : constant Character := '6';
   Digit_Seven : constant Character := '7';
   Digit_Eight : constant Character := '8';
   Digit_Nine  : constant Character := '9';

   Colon          : constant Character := ':';
   Semicolon      : constant Character := ';';
   Less_Than_Sign : constant Character := '<';
   Equals_Sign    : constant Character := '=';
   Greater_Than_Sign : constant Character := '>';
   Question       : constant Character := '?';

   Commercial_At  : constant Character := '@';

   -- Uppercase letters
   UC_A : constant Character := 'A';
   UC_B : constant Character := 'B';
   UC_C : constant Character := 'C';
   UC_D : constant Character := 'D';
   UC_E : constant Character := 'E';
   UC_F : constant Character := 'F';
   UC_G : constant Character := 'G';
   UC_H : constant Character := 'H';
   UC_I : constant Character := 'I';
   UC_J : constant Character := 'J';
   UC_K : constant Character := 'K';
   UC_L : constant Character := 'L';
   UC_M : constant Character := 'M';
   UC_N : constant Character := 'N';
   UC_O : constant Character := 'O';
   UC_P : constant Character := 'P';
   UC_Q : constant Character := 'Q';
   UC_R : constant Character := 'R';
   UC_S : constant Character := 'S';
   UC_T : constant Character := 'T';
   UC_U : constant Character := 'U';
   UC_V : constant Character := 'V';
   UC_W : constant Character := 'W';
   UC_X : constant Character := 'X';
   UC_Y : constant Character := 'Y';
   UC_Z : constant Character := 'Z';

   Left_Square_Bracket  : constant Character := '[';
   Reverse_Solidus      : constant Character := '\';
   Right_Square_Bracket : constant Character := ']';
   Circumflex           : constant Character := '^';
   Low_Line             : constant Character := '_';

   Grave                : constant Character := '`';

   -- Lowercase letters
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

   Left_Curly_Bracket  : constant Character := '{';
   Vertical_Line       : constant Character := '|';
   Right_Curly_Bracket : constant Character := '}';
   Tilde               : constant Character := '~';

   DEL : constant Character := Character'Val (127);

   -------------------------------------------
   -- ISO 6429 Control Characters (C1 set)  --
   -------------------------------------------

   Reserved_128 : constant Character := Character'Val (128);
   Reserved_129 : constant Character := Character'Val (129);
   BPH          : constant Character := Character'Val (130);
   NBH          : constant Character := Character'Val (131);
   Reserved_132 : constant Character := Character'Val (132);
   NEL          : constant Character := Character'Val (133);
   SSA          : constant Character := Character'Val (134);
   ESA          : constant Character := Character'Val (135);
   HTS          : constant Character := Character'Val (136);
   HTJ          : constant Character := Character'Val (137);
   VTS          : constant Character := Character'Val (138);
   PLD          : constant Character := Character'Val (139);
   PLU          : constant Character := Character'Val (140);
   RI           : constant Character := Character'Val (141);
   SS2          : constant Character := Character'Val (142);
   SS3          : constant Character := Character'Val (143);

   DCS          : constant Character := Character'Val (144);
   PU1          : constant Character := Character'Val (145);
   PU2          : constant Character := Character'Val (146);
   STS          : constant Character := Character'Val (147);
   CCH          : constant Character := Character'Val (148);
   MW           : constant Character := Character'Val (149);
   SPA          : constant Character := Character'Val (150);
   EPA          : constant Character := Character'Val (151);
   SOS          : constant Character := Character'Val (152);
   Reserved_153 : constant Character := Character'Val (153);
   SCI          : constant Character := Character'Val (154);
   CSI          : constant Character := Character'Val (155);
   ST           : constant Character := Character'Val (156);
   OSC          : constant Character := Character'Val (157);
   PM           : constant Character := Character'Val (158);
   APC          : constant Character := Character'Val (159);

   ----------------------------
   -- Latin-1 Extension Set  --
   ----------------------------

   No_Break_Space          : constant Character := Character'Val (160);
   NBSP                    : Character renames No_Break_Space;
   Inverted_Exclamation    : constant Character := Character'Val (161);
   Cent_Sign               : constant Character := Character'Val (162);
   Pound_Sign              : constant Character := Character'Val (163);
   Currency_Sign           : constant Character := Character'Val (164);
   Yen_Sign                : constant Character := Character'Val (165);
   Broken_Bar              : constant Character := Character'Val (166);
   Section_Sign            : constant Character := Character'Val (167);
   Diaeresis               : constant Character := Character'Val (168);
   Copyright_Sign          : constant Character := Character'Val (169);
   Feminine_Ordinal_Indicator : constant Character := Character'Val (170);
   Left_Angle_Quotation    : constant Character := Character'Val (171);
   Not_Sign                : constant Character := Character'Val (172);
   Soft_Hyphen             : constant Character := Character'Val (173);
   Registered_Trade_Mark_Sign : constant Character := Character'Val (174);
   Macron                  : constant Character := Character'Val (175);

   Degree_Sign             : constant Character := Character'Val (176);
   Plus_Minus_Sign         : constant Character := Character'Val (177);
   Superscript_Two         : constant Character := Character'Val (178);
   Superscript_Three       : constant Character := Character'Val (179);
   Acute                   : constant Character := Character'Val (180);
   Micro_Sign              : constant Character := Character'Val (181);
   Pilcrow_Sign            : constant Character := Character'Val (182);
   Middle_Dot              : constant Character := Character'Val (183);
   Cedilla                 : constant Character := Character'Val (184);
   Superscript_One         : constant Character := Character'Val (185);
   Masculine_Ordinal_Indicator : constant Character := Character'Val (186);
   Right_Angle_Quotation   : constant Character := Character'Val (187);
   Fraction_One_Quarter    : constant Character := Character'Val (188);
   Fraction_One_Half       : constant Character := Character'Val (189);
   Fraction_Three_Quarters : constant Character := Character'Val (190);
   Inverted_Question       : constant Character := Character'Val (191);

   -- Uppercase letters with diacritics
   UC_A_Grave              : constant Character := Character'Val (192);
   UC_A_Acute              : constant Character := Character'Val (193);
   UC_A_Circumflex         : constant Character := Character'Val (194);
   UC_A_Tilde              : constant Character := Character'Val (195);
   UC_A_Diaeresis          : constant Character := Character'Val (196);
   UC_A_Ring               : constant Character := Character'Val (197);
   UC_AE_Diphthong         : constant Character := Character'Val (198);
   UC_C_Cedilla            : constant Character := Character'Val (199);
   UC_E_Grave              : constant Character := Character'Val (200);
   UC_E_Acute              : constant Character := Character'Val (201);
   UC_E_Circumflex         : constant Character := Character'Val (202);
   UC_E_Diaeresis          : constant Character := Character'Val (203);
   UC_I_Grave              : constant Character := Character'Val (204);
   UC_I_Acute              : constant Character := Character'Val (205);
   UC_I_Circumflex         : constant Character := Character'Val (206);
   UC_I_Diaeresis          : constant Character := Character'Val (207);

   UC_Icelandic_Eth        : constant Character := Character'Val (208);
   UC_N_Tilde              : constant Character := Character'Val (209);
   UC_O_Grave              : constant Character := Character'Val (210);
   UC_O_Acute              : constant Character := Character'Val (211);
   UC_O_Circumflex         : constant Character := Character'Val (212);
   UC_O_Tilde              : constant Character := Character'Val (213);
   UC_O_Diaeresis          : constant Character := Character'Val (214);
   Multiplication_Sign     : constant Character := Character'Val (215);
   UC_O_Oblique_Stroke     : constant Character := Character'Val (216);
   UC_U_Grave              : constant Character := Character'Val (217);
   UC_U_Acute              : constant Character := Character'Val (218);
   UC_U_Circumflex         : constant Character := Character'Val (219);
   UC_U_Diaeresis          : constant Character := Character'Val (220);
   UC_Y_Acute              : constant Character := Character'Val (221);
   UC_Icelandic_Thorn      : constant Character := Character'Val (222);
   LC_German_Sharp_S       : constant Character := Character'Val (223);

   -- Lowercase letters with diacritics
   LC_A_Grave              : constant Character := Character'Val (224);
   LC_A_Acute              : constant Character := Character'Val (225);
   LC_A_Circumflex         : constant Character := Character'Val (226);
   LC_A_Tilde              : constant Character := Character'Val (227);
   LC_A_Diaeresis          : constant Character := Character'Val (228);
   LC_A_Ring               : constant Character := Character'Val (229);
   LC_AE_Diphthong         : constant Character := Character'Val (230);
   LC_C_Cedilla            : constant Character := Character'Val (231);
   LC_E_Grave              : constant Character := Character'Val (232);
   LC_E_Acute              : constant Character := Character'Val (233);
   LC_E_Circumflex         : constant Character := Character'Val (234);
   LC_E_Diaeresis          : constant Character := Character'Val (235);
   LC_I_Grave              : constant Character := Character'Val (236);
   LC_I_Acute              : constant Character := Character'Val (237);
   LC_I_Circumflex         : constant Character := Character'Val (238);
   LC_I_Diaeresis          : constant Character := Character'Val (239);

   LC_Icelandic_Eth        : constant Character := Character'Val (240);
   LC_N_Tilde              : constant Character := Character'Val (241);
   LC_O_Grave              : constant Character := Character'Val (242);
   LC_O_Acute              : constant Character := Character'Val (243);
   LC_O_Circumflex         : constant Character := Character'Val (244);
   LC_O_Tilde              : constant Character := Character'Val (245);
   LC_O_Diaeresis          : constant Character := Character'Val (246);
   Division_Sign           : constant Character := Character'Val (247);
   LC_O_Oblique_Stroke     : constant Character := Character'Val (248);
   LC_U_Grave              : constant Character := Character'Val (249);
   LC_U_Acute              : constant Character := Character'Val (250);
   LC_U_Circumflex         : constant Character := Character'Val (251);
   LC_U_Diaeresis          : constant Character := Character'Val (252);
   LC_Y_Acute              : constant Character := Character'Val (253);
   LC_Icelandic_Thorn      : constant Character := Character'Val (254);
   LC_Y_Diaeresis          : constant Character := Character'Val (255);

end Ada.Characters.Latin_1;
