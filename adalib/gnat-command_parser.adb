-- GNAT.Command_Parser body for Z80
-- Simple command line argument parser implementation

package body GNAT.Command_Parser is

   procedure Set_Arg (A : out Argument_Entry; S : String) is
      Len : constant Natural := Natural'Min (S'Length, Max_Arg_Length);
   begin
      A.Data := (others => ' ');
      for I in 1 .. Len loop
         A.Data (I) := S (S'First + I - 1);
      end loop;
      A.Length := Len;
   end Set_Arg;

   function Arg_To_String (A : Argument_Entry) return String is
   begin
      return String (A.Data (1 .. A.Length));
   end Arg_To_String;

   function Match_Long (O : Option_Entry; S : String) return Boolean is
   begin
      if O.Long_Len /= S'Length then
         return False;
      end if;
      for I in 1 .. O.Long_Len loop
         if O.Long (I) /= S (S'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Match_Long;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (P : out Parser) is
   begin
      P.Args := (others => (Data => (others => ' '),
                            Length => 0,
                            Is_Option => False));
      P.Arg_Count := 0;
      P.Options := (others => (Short => ASCII.NUL,
                               Long => (others => ' '),
                               Long_Len => 0,
                               Has_Value => False,
                               Present => False,
                               Value => (others => ' '),
                               Value_Len => 0));
      P.Opt_Count := 0;
      P.Has_Err := False;
      P.Err_Msg := (others => ' ');
      P.Err_Len := 0;
   end Initialize;

   -----------
   -- Parse --
   -----------

   procedure Parse (P : in Out Parser; Command_Line : String) is
      I         : Positive := Command_Line'First;
      Arg_Start : Natural := 0;
      In_Arg    : Boolean := False;
      In_Quote  : Boolean := False;
   begin
      P.Arg_Count := 0;

      while I <= Command_Line'Last loop
         declare
            C : constant Character := Command_Line (I);
         begin
            if C = '"' then
               In_Quote := not In_Quote;
               if not In_Arg then
                  In_Arg := True;
                  Arg_Start := I + 1;
               end if;
            elsif (C = ' ' or C = ASCII.HT) and not In_Quote then
               if In_Arg then
                  -- End of argument
                  if P.Arg_Count < Max_Arguments then
                     Set_Arg (P.Args (P.Arg_Count),
                              Command_Line (Arg_Start .. I - 1));
                     P.Arg_Count := P.Arg_Count + 1;
                  end if;
                  In_Arg := False;
               end if;
            else
               if not In_Arg then
                  In_Arg := True;
                  Arg_Start := I;
               end if;
            end if;
         end;
         I := I + 1;
      end loop;

      -- Handle last argument
      if In_Arg and P.Arg_Count < Max_Arguments then
         Set_Arg (P.Args (P.Arg_Count),
                  Command_Line (Arg_Start .. Command_Line'Last));
         P.Arg_Count := P.Arg_Count + 1;
      end if;

      -- Process options
      for A in 1 .. P.Arg_Count - 1 loop  -- Skip program name (arg 0)
         declare
            Arg : String renames String (P.Args (A).Data (1 .. P.Args (A).Length));
         begin
            if Arg'Length >= 2 and then Arg (1) = '-' then
               P.Args (A).Is_Option := True;

               if Arg'Length >= 3 and then Arg (2) = '-' then
                  -- Long option (--option or --option=value)
                  declare
                     Eq_Pos : Natural := 0;
                  begin
                     for K in 3 .. Arg'Length loop
                        if Arg (K) = '=' then
                           Eq_Pos := K;
                           exit;
                        end if;
                     end loop;

                     for O in 1 .. P.Opt_Count loop
                        declare
                           Opt_Name : String (1 .. P.Options (O).Long_Len);
                        begin
                           for K in 1 .. P.Options (O).Long_Len loop
                              Opt_Name (K) := P.Options (O).Long (K);
                           end loop;

                           if Eq_Pos > 0 then
                              if Arg (3 .. Eq_Pos - 1) = Opt_Name then
                                 P.Options (O).Present := True;
                                 Set_Arg (Argument_Entry'(
                                   Data => P.Options (O).Value,
                                   Length => P.Options (O).Value_Len,
                                   Is_Option => False),
                                   Arg (Eq_Pos + 1 .. Arg'Last));
                                 P.Options (O).Value_Len :=
                                   Natural'Min (Arg'Last - Eq_Pos, Max_Arg_Length);
                                 for K in 1 .. P.Options (O).Value_Len loop
                                    P.Options (O).Value (K) := Arg (Eq_Pos + K);
                                 end loop;
                              end if;
                           else
                              if Arg (3 .. Arg'Last) = Opt_Name then
                                 P.Options (O).Present := True;
                              end if;
                           end if;
                        end;
                     end loop;
                  end;
               else
                  -- Short option (-x or -x value)
                  for O in 1 .. P.Opt_Count loop
                     if P.Options (O).Short = Arg (2) then
                        P.Options (O).Present := True;
                        -- Check for value in next argument
                        if P.Options (O).Has_Value and A < P.Arg_Count - 1 then
                           declare
                              Next : Argument_Entry renames P.Args (A + 1);
                           begin
                              if Next.Length > 0 and then
                                Next.Data (1) /= '-'
                              then
                                 P.Options (O).Value := Next.Data;
                                 P.Options (O).Value_Len := Next.Length;
                                 Next.Is_Option := True;  -- Mark as consumed
                              end if;
                           end;
                        end if;
                     end if;
                  end loop;
               end if;
            end if;
         end;
      end loop;
   end Parse;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (P : Parser) return Natural is
   begin
      if P.Arg_Count > 0 then
         return P.Arg_Count - 1;  -- Exclude program name
      else
         return 0;
      end if;
   end Argument_Count;

   --------------
   -- Argument --
   --------------

   function Argument (P : Parser; Index : Positive) return String is
   begin
      if Index < P.Arg_Count then
         return Arg_To_String (P.Args (Index));
      else
         return "";
      end if;
   end Argument;

   ------------------
   -- Program_Name --
   ------------------

   function Program_Name (P : Parser) return String is
   begin
      return Arg_To_String (P.Args (0));
   end Program_Name;

   -------------------
   -- Define_Option --
   -------------------

   procedure Define_Option
     (P         : in Out Parser;
      Short     : Character;
      Long      : String;
      Has_Value : Boolean := False)
   is
      Len : Natural;
   begin
      if P.Opt_Count >= Max_Options then
         return;
      end if;

      P.Opt_Count := P.Opt_Count + 1;
      P.Options (P.Opt_Count).Short := Short;

      Len := Natural'Min (Long'Length, Max_Arg_Length);
      P.Options (P.Opt_Count).Long := (others => ' ');
      for I in 1 .. Len loop
         P.Options (P.Opt_Count).Long (I) := Long (Long'First + I - 1);
      end loop;
      P.Options (P.Opt_Count).Long_Len := Len;
      P.Options (P.Opt_Count).Has_Value := Has_Value;
   end Define_Option;

   --------------------
   -- Option_Present --
   --------------------

   function Option_Present (P : Parser; Short : Character) return Boolean is
   begin
      for I in 1 .. P.Opt_Count loop
         if P.Options (I).Short = Short then
            return P.Options (I).Present;
         end if;
      end loop;
      return False;
   end Option_Present;

   function Option_Present (P : Parser; Long : String) return Boolean is
   begin
      for I in 1 .. P.Opt_Count loop
         if Match_Long (P.Options (I), Long) then
            return P.Options (I).Present;
         end if;
      end loop;
      return False;
   end Option_Present;

   ------------------
   -- Option_Value --
   ------------------

   function Option_Value (P : Parser; Short : Character) return String is
   begin
      for I in 1 .. P.Opt_Count loop
         if P.Options (I).Short = Short then
            return String (P.Options (I).Value (1 .. P.Options (I).Value_Len));
         end if;
      end loop;
      return "";
   end Option_Value;

   function Option_Value (P : Parser; Long : String) return String is
   begin
      for I in 1 .. P.Opt_Count loop
         if Match_Long (P.Options (I), Long) then
            return String (P.Options (I).Value (1 .. P.Options (I).Value_Len));
         end if;
      end loop;
      return "";
   end Option_Value;

   ----------------------
   -- Non_Option_Count --
   ----------------------

   function Non_Option_Count (P : Parser) return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. P.Arg_Count - 1 loop
         if not P.Args (I).Is_Option then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Non_Option_Count;

   ----------------
   -- Non_Option --
   ----------------

   function Non_Option (P : Parser; Index : Positive) return String is
      Count : Natural := 0;
   begin
      for I in 1 .. P.Arg_Count - 1 loop
         if not P.Args (I).Is_Option then
            Count := Count + 1;
            if Count = Index then
               return Arg_To_String (P.Args (I));
            end if;
         end if;
      end loop;
      return "";
   end Non_Option;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (P : Parser) return Boolean is
   begin
      return P.Has_Err;
   end Has_Error;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (P : Parser) return String is
   begin
      return String (P.Err_Msg (1 .. P.Err_Len));
   end Error_Message;

end GNAT.Command_Parser;
