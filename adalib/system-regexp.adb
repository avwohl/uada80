-- System.Regexp body for Z80
-- Simple regular expression matching implementation

with Ada.Characters.Handling;

package body System.Regexp is

   -------------
   -- Compile --
   -------------

   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Regexp
   is
      Result : Regexp;
      J      : Natural := 0;
   begin
      Result.Case_Sensitive := Case_Sensitive;

      for I in Pattern'Range loop
         if J >= Max_Pattern_Length then
            raise Error_In_Regexp;
         end if;

         J := J + 1;

         if Glob then
            -- Glob mode: * matches any string, ? matches any char
            case Pattern (I) is
               when '*' =>
                  Result.Pattern (J).Kind := Any_String;
               when '?' =>
                  Result.Pattern (J).Kind := Any_Char;
               when others =>
                  Result.Pattern (J).Kind := Literal;
                  Result.Pattern (J).Char := Pattern (I);
            end case;
         else
            -- Simple regexp: . matches any char, * repeats previous
            case Pattern (I) is
               when '.' =>
                  Result.Pattern (J).Kind := Any_Char;
               when '*' =>
                  -- Convert previous element to Any_String
                  if J > 1 then
                     Result.Pattern (J - 1).Kind := Any_String;
                     J := J - 1;
                  end if;
               when '\' =>
                  -- Escape next character
                  if I < Pattern'Last then
                     Result.Pattern (J).Kind := Literal;
                     Result.Pattern (J).Char := Pattern (I + 1);
                  end if;
               when others =>
                  Result.Pattern (J).Kind := Literal;
                  Result.Pattern (J).Char := Pattern (I);
            end case;
         end if;
      end loop;

      Result.Length := J;
      return Result;
   end Compile;

   -----------
   -- Match --
   -----------

   function Match (S : String; R : Regexp) return Boolean is

      function Match_Char (C1, C2 : Character) return Boolean is
      begin
         if R.Case_Sensitive then
            return C1 = C2;
         else
            return Ada.Characters.Handling.To_Upper (C1) =
                   Ada.Characters.Handling.To_Upper (C2);
         end if;
      end Match_Char;

      function Match_From (SI, PI : Natural) return Boolean is
         S_Idx : Natural := SI;
         P_Idx : Natural := PI;
      begin
         while P_Idx <= R.Length loop
            case R.Pattern (P_Idx).Kind is
               when Literal =>
                  if S_Idx > S'Last or else
                     not Match_Char (S (S_Idx), R.Pattern (P_Idx).Char)
                  then
                     return False;
                  end if;
                  S_Idx := S_Idx + 1;
                  P_Idx := P_Idx + 1;

               when Any_Char =>
                  if S_Idx > S'Last then
                     return False;
                  end if;
                  S_Idx := S_Idx + 1;
                  P_Idx := P_Idx + 1;

               when Any_String =>
                  -- Try all possible matches
                  P_Idx := P_Idx + 1;
                  if P_Idx > R.Length then
                     return True;  -- * at end matches rest
                  end if;

                  for I in S_Idx .. S'Last + 1 loop
                     if Match_From (I, P_Idx) then
                        return True;
                     end if;
                  end loop;
                  return False;

               when Char_Class =>
                  -- Not fully implemented
                  S_Idx := S_Idx + 1;
                  P_Idx := P_Idx + 1;
            end case;
         end loop;

         return S_Idx > S'Last;
      end Match_From;

   begin
      if R.Length = 0 then
         return S'Length = 0;
      end if;

      return Match_From (S'First, 1);
   end Match;

end System.Regexp;
