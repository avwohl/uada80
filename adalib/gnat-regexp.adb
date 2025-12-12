-- GNAT.Regexp body for Z80
-- Simple regular expression matching implementation

with Ada.Characters.Handling;

package body GNAT.Regexp is

   -------------
   -- Compile --
   -------------

   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Regexp
   is
      R : Regexp;
   begin
      R.Length := Natural'Min (Pattern'Length, Max_Pattern_Length);
      R.Pattern (1 .. R.Length) := Pattern (Pattern'First .. Pattern'First + R.Length - 1);
      R.Glob := Glob;
      R.Case_Sensitive := Case_Sensitive;
      return R;
   end Compile;

   -- Helper: match character
   function Match_Char
     (C1, C2         : Character;
      Case_Sensitive : Boolean) return Boolean
   is
      use Ada.Characters.Handling;
   begin
      if Case_Sensitive then
         return C1 = C2;
      else
         return To_Lower (C1) = To_Lower (C2);
      end if;
   end Match_Char;

   -- Recursive glob match
   function Glob_Match
     (S              : String;
      S_Pos          : Natural;
      P              : String;
      P_Pos          : Natural;
      Case_Sensitive : Boolean) return Boolean
   is
   begin
      -- End of pattern
      if P_Pos > P'Last then
         return S_Pos > S'Last;
      end if;

      -- Process pattern character
      if P (P_Pos) = '*' then
         -- Match zero or more characters
         for I in S_Pos .. S'Last + 1 loop
            if Glob_Match (S, I, P, P_Pos + 1, Case_Sensitive) then
               return True;
            end if;
         end loop;
         return False;

      elsif P (P_Pos) = '?' then
         -- Match exactly one character
         if S_Pos > S'Last then
            return False;
         end if;
         return Glob_Match (S, S_Pos + 1, P, P_Pos + 1, Case_Sensitive);

      else
         -- Match literal character
         if S_Pos > S'Last then
            return False;
         end if;
         if not Match_Char (S (S_Pos), P (P_Pos), Case_Sensitive) then
            return False;
         end if;
         return Glob_Match (S, S_Pos + 1, P, P_Pos + 1, Case_Sensitive);
      end if;
   end Glob_Match;

   -- Simple regex match (limited subset)
   function Regex_Match
     (S              : String;
      S_Pos          : Natural;
      P              : String;
      P_Pos          : Natural;
      Case_Sensitive : Boolean) return Boolean
   is
   begin
      -- End of pattern
      if P_Pos > P'Last then
         return S_Pos > S'Last;
      end if;

      -- Handle special regex characters
      if P (P_Pos) = '.' then
         -- Match any single character
         if S_Pos > S'Last then
            return False;
         end if;
         return Regex_Match (S, S_Pos + 1, P, P_Pos + 1, Case_Sensitive);

      elsif P (P_Pos) = '^' and P_Pos = P'First then
         -- Anchor at start (implicit)
         return Regex_Match (S, S_Pos, P, P_Pos + 1, Case_Sensitive);

      elsif P (P_Pos) = '$' and P_Pos = P'Last then
         -- Anchor at end
         return S_Pos > S'Last;

      elsif P_Pos < P'Last and then P (P_Pos + 1) = '*' then
         -- Zero or more of previous
         if P (P_Pos) = '.' then
            -- .* matches anything
            for I in S_Pos .. S'Last + 1 loop
               if Regex_Match (S, I, P, P_Pos + 2, Case_Sensitive) then
                  return True;
               end if;
            end loop;
         else
            -- c* matches zero or more c
            declare
               I : Natural := S_Pos;
            begin
               loop
                  if Regex_Match (S, I, P, P_Pos + 2, Case_Sensitive) then
                     return True;
                  end if;
                  exit when I > S'Last;
                  exit when not Match_Char (S (I), P (P_Pos), Case_Sensitive);
                  I := I + 1;
               end loop;
            end;
         end if;
         return False;

      else
         -- Match literal character
         if S_Pos > S'Last then
            return False;
         end if;
         if not Match_Char (S (S_Pos), P (P_Pos), Case_Sensitive) then
            return False;
         end if;
         return Regex_Match (S, S_Pos + 1, P, P_Pos + 1, Case_Sensitive);
      end if;
   end Regex_Match;

   -----------
   -- Match --
   -----------

   function Match (S : String; R : Regexp) return Boolean is
      Pat : constant String := R.Pattern (1 .. R.Length);
   begin
      if R.Length = 0 then
         return S'Length = 0;
      end if;

      if R.Glob then
         return Glob_Match (S, S'First, Pat, Pat'First, R.Case_Sensitive);
      else
         -- For regex, try matching at each position
         for I in S'Range loop
            if Regex_Match (S, I, Pat, Pat'First, R.Case_Sensitive) then
               return True;
            end if;
         end loop;
         return Regex_Match (S, S'Last + 1, Pat, Pat'First, R.Case_Sensitive);
      end if;
   end Match;

   function Match
     (S              : String;
      Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Boolean
   is
   begin
      return Match (S, Compile (Pattern, Glob, Case_Sensitive));
   end Match;

end GNAT.Regexp;
