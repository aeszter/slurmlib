with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;

package body Slurm.Taint is

   overriding function "&" (Left, Right : Trusted_String) return Trusted_String is
   begin
      return Trusted_String (String (Left) & String (Right));
   end "&";

   procedure Append (Source : in out Trusted_String_List;
                     New_Item : Trusted_String) is
   begin
      Lists.Append (Lists.List (Source), To_Unbounded_String (Value (New_Item)));
   end Append;

   function Element (Position : Cursor) return Trusted_String is
   begin
      return Trusted_String (To_String (Lists.Element (Lists.Cursor (Position))));
   end Element;

   function First (Collection : Trusted_String_List) return Cursor is
   begin
      return Cursor (Lists.First (Lists.List (Collection)));
   end First;

   overriding function Has_Element (Position : Cursor) return Boolean is
   begin
      return Lists.Has_Element (Lists.Cursor (Position));
   end Has_Element;

   function Implicit_Trust (S : String) return Trusted_String is
   begin
      return Trusted_String (S);
   end Implicit_Trust;

   overriding procedure Next (Position : in out Cursor) is
   begin
      Lists.Next (Lists.Cursor (Position));
   end Next;

   function Sanitise (S : String) return Trusted_String is
      function Is_Harmless_Dash (Char : in Character; Where : Positive) return Boolean;
      Output : String := S;

      function Is_Harmless_Dash (Char : in Character; Where : Positive) return Boolean is
      begin
         if Char = '-' and then
           Where > Output'First and then
           Ada.Characters.Handling.Is_Alphanumeric (Output (Where - 1))
         then
            return True; -- a dash, not the first character, and the previous one is alphanumeric
            --  so this does not start a commandline switch
         else
            return False; -- not a dash, or not preceded by a harmless character
         end if;
      end Is_Harmless_Dash;

   begin
      for Pos in Output'Range loop
         if not Ada.Characters.Handling.Is_Letter (Output (Pos))
           and then not Ada.Characters.Handling.Is_Decimal_Digit (Output (Pos))
           and then Output (Pos) /= ';' -- does not start a new command when passed to exec()
           and then Output (Pos) /= ','
           and then Output (Pos) /= '='
           and then Output (Pos) /= '@'
           and then Output (Pos) /= '.'
           and then Output (Pos) /= '*'
           and then not Is_Harmless_Dash (Char  => Output (Pos), Where => Pos)
         then
            Output (Pos) := '_';
         end if;
      end loop;
      return Trusted_String (Output);
   end Sanitise;

   function Sanitise_Number (S : String) return Trusted_String is
      Output : String := Ada.Strings.Fixed.Trim (S, Ada.Strings.Left);
   begin
      for Pos in Output'Range loop
         if not Ada.Characters.Handling.Is_Decimal_Digit (Output (Pos)) then
            Output (Pos) := '_';
         end if;
      end loop;
      return Trusted_String (Output);
   end Sanitise_Number;

   function Trust_As_Command (S : String) return Trusted_Command_Name is
   begin
      return Trusted_Command_Name (S);
   end Trust_As_Command;

   function Value (S : Trusted_String) return String is
   begin
      return String (S);
   end Value;

   function Value (S : Trusted_Command_Name) return String is
   begin
      return String (S);
   end Value;

   function Value (Collection : Trusted_String_List) return String is
      result : Unbounded_String;
      Position : Cursor := Collection.First;
   begin
      while Has_Element (Position) loop
         Append (result, Element (Position));
         Next (Position);
      end loop;
      return To_String (result);
   end Value;

end Slurm.Taint;
