with Ada.Strings.Fixed;
with Interfaces.C.Strings;
package body Slurm.Utils is

   function To_User_Name (User : String) return User_Name is
      use Ada.Strings.Fixed;
   begin
      return User_Name (Head (Source => User,
                              Count  => User_Name'Length));
   end To_User_Name;

   function To_String (User : User_Name) return String is
   begin
      return Ada.Strings.Fixed.Trim (String (User), Ada.Strings.Right);
   end To_String;

   function To_String (Source : Interfaces.C.Strings.chars_ptr) return String is
      use Interfaces.C.Strings;
   begin
      declare
         Result : String := Value (Source);
      begin
         return Result;
      end;
   exception
      when Dereference_Error =>
         return "";
   end To_String;

end Slurm.Utils;
