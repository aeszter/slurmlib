with Ada.Strings.Fixed;
with Interfaces.C.Strings;
with Ada.Calendar.Conversions;
with POSIX.C;
package body Slurm.Utils is

   function getpwuid (c_uid : POSIX.C.uid_t) return POSIX.C.passwd_ptr;
   pragma Import (C, getpwuid, "getpwuid");

   function Convert_String (Source : chars_ptr) return Unbounded_String is
   begin
      return To_Unbounded_String (To_String (Source));
   end Convert_String;

   function Convert_Time (Source : POSIX.C.time_t) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Conversions.To_Ada_Time
        (Interfaces.C.long (Source));
   end Convert_Time;

   function Convert_User (UID : Slurm.C_Types.uint32_t) return User_Name is
      use POSIX.C;
      pw_entry : passwd_ptr;
   begin
      pw_entry := getpwuid (uid_t (UID));
      if pw_entry = null
      then
         raise Constraint_Error;
      end if;
      return To_User_Name (POSIX.To_String (Form_POSIX_String (pw_entry.all.pw_name)));
   end Convert_User;

   function MiB_To_Gigs (Source : Slurm.C_Types.uint64_t) return Gigs is
   begin
      return Gigs (Usage_Number (Source) / 1024);
   end MiB_To_Gigs;

   function MiB_To_Gigs (Source : Slurm.C_Types.uint32_t) return Gigs is
   begin
      return Gigs (Usage_Number (Source) / 1024);
   end MiB_To_Gigs;

   function To_String (Memory : Gigs) return String is
   begin
      return Ada.Strings.Fixed.Trim (Source => Memory'Img,
                                     Side   => Ada.Strings.Right);
   end To_String;

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

   function To_User_Name (User : String) return User_Name is
      use Ada.Strings.Fixed;
   begin
      return User_Name (Head (Source => User,
                              Count  => User_Name'Length));
   end To_User_Name;

end Slurm.Utils;
