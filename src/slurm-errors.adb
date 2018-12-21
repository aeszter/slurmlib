with POSIX; use POSIX;
with POSIX.C; use POSIX.C;

package body Slurm.Errors is

   function slurm_get_errno return int;
   pragma Import (C, slurm_get_errno, "slurm_get_errno");

   function slurm_strerror (errnum : int) return char_ptr;
   pragma Import (C, slurm_strerror, "slurm_strerror");

   function Get_Error (errno : Error) return String is
   begin
      return To_String (Form_POSIX_String (slurm_strerror (int (errno))));
   end Get_Error;

   function Get_Last_Error return Error is
      errno : int := slurm_get_errno;
   begin
      return Error (errno);
   end Get_Last_Error;

end Slurm.Errors;
