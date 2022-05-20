with POSIX.C; use POSIX.C;

package Slurm.Errors is

   Slurm_Error : exception;

   type Error is new int;

   Protocol_Version_Error : constant Error := 1005;
   Socket_Timeout : constant Error := 5004;
--      slurm_get_errno - Return the error code set by the last Slurm API function executed.
--      slurm_perror - Print Slurm error information to standard output.
--      slurm_strerror - Return a string describing a specific Slurm error code.
   function Get_Last_Error return Error;
   function Get_Error (errno : Error) return String;
end Slurm.Errors;
