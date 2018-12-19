with POSIX;
with POSIX.C; use POSIX.C;

package body Slurm.General is

   function slurm_api_version return long;
pragma import (C, slurm_api_version, "slurm_api_version");

   function  API_Version return Natural is
   begin
      return Natural (slurm_api_version);
   end API_Version;
end Slurm.General;
