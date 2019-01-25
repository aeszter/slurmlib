package Slurm.General is
--  slurm_api_version - Get Slurm API version number.
--  slurm_load_ctl_conf - Load system-wide configuration specifications. Free with slurm_free_ctl_conf to avoid memory leak.
--  slurm_print_ctl_conf - Print system-wide configuration specifications.
--  slurm_free_ctl_conf - Free storage allocated by slurm_load_ctl_conf.

   function API_Version return Natural;

end Slurm.General;
