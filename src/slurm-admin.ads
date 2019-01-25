package Slurm.Admin is

--      slurm_reconfigure - Update slurm daemons based upon current slurm.conf configuration file. Use this after updating the configuration file to ensure that it takes effect.
--      slurm_shutdown - Terminate slurm daemons.
--      slurm_update_job - Update state information associated with a given job.
--      slurm_update_node - Update state information associated with a given node. NOTE: Most of a node's characteristics can not be modified.
--      slurm_init_part_desc_msg - Initialize a partition update descriptor. Used this to initialize the data structure used in slurm_update_partition.
--      slurm_update_partition - Update state information associated with a given partition.
--      slurm_delete_partition - Destroy a partition.

   end;
