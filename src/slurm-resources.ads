package Slurm.Resources is

--      slurm_init_job_desc_msg - Initialize the data structure used in resource allocation requests. You can then just set the fields of particular interest and let the others use default values.
--      slurm_job_will_run - Determine if a job would be immediately initiated if submitted now.
--      slurm_allocate_resources - Allocate resources for a job. Response message must be freed using slurm_free_resource_allocation_response_msg to avoid a memory leak.
--      slurm_free_resource_allocation_response_msg - Frees memory allocated by slurm_allocate_resources.
--      slurm_allocate_resources_and_run - Allocate resources for a job and spawn a job step. Response message must be freed using slurm_free_resource_allocation_and_run_response_msg to avoid a memory leak.
--      slurm_free_resource_allocation_and_run_response_msg - Frees memory allocated by slurm_allocate_resources_and_run.
--      slurm_submit_batch_job - Submit a script for later execution. Response message must be freed using slurm_free_submit_response_response_msg to avoid a memory leak.
--      slurm_free_submit_response_response_msg - Frees memory allocated by slurm_submit_batch_job.
--      slurm_confirm_allocation - Test if a resource allocation has already been made for a given job id. Response message must be freed using slurm_free_resource_allocation_response_msg to avoid a memory leak. This can be used to confirm that an allocation is still active or for error recovery.
end;
