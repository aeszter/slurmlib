package Slurm.Job_Steps is

--      slurm_get_job_steps - Load job step information. Free with slurm_free_job_step_info_response_msg to avoid memory leak.
--      slurm_print_job_step_info_msg - Print information about all job steps.
--      slurm_print_job_step_info - Print information about a specific job step.
--      slurm_free_job_step_info_response_msg - Free storage allocated by slurm_get_job_steps.


--      slurm_job_step_create - Initiate a job step. Allocated memory must be freed by slurm_free_job_step_create_response_msg to avoid a memory leak.
--      slurm_free_job_step_create_response_msg - Free memory allocated by slurm_job_step_create.
--      slurm_step_ctx_create - Create job step context. Destroy using slurm_step_ctx_destroy.
--      slurm_step_ctx_destroy - Destroy a job step context created by slurm_step_ctx_create.
--      slurm_step_ctx_get - Get values from job step context.
--      slurm_step_ctx_set - Set values in job step context.
--      slurm_jobinfo_ctx_get - Get values from a jobinfo field as returned by slurm_step_ctx_get.
--      slurm_spawn - Spawn tasks and establish communications.
--      slurm_spawn_kill - Signal spawned tasks.
--
--      slurm_kill_job_step - Signal or cancel a job step.
--      slurm_complete_job_step - Note completion of a job step.
--
end;
