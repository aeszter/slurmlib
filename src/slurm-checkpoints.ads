package Slurm.Checkpoints is

--      slurm_checkpoint_able - Note that a specific job or job step is eligible for checkpoint.
--      slurm_checkpoint_complete - Note that a requested checkpoint has completed.
--      slurm_checkpoint_create - Request a checkpoint for a specific job step. Continue execution upon completion of the checkpoint.
--      slurm_checkpoint_vacate - Request a checkpoint for a specific job step. Terminate execution upon completion of the checkpoint.
--      slurm_checkpoint_disable - Make the identified job step non-checkpointable.
--      slurm_checkpoint_enable - Make the identified job step checkpointable.
--      slurm_checkpoint_error - Get error information for the last checkpoint operation on a given job step.
--      slurm_checkpoint_restart - Request that a previously checkpointed job resume execution.

end;
