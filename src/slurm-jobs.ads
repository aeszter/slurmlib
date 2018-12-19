
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Slurm.Utils; use Slurm.Utils;

package Slurm.Jobs is
--      slurm_pid2jobid - For a given process ID on a node get the corresponding Slurm job ID.
--      slurm_get_end_time - For a given Slurm job ID get the expected termination time.
--      slurm_load_jobs - Load job information. Free with slurm_free_job_info_msg to avoid memory leak.
--      slurm_print_job_info_msg - Print information about all jobs.
--      slurm_print_job_info - Print information about a specific job.
--      slurm_get_select_jobinfo - Get select plugin specific information associated with the job. The information available is will vary by select plugin type configured.
--      slurm_free_job_info_msg - Free storage allocated by slurm_load_jobs.
--     slurm_kill_job - Signal or cancel a job.
--  slurm_complete_job - Note completion of a job. Releases resource allocation for the job.

   type Job is private;
   type List is private;
   type Cursor is private;
   function Element (Position : Cursor) return Job;
   function Has_Element (Position : Cursor) return Boolean;
   procedure Iterate (Collection : List;
                      Process    : not null access procedure (Position : Cursor));
   function Get_ID (J : Job) return Positive;
   function Get_Name (J : Job) return String;
   function Get_Owner (J : Job) return User_Name;
   function Get_Project (J : Job) return String;

   function Load_Jobs return List;

private

   type Job is record
      ID      : Positive;
      Name    : Unbounded_String;
      Owner   : User_Name;
      Project : Unbounded_String;
   end record;

   package Lists is new ada.Containers.Doubly_Linked_Lists (Element_Type => Job);
   type Cursor is new Lists.Cursor;
   type List is record
      Container : Lists.List;
   end record;


end Slurm.Jobs;
