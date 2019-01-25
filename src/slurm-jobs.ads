with Ada.Calendar;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Slurm.Utils; use Slurm.Utils;

package Slurm.Jobs is
--      slurm_pid2jobid - For a given process ID on a node get the corresponding Slurm job ID.
--      slurm_get_end_time - For a given Slurm job ID get the expected termination time.
--      slurm_load_jobs - Load job information.
--         Free with slurm_free_job_info_msg to avoid memory leak.
--      slurm_print_job_info_msg - Print information about all jobs.
--      slurm_print_job_info - Print information about a specific job.
--      slurm_get_select_jobinfo - Get select plugin specific information
--        associated with the job. The information available is will vary by
--        select plugin type configured.
--      slurm_free_job_info_msg - Free storage allocated by slurm_load_jobs.
--     slurm_kill_job - Signal or cancel a job.
--  slurm_complete_job - Note completion of a job. Releases resource allocation for the job.
   type states is (
        JOB_PENDING,
        JOB_RUNNING,
        JOB_SUSPENDED,
        JOB_COMPLETE,
        JOB_CANCELLED,
        JOB_FAILED,
        JOB_TIMEOUT,
        JOB_NODE_FAIL,
        JOB_PREEMPTED,
        JOB_BOOT_FAIL,
        JOB_DEADLINE,
                   JOB_OOM);
   type State_Count is array (states) of Natural;

   type Job is private;
   type List is private;
   type Cursor is private;
   function Element (Position : Cursor) return Job;
   function Has_Element (Position : Cursor) return Boolean;
   procedure Iterate (Collection : List;
                      Process    : not null access procedure (Position : Cursor));
   function Get_Gres (J : Job) return String;
   function Get_ID (J : Job) return Positive;
   function Get_Name (J : Job) return String;
   function Get_Owner (J : Job) return User_Name;
   function Get_Priority (J : Job) return Natural;
   function Get_Project (J : Job) return String;
   function Get_Start_Time (J : Job) return Ada.Calendar.Time;
   function Walltime (J : Job) return Duration;
   function Get_State (J : Job) return String;
   function Get_Submission_Time (J : Job) return Ada.Calendar.Time;
   function Get_Tasks (J : Job) return Positive;
   function Has_Error (J : Job) return Boolean;
   function Is_Running (J : Job) return Boolean;

   function Load_Jobs return List;
   function Load_User (User : String) return List;

   procedure Get_Summary (Collection : List;
                          Jobs, Tasks : out State_Count);

private

   type Job is record
      Gres : Unbounded_String;
      ID      : Positive;
      Name    : Unbounded_String;
      Owner   : User_Name;
      Priority : Natural;
      Project : Unbounded_String;
      Start_Time : Ada.Calendar.Time;
      State      : states;
      Submission_Time : Ada.Calendar.Time;
      Tasks : Natural;
   end record;

   package Lists is new ada.Containers.Doubly_Linked_Lists (Element_Type => Job);
   type Cursor is new Lists.Cursor;
   type List is record
      Container : Lists.List;
   end record;

end Slurm.Jobs;
