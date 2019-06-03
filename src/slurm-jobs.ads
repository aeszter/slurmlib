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

   type state_reasons is (
-- Reasons for job to be pending
        WAIT_NO_REASON, -- not set or job not pending */
        WAIT_PRIORITY,          -- higher priority jobs exist */
        WAIT_DEPENDENCY,        -- dependent job has not completed */
        WAIT_RESOURCES,         -- required resources not available */
        WAIT_PART_NODE_LIMIT,   -- request exceeds partition node limit */
        WAIT_PART_TIME_LIMIT,   -- request exceeds partition time limit */
        WAIT_PART_DOWN,         -- requested partition is down */
        WAIT_PART_INACTIVE,     -- requested partition is inactive */
        WAIT_HELD,              -- job is held by administrator */
        WAIT_TIME,              -- job waiting for specific begin time */
        WAIT_LICENSES,          -- job is waiting for licenses */
        WAIT_ASSOC_JOB_LIMIT,   -- user/bank job limit reached */
         WAIT_ASSOC_RESOURCE_LIMIT, -- user/bank resource limit reached */
        WAIT_ASSOC_TIME_LIMIT,  -- user/bank time limit reached */
        WAIT_RESERVATION,       -- reservation not available */
        WAIT_NODE_NOT_AVAIL,    -- required node is DOWN or DRAINED */
        WAIT_HELD_USER,         -- job is held by user */
        WAIT_FRONT_END,         -- Front end nodes are DOWN */
        FAIL_DOWN_PARTITION,    -- partition for job is DOWN */
        FAIL_DOWN_NODE,         -- some node in the allocation failed */
        FAIL_BAD_CONSTRAINTS,   -- constraints can not be satisfied */
        FAIL_SYSTEM,            -- slurm system failure */
        FAIL_LAUNCH,            -- unable to launch job */
        FAIL_EXIT_CODE,         -- exit code was non-zero */
        FAIL_TIMEOUT,           -- reached end of time limit */
        FAIL_INACTIVE_LIMIT,    -- reached slurm InactiveLimit */
        FAIL_ACCOUNT,           -- invalid account */
        FAIL_QOS,               -- invalid QOS */
        WAIT_QOS_THRES,         -- required QOS threshold has been breached */
        WAIT_QOS_JOB_LIMIT,     -- QOS job limit reached */
         WAIT_QOS_RESOURCE_LIMIT, -- QOS resource limit reached */
        WAIT_QOS_TIME_LIMIT,    -- QOS time limit reached */
        WAIT_BLOCK_MAX_ERR,     -- BLUEGENE Block has too many cnodes
                                 -- in error state to allow more jobs. */
        WAIT_BLOCK_D_ACTION,    -- BLUEGENE Block is being freed,
                           -- can't allow more jobs. */
        WAIT_CLEANING,          -- If a job is requeued and it is
                                 -- still cleaning up from the last run. */
        WAIT_PROLOG,            -- Prolog is running */
        WAIT_QOS,               -- QOS not allowed */
        WAIT_ACCOUNT,           -- Account not allowed */
        WAIT_DEP_INVALID,        -- Dependency condition invalid or never
                           -- satisfied

        WAIT_QOS_GRP_CPU,            -- QOS GrpTRES exceeded (CPU) */
        WAIT_QOS_GRP_CPU_MIN,        -- QOS GrpTRESMins exceeded (CPU) */
        WAIT_QOS_GRP_CPU_RUN_MIN,    -- QOS GrpTRESRunMins exceeded (CPU) */
        WAIT_QOS_GRP_JOB,            -- QOS GrpJobs exceeded */
        WAIT_QOS_GRP_MEM,            -- QOS GrpTRES exceeded (Memory) */
        WAIT_QOS_GRP_NODE,           -- QOS GrpTRES exceeded (Node) */
        WAIT_QOS_GRP_SUB_JOB,        -- QOS GrpSubmitJobs exceeded */
        WAIT_QOS_GRP_WALL,           -- QOS GrpWall exceeded */
        WAIT_QOS_MAX_CPU_PER_JOB,    -- QOS MaxTRESPerJob exceeded (CPU) */
         WAIT_QOS_MAX_CPU_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob exceeded (CPU) */
        WAIT_QOS_MAX_NODE_PER_JOB,   -- QOS MaxTRESPerJob exceeded (Node) */
        WAIT_QOS_MAX_WALL_PER_JOB,   -- QOS MaxWallDurationPerJob exceeded */
        WAIT_QOS_MAX_CPU_PER_USER,   -- QOS MaxTRESPerUser exceeded (CPU) */
        WAIT_QOS_MAX_JOB_PER_USER,   -- QOS MaxJobsPerUser exceeded */
        WAIT_QOS_MAX_NODE_PER_USER,  -- QOS MaxTRESPerUser exceeded (Node) */
        WAIT_QOS_MAX_SUB_JOB,        -- QOS MaxSubmitJobsPerUser exceeded */
        WAIT_QOS_MIN_CPU,            -- QOS MinTRESPerJob not reached (CPU) */
        WAIT_ASSOC_GRP_CPU,          -- ASSOC GrpTRES exceeded (CPU) */
        WAIT_ASSOC_GRP_CPU_MIN,      -- ASSOC GrpTRESMins exceeded (CPU) */
        WAIT_ASSOC_GRP_CPU_RUN_MIN,  -- ASSOC GrpTRESRunMins exceeded (CPU) */
        WAIT_ASSOC_GRP_JOB,          -- ASSOC GrpJobs exceeded */
        WAIT_ASSOC_GRP_MEM,          -- ASSOC GrpTRES exceeded (Memory) */
        WAIT_ASSOC_GRP_NODE,         -- ASSOC GrpTRES exceeded (Node) */
        WAIT_ASSOC_GRP_SUB_JOB,      -- ASSOC GrpSubmitJobs exceeded */
        WAIT_ASSOC_GRP_WALL,         -- ASSOC GrpWall exceeded */
        WAIT_ASSOC_MAX_JOBS,         -- ASSOC MaxJobs exceeded */
        WAIT_ASSOC_MAX_CPU_PER_JOB,  -- ASSOC MaxTRESPerJob exceeded (CPU) */
         WAIT_ASSOC_MAX_CPU_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob
                           -- exceeded (CPU) */
        WAIT_ASSOC_MAX_NODE_PER_JOB, -- ASSOC MaxTRESPerJob exceeded (NODE) */
        WAIT_ASSOC_MAX_WALL_PER_JOB, -- ASSOC MaxWallDurationPerJob
                           -- exceeded */
        WAIT_ASSOC_MAX_SUB_JOB,      -- ASSOC MaxSubmitJobsPerUser exceeded */

        WAIT_MAX_REQUEUE,            -- MAX_BATCH_REQUEUE reached */
        WAIT_ARRAY_TASK_LIMIT,       -- job array running task limit */
        WAIT_BURST_BUFFER_RESOURCE,  -- Burst buffer resources */
        WAIT_BURST_BUFFER_STAGING,   -- Burst buffer file stage-in */
        FAIL_BURST_BUFFER_OP,        -- Burst buffer operation failure */
        WAIT_POWER_NOT_AVAIL,        -- not enough power available */
        WAIT_POWER_RESERVED,         -- job is waiting for available power
                           -- because of power reservations */
        WAIT_ASSOC_GRP_UNK,          -- ASSOC GrpTRES exceeded
                           -- (Unknown) */
        WAIT_ASSOC_GRP_UNK_MIN,      -- ASSOC GrpTRESMins exceeded
                           -- (Unknown) */
        WAIT_ASSOC_GRP_UNK_RUN_MIN,  -- ASSOC GrpTRESRunMins exceeded
                           -- (Unknown) */
        WAIT_ASSOC_MAX_UNK_PER_JOB,  -- ASSOC MaxTRESPerJob exceeded
                           -- (Unknown) */
        WAIT_ASSOC_MAX_UNK_PER_NODE,  -- ASSOC MaxTRESPerNode exceeded
                                       -- (Unknown) */
         WAIT_ASSOC_MAX_UNK_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob
                           -- exceeded (Unknown) */
        WAIT_ASSOC_MAX_CPU_PER_NODE,  -- ASSOC MaxTRESPerNode exceeded (CPU) */
        WAIT_ASSOC_GRP_MEM_MIN,      -- ASSOC GrpTRESMins exceeded
                           -- (Memory) */
        WAIT_ASSOC_GRP_MEM_RUN_MIN,  -- ASSOC GrpTRESRunMins exceeded
                           -- (Memory) */
        WAIT_ASSOC_MAX_MEM_PER_JOB,  -- ASSOC MaxTRESPerJob exceeded (Memory) */
        WAIT_ASSOC_MAX_MEM_PER_NODE,  -- ASSOC MaxTRESPerNode exceeded (CPU) */
         WAIT_ASSOC_MAX_MEM_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob
                           -- exceeded (Memory) */
        WAIT_ASSOC_GRP_NODE_MIN,     -- ASSOC GrpTRESMins exceeded (Node) */
        WAIT_ASSOC_GRP_NODE_RUN_MIN, -- ASSOC GrpTRESRunMins exceeded (Node) */
         WAIT_ASSOC_MAX_NODE_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob
                                          -- exceeded (Node) */
        WAIT_ASSOC_GRP_ENERGY,           -- ASSOC GrpTRES exceeded
                                          -- (Energy) */
        WAIT_ASSOC_GRP_ENERGY_MIN,       -- ASSOC GrpTRESMins exceeded
                                          -- (Energy) */
        WAIT_ASSOC_GRP_ENERGY_RUN_MIN,   -- ASSOC GrpTRESRunMins exceeded
                                          -- (Energy) */
        WAIT_ASSOC_MAX_ENERGY_PER_JOB,   -- ASSOC MaxTRESPerJob exceeded
                                          -- (Energy) */
        WAIT_ASSOC_MAX_ENERGY_PER_NODE,  -- ASSOC MaxTRESPerNode
                                          -- exceeded (Energy) */
         WAIT_ASSOC_MAX_ENERGY_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob
                           -- exceeded (Energy) */
        WAIT_ASSOC_GRP_GRES,          -- ASSOC GrpTRES exceeded (GRES) */
        WAIT_ASSOC_GRP_GRES_MIN,      -- ASSOC GrpTRESMins exceeded (GRES) */
        WAIT_ASSOC_GRP_GRES_RUN_MIN,  -- ASSOC GrpTRESRunMins exceeded (GRES) */
        WAIT_ASSOC_MAX_GRES_PER_JOB,  -- ASSOC MaxTRESPerJob exceeded (GRES) */
        WAIT_ASSOC_MAX_GRES_PER_NODE, -- ASSOC MaxTRESPerNode exceeded (GRES) */
         WAIT_ASSOC_MAX_GRES_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob
                                          -- exceeded (GRES) */
        WAIT_ASSOC_GRP_LIC,          -- ASSOC GrpTRES exceeded
                           -- (license) */
        WAIT_ASSOC_GRP_LIC_MIN,      -- ASSOC GrpTRESMins exceeded
                           -- (license) */
        WAIT_ASSOC_GRP_LIC_RUN_MIN,  -- ASSOC GrpTRESRunMins exceeded
                           -- (license) */
        WAIT_ASSOC_MAX_LIC_PER_JOB,  -- ASSOC MaxTRESPerJob exceeded
                           -- (license) */
         WAIT_ASSOC_MAX_LIC_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob exceeded
                           -- (license) */
        WAIT_ASSOC_GRP_BB,          -- ASSOC GrpTRES exceeded
                           -- (burst buffer) */
        WAIT_ASSOC_GRP_BB_MIN,      -- ASSOC GrpTRESMins exceeded
                           -- (burst buffer) */
        WAIT_ASSOC_GRP_BB_RUN_MIN,  -- ASSOC GrpTRESRunMins exceeded
                           -- (burst buffer) */
        WAIT_ASSOC_MAX_BB_PER_JOB,  -- ASSOC MaxTRESPerJob exceeded
                           -- (burst buffer) */
        WAIT_ASSOC_MAX_BB_PER_NODE, -- ASSOC MaxTRESPerNode exceeded
                           -- (burst buffer) */
        WAIT_ASSOC_MAX_BB_MINS_PER_JOB, -- ASSOC MaxTRESMinsPerJob exceeded
                           -- (burst buffer) */
        WAIT_QOS_GRP_UNK,           -- QOS GrpTRES exceeded (Unknown) */
        WAIT_QOS_GRP_UNK_MIN,       -- QOS GrpTRESMins exceeded (Unknown) */
        WAIT_QOS_GRP_UNK_RUN_MIN,   -- QOS GrpTRESRunMins exceeded (Unknown) */
        WAIT_QOS_MAX_UNK_PER_JOB,   -- QOS MaxTRESPerJob exceeded (Unknown) */
        WAIT_QOS_MAX_UNK_PER_NODE,  -- QOS MaxTRESPerNode exceeded (Unknown) */
        WAIT_QOS_MAX_UNK_PER_USER,  -- QOS MaxTRESPerUser exceeded (Unknown) */
         WAIT_QOS_MAX_UNK_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob
                                       -- exceeded (Unknown) */
        WAIT_QOS_MIN_UNK,           -- QOS MinTRESPerJob exceeded (Unknown) */
        WAIT_QOS_MAX_CPU_PER_NODE,  -- QOS MaxTRESPerNode exceeded (CPU) */
        WAIT_QOS_GRP_MEM_MIN,       -- QOS GrpTRESMins exceeded
                           -- (Memory) */
        WAIT_QOS_GRP_MEM_RUN_MIN,   -- QOS GrpTRESRunMins exceeded
                           -- (Memory) */
         WAIT_QOS_MAX_MEM_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob
                                       -- exceeded (Memory) */
        WAIT_QOS_MAX_MEM_PER_JOB,   -- QOS MaxTRESPerJob exceeded (CPU) */
        WAIT_QOS_MAX_MEM_PER_NODE,  -- QOS MaxTRESPerNode exceeded (MEM) */
        WAIT_QOS_MAX_MEM_PER_USER,  -- QOS MaxTRESPerUser exceeded (CPU) */
        WAIT_QOS_MIN_MEM,           -- QOS MinTRESPerJob not reached (Memory) */
        WAIT_QOS_GRP_ENERGY,        -- QOS GrpTRES exceeded (Energy) */
        WAIT_QOS_GRP_ENERGY_MIN,    -- QOS GrpTRESMins exceeded (Energy) */
        WAIT_QOS_GRP_ENERGY_RUN_MIN, -- QOS GrpTRESRunMins exceeded (Energy) */
        WAIT_QOS_MAX_ENERGY_PER_JOB, -- QOS MaxTRESPerJob exceeded (Energy) */
        WAIT_QOS_MAX_ENERGY_PER_NODE, -- QOS MaxTRESPerNode exceeded (Energy) */
         WAIT_QOS_MAX_ENERGY_PER_USER, -- QOS MaxTRESPerUser exceeded (Energy) */
        WAIT_QOS_MAX_ENERGY_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob
                                          -- exceeded (Energy) */
        WAIT_QOS_MIN_ENERGY,        -- QOS MinTRESPerJob not reached (Energy) */
        WAIT_QOS_GRP_NODE_MIN,     -- QOS GrpTRESMins exceeded (Node) */
        WAIT_QOS_GRP_NODE_RUN_MIN, -- QOS GrpTRESRunMins exceeded (Node) */
        WAIT_QOS_MAX_NODE_MINS_PER_JOB,  -- QOS MaxTRESMinsPerJob
                                          -- exceeded (Node) */
        WAIT_QOS_MIN_NODE,          -- QOS MinTRESPerJob not reached (Node) */
        WAIT_QOS_GRP_GRES,          -- QOS GrpTRES exceeded (GRES) */
        WAIT_QOS_GRP_GRES_MIN,      -- QOS GrpTRESMins exceeded (GRES) */
        WAIT_QOS_GRP_GRES_RUN_MIN,  -- QOS GrpTRESRunMins exceeded (GRES) */
        WAIT_QOS_MAX_GRES_PER_JOB,  -- QOS MaxTRESPerJob exceeded (GRES) */
        WAIT_QOS_MAX_GRES_PER_NODE, -- QOS MaxTRESPerNode exceeded (GRES) */
        WAIT_QOS_MAX_GRES_PER_USER, -- QOS MaxTRESPerUser exceeded
                           -- (GRES) */
         WAIT_QOS_MAX_GRES_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob
                           -- exceeded (GRES) */
        WAIT_QOS_MIN_GRES,          -- QOS MinTRESPerJob not reached (CPU) */
        WAIT_QOS_GRP_LIC,           -- QOS GrpTRES exceeded (license) */
        WAIT_QOS_GRP_LIC_MIN,       -- QOS GrpTRESMins exceeded (license) */
        WAIT_QOS_GRP_LIC_RUN_MIN,   -- QOS GrpTRESRunMins exceeded (license) */
        WAIT_QOS_MAX_LIC_PER_JOB,   -- QOS MaxTRESPerJob exceeded (license) */
        WAIT_QOS_MAX_LIC_PER_USER,  -- QOS MaxTRESPerUser exceeded (license) */
         WAIT_QOS_MAX_LIC_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob exceeded
                                       -- (license) */
        WAIT_QOS_MIN_LIC,           -- QOS MinTRESPerJob not reached
                           -- (license) */
        WAIT_QOS_GRP_BB,            -- QOS GrpTRES exceeded
                           -- (burst buffer) */
        WAIT_QOS_GRP_BB_MIN,        -- QOS GrpTRESMins exceeded
                           -- (burst buffer) */
        WAIT_QOS_GRP_BB_RUN_MIN,    -- QOS GrpTRESRunMins exceeded
                           -- (burst buffer) */
        WAIT_QOS_MAX_BB_PER_JOB,   -- QOS MaxTRESPerJob exceeded
                                    -- (burst buffer) */
        WAIT_QOS_MAX_BB_PER_NODE,  -- QOS MaxTRESPerNode exceeded
                                    -- (burst buffer) */
        WAIT_QOS_MAX_BB_PER_USER,  -- QOS MaxTRESPerUser exceeded
                                    -- (burst buffer) */
         WAIT_QOS_MAX_BB_MINS_PER_JOB, -- QOS MaxTRESMinsPerJob exceeded
                           -- (burst buffer) */
        WAIT_QOS_MIN_BB,           -- QOS MinTRESPerJob not reached
                                    -- (burst buffer) */
        FAIL_DEADLINE,              -- reached deadline */
        -- QOS MaxTRESPerAccount */
        WAIT_QOS_MAX_BB_PER_ACCT,     -- exceeded burst buffer */
        WAIT_QOS_MAX_CPU_PER_ACCT,    -- exceeded CPUs */
        WAIT_QOS_MAX_ENERGY_PER_ACCT, -- exceeded Energy */
        WAIT_QOS_MAX_GRES_PER_ACCT,   -- exceeded GRES */
        WAIT_QOS_MAX_NODE_PER_ACCT,   -- exceeded Nodes */
        WAIT_QOS_MAX_LIC_PER_ACCT,    -- exceeded Licenses */
        WAIT_QOS_MAX_MEM_PER_ACCT,    -- exceeded Memory */
        WAIT_QOS_MAX_UNK_PER_ACCT,    -- exceeded Unknown */
        --*******************/
        WAIT_QOS_MAX_JOB_PER_ACCT,    -- QOS MaxJobPerAccount exceeded */
         WAIT_QOS_MAX_SUB_JOB_PER_ACCT, -- QOS MaxJobSubmitSPerAccount exceeded */
        WAIT_PART_CONFIG,             -- Generic partition configuration reason */
        WAIT_ACCOUNT_POLICY,          -- Generic accounting policy reason */

        WAIT_FED_JOB_LOCK,            -- Can't get fed job lock */
        FAIL_OOM,                     -- Exhausted memory */
        WAIT_PN_MEM_LIMIT,            -- MaxMemPer[CPU|Node] exceeded */

        -- exceeded Billing TRES limits */
        WAIT_ASSOC_GRP_BILLING,             -- GrpTRES           */
        WAIT_ASSOC_GRP_BILLING_MIN,         -- GrpTRESMins       */
        WAIT_ASSOC_GRP_BILLING_RUN_MIN,     -- GrpTRESRunMins    */
        WAIT_ASSOC_MAX_BILLING_PER_JOB,     -- MaxTRESPerJob     */
        WAIT_ASSOC_MAX_BILLING_PER_NODE,    -- MaxTRESPerNode    */
        WAIT_ASSOC_MAX_BILLING_MINS_PER_JOB, -- MaxTRESMinsPerJob */

        WAIT_QOS_GRP_BILLING,               -- GrpTRES           */
        WAIT_QOS_GRP_BILLING_MIN,           -- GrpTRESMins       */
        WAIT_QOS_GRP_BILLING_RUN_MIN,       -- GrpTRESRunMins    */
        WAIT_QOS_MAX_BILLING_PER_JOB,       -- MaxTRESPerJob     */
        WAIT_QOS_MAX_BILLING_PER_NODE,      -- MaxTRESPerNode    */
        WAIT_QOS_MAX_BILLING_PER_USER,      -- MaxTRESPerUser    */
        WAIT_QOS_MAX_BILLING_MINS_PER_JOB,  -- MaxTRESMinsPerJob */
        WAIT_QOS_MAX_BILLING_PER_ACCT,      -- MaxTRESPerAcct    */
        WAIT_QOS_MIN_BILLING,               -- MinTRESPerJob     */

        WAIT_RESV_DELETED             -- Reservation was deleted */
                         );

   type Job is private;
   type List is private;
   type Cursor is private;
   function Element (Position : Cursor) return Job;
   function Has_Element (Position : Cursor) return Boolean;
   function First (Collection : List) return Cursor;
   procedure Append (Collection : in out List; Item : Job);

   procedure Iterate (Collection : List;
                      Process    : not null access procedure (Position : Cursor));
   function Get_Alloc_Node (J : Job) return String;
   function Get_Command (J : Job) return String;
   function Has_Admin_Comment (J : Job) return Boolean;
   function Get_Admin_Comment (J : Job) return String;
   function Has_Comment (J : Job) return Boolean;
   function Get_Comment (J : Job) return String;
   function Get_CPUs (J : Job) return Natural;
   function Get_Dependency (J : Job) return String;
   function Get_Gres (J : Job) return String;
   function Get_Group (J : Job) return User_Name;
   function Get_ID (J : Job) return Positive;
   function Get_Name (J : Job) return String;
   function Get_Nodes (J : Job) return String;
   function Get_Owner (J : Job) return User_Name;
   function Get_Partition (J : Job) return String;
   function Get_Priority (J : Job) return Natural;
   function Get_Project (J : Job) return String;

   function Get_Reservation (J : Job) return String;
   function Has_Start_Time (J : Job) return Boolean;
   function Get_Start_Time (J : Job) return Ada.Calendar.Time;
   function Get_End_Time (J : Job) return Ada.Calendar.Time;

   function Walltime (J : Job) return Duration;
   function Get_State (J : Job) return String;
   function Get_State (J : Job) return states;
   function Get_State_Description (J : Job) return String;
   function Get_State_Reason (J : Job) return state_reasons;
   function Get_Submission_Time (J : Job) return Ada.Calendar.Time;
   function Get_Tasks (J : Job) return Positive;
   function Get_Std_In (J : Job) return String;
   function Get_Std_Out (J : Job) return String;
   function Get_Std_Err (J : Job) return String;
   function Get_TRES_Request (J : Job) return String;
   function Get_TRES_Allocated (J : Job) return String;
   function Get_Working_Directory (J : Job) return String;
   function Has_Error (J : Job) return Boolean;
   function Is_Pending (J : Job) return Boolean;
   function Is_Running (J : Job) return Boolean;
   function Has_Share (J : Job) return Boolean;

   function Extract (Source   : List;
                     Selector : not null access function (J : Job) return Boolean)
                     return List;

   function Load_Jobs return List;
   function Load_User (User : String) return List;

   procedure Get_Summary (Collection : List;
                          Jobs, Tasks : out State_Count);
   function Get_Job (Collection : List; ID : Natural) return Job;

private

   type Job is record
      Comment, Admin_Comment : Unbounded_String;
      Alloc_Node  : Unbounded_String;
      Gres        : Unbounded_String;
      ID          : Positive;
      Name        : Unbounded_String;
      Owner       : User_Name;
      Group       : User_Name;
      Priority    : Natural;
      Project     : Unbounded_String;
      Has_Start_Time : Boolean;
      Start_Time : Ada.Calendar.Time;
      Has_End_Time : Boolean;
      End_Time       : Ada.Calendar.Time;
      Shared         : Boolean;
      State          : states;
      State_Desc     : Unbounded_String;
      State_Reason   : state_reasons;
      Submission_Time : Ada.Calendar.Time;
      Tasks           : Natural;
      CPUs            : Natural;
      Dependency      : Unbounded_String;
      Nodes           : Unbounded_String;
      Partition       : Unbounded_String;
      Reservation     : Unbounded_String;
      Std_In, Std_Err, Std_Out : Unbounded_String;
      Directory                : Unbounded_String;
      Command                  : Unbounded_String;
      TRES_Request, TRES_Allocated : Unbounded_String;
   end record;

   package Lists is new ada.Containers.Doubly_Linked_Lists (Element_Type => Job);
   type Cursor is new Lists.Cursor;
   type List is record
      Container : Lists.List;
   end record;

end Slurm.Jobs;
