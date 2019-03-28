with Ada.Calendar.Conversions;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with POSIX;
with POSIX.C; use POSIX.C;
with Slurm.C_Types; use Slurm.C_Types;
with Slurm.Errors;

package body Slurm.Jobs is
   type job_resources_t is null record;
   type dynamic_plugin_data_t is null record;

   type job_info is record
      account                : chars_ptr;                      -- charge to specified account
      admin_comment          : chars_ptr;      -- administrator's arbitrary comment
      alloc_node             : chars_ptr;    -- local node making resource alloc
      alloc_sid              : uint32_t; -- local sid making resource alloc
      array_bitmap           : chars_ptr; -- actually void*
      --  NOTE: set on unpack
      array_job_id           : uint32_t; -- job_id of a job array or 0 if N/A
      array_task_id          : uint32_t;        -- task_id of a job array
      array_max_tasks        : uint32_t;                     -- Maximum number of running tasks
      array_task_str         : chars_ptr;      -- string expression of task IDs in this record
      assoc_id               : uint32_t; -- association id for job
      batch_flag             : uint16_t; -- 1 if batch: queued job with script
      batch_host             : chars_ptr;    -- name of host running batch script
      bitflags               : uint32_t;                     -- Various job flags
      boards_per_node        : uint16_t; -- boards per node required by job
      burst_buffer           : chars_ptr; -- burst buffer specifications
      burst_buffer_state     : chars_ptr; -- burst buffer state info
      cluster                : chars_ptr; -- name of cluster that the job is on
      cluster_features       : chars_ptr; -- comma separated list of required cluster
                                          --  features
      command                : chars_ptr; -- command to be executed, built from submitted
                                          --  job's argv and NULL for salloc command
      comment                : chars_ptr; -- arbitrary comment
      contiguous             : uint16_t; -- 1 if job requires contiguous nodes
      core_spec              : uint16_t; -- specialized core count
      cores_per_socket       : uint16_t; -- cores per socket required by job
      billable_tres          : Interfaces.C.double;   -- billable TRES cache. updated upon resize
      cpus_per_task          : uint16_t;  -- number of processors required for
                                          --  each task
      cpu_freq_min           : uint32_t;                     -- Minimum cpu frequency
      cpu_freq_max           : uint32_t;                     -- Maximum cpu frequency
      cpu_freq_gov           : uint32_t;                     -- cpu frequency governor
      deadline               : time_t;   -- deadline
      delay_boot             : uint32_t; -- delay boot for desired node state
      dependency             : chars_ptr;      -- synchronize job execution with other jobs
      derived_ec             : uint32_t; -- highest exit code of all job steps
      eligible_time          : time_t;   -- time job is eligible for running
      end_time               : time_t;   -- time of termination, actual or expected
      exc_nodes              : chars_ptr;     -- comma separated list of excluded nodes
      exc_node_inx           : access int32_t;   -- excluded list index pairs into node_table:
      --  start_range_1, end_range_1,
      --  start_range_2, .., -1
      exit_code              : uint32_t; -- exit code for job (status from wait call)
      features               : chars_ptr;            -- comma separated list of required features
      fed_origin_str         : chars_ptr;      -- Origin cluster's name
      fed_siblings_active    : uint64_t;                     -- bitmap of active fed sibling ids
      fed_siblings_active_str : chars_ptr;                  -- string of active sibling names
      fed_siblings_viable    : uint64_t;                     -- bitmap of viable fed sibling ids
      fed_siblings_viable_str : chars_ptr;                  -- string of viable sibling names
      gres                   : chars_ptr;              -- comma separated list of generic resources
      gres_detail_cnt          : uint32_t;                     -- Count of gres_detail_str records,
      --  one per allocated node
      gres_detail_str        : char_ptr_ptr;       -- Details of GRES index alloc per node
      group_id                  : uint32_t;        -- group job submitted as
      job_id                     : uint32_t;        -- job ID
      job_resrcs             : access job_resources_t;       -- opaque data type, job resources
      job_state              : uint32_t;   -- state of the job, see enum job_states
      last_sched_eval        : time_t;     -- last time job was evaluated for scheduling
      licenses               : chars_ptr;  -- licenses required by the job
      max_cpus               : uint32_t; -- maximum number of cpus usable by job
      max_nodes              : uint32_t; -- maximum number of nodes usable by job
      mcs_label              : chars_ptr; -- mcs_label if mcs plugin in use
      name                   : chars_ptr; -- name of the job
      network                : chars_ptr; -- network specification
      nodes                  : chars_ptr; -- list of nodes allocated to job
      nice                   : uint32_t;  -- requested priority change
      node_inx               : access uint32_t; -- list index pairs into node_table for *nodes:
      --  start_range_1, end_range_1,
      --  start_range_2, .., -1
      ntasks_per_core        : uint16_t; -- number of tasks to invoke on each core
      ntasks_per_node        : uint16_t; -- number of tasks to invoke on each node
      ntasks_per_socket      : uint16_t; -- number of tasks to invoke on each socket
      ntasks_per_board       : uint16_t; -- number of tasks to invoke on each board
      num_cpus               : uint32_t; -- minimum number of cpus required by job
      num_nodes              : uint32_t; -- minimum number of nodes required by job
      num_tasks              : uint32_t; -- requested task count
      pack_job_id            : uint32_t; -- lead job ID of pack job leader
      pack_job_id_set        : chars_ptr; -- job IDs for all components
      pack_job_offset        : uint32_t;  -- pack job index
      partition              : chars_ptr; -- name of assigned partition
      pn_min_memory          : uint64_t;  -- minimum real memory per node, default=0
      pn_min_cpus            : uint16_t;  -- minimum # CPUs per node, default=0
      pn_min_tmp_disk        : uint32_t;  -- minimum tmp disk per node, default=0
      power_flags            : uint8_t; -- power management flags,
      --  see SLURM_POWER_FLAGS_
      preempt_time           : time_t;  -- preemption signal time
      pre_sus_time           : time_t;  -- time job ran prior to last suspend
      priority               : uint32_t;        -- relative priority of the job,
      --  0=held, 1=required nodes DOWN/DRAINED
      profile                : uint32_t;        -- Level of acct_gather_profile {all | none}
      qos                    : chars_ptr;             -- Quality of Service
      reboot                 : uint8_t;         -- node reboot requested before start
      req_nodes              : chars_ptr;     -- comma separated list of required nodes
      req_node_inx           : access int32_t;  -- required list index pairs into node_table:
      --  start_range_1, end_range_1,
      --  start_range_2, .., -1
      req_switch             : uint32_t;                     -- Minimum number of switches
      requeue_enabled        : uint16_t;                     -- enable or disable job requeue option
      resize_time            : time_t;  -- time of latest size change
      restart_cnt            : uint16_t;        -- count of job restarts
      resv_name              : chars_ptr;     -- reservation name
      sched_nodes            : chars_ptr;     -- list of nodes scheduled to be used for job
      select_jobinfo         : access dynamic_plugin_data_t; -- opaque data type,
      --  process using
      --  slurm_get_select_jobinfo()

      shared                 : uint16_t;        -- 1 if job can share nodes with other jobs
      show_flags             : uint16_t;        -- conveys level of details requested
      sockets_per_board      : uint16_t; -- sockets per board required by job
      sockets_per_node       : uint16_t;                     -- sockets per node required by job
      start_time             : time_t;   -- time execution begins, actual or expected
      start_protocol_ver     : uint16_t;                     -- Slurm version step was started with
      --        either srun or the lowest slurmd version
      --        it is talking to
      state_desc             : chars_ptr;     -- optional details for state_reason
      state_reason           : uint16_t;        -- reason job still pending or failed, see
      --  slurm.h:enum job_state_reason
      std_err                : chars_ptr;             -- pathname of job's stderr file
      std_in                 : chars_ptr;             -- pathname of job's stdin file
      std_out                : chars_ptr;             -- pathname of job's stdout file
      submit_time            : time_t;  -- time of job submission
      suspend_time           : time_t;  -- time job last suspended or resumed
      time_limit             : uint32_t;        -- maximum run time in minutes or INFINITE
      time_min               : uint32_t;        -- minimum run time in minutes or INFINITE
      threads_per_core       : uint16_t;                     -- threads per core required by job
      tres_req_str           : chars_ptr;     -- tres reqeusted in the job
      tres_alloc_str         : chars_ptr;                  -- tres used in the job
      user_id                : uint32_t;        -- user the job runs as
      user_name              : chars_ptr;     -- user_name or null. not always set, but
      --  accurate if set (and can avoid a local
      --  lookup call)
      wait4switch            : uint32_t;      -- Maximum time to wait for minimum switches
      wckey                  : chars_ptr;     -- wckey for job
      work_dir               : chars_ptr;     -- pathname of working directory
   end record;

   Default_Terminator : job_info;
   pragma Warnings (off, Default_Terminator);
   --  this is a dummy. The job list given by the slurm API isn't
   --  terminated, and subprograms from job_info_ptrs (below) that
   --  use termination must not be called

   type job_array is array (uint32_t range <>) of aliased job_info;
   package job_info_ptrs is new Interfaces.C.Pointers (Index         => uint32_t,
                                                       Element       => job_info,
                                                       Element_Array => job_array,
                                                      Default_Terminator => Default_Terminator);
   subtype job_info_ptr is job_info_ptrs.Pointer;

   type job_info_msg_t is record
      last_update  : time_t;     -- time of latest info
      record_count : uint32_t;   -- number of records
      job_array    : job_info_ptr; -- the job records
   end record;

   type job_info_msg_ptr is access constant job_info_msg_t;
   type job_info_msg_ptr_ptr is access constant job_info_msg_ptr;

   function slurm_load_jobs (update_time       : time_t;
                             job_info_msg_pptr : job_info_msg_ptr_ptr;
                             show_flags        : uint16_t) return int;
   pragma Import (C, slurm_load_jobs, "slurm_load_jobs");

   function slurm_load_job_user (job_info_msg_pptr : job_info_msg_ptr_ptr;
                                           user_id : uint32_t;
                                 show_flags        : uint16_t) return int;
   pragma Import (C, slurm_load_job_user, "slurm_load_job_user");

   procedure slurm_free_job_info_msg (job_info_msg_p : job_info_msg_ptr);
   pragma Import (C, slurm_free_job_info_msg, "slurm_free_job_info_msg");

   type Enum_To_State_Map is array (uint32_t range 0 .. 11) of states;
   Enum_To_State : constant Enum_To_State_Map :=
                     (0 => JOB_PENDING,
                      1 => JOB_RUNNING,
                      2 => JOB_SUSPENDED,
                      3 => JOB_COMPLETE,
                      4 => JOB_CANCELLED,
                      5 => JOB_FAILED,
                      6 => JOB_TIMEOUT,
                      7 => JOB_NODE_FAIL,
                      8 => JOB_PREEMPTED,
                      9 => JOB_BOOT_FAIL,
                      10 => JOB_DEADLINE,
                      11 => JOB_OOM);

   type Enum_To_Reason_Map is array (uint16_t range
                                     0 .. 197) of state_reasons;
   Enum_To_Reason : constant Enum_To_Reason_Map :=
                      (
0 => WAIT_NO_REASON,
1 => WAIT_PRIORITY,
2 => WAIT_DEPENDENCY,
3 => WAIT_RESOURCES,
4 => WAIT_PART_NODE_LIMIT,
5 => WAIT_PART_TIME_LIMIT,
6 => WAIT_PART_DOWN,
7 => WAIT_PART_INACTIVE,
8 => WAIT_HELD,
9 => WAIT_TIME,
10 => WAIT_LICENSES,
11 => WAIT_ASSOC_JOB_LIMIT,
12 => WAIT_ASSOC_RESOURCE_LIMIT,
13 => WAIT_ASSOC_TIME_LIMIT,
14 => WAIT_RESERVATION,
15 => WAIT_NODE_NOT_AVAIL,
16 => WAIT_HELD_USER,
17 => WAIT_FRONT_END,
18 => FAIL_DOWN_PARTITION,
19 => FAIL_DOWN_NODE,
20 => FAIL_BAD_CONSTRAINTS,
21 => FAIL_SYSTEM,
22 => FAIL_LAUNCH,
23 => FAIL_EXIT_CODE,
24 => FAIL_TIMEOUT,
25 => FAIL_INACTIVE_LIMIT,
26 => FAIL_ACCOUNT,
27 => FAIL_QOS,
28 => WAIT_QOS_THRES,
29 => WAIT_QOS_JOB_LIMIT,
30 => WAIT_QOS_RESOURCE_LIMIT,
31 => WAIT_QOS_TIME_LIMIT,
32 => WAIT_BLOCK_MAX_ERR,
33 => WAIT_BLOCK_D_ACTION,
34 => WAIT_CLEANING,
35 => WAIT_PROLOG,
36 => WAIT_QOS,
37 => WAIT_ACCOUNT,
38 => WAIT_DEP_INVALID,
39 => WAIT_QOS_GRP_CPU,
40 => WAIT_QOS_GRP_CPU_MIN,
41 => WAIT_QOS_GRP_CPU_RUN_MIN,
42 => WAIT_QOS_GRP_JOB,
43 => WAIT_QOS_GRP_MEM,
44 => WAIT_QOS_GRP_NODE,
45 => WAIT_QOS_GRP_SUB_JOB,
46 => WAIT_QOS_GRP_WALL,
47 => WAIT_QOS_MAX_CPU_PER_JOB,
48 => WAIT_QOS_MAX_CPU_MINS_PER_JOB,
49 => WAIT_QOS_MAX_NODE_PER_JOB,
50 => WAIT_QOS_MAX_WALL_PER_JOB,
51 => WAIT_QOS_MAX_CPU_PER_USER,
52 => WAIT_QOS_MAX_JOB_PER_USER,
53 => WAIT_QOS_MAX_NODE_PER_USER,
54 => WAIT_QOS_MAX_SUB_JOB,
55 => WAIT_QOS_MIN_CPU,
56 => WAIT_ASSOC_GRP_CPU,
57 => WAIT_ASSOC_GRP_CPU_MIN,
58 => WAIT_ASSOC_GRP_CPU_RUN_MIN,
59 => WAIT_ASSOC_GRP_JOB,
60 => WAIT_ASSOC_GRP_MEM,
61 => WAIT_ASSOC_GRP_NODE,
62 => WAIT_ASSOC_GRP_SUB_JOB,
63 => WAIT_ASSOC_GRP_WALL,
64 => WAIT_ASSOC_MAX_JOBS,
65 => WAIT_ASSOC_MAX_CPU_PER_JOB,
66 => WAIT_ASSOC_MAX_CPU_MINS_PER_JOB,
67 => WAIT_ASSOC_MAX_NODE_PER_JOB,
68 => WAIT_ASSOC_MAX_WALL_PER_JOB,
69 => WAIT_ASSOC_MAX_SUB_JOB,
70 => WAIT_MAX_REQUEUE,
71 => WAIT_ARRAY_TASK_LIMIT,
72 => WAIT_BURST_BUFFER_RESOURCE,
73 => WAIT_BURST_BUFFER_STAGING,
74 => FAIL_BURST_BUFFER_OP,
75 => WAIT_POWER_NOT_AVAIL,
76 => WAIT_POWER_RESERVED,
77 => WAIT_ASSOC_GRP_UNK,
78 => WAIT_ASSOC_GRP_UNK_MIN,
79 => WAIT_ASSOC_GRP_UNK_RUN_MIN,
80 => WAIT_ASSOC_MAX_UNK_PER_JOB,
81 => WAIT_ASSOC_MAX_UNK_PER_NODE,
82 => WAIT_ASSOC_MAX_UNK_MINS_PER_JOB,
83 => WAIT_ASSOC_MAX_CPU_PER_NODE,
84 => WAIT_ASSOC_GRP_MEM_MIN,
85 => WAIT_ASSOC_GRP_MEM_RUN_MIN,
86 => WAIT_ASSOC_MAX_MEM_PER_JOB,
87 => WAIT_ASSOC_MAX_MEM_PER_NODE,
88 => WAIT_ASSOC_MAX_MEM_MINS_PER_JOB,
89 => WAIT_ASSOC_GRP_NODE_MIN,
90 => WAIT_ASSOC_GRP_NODE_RUN_MIN,
91 => WAIT_ASSOC_MAX_NODE_MINS_PER_JOB,
92 => WAIT_ASSOC_GRP_ENERGY,
93 => WAIT_ASSOC_GRP_ENERGY_MIN,
94 => WAIT_ASSOC_GRP_ENERGY_RUN_MIN,
95 => WAIT_ASSOC_MAX_ENERGY_PER_JOB,
96 => WAIT_ASSOC_MAX_ENERGY_PER_NODE,
97 => WAIT_ASSOC_MAX_ENERGY_MINS_PER_JOB,
98 => WAIT_ASSOC_GRP_GRES,
99 => WAIT_ASSOC_GRP_GRES_MIN,
100 => WAIT_ASSOC_GRP_GRES_RUN_MIN,
101 => WAIT_ASSOC_MAX_GRES_PER_JOB,
102 => WAIT_ASSOC_MAX_GRES_PER_NODE,
103 => WAIT_ASSOC_MAX_GRES_MINS_PER_JOB,
104 => WAIT_ASSOC_GRP_LIC,
105 => WAIT_ASSOC_GRP_LIC_MIN,
106 => WAIT_ASSOC_GRP_LIC_RUN_MIN,
107 => WAIT_ASSOC_MAX_LIC_PER_JOB,
108 => WAIT_ASSOC_MAX_LIC_MINS_PER_JOB,
109 => WAIT_ASSOC_GRP_BB,
110 => WAIT_ASSOC_GRP_BB_MIN,
111 => WAIT_ASSOC_GRP_BB_RUN_MIN,
112 => WAIT_ASSOC_MAX_BB_PER_JOB,
113 => WAIT_ASSOC_MAX_BB_PER_NODE,
114 => WAIT_ASSOC_MAX_BB_MINS_PER_JOB,
115 => WAIT_QOS_GRP_UNK,
116 => WAIT_QOS_GRP_UNK_MIN,
117 => WAIT_QOS_GRP_UNK_RUN_MIN,
118 => WAIT_QOS_MAX_UNK_PER_JOB,
119 => WAIT_QOS_MAX_UNK_PER_NODE,
120 => WAIT_QOS_MAX_UNK_PER_USER,
121 => WAIT_QOS_MAX_UNK_MINS_PER_JOB,
122 => WAIT_QOS_MIN_UNK,
123 => WAIT_QOS_MAX_CPU_PER_NODE,
124 => WAIT_QOS_GRP_MEM_MIN,
125 => WAIT_QOS_GRP_MEM_RUN_MIN,
126 => WAIT_QOS_MAX_MEM_MINS_PER_JOB,
127 => WAIT_QOS_MAX_MEM_PER_JOB,
128 => WAIT_QOS_MAX_MEM_PER_NODE,
129 => WAIT_QOS_MAX_MEM_PER_USER,
130 => WAIT_QOS_MIN_MEM,
131 => WAIT_QOS_GRP_ENERGY,
132 => WAIT_QOS_GRP_ENERGY_MIN,
133 => WAIT_QOS_GRP_ENERGY_RUN_MIN,
134 => WAIT_QOS_MAX_ENERGY_PER_JOB,
135 => WAIT_QOS_MAX_ENERGY_PER_NODE,
136 => WAIT_QOS_MAX_ENERGY_PER_USER,
137 => WAIT_QOS_MAX_ENERGY_MINS_PER_JOB,
138 => WAIT_QOS_MIN_ENERGY,
139 => WAIT_QOS_GRP_NODE_MIN,
140 => WAIT_QOS_GRP_NODE_RUN_MIN,
141 => WAIT_QOS_MAX_NODE_MINS_PER_JOB,
142 => WAIT_QOS_MIN_NODE,
143 => WAIT_QOS_GRP_GRES,
144 => WAIT_QOS_GRP_GRES_MIN,
145 => WAIT_QOS_GRP_GRES_RUN_MIN,
146 => WAIT_QOS_MAX_GRES_PER_JOB,
147 => WAIT_QOS_MAX_GRES_PER_NODE,
148 => WAIT_QOS_MAX_GRES_PER_USER,
149 => WAIT_QOS_MAX_GRES_MINS_PER_JOB,
150 => WAIT_QOS_MIN_GRES,
151 => WAIT_QOS_GRP_LIC,
152 => WAIT_QOS_GRP_LIC_MIN,
153 => WAIT_QOS_GRP_LIC_RUN_MIN,
154 => WAIT_QOS_MAX_LIC_PER_JOB,
155 => WAIT_QOS_MAX_LIC_PER_USER,
156 => WAIT_QOS_MAX_LIC_MINS_PER_JOB,
157 => WAIT_QOS_MIN_LIC,
158 => WAIT_QOS_GRP_BB,
159 => WAIT_QOS_GRP_BB_MIN,
160 => WAIT_QOS_GRP_BB_RUN_MIN,
161 => WAIT_QOS_MAX_BB_PER_JOB,
162 => WAIT_QOS_MAX_BB_PER_NODE,
163 => WAIT_QOS_MAX_BB_PER_USER,
164 => WAIT_QOS_MAX_BB_MINS_PER_JOB,
165 => WAIT_QOS_MIN_BB,
166 => FAIL_DEADLINE,
167 => WAIT_QOS_MAX_BB_PER_ACCT,
168 => WAIT_QOS_MAX_CPU_PER_ACCT,
169 => WAIT_QOS_MAX_ENERGY_PER_ACCT,
170 => WAIT_QOS_MAX_GRES_PER_ACCT,
171 => WAIT_QOS_MAX_NODE_PER_ACCT,
172 => WAIT_QOS_MAX_LIC_PER_ACCT,
173 => WAIT_QOS_MAX_MEM_PER_ACCT,
174 => WAIT_QOS_MAX_UNK_PER_ACCT,
175 => WAIT_QOS_MAX_JOB_PER_ACCT,
176 => WAIT_QOS_MAX_SUB_JOB_PER_ACCT,
177 => WAIT_PART_CONFIG,
178 => WAIT_ACCOUNT_POLICY,
179 => WAIT_FED_JOB_LOCK,
180 => FAIL_OOM,
181 => WAIT_PN_MEM_LIMIT,
182 => WAIT_ASSOC_GRP_BILLING,
183 => WAIT_ASSOC_GRP_BILLING_MIN,
184 => WAIT_ASSOC_GRP_BILLING_RUN_MIN,
185 => WAIT_ASSOC_MAX_BILLING_PER_JOB,
186 => WAIT_ASSOC_MAX_BILLING_PER_NODE,
187 => WAIT_ASSOC_MAX_BILLING_MINS_PER_JOB,
188 => WAIT_QOS_GRP_BILLING,
189 => WAIT_QOS_GRP_BILLING_MIN,
190 => WAIT_QOS_GRP_BILLING_RUN_MIN,
191 => WAIT_QOS_MAX_BILLING_PER_JOB,
192 => WAIT_QOS_MAX_BILLING_PER_NODE,
193 => WAIT_QOS_MAX_BILLING_PER_USER,
194 => WAIT_QOS_MAX_BILLING_MINS_PER_JOB,
195 => WAIT_QOS_MAX_BILLING_PER_ACCT,
196 => WAIT_QOS_MIN_BILLING,
197 => WAIT_RESV_DELETED

                         );

   JOB_STATE_BASE : constant uint32_t := 16#ff#;
   JOB_LAUNCH_FAILED : constant uint32_t := 16#00000100#;
   JOB_UPDATE_DB     : constant uint32_t := 16#00000200#; --  Send job start to database again
   JOB_REQUEUE       : constant uint32_t := 16#00000400#; --  Requeue job in completing state
   JOB_REQUEUE_HOLD  : constant uint32_t := 16#00000800#; --  Requeue any job in hold
   JOB_SPECIAL_EXIT  : constant uint32_t := 16#00001000#; --  Requeue an exit job in hold
   JOB_RESIZING      : constant uint32_t := 16#00002000#; --  Size of job about to change, flag set
                                --      before calling accounting functions
                                --      immediately before job changes size
   JOB_CONFIGURING   : constant uint32_t := 16#00004000#; --  Allocated nodes booting
   JOB_COMPLETING    : constant uint32_t := 16#00008000#; --  Waiting for epilog completion
   JOB_STOPPED       : constant uint32_t := 16#00010000#; --  Job is stopped state (holding resources,
                                                          --                      but sent SIGSTOP
   JOB_RECONFIG_FAIL : constant uint32_t := 16#00020000#; --  Node configuration for job failed,
--                                      not job state, just job requeue flag
   JOB_POWER_UP_NODE  : constant uint32_t := 16#00040000#; --  Allocated powered down nodes,
                                                           -- waiting for reboot
   JOB_REVOKED        : constant uint32_t := 16#00080000#; --  Sibling job revoked
   JOB_REQUEUE_FED    : constant uint32_t := 16#00100000#; --  Job is being requeued by federation
   JOB_RESV_DEL_HOLD : constant uint32_t := 16#00200000#; --  Job is hold

   function getpwnam (c_name : chars_ptr) return passwd_ptr;
   pragma Import (C, getpwnam, "getpwnam");

   function getpwuid (c_uid : uid_t) return passwd_ptr;
   pragma Import (C, getpwuid, "getpwuid");

   function getgrgid (c_gid : gid_t) return group_ptr;
   pragma Import (C, getgrgid, "getgrgid");

   procedure Init (J : out Job; Ptr : job_info_ptr);
   function Build_List (Buffer : aliased job_info_msg_ptr) return List;

   function Build_List (Buffer : aliased job_info_msg_ptr) return List is
      use job_info_ptrs;
      Job_Ptr : job_info_ptr;
      J       : Job;
      Result : List;
   begin
      Job_Ptr := Buffer.job_array;
      for I in 1 .. Buffer.record_count loop
         Init (J, Job_Ptr);
         Result.Container.Append (J);
         Increment (Job_Ptr);
      end loop;
      slurm_free_job_info_msg (Buffer);
      return Result;
   end Build_List;

   overriding function Element (Position : Cursor) return Job is
   begin
      return Lists.Element (Lists.Cursor (Position));
   end Element;

   function First (Collection : List) return Cursor is
   begin
      return Cursor (Collection.Container.First);
   end First;

   function Get_CPUs (J : Job) return Natural is
   begin
      return J.CPUs;
   end Get_CPUs;

   function Get_Dependency (J : Job) return String is
   begin
      return To_String (J.Dependency);
   end Get_Dependency;

   function Get_Gres (J : Job) return String is
   begin
      return To_String (J.Gres);
   end Get_Gres;

   function Get_Group (J : Job) return User_Name is
   begin
      return J.Group;
   end Get_Group;

   function Get_ID (J : Job) return Positive is
   begin
      return J.ID;
   end Get_ID;

   function Get_Job (Collection : List; ID : Natural) return Job is
      Position : Cursor := First (Collection);
   begin
      while Has_Element (Position)
      loop
         if Element (Position).ID = ID then
            return Element (Position);
         else
            Next (Position);
         end if;
      end loop;
      raise Constraint_Error with "Job not found";
   end Get_Job;

   function Get_Name (J : Job) return String is
   begin
      return To_String (J.Name);
   end Get_Name;

   function Get_Nodes (J : Job) return String is
   begin
      return To_String (J.Nodes);
   end Get_Nodes;

   function Get_Owner (J : Job) return User_Name is
   begin
      return J.Owner;
   end Get_Owner;

   function Get_Partition (J : Job) return String is
   begin
      return To_String (J.Partition);
   end Get_Partition;

   function Get_Priority (J : Job) return Natural is
   begin
      return J.Priority;
   end Get_Priority;

   function Get_Project (J : Job) return String is
   begin
      return To_String (J.Project);
   end Get_Project;

   function Get_Reservation (J : Job) return String is
   begin
      return To_String (J.Reservation);
   end Get_Reservation;

   function Get_Start_Time (J : Job) return Ada.Calendar.Time is
   begin
      return J.Start_Time;
   end Get_Start_Time;

   function Get_State (J : Job) return String is
   begin
      return J.State'Img;
   end Get_State;

   function Get_State_Description (J : Job) return String is
   begin
      return To_String (J.State_Desc);
   end Get_State_Description;

   function Get_State_Reason (J : Job) return state_reasons is
   begin
      return J.State_Reason;
   end Get_State_Reason;

   function Get_Submission_Time (J : Job) return Ada.Calendar.Time is
   begin
      return J.Submission_Time;
   end Get_Submission_Time;

   procedure Get_Summary (Collection : List;
                          Jobs, Tasks : out State_Count) is
      procedure Increment (Position : Cursor) is
         J : Job := Element (Position);
      begin
         Jobs (J.State) :=  Jobs (J.State) + 1;
         Tasks (J.State) := Tasks (J.State) + J.Tasks;
      end Increment;

   begin
      Jobs := (others => 0);
      Tasks := (others => 0);
      Iterate (Collection, Increment'Access);
   end Get_Summary;

   function Get_Tasks (J : Job) return Positive is
   begin
      return J.Tasks;
   end Get_Tasks;

   overriding function Has_Element (Position : Cursor) return Boolean is
   begin
      return Lists.Has_Element (Lists.Cursor (Position));
   end Has_Element;

   function Has_Error (J : Job) return Boolean is
      pragma Unreferenced (J);
   begin
      return False; -- until we figure out what state is equivalent to sge's error state
   end Has_Error;

   function Has_Start_Time (J : Job) return Boolean is
   begin
      return J.Has_Start_Time;
   end Has_Start_Time;

   procedure Init (J : out Job; Ptr : job_info_ptr) is
   begin
      J.Gres := To_Unbounded_String (To_String (Ptr.all.gres));
      J.ID := Integer (Ptr.all.job_id);
      J.Name := To_Unbounded_String (To_String (Ptr.all.name));
      declare
         Given_Name : String := To_String (Ptr.all.user_name);
         pw_entry : passwd_ptr;
      begin
         if Given_Name /= "" then
            J.Owner := To_User_Name (Given_Name);
         else
            pw_entry := getpwuid (uid_t (Ptr.all.user_id));
            if pw_entry = null
            then
               raise Constraint_Error;
            end if;
            J.Owner := To_User_Name (POSIX.To_String (Form_POSIX_String (pw_entry.all.pw_name)));
         end if;
      end;
      J.Priority := Natural (Ptr.all.priority);
      J.Project := To_Unbounded_String (To_String (Ptr.all.wckey));
      J.Start_Time := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (Ptr.all.start_time));
      if Ptr.all.start_time = 0 then
         J.Has_Start_Time := False;
      else
         J.Has_Start_Time := True;
      end if;
      J.State := Enum_To_State (Ptr.all.job_state and JOB_STATE_BASE);
      J.Submission_Time := Ada.Calendar.Conversions.To_Ada_Time
        (Interfaces.C.long (Ptr.all.submit_time));
      J.Tasks := Integer (Ptr.all.num_tasks);
      J.CPUs := Integer (Ptr.all.num_cpus);
      J.Dependency := To_Unbounded_String (To_String (Ptr.all.dependency));
      declare
         gr_entry : group_ptr;
      begin
         gr_entry := getgrgid (gid_t (Ptr.all.group_id));
         if gr_entry = null
         then
            raise Constraint_Error;
         end if;
         J.Group := To_User_Name (POSIX.To_String (Form_POSIX_String (gr_entry.all.gr_name)));
      end;
      J.Nodes := To_Unbounded_String (To_String (Ptr.all.nodes));
      J.Partition := To_Unbounded_String (To_String (Ptr.all.partition));
      J.Reservation := To_Unbounded_String (To_String (Ptr.all.resv_name));
      J.State_Desc := To_Unbounded_String (To_String (Ptr.all.state_desc));
      J.State_Reason := Enum_To_Reason (Ptr.all.state_reason);
   end Init;

   function Is_Running (J : Job) return Boolean is
   begin
      return J.State = JOB_RUNNING;
   end Is_Running;

   procedure Iterate (Collection : List;
                      Process    : not null access procedure (Position : Cursor)) is
      procedure Wrapper (Position : Lists.Cursor);

      procedure Wrapper (Position : Lists.Cursor) is
      begin
         Process (Cursor (Position));
      end Wrapper;
   begin
      Collection.Container.Iterate (Wrapper'Access);
   end Iterate;

   function Load_Jobs return List is
      use Slurm.Errors;
      use job_info_ptrs;
      E : Error;
      Buffer : aliased job_info_msg_ptr;
   begin
      if slurm_load_jobs (update_time       => 0,
                          job_info_msg_pptr => Buffer'Unchecked_Access,
                          show_flags        => 0) /= 0
      then
         E := Get_Last_Error;
         case E is
            when Protocol_Version_Error =>
               raise Internal_Error with "Incompatible protocol version";
            when Socket_Timeout =>
               raise Internal_Error with "Couldn't contact slurm controller";
            when others =>
               raise Constraint_Error with Get_Error (E);
         end case;
      end if;
      return Build_List (Buffer);
   end Load_Jobs;

   function Load_User (User : String) return List is
      use Slurm.Errors;
      use job_info_ptrs;
      uid : uid_t;
      pw_entry : passwd_ptr := getpwnam (New_String (User));
      E : Error;
      Buffer : aliased job_info_msg_ptr;
   begin
      if pw_entry = null
      then
         raise Constraint_Error;
      end if;
      uid := pw_entry.all.pw_uid;
      if slurm_load_job_user (job_info_msg_pptr => Buffer'Unchecked_Access,
                              user_id           => uint32_t (uid),
                              show_flags        => 0) /= 0
      then
         E := Get_Last_Error;
         case E is
            when Protocol_Version_Error =>
               raise Internal_Error with "Incompatible protocol version";
            when Socket_Timeout =>
               raise Internal_Error with "Couldn't contact slurm controller";
            when others =>
               raise Constraint_Error with Get_Error (E);
         end case;
      end if;
      return Build_List (Buffer);
   end Load_User;

   function Walltime (J : Job) return Duration is
      use Ada.Calendar;
   begin
      if Is_Running (J) then
         return Ada.Calendar.Clock - J.Start_Time;
      else
         return Duration (0);
      end if;
   end Walltime;

end Slurm.Jobs;
