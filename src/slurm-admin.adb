with Interfaces.C.Strings; use Interfaces.C.Strings;

with POSIX.C; use POSIX.C;
with Slurm.C_Types; use Slurm.C_Types;
with Slurm.Errors;

package body Slurm.Admin is

   type dynamic_plugin_data_t is null record;

   type job_desc_msg_t is record
      account : chars_ptr;    --  charge to specified account
      acctg_freq : chars_ptr; --  accounting polling intervals (seconds)
      admin_comment : chars_ptr;  --  administrator's arbitrary comment (update only)
      alloc_node    : chars_ptr; -- node making resource allocation request
         -- NOTE: Normally set by slurm_submit* or
         -- slurm_allocate* function */
      alloc_resp_port : uint16_t; --  port to send allocation confirmation to
      alloc_sid       : uint32_t; -- local sid making resource allocation request
         -- NOTE: Normally set by slurm_submit* or
         -- slurm_allocate* function
         -- NOTE: Also used for update flags, see
         -- ALLOC_SID_* flags */
      argc            : uint32_t;    --  number of arguments to the script
      argv            : char_ptr_ptr;    --  arguments to the script
      array_inx       : chars_ptr;  --  job array index values
      array_bitmap    : chars_ptr; -- actually void*
      --  NOTE: Set by slurmctld
      batch_features : chars_ptr; --  features required for batch script's node
      begin_time        : time_t;  --  delay initiation until this time
      bitflags        : uint64_t;      --  bitflags
      burst_buffer    : chars_ptr; --  burst buffer specifications
      clusters        : chars_ptr;   --  cluster names used for multi-cluster jobs
      cluster_features : chars_ptr; -- required cluster feature specification,
         -- default NONE */
      comment          : chars_ptr;    --  arbitrary comment
      contiguous       : uint16_t;  -- 1 if job requires contiguous nodes,
                                    -- 0 otherwise,default=0 */
      container        : chars_ptr;
      core_spec        : uint16_t; -- specialized core/thread count,
         -- see CORE_SPEC_THREAD */
      cpu_bind         : chars_ptr;   -- binding map for map/mask_cpu - This
         -- currently does not matter to the
         -- job allocation, setting this does
         -- not do anything for steps. */
      cpu_bind_type    : uint16_t; -- see cpu_bind_type_t - This
         -- currently does not matter to the
         -- job allocation, setting this does
         -- not do anything for steps. */
      cpu_freq_min     : uint32_t;  --  Minimum cpu frequency
      cpu_freq_max     : uint32_t;  --  Maximum cpu frequency
      cpu_freq_gov     : uint32_t;  --  cpu frequency governor
      cpus_per_tres     : chars_ptr;  --  semicolon delimited list of TRES=# values
      crontab_entry    : chars_ptr; -- actually void*
      deadline         : time_t;  --  deadline
      delay_boot       : uint32_t;  --  delay boot for desired node state
      dependency       : chars_ptr; --  synchronize job execution with other jobs
      end_time           : time_t;  -- time by which job must complete, used for
         -- job update only now, possible deadline
         -- scheduling in the future */
      environment        : char_ptr_ptr; -- environment variables to set for job,
         --  name=value pairs, one per line */
      env_size         : uint32_t;  --  element count in environment
      extra            : chars_ptr;    --  unused
      exc_nodes        : chars_ptr;  -- comma separated list of nodes excluded
         -- from job's allocation, default NONE */
      features         : chars_ptr;   -- required feature specification,
         -- default NONE */
      fed_siblings_active : uint64_t; --  Bitmap of active fed sibling ids
      fed_siblings_viable : uint64_t; --  Bitmap of viable fed sibling ids
      group_id            : uint32_t;  --  group to assume, if run as root.
      het_job_offset      : uint32_t; --  HetJob component offset
      immediate           : uint16_t; -- 1 if allocate to run or fail immediately,
         -- 0 if to be queued awaiting resources */
      job_id              : uint32_t;  --  job ID, default set by Slurm
      job_id_str            : chars_ptr;      --  string representation of the jobid
      kill_on_node_fail   : uint16_t;  -- 1 if node failure to kill job,
                                       -- 0 otherwise, default=1 */
      licenses              : chars_ptr;   --  licenses required by the job
      mail_type           : uint16_t; --  see MAIL_JOB_ definitions above
      mail_user           : chars_ptr;  --  user to receive notification
      mcs_label           : chars_ptr;  --  mcs_label if mcs plugin in use
      mem_bind            : chars_ptr;   --  binding map for map/mask_cpu
      mem_bind_type       : uint16_t; --  see mem_bind_type_t
      mem_per_tres        : chars_ptr; --  semicolon delimited list of TRES=# values
      name                : chars_ptr;   --  name of the job, default ""
      network             : chars_ptr;    --  network use spec
      nice                : uint32_t;    -- requested priority change,
         -- NICE_OFFSET == no change */
      num_tasks             : uint32_t; -- number of tasks to be started,
         -- for batch only */
      open_mode               : uint8_t;  -- out/err open mode truncate or append,
         -- see OPEN_MODE_* */
      origin_cluster        : chars_ptr; --  cluster name that initiated the job.
      other_port : uint16_t;  --  port to send various notification msg to
      overcommit             : uint8_t; --  over subscribe resources, for batch only
      partition : chars_ptr;  -- name of requested partition,
         -- default in Slurm config */
      plane_size            : uint16_t;   -- plane size when task_dist =
                                          -- SLURM_DIST_PLANE */
      power_flags             : uint8_t;  -- power management flags,
         -- see SLURM_POWER_FLAGS_ */
      priority : uint32_t;  -- relative priority of the job,
         -- explicitly set only for user root,
         -- 0 == held (don't initiate) */
      profile : uint32_t; --  Level of acct_gather_profile {all | none}
      qos : chars_ptr;    --  Quality of Service
      reboot : uint16_t;  --  force node reboot before startup
      resp_host : chars_ptr;  --  NOTE: Set by slurmctld
      restart_cnt : uint16_t; --  count of job restarts
      req_nodes : chars_ptr;  -- comma separated list of required nodes
         -- default NONE */
      s_requeue               : uint16_t; -- actually requeue
      --  enable or disable job requeue option
      reservation : chars_ptr;  --  name of reservation to use
      script : chars_ptr;   --  the actual job script, default NONE
      script_buf : chars_ptr; -- actually void*
      --  job script as mmap buf
      shared : uint16_t;  -- 2 if the job can only share nodes with other
         --   jobs owned by that user,
         -- 1 if job can share nodes with other jobs,
         -- 0 if job needs exclusive access to the node,
         -- or NO_VAL to accept the system default.
         -- SHARED_FORCE to eliminate user control. */
      site_factor : uint32_t; --  factor to consider in priority
      spank_job_env             : char_ptr_ptr; -- environment variables for job prolog/epilog
         -- scripts as set by SPANK plugins */
      spank_job_env_size        : uint32_t; --  element count in spank_env
      submit_line : chars_ptr;
      task_dist : uint32_t; --  see enum task_dist_state
      time_limit : uint32_t;  -- maximum run time in minutes, default is
         -- partition limit */
      time_min : uint32_t;  -- minimum run time in minutes, default is
         -- time_limit */
      tres_bind : chars_ptr;  --  Task to TRES binding directives
      tres_freq : chars_ptr;  --  TRES frequency directives
      tres_per_job : chars_ptr; --  semicolon delimited list of TRES=# values
      tres_per_node : chars_ptr;  --  semicolon delimited list of TRES=# values
      tres_per_socket : chars_ptr;  --  semicolon delimited list of TRES=# values
      tres_per_task : chars_ptr;  --  semicolon delimited list of TRES=# values
      user_id : uint32_t; -- set only if different from current UID,
         -- can only be explicitly set by user root */
      wait_all_nodes : uint16_t; -- 0 to start job immediately after allocation
         -- 1 to start job after all nodes booted
         -- or NO_VAL to use system default */
      warn_flags : uint16_t;  -- flags  related to job signals
         -- (eg. KILL_JOB_BATCH) */
      warn_signal : uint16_t; --  signal to send when approaching end time
      warn_time : uint16_t; --  time before end to send signal (seconds)
      work_dir : chars_ptr;   --  pathname of working directory

      --  job constraints:
      cpus_per_task : uint16_t; -- number of processors required for
         -- each task */
      min_cpus : uint32_t;  -- minimum number of processors required,
         -- default=0 */
      max_cpus : uint32_t;  -- maximum number of processors required,
         -- default=0 */
      min_nodes : uint32_t; -- minimum number of nodes required by job,
         -- default=0 */
      max_nodes : uint32_t; -- maximum number of nodes usable by job,
         -- default=0 */
      boards_per_node : uint16_t; --  boards per node required by job
      sockets_per_board : uint16_t; --  sockets per board required by job
      sockets_per_node : uint16_t; --  sockets per node required by job
      cores_per_socket : uint16_t; --  cores per socket required by job
      threads_per_core : uint16_t; --  threads per core required by job
      ntasks_per_node : uint16_t; --  number of tasks to invoke on each node
      ntasks_per_socket : uint16_t; -- number of tasks to invoke on
            -- each socket */
      ntasks_per_core : uint16_t; --  number of tasks to invoke on each core
      ntasks_per_board          : uint16_t; --  number of tasks to invoke on each board
      ntasks_per_tres : uint16_t;
      pn_min_cpus : uint16_t;    --  minimum # CPUs per node, default=0
      pn_min_memory : uint64_t;  -- minimum real memory per node OR
          -- real memory per CPU | MEM_PER_CPU,
          -- default=0 (no limit) */
      pn_min_tmp_disk : uint32_t;   -- minimum tmp disk per node,
                                    -- default=0 */
      req_context : chars_ptr;
      req_switch : uint32_t;    --  Minimum number of switches
      select_jobinfo              : access dynamic_plugin_data_t; -- opaque data type,
                                                                  -- Slurm internal use only */
      selinux_context : chars_ptr;
      std_err : chars_ptr;    --  pathname of stderr
      std_in : chars_ptr;   --  pathname of stdin
      std_out : chars_ptr;    --  pathname of stdout
      tres_req_cnt                : access uint64_t;  -- used internally in the slurmctld,
                                                      -- DON'T PACK */
      wait4switch : uint32_t;   --  Maximum time to wait for minimum switches
      wckey : chars_ptr;            --  wckey for job
      x11 : uint16_t;   --  --x11 flags
      x11_magic_cookie : chars_ptr; --  automatically stolen from submit node
      x11_target : chars_ptr; --  target hostname, or unix socket if port == 0
      x11_target_port : uint16_t; --  target tcp port, 6000 + the display number
   end record;

   type job_desc_msg_ptr is access constant job_desc_msg_t;

   type update_node_msg_t is record
   -- NOTE: If setting node_addr and/or node_hostname then comma separate names
   -- and include an equal number of node_names
      comment      : chars_ptr;
      cpu_bind     : uint32_t;  -- default CPU binding type
      extra        : chars_ptr;
      features     : chars_ptr;   -- new available feature for node
      features_act : chars_ptr; -- new active feature for node
      gres         : chars_ptr;   -- new generic resources for node
      node_addr    : chars_ptr;  -- communication name (optional)
      node_hostname : chars_ptr;  -- node's hostname (optional)
      node_names   : chars_ptr; -- nodelist expression
      node_state   : uint32_t;  -- see enum node_states
      reason       : chars_ptr;   -- reason for node being DOWN or DRAINING
      reason_uid   : uint32_t;  -- user ID of sending (needed if user
         -- root is sending message)
      weight       : uint32_t;  -- new weight for node
   end record;

   NODE_STATE_DOWN : constant uint32_t := 16#00000001#;
   NODE_STATE_UNDRAIN : constant uint32_t := 16#00000040#;
   NODE_STATE_RESUME : constant uint32_t := 16#00000100#;
   NODE_STATE_DRAIN : constant uint32_t := 16#00000200#;

   type update_node_msg_ptr is access constant update_node_msg_t;

   function slurm_kill_job (job_id : uint32_t;
                            signal : uint16_t;
                            flags  : uint16_t) return int;
   pragma Import (C, slurm_kill_job, "slurm_kill_job");

   function slurm_update_job (job_msg : job_desc_msg_ptr) return int;
   pragma Import (C, slurm_update_job, "slurm_update_job");

   procedure slurm_init_job_desc_msg (job_msg : job_desc_msg_ptr);
   pragma Import (C, slurm_init_job_desc_msg, "slurm_init_job_desc_msg");

   function slurm_update_node (node_msg : update_node_msg_ptr) return int;
   pragma Import (C, slurm_update_node, "slurm_update_node");

   procedure slurm_init_update_node_msg (node_msg : update_node_msg_ptr);
   pragma Import (c, slurm_init_update_node_msg, "slurm_init_update_node_msg");

   procedure Down_Node (Name, Reason : String; uid : uid_t) is
      use Slurm.Errors;

      node_msg : aliased update_node_msg_t;
      Result   : int;
      API_Err  : Error;
   begin
      slurm_init_update_node_msg (node_msg'Unchecked_Access);
      node_msg.node_names := New_String (Name);
      node_msg.reason := New_String (Reason);
      node_msg.node_state := NODE_STATE_DOWN;
      node_msg.reason_uid := uint32_t (uid);
      Result := slurm_update_node (node_msg'Unchecked_Access);
      if Result /= 0 then
         API_Err := Get_Last_Error;
         if Get_Last_Error = User_ID_Missing then
            raise Slurm_Error with "Not allowed to down node" & Name;
         else
            raise Slurm_Error with Get_Error (API_Err);
         end if;
      end if;
   end Down_Node;

   procedure Drain_Node (Name, Reason : String; UID : uid_t) is
      use Slurm.Errors;

      node_msg : aliased update_node_msg_t;
      Result   : int;
      API_Err  : Error;
   begin
      slurm_init_update_node_msg (node_msg'Unchecked_Access);
      node_msg.node_names := New_String (Name);
      node_msg.reason := New_String (Reason);
      node_msg.reason_uid := uint32_t (UID);
      node_msg.node_state := NODE_STATE_DRAIN;
      Result := slurm_update_node (node_msg'Unchecked_Access);
      if Result /= 0 then
         API_Err := Get_Last_Error;
         if Get_Last_Error = User_ID_Missing then
            raise Slurm_Error with "Not allowed to drain node" & Name;
         else
            raise Slurm_Error with Get_Error (API_Err);
         end if;
      end if;
   end Drain_Node;

   procedure Kill_Job (ID : Positive) is
      use Slurm.Errors;
      Result : int;
      Reason : Error;
   begin
      Result := slurm_kill_job (job_id => uint32_t (ID),
                               signal => 9,
                               flags  => 0);
      if Result /= 0 then
         Reason := Get_Last_Error;
         if Get_Last_Error = User_ID_Missing then
            raise Slurm_Error with "Not allowed to kill job" & ID'Img;
         else
            raise Slurm_Error with Get_Error (Reason);
         end if;
      end if;
   end Kill_Job;

   procedure Release_Job (ID : Positive) is
      use Slurm.Errors;

      job_msg : aliased job_desc_msg_t;
      Result  : int;
      Reason  : Error;
   begin
      slurm_init_job_desc_msg (job_msg'Unchecked_Access);
      job_msg.job_id := uint32_t (ID);
      job_msg.priority := uint32_t'Last;
      Result := slurm_update_job (job_msg'Unchecked_Access);
      if Result /= 0 then
         Reason := Get_Last_Error;
         if Get_Last_Error = User_ID_Missing then
            raise Slurm_Error with "Not allowed to release job" & ID'Img;
         else
            raise Slurm_Error with Get_Error (Reason);
         end if;
      end if;
   end Release_Job;

   procedure Resume_Node (Name : String) is
      use Slurm.Errors;

      node_msg : aliased update_node_msg_t;
      Result  : int;
      Reason  : Error;
   begin
      slurm_init_update_node_msg (node_msg'Unchecked_Access);
      node_msg.node_names := New_String (Name);
      node_msg.node_state := NODE_STATE_RESUME;
      Result := slurm_update_node (node_msg'Unchecked_Access);
      if Result /= 0 then
         Reason := Get_Last_Error;
         if Get_Last_Error = User_ID_Missing then
            raise Slurm_Error with "Not allowed to resume node" & Name;
         else
            raise Slurm_Error with Get_Error (Reason);
         end if;
      end if;
   end Resume_Node;

   procedure Undrain_Node (Name : String) is
      use Slurm.Errors;

      node_msg : aliased update_node_msg_t;
      Result  : int;
      Reason  : Error;
   begin
      slurm_init_update_node_msg (node_msg'Unchecked_Access);
      node_msg.node_names := New_String (Name);
      node_msg.node_state := NODE_STATE_UNDRAIN;
      Result := slurm_update_node (node_msg'Unchecked_Access);
      if Result /= 0 then
         Reason := Get_Last_Error;
         if Get_Last_Error = User_ID_Missing then
            raise Slurm_Error with "Not allowed to undrain node" & Name;
         else
            raise Slurm_Error with Get_Error (Reason);
         end if;
      end if;
   end Undrain_Node;

end Slurm.Admin;
