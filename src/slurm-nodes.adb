with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with POSIX.C; use POSIX.C;
with Slurm.C_Types; use Slurm.C_Types;
with Slurm.Errors;
with Slurm.General;
with Slurm.Utils; use Slurm.Utils;
with Ada.Calendar;

package body Slurm.Nodes is

   type acct_gather_energy_t is null record;
   type ext_sensors_data_t is null record;
   type power_mgmt_data_t is null record;
   type dynamic_plugin_data_t is null record;

   type node_info is record
      arch              : chars_ptr;                -- computer architecture
      boards            : uint16_t;        -- total number of boards per node
      boot_time         : time_t;      -- time of node boot
      cluster_name      : chars_ptr;
      cores             : uint16_t;         -- number of cores per socket
      core_spec_cnt     : uint16_t;      -- number of specialized cores on node
      cpu_load          : uint32_t;      -- CPU load * 100
      free_mem          : uint64_t;      -- free memory in MiB
      cpus              : uint16_t;      -- configured count of cpus running on
                                          -- the node
      cpu_spec_list     : chars_ptr;     -- node's specialized cpus
      energy            : access acct_gather_energy_t;     -- energy data
      ext_sensors       : access ext_sensors_data_t; -- external sensor data
      power             : access power_mgmt_data_t;        -- power management data
      features          : chars_ptr;     -- list of a node's available features
      features_act      : chars_ptr;     -- list of a node's current acitve features,
                                          -- Same as "features" if NULL
      gres              : chars_ptr;     -- list of a node's generic resources
      gres_drain        : chars_ptr; -- list of drained GRES
      gres_used         : chars_ptr;  -- list of GRES in current use
      mcs_label         : chars_ptr;   -- mcs label if mcs plugin in use
      mem_spec_limit    : uint64_t; -- MB memory limit for specialization
      name              : chars_ptr;         -- node name to slurm
      node_addr         : chars_ptr;  -- communication name (optional)
      node_hostname     : chars_ptr;      -- node's hostname (optional)
      node_state        : uint32_t;  -- see enum node_states
      os                : chars_ptr;         -- operating system currently running
      owner             : uint32_t;         -- User allowed to use this node or NO_VAL
      partitions        : chars_ptr;  -- Comma separated list of partitions containing
                                      -- this node, NOT supplied by slurmctld, but
                                      -- populated by scontrol */
      port              : uint16_t;    -- TCP port number of the slurmd */
      real_memory       : uint64_t;  -- configured MB of real memory on the node
      reason            : chars_ptr;               -- reason for node being DOWN or DRAINING
      reason_time       : time_t;      -- Time stamp when reason was set, ignore if
                                       -- no reason is set.
      reason_uid        : uint32_t;    -- User that set the reason, ignore if
                                       -- no reason is set.
      select_nodeinfo   : access dynamic_plugin_data_t;  -- opaque data structure,
                                      -- use
                                      -- slurm_get_select_nodeinfo()
                                      -- to access contents
      slurmd_start_time : time_t; -- time of slurmd startup
      sockets           : uint16_t;       -- total number of sockets per node
      threads           : uint16_t;       -- number of threads per core
      tmp_disk          : uint32_t;   -- configured MB of total disk in TMP_FS
      weight            : uint32_t;       -- arbitrary priority of node for scheduling
      tres_fmt_str      : chars_ptr; -- str representing configured TRES on node
      version           : chars_ptr;               -- Slurm version number
   end record;

   Default_Terminator : node_info;
   pragma Warnings (off, Default_Terminator);
   --  this is a dummy. The node list given by the slurm API isn't
   --  terminated, and subprograms from node_info_ptrs (below) that
   --  use termination must not be called

   type node_array is array (uint32_t range <>) of aliased node_info;
   package node_info_ptrs is new Interfaces.C.Pointers (Index         => uint32_t,
                                                       Element       => node_info,
                                                       Element_Array => node_array,
                                                      Default_Terminator => Default_Terminator);
   subtype node_info_ptr is node_info_ptrs.Pointer;

   type node_info_msg_t is record
      last_update : time_t;
      node_scaling : uint32_t;
      record_count : uint32_t;
      node_array   : node_info_ptr;
   end record;

   type node_info_msg_ptr is access constant node_info_msg_t;
   type node_info_msg_ptr_ptr is access constant node_info_msg_ptr;

   function slurm_load_node (update_time       : time_t;
                             node_info_msg_pptr : node_info_msg_ptr_ptr;
                             show_flags        : uint16_t) return int;
   pragma Import (C, slurm_load_node, "slurm_load_node");

   procedure slurm_free_node_info_msg (node_info_msg_p : node_info_msg_ptr);

   pragma Import (C, slurm_free_node_info_msg, "slurm_free_node_info_msg");

   type Enum_To_State_Map is array (uint32_t range 0 .. 6) of states;
   Enum_To_State : constant Enum_To_State_Map := (
              0 => NODE_STATE_UNKNOWN,
              1 => NODE_STATE_DOWN,
              2 => NODE_STATE_IDLE,
              3 => NODE_STATE_ALLOCATED,
              4 => NODE_STATE_ERROR,
              5 => NODE_STATE_MIXED,
                                                  6 => NODE_STATE_FUTURE);

   procedure Init (N : out Node; Ptr : node_info_ptr);
   function Build_List (Buffer : aliased node_info_msg_ptr) return List;

   function Build_List (Buffer : aliased node_info_msg_ptr) return List is
      use node_info_ptrs;
      Node_Ptr : node_info_ptr;
      N        : Node;
      Result   : List;
   begin
      Node_Ptr := Buffer.node_array;
      for I in 1 .. Buffer.record_count loop
         Init (N, Node_Ptr);
         Result.Container.Append (N);
         Increment (Node_Ptr);
      end loop;
      slurm_free_node_info_msg (Buffer);
      return Result;
   end Build_List;

   -----------------
   -- Color_Class --
   --  Purpose: translate a load value to a string suitable
   --  for use as a CSS class
   --  Parameter Load: the load to classify
   --  Returns: one of "load_cold", "load_low", "load_normal",
   --  "load_high", "load_extreme"
   -----------------

   function Color_Class (Load : Node_Properties.Load) return String is
   begin
      if Load < 0.1 then
         return "load_cold";
      elsif Load < 0.8 then
         return "load_low";
      elsif Load < 1.1 then
         return "load_normal";
      elsif Load < 1.5 then
         return "load_high";
      else
         return "load_extreme";
      end if;
   end Color_Class;

   -----------------
   -- Color_Class --
   --  Purpose: translate a percentage to a string suitable
   --  for use as a CSS class
   --  Parameter P : percentage to classify
   --  Returns: one of "pct_cold", "pct_low", "pct_med",
   --  "pct_high", or "pct_hot"
   -----------------

   function Color_Class (P : Percent) return String is
   begin
      if P < 10 then
         return "pct_cold";
      elsif P < 30 then
         return "pct_low";
      elsif P < 60 then
         return "pct_med";
      elsif P < 90 then
         return "pct_high";
      else
         return "pct_hot";
      end if;
   end Color_Class;

   overriding function Element (Position : Cursor) return Node is
   begin
      return Lists.Element (Lists.Cursor (Position));
   end Element;

   function Get_CPUs (N : Node) return Positive is
   begin
      return N.CPUs;
   end Get_CPUs;

   function Get_Memory (N : Node) return String is
   begin
      return To_String (N.Real_Memory);
   end Get_Memory;

   function Get_Name (N : Node) return String is
   begin
      return To_String (N.Name);
   end Get_Name;

   function Get_State (N : Node) return states is
   begin
      return Enum_To_State (N.State and 16#f#);
   end Get_State;

   function Get_State (N : Node) return String is
   begin
      return states'(Get_State (N))'Img;
   end Get_State;

   overriding function Has_Element (Position : Cursor) return Boolean is
   begin
      return Lists.Has_Element (Lists.Cursor (Position));
   end Has_Element;

   procedure Init (N : out Node; Ptr : node_info_ptr) is
   begin
      N.Architecture := Convert_String (Ptr.all.arch);
      N.Boards := Natural (Ptr.all.boards);
      N.Boot_Time := Convert_Time (Ptr.all.boot_time);
      N.Cores_Per_Socket := Natural (Ptr.all.cores);
      N.Load := Usage_Number (Ptr.all.cpu_load) / 100;
      begin
         N.Free_Memory := MiB_To_Gigs (Ptr.all.free_mem);
      exception
         when Constraint_Error =>
            N.Free_Memory := Gigs (0);
      end;
      N.CPUs := Natural (Ptr.all.cpus);
      N.Features := Convert_String (Ptr.all.features);
      N.GRES := Gres.Init (To_String (Ptr.all.gres));
      N.GRES_Drain := Gres.Init (To_String (Ptr.all.gres_drain));
      N.GRES_Used := Gres.Init (To_String (Ptr.all.gres_used));
      N.Name := Convert_String (Ptr.all.name);
      N.State := Ptr.all.node_state;
      N.OS := Convert_String (Ptr.all.os);
      begin
         N.Owner := Convert_User (Ptr.all.owner);
      exception -- numbers like -2 have been seen
            -- even though this violates the specs
         when others =>
            N.Owner := To_User_Name ("");
      end;
      N.Real_Memory := MiB_To_Gigs (Ptr.all.real_memory);
      N.Reason := Convert_String (Ptr.all.reason);
      if N.Reason = "" then
         N.Reason_Time := Convert_Time (0);
         N.Reason_User := To_User_Name ("");
      else
         N.Reason_Time := Convert_Time (Ptr.all.reason_time);
         N.Reason_User := Convert_User (Ptr.all.reason_uid);
      end if;
      N.Start_Time := Convert_Time (Ptr.all.slurmd_start_time);
      N.Sockets := Natural (Ptr.all.sockets);
      N.Threads_Per_Core := Natural (Ptr.all.threads);
      N.Tmp_Total := Natural (Ptr.all.tmp_disk);
      N.Weight := Natural (Ptr.all.weight);
      N.Tres := Convert_String (Ptr.all.tres_fmt_str);
      N.Version := Convert_String (Ptr.all.version);
   end Init;

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

   procedure Iterate_GRES (N       : Node;
                           Process : not null access procedure (R : Slurm.Gres.Resource)) is
      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor);

      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor) is
      begin
         Process (Slurm.Gres.Lists.Element (Position));
      end Wrapper;

   begin
      N.GRES.Iterate (Wrapper'Access);
   end Iterate_GRES;

   procedure Iterate_GRES_Drain (N       : Node;
                           Process : not null access procedure (R : Slurm.Gres.Resource)) is
      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor);

      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor) is
      begin
         Process (Slurm.Gres.Lists.Element (Position));
      end Wrapper;

   begin
      N.GRES_Drain.Iterate (Wrapper'Access);
   end Iterate_GRES_Drain;

   procedure Iterate_GRES_Used (N       : Node;
                           Process : not null access procedure (R : Slurm.Gres.Resource)) is
      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor);

      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor) is
      begin
         Process (Slurm.Gres.Lists.Element (Position));
      end Wrapper;

   begin
      N.GRES_Used.Iterate (Wrapper'Access);
   end Iterate_GRES_Used;

   procedure Iterate_Jobs (N : Node; Process : not null access procedure (J : Job)) is
   begin
      null; -- FIXME: not yet supported
   end Iterate_Jobs;

   procedure Iterate_Partitions (N : Node; Process : not null access procedure (P : Partition)) is
   begin
      null; -- FIXME: not yet supported
   end Iterate_Partitions;

   function Load_Nodes return List is
      use Slurm.Errors;
      E : Error;
      Buffer : aliased node_info_msg_ptr;
   begin
      if Slurm.General.API_Version /= 16#200000# then
         raise Program_Error with "unsupported Slurm API version";
      end if;
      if slurm_load_node (update_time       => 0,
                          node_info_msg_pptr => Buffer'Unchecked_Access,
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
   end Load_Nodes;

   function Load_Per_Core (N : Node) return Load is
   begin
      return Load (N.Load) / N.CPUs;
   end Load_Per_Core;

   function Mem_Percentage (N : Node) return Percent is
   begin
      return 100 - Percent (100 * N.Free_Memory / N.Real_Memory);
   end Mem_Percentage;

end Slurm.Nodes;
