with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Slurm.C_Types;
with Slurm.Jobs; use Slurm.Jobs;
with Slurm.Node_Properties; use Slurm.Node_Properties;
with Slurm.Partitions; use Slurm.Partitions;
with Slurm.Utils; use Slurm.Utils;
with Slurm.Gres;

package Slurm.Nodes is

   --   slurm_load_node - Load node information. Free with slurm_free_node_info
   --         to avoid memory leak.
   --   slurm_print_node_info_msg - Print information about all nodes.
   --   slurm_print_node_table - Print information about a specific node.
   --   slurm_free_node_info - Free storage allocated by slurm_load_node.

   type states is (
      NODE_STATE_UNKNOWN,
      NODE_STATE_DOWN,
      NODE_STATE_IDLE,
      NODE_STATE_ALLOCATED,
      NODE_STATE_ERROR,
      NODE_STATE_MIXED,
      NODE_STATE_FUTURE);

   type Node is private;
   type List is private;
   type Cursor is private;
   function Element (Position : Cursor) return Node;
   function Has_Element (Position : Cursor) return Boolean;
   procedure Iterate (Collection : List;
                      Process    : not null access procedure (Position : Cursor));
   function Load_Nodes return List;

   type Percent is range 0 .. 100;

   function Color_Class (P : Percent) return String;
   function Color_Class (Load : Node_Properties.Load) return String;

   function Get_CPUs (N : Node) return Positive;
   function Get_Memory (N : Node) return String;
   function Get_Name (N : Node) return String;
   function Get_State (N : Node) return states;
   function Get_State (N : Node) return String;
   function Load_Per_Core (N : Node) return Load;
   function Mem_Percentage (N : Node) return Percent;

   procedure Iterate_Jobs (N : Node; Process : not null access procedure (J : Job));
   procedure Iterate_Partitions (N : Node; Process : not null access procedure (P : Partition));
   procedure Iterate_GRES (N       : Node;
                           Process : not null access procedure (R : Slurm.Gres.Resource));
   procedure Iterate_GRES_Drain (N       : Node;
                                 Process : not null access procedure (R : Slurm.Gres.Resource));
   procedure Iterate_GRES_Used (N       : Node;
                                Process : not null access procedure (R : Slurm.Gres.Resource));
private

   type Node is record
      Architecture     : Unbounded_String;
      Boards           : Natural;
      Boot_Time        : Ada.Calendar.Time;
      Cores_Per_Socket : Natural;
      Sockets          : Natural;
      Threads_Per_Core : Natural;
      CPUs             : Natural;
      Start_Time       : Ada.Calendar.Time;
      Load             : Usage_Number;
      Free_Memory      : Gigs;
      Real_Memory      : Gigs;
      Features         : Unbounded_String;
      GRES,
      GRES_Drain,
      GRES_Used        : Slurm.Gres.List;
      Name             : Unbounded_String;
      State            : Slurm.C_Types.uint32_t;
      OS               : Unbounded_String;
      Owner            : User_Name;
      Reason           : Unbounded_String;
      Reason_Time      : Ada.Calendar.Time;
      Reason_User      : User_Name;
      Tmp_Total        : Gigs;
      Weight           : Natural;
      Tres             : Unbounded_String;
      Version : Unbounded_String;

   end record;

   package Lists is new ada.Containers.Doubly_Linked_Lists (Element_Type => Node);
   type Cursor is new Lists.Cursor;
   type List is record
      Container : Lists.List;
   end record;

end Slurm.Nodes;
