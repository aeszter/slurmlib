with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Slurm.C_Types;
with Slurm.Node_Properties; use Slurm.Node_Properties;
with Slurm.Partitions; use Slurm.Partitions;
with Slurm.Utils; use Slurm.Utils;
with Slurm.Gres;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Slurm.Jobs;

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
   function First (Collection : List) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Append (Collection : in out List; Item : Node);
   procedure Iterate (Collection : List;
                      Process    : not null access procedure (Position : Cursor));
   function Load_Nodes return List;

   type Percent is range 0 .. 100;

   function Color_Class (P : Percent) return String;
   function Color_Class (Load : Node_Properties.Load) return String;

   function Get_Architecture (N : Node) return String;
   function Get_Boards (N : Node) return Positive;
   function Get_Boot_Time (N : Node) return Ada.Calendar.Time;
   function Get_Cores_Per_Socket (N : Node) return Positive;

   function Get_CPUs (N : Node) return Positive;
   function Get_Free_CPUs (N : Node) return Natural;
   function Get_Used_CPUs (N : Node) return Natural;
   function Get_Features (N : Node) return String;
   function Get_Free_Memory (N : Node) return String;
   function Get_Memory (N : Node) return String;
   function Get_Name (N : Node) return Node_Name;
   function Get_OS (N : Node) return String;
   function Get_Owner (N : Node) return User_Name;
   function Get_Partitions (N : Node) return String;
   function Get_Sockets (N : Node) return Positive;
   function Get_State (N : Node) return states;
   function Get_State (N : Node) return String;
   function Get_Reason (N : Node) return String;
   function Get_Reason_User (N : Node) return User_Name;
   function Get_Reason_Time (N : Node) return Ada.Calendar.Time;
   function Get_Start_Time (N : Node) return Ada.Calendar.Time;
   function Get_Threads_Per_Core (N : Node) return Positive;
   function Get_Tmp_Total (N : Node) return Gigs;
   function Get_TRES (N : Node) return String;
   function Get_Version (N : Node) return String;
   function Get_Weight (N : Node) return Integer;
   function Get_Properties (N : Node) return Set_Of_Properties;

   function Is_Draining (N : Node) return Boolean;
   function Is_Completing (N : Node) return Boolean;
   function Is_Not_Responding (N : Node) return Boolean;
   function Is_Power_Saving (N : Node) return Boolean;
   function Is_Failing (N : Node) return Boolean;
   function Is_Powering_Up (N : Node) return Boolean;
   function Is_Maintenance (N : Node) return Boolean;

   function Load_Per_Core (N : Node) return Load;
   function Mem_Percentage (N : Node) return Percent;

   procedure Iterate_Jobs (N : Node; Process : not null access procedure (ID : Positive));
   procedure Iterate_Partitions (N : Node; Process : not null access procedure (P : Partition));
   procedure Iterate_GRES (N       : Node;
                           Process : not null access procedure (R : Slurm.Gres.Resource));
   procedure Iterate_GRES_Drain (N       : Node;
                                 Process : not null access procedure (R : Slurm.Gres.Resource));
   procedure Iterate_GRES_Used (N       : Node;
                                Process : not null access procedure (R : Slurm.Gres.Resource));

   function Get_Node (Collection : List; Name : String) return Node;

   procedure Add_Jobs (From : Slurm.Jobs.List; To : in out Node);
   procedure Add_Jobs (From : Slurm.Jobs.List; To : in out List);

private

   package Job_Lists is new ada.Containers.Ordered_Sets (Element_Type => Positive);

   type Node is record
      Architecture     : Unbounded_String;
      Boards           : Natural;
      Boot_Time        : Ada.Calendar.Time;
      Cores_Per_Socket : Natural;
      Sockets          : Natural;
      Threads_Per_Core : Natural;
      CPUs             : Natural;
      Used_CPUs        : Natural := 0;
      Start_Time       : Ada.Calendar.Time;
      Load             : Usage_Number;
      Free_Memory      : Gigs;
      Real_Memory      : Gigs;
      Features         : Unbounded_String;
      GRES,
      GRES_Drain,
      GRES_Used        : Slurm.Gres.List;
      Name             : Node_Name;
      State            : Slurm.C_Types.uint32_t;
      OS               : Unbounded_String;
      Owner            : User_Name;
      Partitions       : Unbounded_String;
      Reason           : Unbounded_String;
      Reason_Time      : Ada.Calendar.Time;
      Reason_User      : User_Name;
      Tmp_Total        : Gigs;
      Weight           : Natural;
      Tres             : Unbounded_String;
      Version          : Unbounded_String;
      Jobs             : Job_Lists.Set;
      Properties       : Set_Of_Properties;

   end record;

   package Lists is new ada.Containers.Ordered_Maps (Element_Type => Node,
                                                     Key_Type => Node_Name);
   type Cursor is new Lists.Cursor;
   type List is record
      Container : Lists.Map;
   end record;

end Slurm.Nodes;
