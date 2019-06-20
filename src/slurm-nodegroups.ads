with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with Slurm.Node_Properties; use Slurm.Node_Properties;
with Slurm.Nodes;

package Slurm.Nodegroups is
   type Nodegroup is private;

   package Countable_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Node_Name);
   package Countable_Maps is new Ada.Containers.Ordered_Maps (Key_Type     => Node_Name,
                                                              Element_Type => Natural);
   type Countable_Map is new Countable_Maps.Map with null record;
   type Summarized_List is private;

   overriding procedure Include
     (Container : in out Countable_Map;
      Key       : Node_Name;
      New_Item  : Natural);

   function Sum (Over : Countable_Map) return Natural;

   function Load return Summarized_List;

   type State is (total, available, used, reserved, disabled, offline);
   type State_Count is array (State) of Countable_Map;

   function To_String (Source : State) return String;

   procedure Iterate (Collection : Summarized_List;
                      Process    : not null access procedure (G : Nodegroup));
   procedure Iterate_Summary (Process : not null access procedure (Item : State));
   function Get_Summary (List : Summarized_List; From : State) return Natural;

   function New_Nodegroup (Template : Slurm.Nodes.Node) return Nodegroup;
   function "=" (Left : Nodegroup; Right : Slurm.Nodes.Node) return Boolean;
   function "=" (Left : Slurm.Nodes.Node; Right : Nodegroup) return Boolean;

   function Get_Available_Nodes (G : Nodegroup) return Natural;
   function Get_Total_Nodes (G : Nodegroup) return Natural;
   function Get_Offline_Nodes (G : Nodegroup) return Natural;
   function Get_Used_Nodes (G : Nodegroup) return Natural;
   function Get_Drained_Nodes (G : Nodegroup) return Natural;

   function Get_Available_Cores (G : Nodegroup) return Natural;
   function Get_Total_Cores (G : Nodegroup) return Natural;
   function Get_Offline_Cores (G : Nodegroup) return Natural;
   function Get_Used_Cores (G : Nodegroup) return Natural;
   function Get_Drained_Cores (G : Nodegroup) return Natural;

   function Get_Network (G : Nodegroup) return String;
   function Get_GPU (G : Nodegroup) return String;
   function Get_GPU_Memory (G : Nodegroup) return String;
   function Get_Memory (G : Nodegroup) return String;
   function Get_CPU_Model (G : Nodegroup) return String;
   function Get_CPUs (G : Nodegroup) return Natural;

private
   type Nodegroup is record
      Available_Nodes, Total_Nodes,
      Draining_Nodes,
      Offline_Nodes, Used_Nodes : Countable_Sets.Set;
      Available_CPUs, Total_CPUs,
      Draining_CPUs,
      Offline_CPUs, Used_CPUs : Countable_Map;

      Properties : Set_Of_Properties;
   end record;

   package Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Nodegroup);

   type Summarized_List is new Lists.List with
   record
     Summary : State_Count;
   end record;

   overriding function Copy (Source : Summarized_List) return Summarized_List;

end Slurm.Nodegroups;
