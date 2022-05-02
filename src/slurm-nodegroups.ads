with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with Slurm.Node_Properties; use Slurm.Node_Properties;
with Slurm.Nodes;
with Slurm.Gres;
with Slurm.Utils; use Slurm.Utils;
with Slurm.Tres;
with Slurm.Loggers;
with Slurm.Hostlists; use Slurm.Hostlists;

package Slurm.Nodegroups is
   type Nodegroup is new Slurm.Loggers.Logger with private;

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

   type State is (total, available, used, reserved, draining, drained, offline);
   type State_Count is array (State) of Countable_Map;

   function To_String (Source : State) return String;

   procedure Iterate (Collection : Summarized_List;
                      Process    : not null access procedure (G : Nodegroup));
   procedure Iterate_Summary (Process : not null access procedure (Item : State));
   function Get_Summary (List : Summarized_List; From : State) return Natural;

   function New_Nodegroup (Properties : Set_Of_Properties; Meta : Boolean := False) return Nodegroup;
   function "=" (Left : Nodegroup; Right : Slurm.Nodes.Node) return Boolean;
   function "=" (Left : Slurm.Nodes.Node; Right : Nodegroup) return Boolean;

   function Get_Available_Nodes (G : Nodegroup) return Natural;
   function Get_Total_Nodes (G : Nodegroup) return Natural;
   function Get_Offline_Nodes (G : Nodegroup) return Natural;
   function Get_Used_Nodes (G : Nodegroup) return Natural;
   function Get_Drained_Nodes (G : Nodegroup) return Natural;
   function Get_Draining_Nodes (G : Nodegroup) return Natural;

   function Get_Available_Cores (G : Nodegroup) return Natural;
   function Get_Total_Cores (G : Nodegroup) return Natural;
   function Get_Offline_Cores (G : Nodegroup) return Natural;
   function Get_Used_Cores (G : Nodegroup) return Natural;
   function Get_Drained_Cores (G : Nodegroup) return Natural;

   function Get_Memory (G : Nodegroup) return Gigs;
   function Get_CPUs (G : Nodegroup) return Natural;
   function Get_GRES (G : Nodegroup) return Slurm.Gres.List;
   function Get_TRES (G : Nodegroup) return Slurm.Tres.List;
   function Get_Features (G : Nodegroup) return String;
   function Has_IB (G : Nodegroup) return Boolean;
   function Is_Meta_Group (G : Nodegroup) return Boolean;

private
   type Nodegroup is new Slurm.Loggers.Logger with record
      Available_Nodes, Total_Nodes,
      Drained_Nodes, Draining_Nodes,
      Offline_Nodes, Used_Nodes : Countable_Sets.Set;
      Available_CPUs,
      Draining_CPUs,
      Offline_CPUs, Used_CPUs   : Countable_Map;
      Meta : Boolean := False;

      Properties : Set_Of_Properties;
   end record;

   package Lists is new Ada.Containers.Ordered_Maps (Element_Type => Nodegroup,
                                                    Key_Type => Set_Of_Properties);

   type Summarized_List is new Lists.Map with
   record
     Summary : State_Count;
   end record;

   overriding function Copy (Source : Summarized_List) return Summarized_List;

end Slurm.Nodegroups;
