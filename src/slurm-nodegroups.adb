with Slurm.Jobs;
with Slurm.Nodes; use Slurm.Nodes;
with Slurm.Utils; use Slurm.Utils;
with Slurm.Tres;

package body Slurm.Nodegroups is

   function "=" (Left : Nodegroup; Right : Slurm.Nodes.Node) return Boolean is
   begin
      return Left.Properties = Get_Properties (Right);
   end "=";

   function "=" (Left : Slurm.Nodes.Node; Right : Nodegroup) return Boolean is
   begin
      return Right = Left;
   end "=";

   overriding function Copy
     (Source : Summarized_List)
      return Summarized_List
   is
   begin
      return (Lists.Copy (Lists.Map (Source)) with
        Summary => Source.Summary);
   end Copy;

   function Get_Available_Cores (G : Nodegroup) return Natural is
   begin
      return  Sum (G.Available_CPUs);
   end Get_Available_Cores;

   function Get_Available_Nodes (G : Nodegroup) return Natural is
   begin
      return Natural (G.Available_Nodes.Length);
   end Get_Available_Nodes;

   function Get_CPUs (G : Nodegroup) return Natural is
   begin
      return Get_CPUs (G.Properties);
   end Get_CPUs;

   function Get_Drained_Cores (G : Nodegroup) return Natural is
   begin
      return  Sum (G.Draining_CPUs);
   end Get_Drained_Cores;

   function Get_Drained_Nodes (G : Nodegroup) return Natural is
   begin
      return  Natural (G.Draining_Nodes.Length);
   end Get_Drained_Nodes;

   function Get_Features (G : Nodegroup) return String is
   begin
      return Get_Features (G.Properties);
   end Get_Features;

   function Get_GRES (G : Nodegroup) return Slurm.Gres.List is
   begin
      return Get_GRES (G.Properties);
   end Get_GRES;

   function Get_Memory (G : Nodegroup) return Gigs is
   begin
      return Get_Memory (G.Properties);
   end Get_Memory;

   function Get_Offline_Cores (G : Nodegroup) return Natural is
   begin
      return  Sum (G.Offline_CPUs);
   end Get_Offline_Cores;

   function Get_Offline_Nodes (G : Nodegroup) return Natural is
   begin
      return Natural (G.Offline_Nodes.Length);
   end Get_Offline_Nodes;

   function Get_Summary
     (List : Summarized_List;
      From : State)
      return Natural
   is
   begin
      return Sum (List.Summary (From));
   end Get_Summary;

   function Get_Total_Cores (G : Nodegroup) return Natural is
   begin
      return  Natural (G.Total_Nodes.Length) * Get_CPUs (G.Properties);
   end Get_Total_Cores;

   function Get_Total_Nodes (G : Nodegroup) return Natural is
   begin
      return Natural (G.Total_Nodes.Length);
   end Get_Total_Nodes;

   function Get_TRES (G : Nodegroup) return Slurm.Tres.List is
   begin
      return Get_TRES (G.Properties);
   end Get_TRES;

   function Get_Used_Cores (G : Nodegroup) return Natural is
   begin
      return Sum (G.Used_CPUs);
   end Get_Used_Cores;

   function Get_Used_Nodes (G : Nodegroup) return Natural is
   begin
      return  Natural (G.Used_Nodes.Length);
   end Get_Used_Nodes;

   overriding procedure Include
     (Container : in out Countable_Map;
      Key       : Node_Name;
      New_Item  : Natural) is
      use Countable_Maps;
      procedure Take_Maximum (Key : Node_Name; Element : in out Natural);

      Previous : Countable_Maps.Cursor := Find (Container => Container,
                                                Key       => Key);
      procedure Take_Maximum (Key : Node_Name; Element : in out Natural) is
         pragma Unreferenced (Key);
      begin
         if New_Item > Element then
            Element := New_Item;
         end if;
      end Take_Maximum;

   begin
      if Previous = No_Element then
         Insert (Container => Container,
                 Key       => Key,
                 New_Item  => New_Item);
      else
         Update_Element (Container => Container,
                         Position  => Previous,
                         Process   => Take_Maximum'Access);
      end if;
   end Include;

   procedure Iterate
     (Collection : Summarized_List;
      Process    : not null access procedure (G : Nodegroup))
   is
      procedure Wrapper (Position : Lists.Cursor);
      procedure Wrapper (Position : Lists.Cursor) is
      begin
         Process (Lists.Element (Position));
      end Wrapper;

   begin
      Iterate (Collection, Wrapper'Access);
   end Iterate;

   procedure Iterate_Summary
     (Process : not null access procedure (Item : State))
   is
   begin
      null;
   end Iterate_Summary;

   function Load return Summarized_List is
      procedure Update_Slot_And_Node_Count (Key : Set_Of_Properties; Element : in out Nodegroup);
      N : Node;
      Position : Slurm.Nodes.Cursor;
      Group_List : Summarized_List;
      Node_List  : Nodes.List := Nodes.Load_Nodes;
      Properties : Set_Of_Properties;

      procedure Update_Slot_And_Node_Count (Key : Set_Of_Properties; Element : in out Nodegroup) is
         pragma Unreferenced (Key);
      begin
         --  Update totals
         Element.Total_Nodes.Include (Get_Name (N));
         Group_List.Summary (total).Include (Key      => Get_Name (N),
                                             New_Item => Get_CPUs (N));
         if Is_Not_Responding (N) then
            Element.Offline_CPUs.Include (Key => Get_Name (N),
                                New_Item => Get_CPUs (N));
            Element.Offline_Nodes.Include (Get_Name (N));
            Group_List.Summary (offline).Include (Key      => Get_Name (N),
                                                  New_Item => Get_CPUs (N));
         elsif Is_Draining (N) then
            Element.Draining_CPUs.Include (Key      => Get_Name (N),
                                     New_Item => Get_CPUs (N));
            Element.Draining_Nodes.Include (Get_Name (N));
            Group_List.Summary (disabled).Include (Key      => Get_Name (N),
                                             New_Item => Get_CPUs (N));
         else
            if Get_Used_CPUs (N) > 0 then
               Element.Used_Nodes.Include (Get_Name (N));
               Element.Used_CPUs.Include (Key => Get_Name (N),
                                    New_Item => Get_Used_CPUs (N));
               Group_List.Summary (used).Include (Key      => Get_Name (N),
                                                  New_Item => Get_Used_CPUs (N));
            end if;
            Element.Available_CPUs.Include (Key      => Get_Name (N),
                                      New_Item => Get_Free_CPUs (N));
            if Get_Used_CPUs (N) = 0 and then
              Get_Free_CPUs (N) = Get_CPUs (N)
            then
               Element.Available_Nodes.Include (Get_Name (N));
            end if;
            Group_List.Summary (available).Include (Key      => Get_Name (N),
                                                    New_Item => Get_Free_CPUs (N));
         end if;
      end Update_Slot_And_Node_Count;

   begin
      Add_Jobs (From => Jobs.Load_Jobs,
                To   => Node_List);
      Group_List.Clear;
      Position :=  First (Node_List);
      nodes :
      while Has_Element (Position) loop
         N := Element (Position);
         Properties := Get_Properties (N);
         if not Group_List.Contains (Properties) then
            Group_List.Insert (Properties, New_Nodegroup (Properties));
         end if;
         Group_List.Update_Element (Group_List.Find (Properties),
                                    Update_Slot_And_Node_Count'Access);
         Next (Position);
      end loop nodes;
      return Group_List;
   end Load;

   function New_Nodegroup (Properties : Set_Of_Properties) return Nodegroup is
      G : Nodegroup;
   begin
      G.Properties := Properties;
      return G;
   end New_Nodegroup;

   function Sum (Over : Countable_Map) return Natural is
      procedure Count (Position : Countable_Maps.Cursor);
      Total : Natural := 0;

      procedure Count (Position : Countable_Maps.Cursor) is
      begin
         Total := Total + Countable_Maps.Element (Position);
      end Count;

   begin
      Over.Iterate (Count'Access);
      return Total;
   end Sum;

   function To_String (Source : State) return String is
   begin
      case Source is
         when total =>
            return "Total";
         when reserved =>
            return "Reserved";
         when used =>
            return "Used";
         when offline =>
            return "Offline";
         when available =>
            return "Available";
         when disabled =>
            return "Disabled";
      end case;
   end To_String;

end Slurm.Nodegroups;
