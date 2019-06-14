with Slurm.Nodes; use Slurm.Nodes;

package body Slurm.Nodegroups is

   overriding function Copy
     (Source : Summarized_List)
      return Summarized_List
   is
   begin
      return (Lists.Copy (Lists.List (Source)) with
        Summary => Source.Summary);
   end Copy;

   function Get_Available_Cores (G : Nodegroup) return Natural is
      pragma Unreferenced (G);
   begin
      --        return g.Available_Cores;
      return 0;
   end Get_Available_Cores;

   function Get_Available_Nodes (G : Nodegroup) return Natural is
   begin
      return Natural (G.Available_Nodes.Length);
   end Get_Available_Nodes;

   function Get_CPU_Model (G : Nodegroup) return String is
   begin
      return Get_CPU_Model (G.Properties);
   end Get_CPU_Model;

   function Get_CPUs (G : Nodegroup) return Natural is
   begin
      return Get_CPUs (G.Properties);
   end Get_CPUs;

   function Get_Drained_Cores (G : Nodegroup) return Natural is
      pragma Unreferenced (G);
   begin
      return 0;
   end Get_Drained_Cores;

   function Get_Drained_Nodes (G : Nodegroup) return Natural is
   begin
      return  Natural (G.Draining_Nodes.Length);
   end Get_Drained_Nodes;

   function Get_GPU (G : Nodegroup) return String is
   begin
      return Get_GPU (G.Properties);
   end Get_GPU;

   function Get_GPU_Memory (G : Nodegroup) return String is
   begin
      return Get_GPU_Memory (G.Properties);
   end Get_GPU_Memory;

   function Get_Memory (G : Nodegroup) return String is
   begin
      return Get_Memory (G.Properties);
   end Get_Memory;

   function Get_Network (G : Nodegroup) return String is
   begin
      return Get_Network (G.Properties);
   end Get_Network;

   function Get_Offline_Cores (G : Nodegroup) return Natural is
      pragma Unreferenced (G);
   begin
      return 0;

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
      pragma Unreferenced (G);
   begin
--        return g.Total_Cores;
      return 0;
   end Get_Total_Cores;

   function Get_Total_Nodes (G : Nodegroup) return Natural is
   begin
      return Natural (G.Total_Nodes.Length);
   end Get_Total_Nodes;

   function Get_Used_Cores (G : Nodegroup) return Natural is
      pragma Unreferenced (G);
   begin
      return 0;
   end Get_Used_Cores;

   function Get_Used_Nodes (G : Nodegroup) return Natural is
   begin
      return  Natural (G.Used_Nodes.Length);
   end Get_Used_Nodes;

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
      G : Nodegroup;
      N : Node;
      Position : Slurm.Nodes.Cursor;
      Group_List : Summarized_List;
      Node_List : Nodes.List := Nodes.Load_Nodes;
   begin
      Group_List.Clear;
      Position :=  First (Node_List);
      --  Create Nodegroup according to first Node
      G := New_Nodegroup (Element (Position));
      while Has_Element (Position) loop
         N := Element (Position);
         --  New Nodegroup?
         if G /= N then
            --  Yes. Store previous one.
            Group_List.Append (G);
            G := New_Nodegroup (N);
         end if;

         begin
            --  Update totals
            G.Total_Nodes.Include (Get_Name (N));
            Group_List.Summary (total).Include (Key      => Get_Name (N),
                                                New_Item => Get_CPUs (N));
            if Is_Not_Responding (N) then
               G.Offline_Cores.Include (Key => Get_Name (N),
                                   New_Item => Get_CPUs (N));
               G.Offline_Nodes.Include (Get_Name (N));
               Group_List.Summary (offline).Include (Key      => Get_Name (N),
                                                     New_Item => Get_CPUs (N));
            elsif Is_Draining (N) then
               G.Draining_Cores.Include (Key      => Get_Name (N),
                                         New_Item => Get_CPUs (N));
               G.Draining_Nodes.Include (Get_Name (N));
               Group_List.Summary (disabled).Include (Key      => Get_Name (N),
                                                New_Item => Get_CPUs (N));
            else
               if Get_Used_Cores (N) > 0 then
                  G.Used_Nodes.Include (Get_Name (N));
                  G.Used_Cores := G.Used_Cores + Get_Used_Core (N);
                  Group_List.Summary (used).Include (Key      => Get_Name (N),
                                                     New_Item => Get_Used_Cores (N));
               end if;
               G.Available_Cores.Include (Key      => Get_Name (N),
                                               New_Item => Get_Free_Cores (N));
               if Get_Used_Cores (N) = 0 and then
                 Get_Free_Cores (N) = Get_CPUs (N)
               then
                  G.Available_Nodes.Include (Get_Name (N));
               end if;
               Group_List.Summary (available).Include (Key      => Get_Name (N),
                                                       New_Item => Get_Free_Cores (N));
            end if;
         end;
         --  Advance
         Next (Position);
      end loop;
      --  That's it. Store final Nodegroup.
      Group_List.Append (G);
      return Group_List;
   end Load;

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
