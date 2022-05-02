with Slurm.Nodes; use Slurm.Nodes;
with Slurm.Utils; use Slurm.Utils;
with Slurm.Tres;
with Ada.Exceptions; use Ada.Exceptions;

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
      return  Natural (G.Drained_Nodes.Length);
   end Get_Drained_Nodes;

   function Get_Draining_Nodes (G : Nodegroup) return Natural is
   begin
      return  Natural (G.Draining_Nodes.Length);
   end Get_Draining_Nodes;

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

   function Has_IB (G : Nodegroup) return Boolean is
   begin
      return Has_IB (G.Properties);
   end Has_IB;

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

   function Is_Meta_Group (G : Nodegroup) return Boolean is
   begin
      return G.Meta;
   end Is_Meta_Group;

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
      Inserted   : Boolean;
      The_Props : Lists.Cursor;

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
            if Get_Used_CPUs (N) > 0 then
               Element.Draining_CPUs.Include (Key      => Get_Name (N),
                                     New_Item => Get_CPUs (N));
               Element.Draining_Nodes.Include (Get_Name (N));
               Group_List.Summary (draining).Include (Key      => Get_Name (N),
                                                   New_Item => Get_CPUs (N));
            else
               Element.Drained_Nodes.Include (Get_Name (N));
               Group_List.Summary (drained).Include (Key      => Get_Name (N),
                                                   New_Item => Get_CPUs (N));
            end if;
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
         if N.Has_Errors then
            Element.Record_Error ("Node " & To_String (Get_Name (N)) & " has errors");
         end if;
      exception
         when E : others =>
            Element.Record_Error ("Node " & To_String (Get_Name (N)) & " raised:"
                                 & Exception_Message (E));

      end Update_Slot_And_Node_Count;

      use Lists;

   begin
      Add_Jobs (To => Node_List);
      Group_List.Clear;
      Position :=  First (Node_List);
      nodes :
      while Has_Element (Position) loop
         N := Element (Position);
         Properties := Get_Properties (N);
         The_Props := Group_List.Find (Properties);
         if The_Props = Lists.No_Element then
            Group_List.Insert (Properties, New_Nodegroup (Properties), The_Props, Inserted);
            if not Inserted then
               N.Record_Error ("Couldnt insert properties into Nodegroup");
               raise Constraint_Error;
            end if;
         end if;
         Group_List.Update_Element (The_Props,
                                    Update_Slot_And_Node_Count'Access);
         if Slurm.Node_Properties.Has_IB (Properties) then
            Init_Features (Properties, "ib00-meta");
            The_Props := Group_List.Find (Properties);
            if The_Props = Lists.No_Element then
               Group_List.Insert (Properties, New_Nodegroup (Properties, True), The_Props, Inserted);
               if not Inserted then
                  N.Record_Error ("Couldnt insert meta properties into Nodegroup");
                  raise Constraint_Error;
               end if;
            end if;
            Group_List.Update_Element (The_Props,
                                       Update_Slot_And_Node_Count'Access);
         end if;
         Next (Position);
      end loop nodes;
      return Group_List;
      exception
         when Constraint_Error => return Group_List;
   end Load;

   function New_Nodegroup (Properties : Set_Of_Properties; Meta : Boolean := False) return Nodegroup is
      G : Nodegroup;
   begin
      G.Properties := Properties;
      G.Meta := Meta;
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
         when draining =>
            return "Draining";
         when drained =>
            return "Drained";
      end case;
   end To_String;

end Slurm.Nodegroups;
