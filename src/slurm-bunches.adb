with Slurm.Jobs; use Slurm.Jobs;

package body Slurm.Bunches is

   function "<" (Left, Right : Set_Of_Requirements) return Boolean is
   begin
      if Left.CPUs < Right.CPUs then
         return True;
      elsif Left.CPUs > Right.CPUs then
         return False;
      elsif Left.Gres < Right.Gres then
         return True;
      elsif Left.Gres > Right.Gres then
         return False;
      else
         return Left.TRES < Right.TRES;
      end if;
   end "<";

   function Get_CPUs (B : Bunch) return Natural is
   begin
      return B.Requirements.CPUs;
   end Get_CPUs;

   function Get_CPUs (Requirements : Set_Of_Requirements) return Natural is
   begin
      return Requirements.CPUs;
   end Get_CPUs;

   function Get_Depending_Jobs (B : Bunch) return Natural is
   begin
      return B.Dependency;
   end Get_Depending_Jobs;

   function Get_Gres (B : Bunch) return String is
   begin
      return To_String (B.Requirements.Gres);
   end Get_Gres;

   function Get_Gres (Requirements : Set_Of_Requirements) return String is
   begin
      return To_String (Requirements.Gres);
   end Get_Gres;

   function Get_Other_Jobs (B : Bunch) return Natural is
   begin
      return B.Other_State;
   end Get_Other_Jobs;

   function Get_Total_Jobs (B : Bunch) return Natural is
   begin
      return B.Total;
   end Get_Total_Jobs;

   function Get_TRES (B : Bunch) return String is
   begin
      return To_String (B.Requirements.TRES);
   end Get_TRES;

   function Get_TRES (Requirements : Set_Of_Requirements) return String is
   begin
      return To_String (Requirements.TRES);
   end Get_TRES;

   function Get_Waiting_Jobs (B : Bunch) return Natural is
   begin
      return B.Waiting;
   end Get_Waiting_Jobs;

   function Has_Waiting (B : Bunch) return Boolean is
   begin
      return B.Waiting > 0;
   end Has_Waiting;

   procedure Init (Requirements : in out Set_Of_Requirements;
                   CPUs         : Natural;
                   Gres, TRES   : String) is
   begin
      Requirements.CPUs := CPUs;
      Requirements.Gres := To_Unbounded_String (Gres);
      Requirements.TRES := To_Unbounded_String (TRES);
   end Init;

   procedure Iterate
     (Collection : List;
      Process    : not null access procedure (B : Bunch))
   is
      use Lists;
      procedure Wrapper (Position : Lists.Cursor);

      procedure Wrapper (Position : Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      Lists.Iterate (Lists.Map (Collection), Wrapper'Access);
   end Iterate;

   function Load return List is
      use Lists;

      procedure Update_Node_Counts (Key : Set_Of_Requirements; Element : in out Bunch);

      J : Job;
      Position : Slurm.Jobs.Cursor;
      Bunch_List : List;
      Job_List   : Jobs.List := Jobs.Load_Jobs;
      Requirements : Set_Of_Requirements;

      procedure Update_Node_Counts (Key : Set_Of_Requirements; Element : in out Bunch) is
         pragma Unreferenced (Key);
      begin
         Element.Total := Element.Total + 1;
         if Is_Pending (J) then
            if Get_State_Reason (J) = WAIT_DEPENDENCY then
               Element.Dependency := Element.Dependency + 1;
            else
               Element.Waiting := Element.Waiting + 1;
            end if;
         else
            Element.Record_Error ("found state " & states'Image (Get_State (J))
                                  & "; see Bug #3262");
            Element.Other_State := Element.Other_State + 1;
         end if;
      end Update_Node_Counts;

   begin
      Bunch_List.Clear;
      Position := First (Job_List);
      while Has_Element (Position) loop
         J := Element (Position);
         if not J.Is_Running
           and then Get_State (J) /= JOB_COMPLETE
         then
            Requirements.Gres := To_Unbounded_String (Get_Gres (J));
            Requirements.TRES := To_Unbounded_String (Get_TRES_Request (J));
            Requirements.CPUs := Get_CPUs (J);
            if not Bunch_List.Contains (Requirements) then
               Bunch_List.Insert (Requirements, New_Bunch (Requirements));
            end if;
            Bunch_List.Update_Element (Bunch_List.Find (Requirements),
                                       Update_Node_Counts'Access);
         end if;
         Next (Position);
      end loop;
      return Bunch_List;
   end Load;

   function New_Bunch (Requirements : Set_Of_Requirements) return Bunch is
   begin
      return Bunch'(Loggers.Logger with
                    Total      => 0,
                    Waiting    => 0,
                    Dependency => 0,
                    Other_State => 0,
                   Requirements => Requirements);
   end New_Bunch;

end Slurm.Bunches;
