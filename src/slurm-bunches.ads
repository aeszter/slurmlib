with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Slurm.Loggers;

package Slurm.Bunches is
   type Bunch is new Slurm.Loggers.Logger with private;
   type List is private;
   type Set_Of_Requirements is private;

   function Load return List;
   procedure Iterate (Collection : List;
                      Process    : not null access procedure (B : Bunch));

   function Has_Waiting (B : Bunch) return Boolean;
   function Get_CPUs (B : Bunch) return Natural;
   function Get_Gres (B : Bunch) return String;
   function Get_TRES (B : Bunch) return String;
   function Get_Total_Jobs (B : Bunch) return Natural;
   function Get_Waiting_Jobs (B : Bunch) return Natural;
   function Get_Depending_Jobs (B : Bunch) return Natural;

   function New_Bunch (Requirements : Set_Of_Requirements) return Bunch;
   procedure Init (Requirements : in out Set_Of_Requirements;
                   CPUs         : Natural;
                   Gres, TRES   : String);
   function Get_CPUs (Requirements : Set_Of_Requirements) return Natural;
   function Get_Gres (Requirements : Set_Of_Requirements) return String;
   function Get_TRES (Requirements : Set_Of_Requirements) return String;

private
   type Set_Of_Requirements is record
      Gres : Unbounded_String;
      TRES : Unbounded_String;
      CPUs : Natural;
   end record;

   function "<" (Left, Right : Set_Of_Requirements) return Boolean;

   type Bunch is new Slurm.Loggers.Logger with record
      Requirements : Set_Of_Requirements;
      Total, Waiting, Dependency : Natural;
   end record;

   package Lists is new Ada.Containers.Ordered_Maps (Key_Type     => Set_Of_Requirements,
                                                     Element_Type => Bunch);

   type List is new Lists.Map with null record;
end Slurm.Bunches;
