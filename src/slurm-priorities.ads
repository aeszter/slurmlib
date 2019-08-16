with Ada.Containers.Ordered_Maps;

package Slurm.Priorities is

   type Priority is record
      Total, Age, Fairshare, Job_Size, Partition : Natural;
   end record;

   Undefined : constant Priority := (others => 0);

   Format_Error : exception; -- unexpected data from sprio

   procedure Load;
   function Get_Priority (J : Positive) return Priority;

   package Maps is new ada.Containers.Ordered_Maps (Key_Type     => Positive,
                                                    Element_Type => Priority);
end Slurm.Priorities;
