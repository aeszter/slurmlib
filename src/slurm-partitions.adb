package body Slurm.Partitions is

   function Get_Name (P : Partition) return String is
   begin
      return To_String (P.Name);
   end Get_Name;

end Slurm.Partitions;
