with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.C_Types; use Slurm.C_Types;

package Slurm.Partitions is

   -- slurm_load_partitions - Load partition (queue) information.
   --        Free with slurm_free_partition_info to avoid memory leak.
--      slurm_print_partition_info_msg - Print information about all partitions.
--      slurm_print_partition_info - Print information about a specific partition.
--      slurm_free_partition_info - Free storage allocated by slurm_load_partitions.

   type Partition is private;

   function Get_Name (P : Partition) return String;

private
   type Partition is record
      Allocation_Nodes          : Unbounded_String;
      Accounts                  : Unbounded_String;
      Deny_Accounts             : Unbounded_String;
      Groups                    : Unbounded_String;
      QOS                       : Unbounded_String;
      CR_Type                   : uint16_t;
      Deny_QOS                  : Unbounded_String;
      Alternate                 : Unbounded_String;
      Billing_Weights           : Unbounded_String;
      Default_Memory_Per_CPU    : Integer;
      Default_Time              : Duration;
      Flags                     : uint16_t;
      Grace_Time                : Duration;
      Max_CPUs_Per_Node         : Integer;
      Max_Memory_Per_CPU        : Integer;
      Max_Nodes                 : Integer;
      Max_Share                 : Integer;
      Max_Time                  : Duration;
      Min_Nodes                 : Integer;
      Name                      : Unbounded_String;
      Nodes                     : Unbounded_String;
      Preempt_Mode              : uint16_t;
      Priority_Job_Factor       : Integer;
      Priority_Tier             : Integer;
      QOS_Name                  : Unbounded_String;
      State_Submit, State_Sched : Boolean;
      Total_CPUs                : Natural;
      Total_Nodes               : Natural;
      TRES                      : Unbounded_String;
   end record;
end Slurm.Partitions;
