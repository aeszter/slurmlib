with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.Utils; use Slurm.Utils;

package Slurm.Node_Properties is
   type Fixed is delta 0.01 digits 6 range 0.0 .. 1000.0;
   type Load is new Fixed range 0.0 .. 1000.0;
   type Node_Name is new Unbounded_String;
   overriding function "<" (Left, Right : Node_Name) return Boolean;
   pragma Inline ("<");
   overriding function "=" (Left, Right : Node_Name) return Boolean;
   pragma Inline ("=");
   function To_String (Source : Node_Name) return String;

   type Set_Of_Properties is private;

   function Get_CPU_Model (Item : Set_Of_Properties) return String;
   function Get_CPUs (Item : Set_Of_Properties) return Natural;
   function Get_GPU (Item : Set_Of_Properties) return String;
   function Get_GPU_Memory (Item : Set_Of_Properties) return String;
   function Get_Memory (Item : Set_Of_Properties) return String;
   function Get_Network (Item : Set_Of_Properties) return String;

private

   type Set_Of_Properties is record
      CPUs, GPUs : Natural;
      CPU_Model, GPU_Model : Unbounded_String;
      Memory, GPU_Memory   : Gigs;
      Network              : Unbounded_String;
   end record;

end Slurm.Node_Properties;
