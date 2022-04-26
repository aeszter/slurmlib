with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.Gres;
with Slurm.Tres;
with Slurm.Utils; use Slurm.Utils;

package Slurm.Node_Properties is
   type Fixed is delta 0.01 digits 6 range 0.0 .. 1000.0;
   type Load is new Fixed range 0.0 .. 1000.0;

   type Set_Of_Properties is private;
   function "<" (Left, Right : Set_Of_Properties) return Boolean;
   pragma Inline ("<");
   overriding function "=" (Left, Right : Set_Of_Properties) return Boolean;
   pragma Inline ("<");

   function Get_CPUs (Item : Set_Of_Properties) return Natural;
   function Get_Features (From : Set_Of_Properties) return String;
   function Has_IB (Source : Set_Of_Properties) return Boolean;
   function Get_Memory (From : Set_Of_Properties) return Gigs;
   function Get_GRES (From : Set_Of_Properties) return Slurm.Gres.List;
   function Get_TRES (From : Set_Of_Properties) return Slurm.Tres.List;
   procedure Iterate_GRES (Item    : Set_Of_Properties;
                           Process : not null access procedure (R : Slurm.Gres.Resource));
   procedure Iterate_TRES (Item    : Set_Of_Properties;
                           Process : not null access procedure (R : Slurm.Tres.Resource));

   procedure Init_GRES (Item : in out Set_Of_Properties; Source : Slurm.Gres.List);
   procedure Init_TRES (Item : in out Set_Of_Properties; Source : Slurm.Tres.List);
   procedure Init_Memory (Item : in out Set_Of_Properties; Source : Gigs);
   procedure Init_Features (Item : in out Set_Of_Properties; Source : String);
   procedure Init_CPUs (Item : in out Set_Of_Properties; Source : Natural);

private

   type Set_Of_Properties is record
      GRES     : Slurm.Gres.List;
      TRES     : Slurm.Tres.List;
      Memory   : Gigs;
      CPUs     : Positive;
      Features : Unbounded_String;
      Has_IB   : Boolean;
   end record;

end Slurm.Node_Properties;
