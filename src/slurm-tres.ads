with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Ordered_Sets;

package Slurm.Tres is
   type Resource is record
      Name   : Unbounded_String;
      Number : Positive;
   end record;

   function "<" (Left, Right : Resource) return Boolean;
   function ">" (Left, Right : Resource) return Boolean;

   package Lists is new ada.containers.Ordered_Sets (Resource);
   subtype List is Lists.Set;

   function Init (Source : String) return List;
   function New_Resource (Source : String) return Resource;
   function "<" (Left, Right : List) return Boolean;
   function ">" (Left, Right : List) return Boolean;

private

end Slurm.Tres;
