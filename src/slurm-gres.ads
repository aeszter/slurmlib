with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

package Slurm.Gres is
   type Resource is record
      Category, Name : Unbounded_String;
      Number          : Positive;
   end record;

   package Lists is new ada.containers.doubly_linked_lists (Resource);
   subtype List is Lists.List;

   function Init (Source : String) return List;
   function New_Resource (Source : String) return Resource;

private

end Slurm.Gres;
