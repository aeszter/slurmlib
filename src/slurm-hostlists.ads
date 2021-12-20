with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Ordered_Sets;

package Slurm.Hostlists is

   --  slurm_hostlist_create - Translate a Slurm node name expression into
   --    a record used for parsing. Use slurm_hostlist_destroy to free the
   --    allocated storage.
   --  slurm_hostlist_shift - Get the next node name.
   --  slurm_hostlist_destroy - Release storage allocated by slurm_hostlist_create.

   type Node_Name is new Unbounded_String;
   Null_Node_Name : Node_Name := Node_Name (Null_Unbounded_String);

   overriding function "<" (Left, Right : Node_Name) return Boolean;
   pragma Inline ("<");
   overriding function "=" (Left, Right : Node_Name) return Boolean;
   pragma Inline ("=");

   overriding function To_String (Source : Node_Name) return String;
   function To_Node_Name (Source : String) return Node_Name;

   package Name_Sets is
     new Ada.Containers.Ordered_Sets (Element_Type => Node_Name);
   subtype Hostlist is Name_Sets.Set;

   function To_Hostlist (Source : String) return Hostlist;

private

end Slurm.Hostlists;
