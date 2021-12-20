with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Slurm.Hostlists is
   type hostlist_opaque is null record;
   type hostlist_t is access hostlist_opaque;

   function slurm_hostlist_create (hl : chars_ptr) return hostlist_t;
   pragma Import (C, slurm_hostlist_create, "slurm_hostlist_create");

   procedure slurm_hostlist_destroy (hl : hostlist_t);
   pragma Import (C, slurm_hostlist_destroy, "slurm_hostlist_destroy");

   function slurm_hostlist_shift (hl : hostlist_t) return chars_ptr;
   pragma Import (C, slurm_hostlist_shift, "slurm_hostlist_shift");

   overriding function "<" (Left, Right : Node_Name) return Boolean is
   begin
      return Ada.Strings.Unbounded."<" (Unbounded_String (Left), Unbounded_String (Right));
   end "<";

   overriding function "=" (Left, Right : Node_Name) return Boolean is
   begin
      return Ada.Strings.Unbounded."=" (Unbounded_String (Left), Unbounded_String (Right));
   end "=";

   function To_Hostlist (Source : String) return Hostlist is
      Result : Hostlist := Name_Sets.Empty_Set;
      Next_Host : chars_ptr;
      hl : hostlist_t := slurm_hostlist_create (New_String (Source));
   begin
      loop
         Next_Host := slurm_hostlist_shift (hl);
         exit when Next_Host = Null_Ptr;
         Result.Include (To_Node_Name (Value (Next_Host)));
         Free (Next_Host);
      end loop;
      slurm_hostlist_destroy (hl);
      return Result;
   end To_Hostlist;

   function To_Node_Name (Source : String) return Node_Name is
   begin
      return Node_Name (Ada.Strings.Unbounded.To_Unbounded_String (Source));
   end To_Node_Name;

   overriding function To_String (Source : Node_Name) return String is
   begin
      return To_String (Unbounded_String (Source));
   end To_String;

end Slurm.Hostlists;
