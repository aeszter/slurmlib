with Ada.Finalization;
with POSIX;
with POSIX.C; use POSIX.C;
with Slurm.C_Types; use Slurm.C_Types;
with Slurm.Errors;
with Interfaces.C;

package body Slurm.Jobs is

   function slurm_load_jobs (update_time       : time_t;
                             job_info_msg_pptr : job_info_msg_ptr_ptr;
                             show_flags          : uint16_t) return int;
   pragma import (C, slurm_load_jobs, "slurm_load_jobs");

   procedure slurm_free_job_info_msg (job_info_msg_p : job_info_msg_ptr);
   pragma import (C, slurm_free_job_info_msg, "slurm_free_job_info_msg");

   procedure Load_Jobs (Destination : in out List) is
      use Slurm.Errors;
      E : Error;
   begin
      if slurm_load_jobs (update_time       => 0,
                          job_info_msg_pptr => Destination.List_Ptr,
                          show_flags        => 0) /= 0
      then
         E := Get_Last_Error;
         case E is
            when Protocol_Version_Error =>
               raise Internal_Error with "Incompatible protocol version";
            when Socket_Timeout =>
               raise Internal_Error with "Couldn't contact slurm controller";
            when others =>
               raise Constraint_Error with Get_Error (E);
         end case;
      end if;
   end Load_Jobs;

   overriding procedure Finalize (Object : in out List) is
   begin
      slurm_free_job_info_msg (Object.List_Ptr.all);
   end Finalize;

   function First (Collection : List) return Iterator is
   begin
      return Iterator'(Container => Collection.List_Ptr.all,
                       Position  => Collection.List_Ptr.all.all.job_array,
                       Last      => Collection.List_Ptr.all.all.job_array +
                       Interfaces.C.ptrdiff_t (Collection.List_Ptr.all.record_count));
   end First;

   function Last (Collection : List) return Iterator is
      use job_info_ptrs;
   begin
      return Iterator'(Container => Collection.List_Ptr.all,
                       Position  => Collection.List_Ptr.all.all.job_array + ptrdiff_t (Collection.List_Ptr.all.all.record_count),
                       Last => Collection.List_Ptr.all.all.job_array + ptrdiff_t (Collection.List_Ptr.all.all.record_count));
   end Last;

   function At_End (Position : Iterator) return Boolean is
      use job_info_ptrs;
   begin
      return Position.Position = Position.Last;
   end At_End;

   procedure Next (Position : in out Iterator) is
   begin
      job_info_ptrs.Increment (Position.Position);
   end Next;

   function Get_ID (Position : Iterator) return Positive is
   begin
      if Position.Position >=  Position.Last then
         raise Constraint_Error;
      end if;
      return Natural (Position.Position.all.job_id);
   end Get_ID;

end Slurm.Jobs;
