with Interfaces.C.Strings; use Interfaces.C.Strings;
package body Slurm is
   Is_Inited : Boolean := False;

   procedure slurm_init (conf : chars_ptr);
   pragma Import (C, slurm_init, "slurm_init");

   procedure slurm_fini;
   pragma Import (C, slurm_fini, "slurm_fini");

   procedure Finish is
   begin
      if not Is_Inited then
         return;
      end if;
      slurm_fini;
      Is_Inited := False;
   end Finish;

   procedure Init is
   begin
      if Is_Inited then
         return;
      end if;
      slurm_init (Null_Ptr);
      Is_Inited := True;
   end Init;

begin
   Init;
end Slurm;
