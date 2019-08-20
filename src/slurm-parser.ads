with POSIX.Process_Environment;
with Slurm.Spread_Sheets;
with Slurm.Taint; use Slurm.Taint;
with Ada.Characters.Latin_1;

package Slurm.Parser is
   package Env renames POSIX.Process_Environment;

   Parser_Error : exception;

   procedure Setup (Command   : Trusted_Command_Name;
                    Arguments : Trusted_String_List;
                    Output    : out Spread_Sheets.Spread_Sheet;
                    Exit_Status : out Natural;
                    Field_Separator : Character := Ada.Characters.Latin_1.HT;
                    Standard_Separator : Boolean := True);

private
   Std_Path : String := "/usr/bin/";

end Slurm.Parser;
