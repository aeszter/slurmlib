with Ada.Exceptions; use Ada.Exceptions;
with Slurm.Plain_Pipe_Streams; use Slurm.Plain_Pipe_Streams;

package body Slurm.Parser is

   procedure Setup (Command  : Trusted_Command_Name;
                    Selector : Trusted_String;
                    Output : out Spread_Sheets.Spread_Sheet;
                    Exit_Status : out Natural) is
      The_Pipe : Plain_Pipe_Stream;
      Table  : Spread_Sheets.Spread_Sheet;
   begin
      The_Pipe.Execute (Command     => Trust_As_Command (Std_Path & Value (Command)),
                        Arguments   => Selector);
      Table.Parse (The_Pipe);
      The_Pipe.Close (Exit_Status);
      Output := Table;
   exception
      when Plain_Pipe_Streams.Failed_Creation_Error =>
         raise Parser_Error with "Failed to spawn """ & Value (Command)
           & """ with """ & Value (Selector) & """";
      when Plain_Pipe_Streams.Exception_Error =>
         raise Parser_Error with """" & Value (Command)
           & """ terminated because of an unhandled exception";
      when E : others => raise Parser_Error with "Error when calling "
           & Value (Command) & " with " & Value (Selector) & ": "
           & Exception_Message (E);
   end Setup;

end Slurm.Parser;
