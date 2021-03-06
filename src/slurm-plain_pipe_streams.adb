with Ada.IO_Exceptions;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;

package body Slurm.Plain_Pipe_Streams is

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Plain_Pipe_Stream; Exit_Status : out Natural) is
      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status, Child => Input.PID);
      Exit_Status := Natural (Exit_Status_Of (Status));
      case Exit_Status_Of (Status) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Failed_Creation_Error;
         when Unhandled_Exception_Exit => raise Exception_Error;
         when others => return;
      end case;
   end Close;

   function Eof (From : Plain_Pipe_Stream) return Boolean is
   begin
      return From.Eof_Reached;
   end Eof;

   procedure Execute (P : in out Plain_Pipe_Stream;
                      Command : Trusted_Command_Name;
                      Arguments : Trusted_String_List)
   is
      To_QView : POSIX.IO.File_Descriptor;
      Template : Process_Template;
      POSIX_Arguments : POSIX_String_List;
      Position : Taint.Cursor;
   begin
      POSIX.IO.Create_Pipe (Read_End  => P.Pipe,
                            Write_End => To_QView);
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => P.Pipe);
      Set_File_Action_To_Duplicate (Template  => Template,
                                    File      => Standard_Output,
                                    From_File => To_QView);
      POSIX.Append (POSIX_Arguments, To_POSIX_String (Value (Command)));

      Position := Arguments.First;
      while Has_Element (Position) loop
         POSIX.Append (POSIX_Arguments, To_POSIX_String (Value (Element (Position))));
         Next (Position);
      end loop;
      Start_Process (Child    => P.PID,
                     Pathname => To_POSIX_String (Value (Command)),
                     Template => Template,
                     Arg_List => POSIX_Arguments);
      Close (File => To_QView);
   end Execute;

   procedure Next_Char
     (From : in out Plain_Pipe_Stream;
      C    : out Character)
   is
   begin
      if From.Position >= Integer (From.Last_Read) then
         POSIX.IO.Read (File           => From.Pipe,
                        Buffer         => From.Buffer,
                        Last           => From.Last_Read);
         From.Position := 0;
      end if;
      From.Position := From.Position + 1;
      C := Standard.Character (From.Buffer (From.Position));
   exception
      when Ada.IO_Exceptions.End_Error =>
         From.Eof_Reached := True;
         C := Standard.Character (LF);
      when others =>
         raise;
   end Next_Char;

end Slurm.Plain_Pipe_Streams;
