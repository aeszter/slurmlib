with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Slurm.Loggers is

   Global : Logger;

   function Errors_Exist return Boolean is
   begin
      return Global.Has_Errors;
   end Errors_Exist;

   function Has_Errors (Object : Logger) return Boolean is
   begin
      return not Object.Error_Log.Is_Empty;
   end Has_Errors;

   procedure Iterate_Errors (Object  : Logger;
                             Process : not null access procedure (Message : String)) is
      procedure Wrapper (Position : Utils.String_Lists.Cursor);

      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Utils.String_Lists.Element (Position)));
      end Wrapper;

   begin
      Object.Error_Log.Iterate (Wrapper'Access);
   end Iterate_Errors;

   procedure Iterate_Errors (Process : not null access procedure (Message : String)) is
   begin
      Global.Iterate_Errors (Process);
   end Iterate_Errors;

   procedure Record_Error (Object : in out Logger; Message : String) is
   begin
      Object.Error_Log.Append (To_Unbounded_String (Message));
   end Record_Error;

   procedure Record_Error (Message : String) is
   begin
      Global.Error_Log.Append (To_Unbounded_String (Message));
   end Record_Error;

end Slurm.Loggers;
