with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Slurm.Gres is

   function Init (Source : String) return List is
      Result : List := Lists.Empty_List;
      First  : Natural := Source'First;
      Comma : Natural;
   begin
      while First < Source'Last loop
         Comma := Index (Source  => Source,
                         From => First,
                         Pattern => ",");
         if Comma = 0 then
            Comma := Source'Last + 1;
         end if;
         Result.Append (New_Resource (Source (First .. Comma - 1)));
         First := Comma + 1;
      end loop;
      return Result;
   end Init;

   function New_Resource (Source : String) return Resource is
      Second : Natural := Index (Source  => Source,
                                 Pattern => ":");
      Third  : Natural := Index (Source => Source,
                                 From   => Second+1,
                                 Pattern => ":");
      Result : Resource;
   begin
      Result.Category := To_Unbounded_String (Source (Source'First .. Second - 1));
      Result.Name := To_Unbounded_String (Source (Second + 1 .. Third - 1));
      Result.Number := Integer'Value (Source (Third + 1 .. Source'Last));
      return Result;
   end New_Resource;

end Slurm.Gres;
