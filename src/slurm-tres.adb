with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Slurm.Tres is

   function "<" (Left, Right : Resource) return Boolean is
   begin
      if Left.Name < Right.Name then
         return True;
      elsif Left.Name > Right.Name then
         return False;
      else
         return Left.Number < Right.Number;
      end if;
   end "<";

   function "<" (Left, Right : List) return Boolean is
      use Ada.Containers;
      use Lists;

      Left_Position, Right_Position : Lists.Cursor;
   begin
      if Left.Length < Right.Length then
         return True;
      elsif Left.Length > Right.Length then
         return False;
      else -- they have the same number of elements
         Left_Position := Left.First;
         Right_Position := Right.First;
         while Has_Element (Left_Position) loop
            if Element (Left_Position) < Element (Right_Position) then
               return True;
            elsif Element (Left_Position) > Element (Right_Position) then
               return False;
            end if;
            -- they're the same, so look for the next element in the list
            Next (Left_Position);
            Next (Right_Position);
         end loop;
         return False;
      end if;
   end"<";

   function ">" (Left, Right : Resource) return Boolean is
   begin
      return Right < Left;
   end">";

   function ">" (Left, Right : List) return Boolean is
   begin
      return Right < Left;
   end ">";

   function Init (Source : String) return List is
      Result : List := Lists.Empty_Set;
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
         Result.Include (New_Resource (Source (First .. Comma - 1)));
         First := Comma + 1;
      end loop;
      return Result;
   end Init;

   function New_Resource (Source : String) return Resource is
      Equal : Natural := Index (Source  => Source,
                                 Pattern => "=");
      Unit  : Natural := Index (Source => Source,
                                 From   => Equal + 1,
                                 Pattern => "M");
      Result : Resource;
   begin
      if Unit = 0 then
         Unit := Source'Last + 1;
      end if;
         Result.Name := To_Unbounded_String (Source (Source'First .. Equal - 1));
         Result.Number := Integer'Value (Source (Equal + 1 .. Unit - 1));
         return Result;
   end New_Resource;

end Slurm.Tres;
