with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Slurm.Gres is

   function "<" (Left, Right : Resource) return Boolean is
   begin
      if Left.Number < Right.Number then
         return True;
      elsif Left.Number > Right.Number then
         return False;
      elsif Left.Category < Right.Category then
         return True;
      elsif Left.Category > Right.Category then
         return False;
      else
         return Left.Name < Right.Name;
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
   end "<";

   function "=" (Left, Right : List) return Boolean is
   begin
      return Lists."=" (Left, Right);
   end "=";

   function ">" (Left, Right : Resource) return Boolean is
   begin
      return Right < Left;
   end ">";

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
      Second : Natural := Index (Source  => Source,
                                 Pattern => ":");
      Third  : Natural := Index (Source => Source,
                                 From   => Second + 1,
                                 Pattern => ":");
      Result : Resource;
   begin
      Result.Category := To_Unbounded_String (Source (Source'First .. Second - 1));
      Result.Name := To_Unbounded_String (Source (Second + 1 .. Third - 1));
      Result.Number := Integer'Value (Source (Third + 1 .. Source'Last));
      return Result;
   end New_Resource;

   function To_String (Item : Resource) return String is
   begin
      return Item.Number'Img & " "
        & To_String (Item.Category) & ":"
        & To_String (Item.Name);
   end To_String;

end Slurm.Gres;
