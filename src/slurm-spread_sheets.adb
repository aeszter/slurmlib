with Ada.Characters.Latin_1; use Ada.Characters;

package body Slurm.Spread_Sheets is
   use Cell_Lists;

   function At_End (Sheet : Spread_Sheet) return Boolean is
   begin
      return Sheet.Position = Sheet.Cells.Last;
   end At_End;

   function At_Separator (Sheet : Spread_Sheet) return Boolean is
   begin
      return Element (Sheet.Position).Line_Separator;
   end At_Separator;

   function Current (Sheet : Spread_Sheet) return String is
   begin
      if Cell_Lists.Element (Sheet.Position).Line_Separator then
         raise Output_Error with "unexpected separator found";
      else
         return To_String (Cell_Lists.Element (Sheet.Position).Contents);
      end if;
   end Current;

   function New_Cell (Text : Unbounded_String) return Cell is
   begin
      return (Contents => Text,
             Line_Separator => False);
   end New_Cell;

   procedure Next (Sheet : in out Spread_Sheet) is
   begin
      Sheet.Position := Next (Sheet.Position);
   end Next;

   procedure Parse (Sheet   : out Spread_Sheet;
                    Input   : in out Plain_Pipe_Stream;
                    Field_Separator : Character;
                    Standard_Separator : Boolean)
   is
      Buffer : Unbounded_String;
      C      : Character;
      New_Cell_Pending : Boolean := False;
   begin
      while not Input.Eof loop
         Input.Next_Char (C);
         if Standard_Separator then
            case C is
               when  Latin_1.HT =>
                  Sheet.Cells.Append (New_Cell (Buffer));
                  Buffer := Null_Unbounded_String;
               when Latin_1.LF =>
                  if Buffer /= Null_Unbounded_String then
                     Sheet.Cells.Append (New_Cell (Buffer));
                     Buffer := Null_Unbounded_String;
                  end if;
                  Sheet.Cells.Append (Line_Separator);
               when Latin_1.Space =>
                  New_Cell_Pending := True;
               when others =>
                  if New_Cell_Pending then
                     Sheet.Cells.Append (New_Cell (Buffer));
                     Buffer := Null_Unbounded_String;
                     New_Cell_Pending := False;
                  end if;
                  Append (Buffer, C);
            end case;
         else
            if C = Field_Separator then
               Sheet.Cells.Append (New_Cell (Buffer));
               Buffer := Null_Unbounded_String;
            elsif C = Latin_1.LF then
               if Buffer /= Null_Unbounded_String then
                  Sheet.Cells.Append (New_Cell (Buffer));
                  Buffer := Null_Unbounded_String;
               end if;
               Sheet.Cells.Append (Line_Separator);
            else
               Append (Buffer, C);
            end if;
         end if;
      end loop;
   end Parse;

   procedure Rewind (Sheet : in out Spread_Sheet) is
   begin
      Sheet.Position := Sheet.Cells.First;
   end Rewind;

   -----------
   -- Parse --
   -----------

end Slurm.Spread_Sheets;
